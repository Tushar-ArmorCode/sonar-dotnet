[CmdletBinding(PositionalBinding = $false)]
param
(
    [Parameter(HelpMessage = "Version of MS Build: 14.0, 15.0, 16.0 or Current")]
    [ValidateSet("14.0", "15.0", "16.0", "Current")]
    [string]
    $msbuildVersion = "Current",

    [Parameter(HelpMessage = "The key of the rule to test, e.g. S1234. If omitted, all rules will be tested.")]
    [ValidatePattern("^S[0-9]+")]
    [string]
    $ruleId,

    [Parameter(HelpMessage = "The name of single project to build. If ommited, all projects will be build.")]
    [ValidateSet("AnalyzeGenerated.CS", "AnalyzeGenerated.VB", "akka.net", "AutoMapper", "BlazorSample", "Ember-MM", "Nancy", "NetCore31", "Net5", "Net6", "Net7", "Net8", "NetCore31WithConfigurableRules" , "ManuallyAddedNoncompliantIssues.CS", "ManuallyAddedNoncompliantIssues.VB", "RazorSample", "Roslyn.1.3.1", "SkipGenerated.CS", "SkipGenerated.VB", "SonarLintExclusions", "WebConfig")]
    [string]
    $project
)

Set-StrictMode -version 2.0
$ErrorActionPreference = "Stop"
$msBuildVersions = @("14.0", "15.0", "16.0", "Current")

. .\create-issue-reports.ps1

$InternalProjects = @("ManuallyAddedNoncompliantIssues.CS", "ManuallyAddedNoncompliantIssues.VB")

if ($PSBoundParameters['Verbose'] -Or $PSBoundParameters['Debug']) {
    $global:DebugPreference = "Continue"
}

function Prepare-Project([string]$ProjectName){
    $Output = ".\output\$ProjectName"
    New-Item -ItemType directory -Path $Output | out-null

    $SourcePath = ".\config\$ProjectName\SonarLint.xml"
    if(-Not (Test-Path $SourcePath)){
        $SourcePath = ".\config\SonarLint.xml"
    }
    $Content = Get-Content -Path $SourcePath -Raw

    if($ruleId){
        $RuleFragment = "    <Rule><Key>$ruleId</Key></Rule>"
    } else {
        $HotspotFiles = Get-ChildItem ..\rspec -Filter *.json -Recurse | Select-String "SECURITY_HOTSPOT" | Select-Object -ExpandProperty FileName
        $HotspotIDs = $HotspotFiles -Replace ".json", "" | Select-Object -Unique
        $RuleFragment = ""
        foreach($HotspotID in $HotspotIDs){
            $RuleFragment = $RuleFragment + "    <Rule><Key>$HotspotID</Key></Rule>`n"
        }
    }

    $Content = $Content -Replace "<Rules>", "<Rules>`n$RuleFragment"
    Set-Content -Path "$Output\SonarLint.xml" -Value $Content

    Write-Host "Using $Output\SonarLint.xml"
}

function Build-Project-MSBuild([string]$ProjectName, [string]$SolutionRelativePath, [int]$CpuCount = 4) {
    if ($project -And -Not ($ProjectName -eq $project)) {
        Write-Host "Build skipped: $ProjectName"
        return
    }

    Prepare-Project($ProjectName)

    $solutionPath = Resolve-Path ".\sources\${ProjectName}\${SolutionRelativePath}"

    # The PROJECT env variable is used by 'Directory.Build.targets'
    Write-Debug "Setting PROJECT environment variable to '${ProjectName}'"
    $Env:PROJECT = $ProjectName

    Restore-Packages $msbuildVersion $solutionPath
    # Note: Summary doesn't work for MSBuild 14
    Invoke-MSBuild $msbuildVersion $solutionPath `
        /m:$CpuCount `
        /t:rebuild `
        /p:Configuration=Debug `
        /clp:"Summary;ErrorsOnly"
}

function Build-Project-DotnetTool([string]$ProjectName, [string]$SolutionRelativePath) {
    if ($project -And -Not ($ProjectName -eq $project)) {
        Write-Host "Build skipped: $ProjectName"
        return
    }

    $projectGlobalJsonPath = ".\sources\${ProjectName}\global.json"
    $globalJsonContent = $(Get-Content $projectGlobalJsonPath)
    Write-Host "Will build dotnet project: '${ProjectName}' (with ${projectGlobalJsonPath}) with dotnet version '${globalJsonContent}'."

    Prepare-Project($ProjectName)

    $solutionPath = Resolve-Path ".\sources\${ProjectName}\${SolutionRelativePath}"

    # The PROJECT env variable is used by 'Directory.Build.targets'
    Write-Debug "Setting PROJECT environment variable to '${ProjectName}'"
    $Env:PROJECT = $ProjectName

    # Copy the global.json in the folder where we do analysis.
    $tempGlobalJsonPath = ".\global.json"
    Copy-Item $projectGlobalJsonPath $tempGlobalJsonPath

    dotnet --version
    dotnet restore --locked-mode $solutionPath

    # To change the verbosity, comment out the '-clp' parameter and add the '-v' parameter.
    Exec { & dotnet build $solutionPath `
        --no-restore `
        -t:rebuild `
        -p:Configuration=Debug `
        -clp:"Summary;ErrorsOnly" `
        -fl `
        -flp:"logFile=output\${ProjectName}.log;verbosity=d" `
    } -errorMessage "ERROR: Build FAILED."

    Remove-Item $tempGlobalJsonPath
}

function Initialize-ActualFolder() {
    $methodTimer = [system.diagnostics.stopwatch]::StartNew()

    Write-Host "Initializing the actual issues folder with the expected result"
    if (Test-Path .\actual) {
        Write-Host "Removing existing folder 'actual'"
        Remove-Item -Recurse -Force actual
    }

    # this copies no files if ruleId is not set, and all but ending with ruleId if set
    Copy-FolderRecursively -From .\expected -To .\actual -Exclude "*${ruleId}.json"
    $methodTimerElapsed = $methodTimer.Elapsed.TotalSeconds
    Write-Debug "Initialized actual folder in '${methodTimerElapsed}'"
}

function Initialize-OutputFolder() {
    $methodTimer = [system.diagnostics.stopwatch]::StartNew()

    Write-Host "Initializing the output folder"
    if (Test-Path .\output) {
        Write-Host "Removing existing folder 'output'"
        Remove-Item -Recurse -Force output
    }

    Write-Debug "Creating folder 'output'"
    New-Item -ItemType directory -Path .\output | out-null

    if ($ruleId) {
        Write-Host "Running ITs with only rule ${ruleId} turned on."
        $template = Get-Content -Path ".\SingleRule.ruleset.template" -Raw
        $rulesetWithOneRule = $template.Replace("<Rule Id=`"$ruleId`" Action=`"None`" />", `
                                                "<Rule Id=`"$ruleId`" Action=`"Warning`" />")
        Set-Content -Path ".\output\AllSonarAnalyzerRules.ruleset" -Value $rulesetWithOneRule
    }
    else {
        Write-Host "Running ITs with all rules turned on."
        Copy-Item ".\AllSonarAnalyzerRules.ruleset" -Destination ".\output"
    }

    Write-Host "The rule set we use is .\output\AllSonarAnalyzerRules.ruleset."

    $methodTimerElapsed = $methodTimer.Elapsed.TotalSeconds
    Write-Debug "Initialized output folder in '${methodTimerElapsed}'"
}

function Get-FullPath($Folder) {
    return [System.IO.Path]::GetFullPath((Join-Path (Get-Location).Path $Folder))
}

function Copy-FolderRecursively($From, $To, $Include, $Exclude) {
    $fromPath = Get-FullPath -Folder $From
    $toPath   = Get-FullPath -Folder $To

    $files = if ($Include) {
        Get-ChildItem -Path $fromPath -Recurse -Include $Include
    } else {
        Get-ChildItem -Path $fromPath -Recurse -Exclude $Exclude
    }

    foreach ($file in $files) {
        $path = Join-Path $toPath $file.FullName.Substring($fromPath.length)
        $parent = split-path $path -parent

        if (-Not (Test-Path $parent)) {
            New-Item $parent -Type Directory -Force | out-null
        }
        Copy-Item $file.FullName -Destination $path
    }
}

function Show-DiffResults() {
    if (Test-Path .\diff) {
        Write-Host "Removing existing folder 'diff'"
        Remove-Item -Recurse -Force .\diff
    }

    $errorMsg = "ERROR: There are differences between the actual and the expected issues."


    if (!$ruleId -And !$project)
    {
        Write-Host "Will find differences for all projects, all rules."

        Write-Debug "Running 'git diff' between 'actual' and 'expected'."

        Exec { & git diff --no-index --exit-code ./expected ./actual `
        } -errorMessage $errorMsg

        return
    }

    # do a partial diff
    New-Item ".\diff" -Type Directory | out-null
    New-Item ".\diff\actual" -Type Directory | out-null
    New-Item ".\diff\expected" -Type Directory | out-null

    if (!$ruleId -And $project) {
        Write-Host "Will find differences for '${project}', all rules."

        Copy-FolderRecursively -From ".\expected\${project}" -To .\diff\expected
        Copy-FolderRecursively -From ".\actual\${project}"   -To .\diff\actual

    } elseif ($ruleId -And !$project) {
        Write-Host "Will find differences for all projects, rule ${ruleId}."

        Copy-FolderRecursively -From .\expected -To .\diff\expected -Include "*${ruleId}.json"
        Copy-FolderRecursively -From .\actual   -To .\diff\actual   -Include "*${ruleId}.json"

    } else {
        Write-Host "Will find differences for '${project}', rule ${ruleId}."

        Copy-FolderRecursively -From ".\expected\${project}" -To .\diff\expected -Include "*${ruleId}.json"
        Copy-FolderRecursively -From ".\actual\${project}"   -To .\diff\actual   -Include "*${ruleId}.json"
    }

    Exec { & git diff --no-index --exit-code .\diff\expected .\diff\actual `
    } -errorMessage $errorMsg
}

function CreateIssue($fileName, $lineNumber, $issueId, $message){
    $uri = Create-FullUriForIssue $fileName $lineNumber $lineNumber
    return New-Object PSObject -Property @{
        FileName = $uri
        LineNumber = $lineNumber
        IssueId = $issueId
        Message = $message
    }
}

function LoadExpectedIssues($file, $regex){
    # Unfortunately regex named groups don't work.
    # In the current context:
    # - $_.Matches.Groups[3].Value is IssueId
    # - $_.Matches.Groups[4].Value is Message
    $issues = $file | Select-String -Pattern $regex | ForEach-Object { CreateIssue $_.Path $_.LineNumber $_.Matches.Groups[3].Value $_.Matches.Groups[4].Value }

    if ($issues -eq $null){
        return @()
    }

    $id = $issues | where { $_.IssueId -ne "" } | select -ExpandProperty IssueId | unique

    if ($id -eq $null){
        throw "Please specify the rule id in the following file: $($file.FullName)"
    }

    if ($id -is [system.array]){
        throw "Only one rule can be verified per file. Multiple rule identifiers are defined ($id) in $($file.FullName)"
    }

    foreach($issue in $issues){
        $issue.IssueId = $id
    }

    return $issues
}

function LoadExpectedIssuesByProjectType($project, $regex, $extension){
    $issues = @()

    foreach($file in Get-ChildItem sources/$project -filter $extension -recurse){
        $fileIssues = LoadExpectedIssues $file $regex
        $issues = $issues + $fileIssues
    }

    return ,$issues # "," to avoid reducing empty array to $null
}

function LoadExpectedIssuesForInternalProject($project){
    $csRegex = "\/\/\s*Noncompliant(\s*\((?<ID>S\d+)\))?(\s*\{\{(?<Message>.+)\}\})?"
    $vbRegex = "'\s*Noncompliant(\s*\((?<ID>S\d+)\))?(\s*\{\{(?<Message>.+)\}\})?"

    return (LoadExpectedIssuesByProjectType $project $csRegex "*.cs") +
           (LoadExpectedIssuesByProjectType $project $vbRegex "*.vb")
}

function IssuesAreEqual($actual, $expected){
    return ($expected.issueId -eq $actual.issueId) -and
           ($expected.lineNumber -eq $actual.lineNumber) -and
           # The file name extracted from roslyn report ($actual) is relative but the one from expected issue is absolute.
           ($expected.fileName.endswith($actual.fileName) -and
           ($expected.message -eq "" -or $expected.message -eq $actual.message))
}

# checks if both paths point to the same file
# e.g.
# path1: a/b/Program.cs#12
# path2: b/Program.cs#34
# Result: true
function IsSameFile([string]$path1, [string]$path2) {
    $path1WithoutLineNumbers = $path1 -replace "#L\d.*", ""
    $path2WithoutLineNumbers = $path2 -replace "#L\d.*", ""
    return $path1WithoutLineNumbers.EndsWith($path2WithoutLineNumbers)
}

function VerifyUnexpectedIssues($actualIssues, $expectedIssues){
    $unexpectedIssues = @()

    foreach ($actualIssue in $actualIssues | Where-Object { $ruleId -eq "" -or $_.IssueId -eq $ruleId }){
        $found = $false

        foreach($expectedIssue in $expectedIssues){
            if (IssuesAreEqual $actualIssue $expectedIssue){
                $found = $true
                break
            }
        }

        if ($found -eq $false) {
            # There might be the case when different rules fire for the same class. Since we want to reduce the noise and narrow the focus,
            # we can have only one rule verified per class (this is done by checking the specified id in the first Noncompliant message).
            $expectedIssueInFile = $expectedIssues | where { IsSameFile($_.FileName, $actualIssue.FileName) } | unique

            # There are three cases to cover:
            # - the issue was raised for a file that has a Noncompliant comment with that issue id
            # - the issue was raised for a file that doesn't have a Noncompliant comment with an issue id
            # - the issue was raised for a file that has a Noncompliant comment with a different issue id
            # In the first two cases the unexpected issue needs to be reported but in the last one we should ignore it.
            if ($expectedIssueInFile -eq $null -or $expectedIssueInFile.issueId -eq $actualIssue.issueId){
                $unexpectedIssues = $unexpectedIssues + $actualIssue
            }
        }
    }

    if ($unexpectedIssues.Count -ne 0){
        Write-Warning "Unexpected issues:"
        Write-Host ($unexpectedIssues | Format-Table | Out-String)
    }

    return $unexpectedIssues
}

function VerifyExpectedIssues ($actualIssues, $expectedIssues){
    $expectedButNotRaisedIssues = @()

    foreach ($expectedIssue in $expectedIssues | Where-Object { $ruleId -eq "" -or $_.IssueId -eq $ruleId }){
        $found = $false
        foreach($actualIssue in $actualIssues){
            if (IssuesAreEqual $actualIssue $expectedIssue){
                $found = $true
                break
            }
        }

        if ($found -eq $false) {
            $expectedButNotRaisedIssues = $expectedButNotRaisedIssues + $expectedIssue
        }
    }

    if ($expectedButNotRaisedIssues.Count -ne 0){
        Write-Warning "Issues not raised:"
        Write-Host ($expectedButNotRaisedIssues | Format-Table | Out-String)
    }

    return $expectedButNotRaisedIssues
}

function CompareIssues($actualIssues, $expectedIssues){
    $unexpectedIssues = VerifyUnexpectedIssues $actualIssues $expectedIssues
    $expectedButNotRaisedIssues = VerifyExpectedIssues $actualIssues $expectedIssues

    return $unexpectedIssues.Count -eq 0 -and $expectedButNotRaisedIssues.Count -eq 0
}

function LoadActualIssues($project){
    $analysisResults = Get-ChildItem output/$project -filter *.json -recurse
    $issues = @()

    foreach($fileName in $analysisResults){
        $issues += GetActualIssues($fileName.FullName) | Foreach-Object {
            $location = $_.location

            # location can be an array if the "relatedLocations" node is populated (since these are appended by "Get-IssueV3" function).
            # Since we only care about the real location we consider only the first element of the array.
            if ($location -is [system.array]){
                $location = $location[0]
            }

            CreateIssue $location.uri $location.region.startLine $_.id $_.message
        }
    }

    return $issues
}

function CheckDiffsForInternalProject($project){
    $actualIssues = LoadActualIssues $project
    $expectedIssues = LoadExpectedIssuesForInternalProject $project
    $result = CompareIssues $actualIssues $expectedIssues

    if ($result -eq $false){
       throw "There are differences between actual and expected issues for $project!"
    }
}

function CheckInternalProjectsDifferences(){
    Write-Host "Check differences for internal projects"
    $internalProjTimer = [system.diagnostics.stopwatch]::StartNew()

    foreach ($currentProject in $InternalProjects){
        # we need to verify only the specified project if the "-project" parameter has a value
        if ($project -eq "" -or $currentProject -eq $project){
            CheckDiffsForInternalProject $currentProject
        }
    }

    $internalProjTimerElapsed = $internalProjTimer.Elapsed.TotalSeconds
    Write-Debug "Internal project differences verified in '${internalProjTimerElapsed}'"
}

try {
    $scriptTimer = [system.diagnostics.stopwatch]::StartNew()
    . (Join-Path $PSScriptRoot "..\..\scripts\build\build-utils.ps1")
    Push-Location $PSScriptRoot
    Test-FileExists "..\packaging\binaries\SonarAnalyzer.dll"
    Test-FileExists "..\packaging\binaries\SonarAnalyzer.CFG.dll"

    Write-Header "Initializing the environment"
    Initialize-ActualFolder
    Initialize-OutputFolder

    # Note: Automapper has multiple configurations that are built simultaneously and sometimes
    # it happens that a the same project is built in parallel in different configurations. The
    # protobuf-generating rules try to write their output in the same folder and fail, even
    # though there is a basic lock, because it is process-wide and not machine-wide.
    # Parallel builds are not a problem when run through the SonarScanner for .NET because it
    # redirects the outputs of the different configurations in separate folders.

    # Do not forget to update ValidateSet of -project parameter when new project is added.
    Build-Project-MSBuild "ManuallyAddedNoncompliantIssues.CS" "ManuallyAddedNoncompliantIssues.CS.sln"
    Build-Project-MSBuild "ManuallyAddedNoncompliantIssues.VB" "ManuallyAddedNoncompliantIssues.VB.sln"
    CheckInternalProjectsDifferences

    Build-Project-MSBuild "AnalyzeGenerated.CS" "AnalyzeGenerated.CS.sln"
    Build-Project-MSBuild "AnalyzeGenerated.VB" "AnalyzeGenerated.VB.sln"
    Build-Project-MSBuild "Ember-MM" "Ember Media Manager.sln"
    Build-Project-MSBuild "Nancy" "Nancy.sln"
    Build-Project-MSBuild "Roslyn.1.3.1" "Roslyn.1.3.1.sln"
    Build-Project-MSBuild "SkipGenerated.CS" "SkipGenerated.CS.sln"
    Build-Project-MSBuild "SkipGenerated.VB" "SkipGenerated.VB.sln"
    Build-Project-MSBuild "WebConfig" "WebConfig.sln"

    Build-Project-DotnetTool "NetCore31" "NetCore31.sln"
    Build-Project-DotnetTool "Net5" "Net5.sln"
    Build-Project-DotnetTool "Net6" "Net6.sln"
    Build-Project-DotnetTool "Net7" "Net7.sln"
    Build-Project-DotnetTool "Net8" "Net8.sln"
    Build-Project-DotnetTool "NetCore31WithConfigurableRules" "NetCore31WithConfigurableRules.sln"
    Build-Project-DotnetTool "akka.net" "src\Akka.sln"
    Build-Project-DotnetTool "AutoMapper" "AutoMapper.sln"
    Build-Project-DotnetTool "SonarLintExclusions" "SonarLintExclusions.sln"
    Build-Project-DotnetTool "RazorSample" "RazorSample.sln"
    Build-Project-DotnetTool "BlazorSample" "BlazorSample.sln"

    Write-Header "Processing analyzer results"

    # Not needed when $project is internal
    if ($project -eq "" -or -not $InternalProjects.Contains($project)) {
        Write-Host "Normalizing the SARIF reports"
        $sarifTimer = [system.diagnostics.stopwatch]::StartNew()

        # Normalize & overwrite all *.json SARIF files found under the "actual" folder
        Get-ChildItem output -filter *.json -recurse | where { $_.FullName -notmatch 'ManuallyAddedNoncompliantIssues' } | Foreach-Object { New-IssueReports $_.FullName }

        $sarifTimerElapsed = $sarifTimer.Elapsed.TotalSeconds
        Write-Debug "Normalized the SARIF reports in '${sarifTimerElapsed}'"

        Write-Host "Checking for differences..."
        $diffTimer = [system.diagnostics.stopwatch]::StartNew()
        Show-DiffResults
        $diffTimerElapsed = $diffTimer.Elapsed.TotalSeconds
        Write-Debug "Checked for differences in '${diffTimerElapsed}'"
    }

    Write-Host -ForegroundColor Green "SUCCESS: ITs were successful! No differences were found!"
    exit 0
}
catch {
    Write-Host -ForegroundColor Red $_
    Write-Host $_.Exception
    Write-Host $_.ScriptStackTrace
    exit 1
}
finally {
    Pop-Location

    Remove-Item -ErrorAction Ignore -Force global.json

    $scriptTimer.Stop()
    $totalTimeInSeconds = [int]$scriptTimer.Elapsed.TotalSeconds
    if ($ruleId) {
        Write-Debug "Analyzed ${ruleId} in ${totalTimeInSeconds}s"
    } else {
        Write-Debug "Analyzed all rules in ${totalTimeInSeconds}s"
    }
}
