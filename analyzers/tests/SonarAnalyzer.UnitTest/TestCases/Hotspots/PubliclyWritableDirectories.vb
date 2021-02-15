﻿Imports System
Imports System.IO

Namespace Tests.TestCases

    Public Class Program

        Public Sub Compliant()
            Dim RandomPath As String = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName()) ' Compliant
            Using TmpDir = New StreamReader("Path is /tmp/f") ' Compliant
            End Using
            Using TmpDir = New StreamReader("C:\\Windows\\Tempete") ' Compliant;
            End Using
            Dim url As String = "http://example.domain/tmp/f" ' Compliant
        End Sub

        Public Function CompliantWithArg(Optional Dir As String = "/other/tmp") As String ' Compliant
            Return Dir
        End Function

        Public Sub NonCompliant(PartOfPath As String)
            ' Environment
            Dim Tmp As String = Path.GetTempPath() ' Sensitive
            Tmp = Environment.GetEnvironmentVariable("TMPDIR") ' Noncompliant {{Make sure publicly writable directories are used safely here.}}
            '                                        ^^^^^^^^
            Tmp = Environment.GetEnvironmentVariable("TMP") ' Noncompliant
            Tmp = Environment.GetEnvironmentVariable("TEMP") ' Noncompliant
            Tmp = Environment.ExpandEnvironmentVariables("%TMPDIR%") ' Noncompliant
            Tmp = Environment.ExpandEnvironmentVariables("%TMP%") ' Noncompliant
            Tmp = Environment.ExpandEnvironmentVariables("%TEMP%") ' Noncompliant
            Tmp = "%USERPROFILE%\AppData\Local\Temp\f" ' Noncompliant
            Tmp = "%TEMP%\f" ' Noncompliant
            Tmp = "%TMP%\f" ' Noncompliant
            Tmp = "%TMPDIR%\f" ' Noncompliant

            ' Common
            Using TmpDir = New StreamReader("/tmp/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/tmp") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/var/tmp/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/usr/tmp/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/dev/shm/f") ' Noncompliant
            End Using

            ' Linux
            Using TmpDir = New StreamReader("/dev/mqueue/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/run/lock/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/var/run/lock/f") ' Noncompliant
            End Using

            ' MacOS
            Using TmpDir = New StreamReader("/Library/Caches/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/Users/Shared/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/private/tmp/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("/private/var/tmp/f") ' Noncompliant
            End Using

            ' Windows
            Using TmpDir = New StreamReader("C:\Windows\Temp\f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("C:\Temp\f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("C:\TEMP\f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("C:\TMP\f") ' Noncompliant
            End Using

            ' Variates
            Using TmpDir = New StreamReader("/tmp/f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("\\Windows\Temp\f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("C:\Windows\Temp\f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("D:\Windows\Temp\f") ' Noncompliant
            End Using
            Using TmpDir = New StreamReader("\Windows\Temp\f") ' Noncompliant
            End Using
            Tmp = "/tmp/f" ' Noncompliant
            Tmp = "/TeMp/f" ' Noncompliant

            ' Interpolated
            Tmp = $"/tmp/{PartOfPath}" ' Noncompliant
            '     ^^^^^^^^^^^^^^^^^^^^

        End Sub

        Public Function NonCompliantWithArg(Optional Dir As String = "/tmp") As String ' Noncompliant
            Return Dir
        End Function

    End Class

End Namespace
