﻿namespace Nancy.ViewEngines.Razor.BuildProviders
{
    using System;
    using System.CodeDom;
    using System.CodeDom.Compiler;
    using System.Globalization;
    using System.Web.Compilation;
    using System.Web.Razor;

    [BuildProviderAppliesTo(BuildProviderAppliesTo.Code | BuildProviderAppliesTo.Web)]
    public class NancyCSharpRazorBuildProvider : BuildProvider
    {
        private readonly RazorEngineHost host;

        private readonly CompilerType compilerType;

        private CodeCompileUnit generatedCode;

        /// <summary>
        /// Initializes a new instance of the <see cref="NancyCSharpRazorBuildProvider"/> class.
        /// </summary>
        public NancyCSharpRazorBuildProvider()
        {
            this.compilerType = this.GetDefaultCompilerTypeForLanguage("C#");

            this.host = new NancyRazorEngineHost(new CSharpRazorCodeLanguage());
        }

        /// <summary>
        /// Represents the compiler type used by a build provider to generate source code for a custom file type.
        /// </summary>
        /// <returns>A read-only <see cref="T:System.Web.Compilation.CompilerType"/> representing the code generator, code compiler, and compiler settings used to build source code for the virtual path. The base class returns null.</returns>
        public override CompilerType CodeCompilerType
        {
            get { return this.compilerType; }
        }

        /// <summary>
        /// Generates source code for the virtual path of the build provider, and adds the source code to a specified assembly builder.
        /// </summary>
        /// <param name="assemblyBuilder">The assembly builder that references the source code generated by the build provider.</param>
        public override void GenerateCode(AssemblyBuilder assemblyBuilder)
        {
            assemblyBuilder.AddCodeCompileUnit(this, this.GetGeneratedCode());

            assemblyBuilder.GenerateTypeFactory(string.Format(CultureInfo.InvariantCulture, "{0}.{1}", new object[] { this.host.DefaultNamespace, this.host.DefaultClassName }));
        }

        /// <summary>
        /// Returns a type generated by the build provider from the virtual path.
        /// </summary>
        /// <returns>The type that is generated by the build provider for the virtual path. The base class returns null.</returns>
        /// <param name="results">The compilation results for the build provider's virtual path.</param>
        public override Type GetGeneratedType(CompilerResults results)
        {
            return results.CompiledAssembly.GetType(string.Format(CultureInfo.CurrentCulture, "{0}.{1}", new object[] { this.host.DefaultNamespace, this.host.DefaultClassName }));
        }

        private CodeCompileUnit GetGeneratedCode()
        {
            if (this.generatedCode == null)
            {
                var engine = new RazorTemplateEngine(this.host);
                GeneratorResults results;
                using (var reader = this.OpenReader())
                {
                    results = engine.GenerateCode(reader);
                }

                if (!results.Success)
                {
                    throw new InvalidOperationException(results.ToString());
                }

                this.generatedCode = results.GeneratedCode;
            }

            return this.generatedCode;
        }
    }
}