// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

volatile bool g_continueDebug = false;

API BOOL Codegen(const char* backend, const char* output, const char* res, const char* dgml, const char* dgmlDetailed,
                 ParseTreeNodePtr root)
{
    // If this environment variable is set
    // then wait for a debugger to attach and set g_continueDebug = true
    if (getenv("PD_WAIT_FOR_DEBUG"))
    {
        std::cout << "PD_WAIT_FOR_DEBUG is set, waiting\n";

        while (!g_continueDebug)
        {
        }
    }

    bool runInternalTests = false;
    bool compileToVerilog = false;
    bool outputDgml = false;
    bool outputResourceUsage = false;
    bool outputDetailedDgml = false;

    std::string baseFileName(output);

    if (0 == _stricmp("sv", backend))
    {
        compileToVerilog = true;
    }

    if (!std::string(dgml).empty())
    {
        outputDgml = true;
    }

    if (!std::string(res).empty())
    {
        outputResourceUsage = true;
    }

    if (!std::string(dgmlDetailed).empty())
    {
        outputDetailedDgml = true;
    }

    try
    {
        Compiler& compiler = *g_compiler;

        std::unique_ptr<CompileTimer> outerCt = std::make_unique<CompileTimer>(compiler, "Middle-end (inclusive)");

        compiler.SetRoot(root, compileToVerilog);

        // Local function parameters which are modified
        compiler.FindModifiedParameters();

        // Type check
        {
            CompileTimer ct(compiler, "Type Check");
            compiler.TypeCheck();
        }

        // Verify no structs/unions/functions with duplicate names
        // This must happen after type checking
        compiler.CheckTypeNames();

        // Move declarations earlier on the parse tree
        // to enable the code beyond this point to not need to handle
        // variable references before declarations
        compiler.ReorderDeclarations();

        // Device configuration is contained in a special Kanagawa module.
        // Enumerate the definitions in this module and store the device configuration
        // away for later use.
        compiler.ExtractDeviceConfig();

        // Do any additional checks that require the device configuration, for example if
        // there are ECC memories, then the device must support ECC.
        compiler.DeviceCapabilityCheck();

        // Ensure static local variables have unique names within a particular function
        compiler.RenameStaticLocals();

        // Count the number of compilations
        const std::list<CompiledModule> compiledModules = compiler.EnumerateCompiledModules(baseFileName);

        // Create a single output file stream that will receive all symbols
        assert(!compiledModules.empty());
        const CompiledModule& defaultModule = compiledModules.front();
        assert(defaultModule._isDefaultPass);

        // Any ParseTreeNode created after this point
        // will be destroyed at the end of the corresponding CompiledModule
        compiler.MarkPermanentNodes();

        // Iterate over all compiled modules, generating back-end output
        for (const CompiledModule& moduleToCompile : compiledModules)
        {
            // Reset compiler data structures between compilations
            compiler.Reset(moduleToCompile);

            // Remove unnecessary portions of external classes
            compiler.ExternalizeClasses();

            // Register objects with associated class types
            // (not included static local objects)
            compiler.RegisterObjects(moduleToCompile);

            // Count the number of instances of each function
            {
                CompileTimer ct(compiler, "Enumerate function instances");
                compiler.EnumerateFunctionInstances(moduleToCompile);

                if (compiler.HadCompileError())
                {
                    throw std::runtime_error("Function instance enumeration failed");
                }
            }

            // Generate intermediate representation
            const Program* program = nullptr;

            {
                CompileTimer ct(compiler, "Lowering (inclusive)");
                program = compiler.GenerateIR(moduleToCompile);

                if (compiler.HadCompileError())
                {
                    throw std::runtime_error("IR generation failed");
                }
            }

            if (moduleToCompile._isDefaultPass)
            {
                // Skip output for default module
                continue;
            }

            CompileTimer ct(compiler, "Write Output");

            // Compressed reports use the 'pdrptx' extension, uncompressed ones use 'pdrpt'
            const std::string reportExtension = ".pdrpt";

            // Write C++ interop header
            const std::string cppFileName = moduleToCompile._baseFileName + ".cpp";
            const std::string headerFileName = moduleToCompile._baseFileName + ".h";
            const std::string svFileName = moduleToCompile._baseFileName + ".sv";
            const std::string svPackageFileName = moduleToCompile._baseFileName + "_types.sv";
            const std::string tclFileName = moduleToCompile._baseFileName + ".tcl";
            const std::string symbolFileName = moduleToCompile._baseFileName + "Symbols.csv";
            const std::string debugSymbolFileName = moduleToCompile._baseFileName + "DebugSymbols.csv";
            const std::string hwConfigMkFileName = moduleToCompile._baseFileName + "HwConfig.mk";
            const std::string clockGatingReportFileName =
                moduleToCompile._baseFileName + "ClockGating" + reportExtension;
            const std::string pathLengthReportFileName = moduleToCompile._baseFileName + "PathLength" + reportExtension;
            const std::string rtlMapFileName = moduleToCompile._baseFileName + "RtlMap.json";
            const std::string circtAsmFileName = moduleToCompile._baseFileName + ".mlir";

            BasicBlockDebugMaps blockMaps;
            if (GetCodeGenConfig()._debug)
            {
                // BasicBlockDebugMaps contains maps that are required for debug symbols:
                // 1. Given a Basic Block ID, returns the Locals map
                // 2. Given a Basic Block ID, returns a vector<bool> to determine which stages are empty
                // 3. Maps for the bypass memory optimization
                blockMaps = ComputeBasicBlockDebugMaps(*program);
            }

            if (compileToVerilog)
            {
                CompileVerilog(svFileName.c_str(), svPackageFileName.c_str(), cppFileName.c_str(),
                               headerFileName.c_str(), tclFileName.c_str(), hwConfigMkFileName.c_str(),
                               moduleToCompile._baseFileName.c_str(), rtlMapFileName.c_str(), circtAsmFileName.c_str(),
                               *program);
            }
            else
            {
                // This is not an error case (to allow the parsing tests to run)
                if (Verbosity() > Normal)
                {
                    std::cout << "No codegen backend specified\n";
                }
            }

            // Debug Symbols
            if (GetCodeGenConfig()._debug)
            {
                CompileTimer db(compiler, "DebugSymbols");

                const size_t debugSymbolsHash =
                    CreateDebugSymbols(debugSymbolFileName, *program, blockMaps, 0);
            }

            const std::string dmlFileName = moduleToCompile.ModifyFileName(dgml);
            const std::string resourceFileName = moduleToCompile.ModifyFileName(res);
            const std::string detailedDirectoryName = moduleToCompile.ModifyFileName(dgmlDetailed);

            CompileTimer rpt(compiler, "Report files");

            if (outputDgml)
            {
                WriteDgml(dmlFileName.c_str(), *program);
            }

            if (outputResourceUsage)
            {
                WriteResourceUsage(resourceFileName.c_str(), *program);

                auto pos = std::min(resourceFileName.rfind('.'), resourceFileName.size());
                const std::string rptFileName =
                    std::string(resourceFileName).replace(pos, std::string::npos, reportExtension);
                WriteResourceReport(rptFileName, *program);
            }

            if (outputDetailedDgml)
            {
                WriteDetailedDgml(detailedDirectoryName.c_str(), *program);
            }

            if (GetCodeGenConfig()._dumpMemoryMetadata)
            {
                // Whatever string is passed to CompileVerilog as a memFileBase argument above should be passed to
                // DumpMemoryMetadata as memoryMetadataFileNameBase
                DumpMemoryMetadata(moduleToCompile._baseFileName.c_str(), *program);
            }

            if (GetCodeGenConfig().ControlClockGatingEnabled())
            {
                WriteClockGatingReport(clockGatingReportFileName, *program);
            }

            if (GetCodeGenConfig().PathLengthReportEnabled())
            {
                WritePathLengthReport(pathLengthReportFileName, *program);
            }
        }

        // To capture full middle-end compilation time
        outerCt.reset();

#ifndef KANAGAWA_SKIP_CONSISTENCY_CHECKS
        std::cout << "Running internal consistency checks.\n";
#endif // KANAGAWA_SKIP_CONSISTENCY_CHECKS

        if (Verbosity() > Normal)
        {
            std::cout << "Compilation times:\n";
            compiler.PrintCompileTime(std::cout);
        }
    }
    catch (std::runtime_error& e)
    {
        std::cout << "Error: " << e.what() << "\n";
        std::cout << std::flush;
        return false;
    }

    // Explicit flushing is necessary because the process calling this function
    // is not implemented in C++ and will not flush cout before exiting.
    std::cout << std::flush;
    return true;
}
