{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Options.CmdArgs
    ( getOptions
    ) where

import Language.Kanagawa.Warning
import Options
import ReleaseVersion
import System.Console.CmdArgs

compile :: Options
compile = Compile
    { files                 = def                         &= typFile &= args
    , additional_latency    = 1                           &= groupname "Logic"           &= help "Extra latency added to ease routing" &= name "additional-latency" &= explicit
    , auto_pipeline_scale   = 0                           &= groupname "Logic"           &= help "The maximum number of extra pipeline registers to insert on function entry points. On platforms that support it, the actual pipeline depth is determined by the EDA tool." &= name "auto-pipeline-ratio" &= explicit
    , auto_pipeline_cross_region = 0                      &= groupname "Logic"           &= help "The maximum number of extra pipeline registers to insert on cross-region function entry points. On platforms that support it, the actual pipeline depth is determined by the EDA tool." &= name "auto-pipeline-cross-region" &= explicit
    , carry_chain_width     = 40                          &= groupname "Logic"           &= help "The number of output bits in a carry-chain operation (add/sub) that is considered to be 1 level of logic" &= name "carry-chain-width" &= explicit
    , carry_select          = def                         &= groupname "Logic"           &= help "Reduce pipeline length by implementing add/sub/compare operations with replicated, speculative computations"
    , rmw_memory_write_delay= 1                           &= groupname "Logic"           &= help "Additional delay to add to memory writes for read-modify-write memory operations"
    , rmw_memory_read_delay = 0                           &= groupname "Logic"           &= help "Additional delay to add to memory reads for read-modify-write memory operations"
    , rmw_hardened_write_delay = 1                        &= groupname "Logic"           &= help "Additional delay to add to memory writes for read-modify-write memory operations (when hardened forwarding logic is used)"
    , rmw_hardened_max_width = (maxBound::Int)            &= groupname "Logic"           &= help "Prevent hardened forwarding logic from being used when BRAM width is > rmw_hardened_max_width AND BRAM height is <= rmw_hardened_max_height"
    , rmw_hardened_max_height = 0                         &= groupname "Logic"           &= help "Prevent hardened forwarding logic from being used when BRAM width is > rmw_hardened_max_width AND BRAM height is <= rmw_hardened_max_height"
    , rmw_memory_favor_area = def                         &= groupname "Logic"           &= help "Optimize read-modify-write memory operations for smaller area. By default optimize for Fmax."
    , strict_memory_read_write = def                      &= groupname "Logic"           &= help "Require concurrent memory read-write operations to the same address to result in the read seeing the old data (at a potential area cost)"
    , io_register_delay     = 0                           &= groupname "Logic"           &= help "Delay to account for routing data to/from DSP/BRAM input/output registers (measured in levels of logic)"
    , mem_to_reg            = 0.0                         &= groupname "Logic"           &= help "Upper threshold to move data from registers to memory (measured in memory bits per register bit).  Higher values lead to more memory consumption."
    , fifo_to_dsp           = 0                           &= groupname "Logic"           &= help "Local data propagation FIFOs for a number of pipeline stages less than or equal to this threshold will be converted to DSPs"
    , fifo_write_delay      = 1                           &= groupname "Logic"           &= help "Number of cycles to add to fifo writes"
    , fifo_merge_distance   = 4                           &= groupname "Logic"           &= help "Maximum distance (pipeline stages) between merged fifo enqueue operations"
    , frequency             = 0                           &= groupname "Logic"           &= help "Clock 0 frequency" &= typ "MHz" &= name "f"
    , max_block_ram         = -1                          &= groupname "Logic"           &= help "Maximum number of Block RAMs generated"
    , block_ram_util_threshold = 20                       &= groupname "Logic"           &= help "Percent minimum Block RAM utilization"
    , max_deep_ram          = -1                          &= groupname "Logic"           &= help "Maximum number of Deep RAMs generated"
    , deep_ram_util_threshold  = 20                       &= groupname "Logic"           &= help "Percent minimum Deep RAM utilization"
    , max_mux_sources       = 16                          &= groupname "Logic"           &= help "Decompose multiplexers with more than this many dynamic inputs"
    , max_mul_src_width     = 32                          &= groupname "Logic"           &= help "Decompose multipliers with inputs wider than this"
    , max_threads_default   = 512                         &= groupname "Logic"           &= help "Default value for the maximum number of threads allowed in a function at one time"
    , max_threads_limit     = 512                         &= groupname "Logic"           &= help "Upper bound on the maximum number of threads allowed in a function at one time"
    , schedule_limit        = 4096                        &= groupname "Logic"           &= help "Maximum number of iterations during instruction scheduling"
    , register_ratio        = 2                           &= groupname "Logic"           &= help "Insert pipeline registers after every N levels of combinatorial logic"
    , max_register_ratio    = 0                           &= groupname "Logic"           &= help "Atomic blocks can relax scheduling to insert pipeline register after every N levels of combinatorial logic"
    , no_reset_fanout       = def                         &= groupname "Logic"           &= help "Disable generation of reset fanout and rely on CAD tools to perform fanout"
    , reset_cycles          = 3                           &= groupname "Logic"           &= help "Number of cycles to use to propagate the reset signal across the chip." &= name "reset-cycles" &= explicit
    , reset_fan_out_cycles  = 1                           &= groupname "Logic"           &= help "Number of cycles used to fan-out the reset signal"
    , reset_duration_cycles = 0                           &= groupname "Logic"           &= help "Number of extra cycles to hold the reset signal high"
    , return_stages         = 3                           &= groupname "Logic"           &= help "Number of extra pipeline stages to add for returns from functions with multiple call sites"
    , stallable_pipelines   = def                         &= groupname "Logic"           &= help "Generate pipelines where individual stages can be stalled (reduces FIFO depth)"
    , clock_gating          = 0                           &= groupname "Logic"           &= help "Level of clock gating to apply (0 to disable)"
    , clock_gating_threshold = 5                          &= groupname "Logic"           &= help "Instantiate a clock gate if it will apply to more than N register bits"
    , conditional_memory_reads = def                      &= groupname "Logic"           &= help "For memory reads that are conditional, try and make the memory read enable signal conditional as well. This can lower power consumption, but in some cases may negatively impact the maximum clock frequency"
    , semaphore_delay       = 3                           &= groupname "Logic"           &= help "Number of pipeline registers to add to the input signals that drive an internal semaphore module that is used to limit threads within a function"
    , sparse_reg_opt        = def                         &= groupname "Logic"           &= help "Eliminate unused or constant bits in registers."
    , jobs                  = def                         &= groupname "Compiler"        &= help "Number of parallel jobs to use during compilation. By default equal to number of processors. Negative value can be used to specify how many fewer jobs than number of processors to run."
    , import_dir            = def                         &= groupname "Compiler"        &= help "Directories to search for imported files" &= typDir &= explicit &= name "import-dir"
    , define                = def                         &= groupname "Compiler"        &= help "Define a global constant" &= name "define" &= name "d" &= explicit &= typ "name#val"
    , using                 = def                         &= groupname "Compiler"        &= help "Define a global type alias" &= name "using" &= name "u" &= explicit &= typ "name#type"
    , no_implicit_base      = def                         &= groupname "Compiler"        &= help "Disable implicit import of base library"
    , base_library          = "base.k"                    &= groupname "Compiler"        &= help "Base library file path" &= typFile
    , parse_docs            = def                         &= groupname "Compiler"        &= help "Parse documentation comments"
    , output                = def                         &= groupname "Output"          &= help "Base name of output file(s)" &= typ "STRING"
    , dgml                  = def                         &= groupname "Output"          &= help "Output design in Directed Graph Markup format" &= typFile
    , dgml_detailed         = def                         &= groupname "Output"          &= help "Output each basic block in Directed Graph Markup format" &= typDir
    , resource_usage        = def                         &= groupname "Output"          &= help "Output resource utilization statistics" &= typFile
    , file_list             = def                         &= groupname "Output"          &= help "Update specified file whenever list compiled files (including imported files) changes" &= typFile
    , identifier_length     = 100                         &= groupname "Output"          &= help "Maximum length of identifiers in generated code"
    , backend               = None                        &= groupname "Code generation" &= help "Code generation backend: sv (SystemVerilog) or not specified (produce metadata only)" &= name "target" &= name "backend"
    , debug                 = def                         &= groupname "Code generation" &= help "Debug mode"
    , dump_memory_metadata  = def                         &= groupname "Code generation" &= help "Output metadata files for memories and fifos"
    , strip_debug_symbols   = def                         &= groupname "Code generation" &= help "Generate debug symbols without full file path names"
    , force_mem_to_array    = 0                           &= groupname "Code generation" &= help "Regardless of cost, forces conversion of memories less than the specified size to arrays. For testing purposes only"
    , max_stack_depth       = 64                          &= groupname "Code generation" &= help "Maximum number of stack frames to inserted into the generated code"
    , no_fifo_stutter       = def                         &= groupname "Code generation" &= help "Disable fifos stutter"
    , no_debug_view         = def                         &= groupname "Code generation" &= help "Disables debug views"
    , no_verilog_header     = def                         &= groupname "Code generation" &= help "Don't output the informational header on top of generated Verilog files"
    , code_coverage         = def                         &= groupname "Code generation" &= help "Enable generation of code coverage using SystemVerilog coverpoints"
    , code_coverage_mux_threshold = 0                     &= groupname "Code generation" &= help "Only generate code coverage code for muxes with this number of cases; the default value of 0 means to generate for all muxes"
    , optimize              = 2                           &= groupname "Code generation" &= help "Optimize generated code" &= typ "LEVEL" &= name "O"
    , target_device         = "mock"                      &= groupname "Code generation" &= help "Target FPGA device name" &= typ "STRING"
    , stall                 = 0                           &= groupname "Code generation" &= help "Randomly force each basic block to stall (acceptable values [0,3]; default is 0 meaning no stalls)"
    , work_list_size        = 0                           &= groupname "Code generation" &= help "Optimization work list upper bound (log2)"
    , place_display         = def                         &= groupname "Placement"       &= help "Display animation of the placement process"
    , place_iterations      = 3000                        &= groupname "Placement"       &= help "The number of iterations to run placement for"
    , place_pull            = 100.0                       &= groupname "Placement"       &= help "Coefficient of linear force that pulls connected nodes together"
    , place_push            = 0.001                       &= groupname "Placement"       &= help "Coefficient of inverse square force that pushes nodes away from others"
    , place_seed            = 1                           &= groupname "Placement"       &= help "Random number seed"
    , place_update_rate     = 0.005                       &= groupname "Placement"       &= help "Maximum length of gradient vectors"
    , place_wall            = 0.001                       &= groupname "Placement"       &= help "Coefficient of inverse cube force that pushes nodes away from walls"
    , check_narrowing_assignment = def                    &= groupname "Diagnostics"     &= help "Assert that the input to a narrowing conversion can be represented by the result type"
    , dump_IR               = def                         &= groupname "Diagnostics"     &= help "Dump IR"
    , dump_opt              = def                         &= groupname "Diagnostics"     &= help "Dump optimizations"
    , dump_parse            = def                         &= groupname "Diagnostics"     &= help "Dump abstract syntax tree(s)"
    , dump_program          = def                         &= groupname "Diagnostics"     &= help "Dump desugared abstract syntax tree"
    , dump_types            = def                         &= groupname "Diagnostics"     &= help "Dump abstract syntax tree with type annotations"
    , dump_source           = def                         &= groupname "Diagnostics"     &= help "Dump desugared source code of the program"
    , passes                = (maxBound::Int)             &= groupname "Diagnostics"     &= help "Subset of frontend passes to run, default all" &= name "frontend-passes" &= explicit &= opt (maxBound::Int)
    , template_iterations   = (maxBound::Int)             &= groupname "Diagnostics"     &= help "Number of iterations of template instantiation to run, default all" &= name "template-iterations" &= explicit &= opt (maxBound::Int)
    , template_passes       = (maxBound::Int)             &= groupname "Diagnostics"     &= help "Subset of desugaring passes to run after the final template instantiation iteration specified via --template-iterations, default all" &= name "template-passes" &= explicit &= opt (maxBound::Int)
    , write_ir_post_opt     = def                         &= groupname "Diagnostics"     &= help "Serialize IR to json after optimizations"
    , write_circt_ir        = def                         &= groupname "Diagnostics"     &= help "Serialize to CIRCT IR before CIRCT passes"
    , no_asserts            = def                         &= groupname "Diagnostics"     &= help "Disable asserts"
    , warnings_as_errors    = def                         &= groupname "Warnings"        &= help "Treat warnings as errors" &= name "Werror" &= explicit
    , warnings_state        = enum
                            [  -- warnings enabled by default
                              [ Enable RedundantImport
                              , Enable Deprecated
                              , Enable InitializerOverrides
                              , Enable Shadow
                              ] &= ignore
                              -- warnings enabled by --Wall
                            , [ Enable Conversion
                              , Enable TransactionSizeMissing
                              ]                                  &= groupname "Warnings" &= help "Enable all warnings" &= name "Wall" &= explicit
                              -- options to enable individual warnings
                            , [ Enable Conversion ]              &= groupname "Warnings" &= help "Warn for implicit conversions that may alter a value" &= name (enableSwitch Conversion) &= explicit
                            , [ Enable Deprecated ]              &= groupname "Warnings" &= help "Warn for deprecated features" &= name (enableSwitch Deprecated) &= explicit
                            , [ Enable InitializerOverrides ]    &= groupname "Warnings" &= help "Warn for overriding designated initializers" &= name (enableSwitch InitializerOverrides) &= explicit
                            , [ Enable RedundantImport ]         &= groupname "Warnings" &= help "Warn for redundant imports" &= name (enableSwitch RedundantImport) &= explicit
                            , [ Enable Shadow ]                  &= groupname "Warnings" &= help "Warn for shadowing declarations" &= name (enableSwitch Shadow) &= explicit
                            , [ Enable TransactionSizeMissing ]  &= groupname "Warnings" &= help "Warn for missing [[transaction_size]] attribute at calls to functions with the [[last]] attribute" &= name (enableSwitch TransactionSizeMissing) &= explicit
                              -- options to disable individual warnings
                            , [ Disable Conversion ]             &= groupname "Warnings" &= help "Don't warn for implicit conversions that may alter a value" &= name (disableSwitch Conversion) &= explicit
                            , [ Disable Deprecated ]             &= groupname "Warnings" &= help "Don't warn for deprecated features" &= name (disableSwitch Deprecated) &= explicit
                            , [ Disable InitializerOverrides ]   &= groupname "Warnings" &= help "Don't warn for overriding designated initializers" &= name (disableSwitch InitializerOverrides) &= explicit
                            , [ Disable RedundantImport ]        &= groupname "Warnings" &= help "Don't warn for redundant imports" &= name (disableSwitch RedundantImport) &= explicit
                            , [ Disable Shadow ]                 &= groupname "Warnings" &= help "Don't warn for shadowing declarations" &= name (disableSwitch Shadow) &= explicit
                            , [ Disable TransactionSizeMissing ] &= groupname "Warnings" &= help "Don't warn for missing [[transaction_size]] attribute at calls to functions with the [[last]] attribute" &= name (disableSwitch TransactionSizeMissing) &= explicit
                            ]
    } &=
    name "compile" &=
    help "Compile Kanagawa program" &=
    details [ "Usage example:"
            , "    Compile source.k and generate SystemVerilog RTL:"
            , "      kanagawa --backend=sv source.k"
            ]

pretty :: Options
pretty = PrettyPrint
    { line_width    = 80        &= help "Width of the page in characters; default 80" &= name "w"
    , indent        = 4         &= help "Number of spaces per indentation level; default 4"
    , ribbon        = 1.0       &= help "Fraction of line that should be filled before trying to break it up; default 1.0"
    , layout        = Pretty    &= help "Layout variant to be used: pretty or smart"
    } &=
    name "pretty" &=
    help "Parse and pretty print source file(s)"

mode :: Mode (CmdArgs Options)
mode = cmdArgsMode $ modes [compile &= auto, pretty] &=
    program "kanagawa" &=
    verbosity &=
    help "Kanagawa compiler" &=
    summary ("Kanagawa Compiler " ++ makeReleaseName releaseVersion releaseBuildChannel ++ ", (C) Microsoft")

getOptions :: IO Options
getOptions = cmdArgsRun mode
