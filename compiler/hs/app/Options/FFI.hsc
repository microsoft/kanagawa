{-# OPTIONS_GHC -Wno-missing-methods -Wno-orphans #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Options.FFI
    ( withOptions
    , withStringArray
    , module Options
    ) where

import qualified Data.Set as Set
import Foreign
import Foreign.C.String
import Foreign.Storable()
import Options
import System.Console.CmdArgs (getVerbosity)
import Language.Kanagawa.Warning

#include "options.h"

-- Allocate and fill Options struct and then pass it to provided function.
-- Note that any pointers are valid only until the called function returns.
withOptions :: Options -> String -> [FilePath] -> (Ptr Options -> IO a) -> IO a
withOptions opt@Compile{..} cmdArgs allFiles f =
    alloca                                      $ \ptr ->
    withCAString cmdArgs                        $ \strCmdArgs ->
    withCAString target_device                  $ \strTargetDevice ->
    withStringArray allFiles                    $ \ptrFileNames -> do
      let ptrCodegen = #{ptr Options, _codegenOptions} ptr
      poke ptr opt
      verbosity <- getVerbosity
      #{poke CodeGenOptions, _verbosity } ptrCodegen $ fromEnum verbosity
      #{poke Options, _cmdArgs          } ptr strCmdArgs
      #{poke Options, _deviceName       } ptr strTargetDevice
      #{poke Options, _fileNames        } ptr ptrFileNames
      f ptr
withOptions _ _ _ _ = undefined

-- Allocate an array of C strings and pass it to provided function.
withStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withStringArray xs f =
    allocaBytes (#{size char*} * (length xs + 1)) $ \ptrStrings ->
    allocaStrings xs ptrStrings $ \_ ->
      f ptrStrings

-- Allocate C strings, storing them at supplied array and then
-- pass the end of the array to provided function.
allocaStrings :: [String] -> Ptr CString -> (Ptr CString -> IO a) -> IO a
allocaStrings (x:xs) ptr f = withCAString x $ \s -> do
    poke ptr s
    allocaStrings xs (ptr `plusPtr` sizeOf s) f
allocaStrings [] ptr f = do
    poke ptr nullPtr
    f ptr

instance Storable Options where
    alignment _ = #{alignment Options}
    sizeOf _ = #{size Options}

    poke ptr Compile{..} = do
        let ptrCodegen = #{ptr Options, _codegenOptions} ptr
            ptrPlacement = #{ptr Options, _placementOptions} ptr
        #{poke Options, _frequency                                  } ptr frequency
        #{poke CodeGenOptions, _additionalLatency                   } ptrCodegen additional_latency
        #{poke CodeGenOptions, _additionalResetCycles               } ptrCodegen reset_duration_cycles
        #{poke CodeGenOptions, _autoPipelineScale                   } ptrCodegen auto_pipeline_scale
        #{poke CodeGenOptions, _autoPipelineCrossRegion             } ptrCodegen auto_pipeline_cross_region
        #{poke CodeGenOptions, _maxBlockRam                         } ptrCodegen max_block_ram
        #{poke CodeGenOptions, _minBlockRamUtilization              } ptrCodegen block_ram_util_threshold
        #{poke CodeGenOptions, _maxDeepRam                          } ptrCodegen max_deep_ram
        #{poke CodeGenOptions, _minDeepRamUtilization               } ptrCodegen deep_ram_util_threshold
        #{poke CodeGenOptions, _maxSelectInputs                     } ptrCodegen max_mux_sources
        #{poke CodeGenOptions, _maxMulSrcWidth                      } ptrCodegen max_mul_src_width
        #{poke CodeGenOptions, _maxThreadsDefault                   } ptrCodegen max_threads_default
        #{poke CodeGenOptions, _maxThreadsLimit                     } ptrCodegen max_threads_limit
        #{poke CodeGenOptions, _scheduleLimit                       } ptrCodegen schedule_limit
        #{poke CodeGenOptions, _carryChainWidthPerLogicLevel        } ptrCodegen carry_chain_width
        #{poke CodeGenOptions, _rmwMemoryWriteDelay                 } ptrCodegen rmw_memory_write_delay
        #{poke CodeGenOptions, _rmwMemoryReadDelay                  } ptrCodegen rmw_memory_read_delay
        #{poke CodeGenOptions, _rmwHardenedMemoryWriteDelay         } ptrCodegen rmw_hardened_write_delay
        #{poke CodeGenOptions, _rmwHardenedMemoryMaxWidth           } ptrCodegen rmw_hardened_max_width
        #{poke CodeGenOptions, _rmwHardenedMemoryMaxHeight          } ptrCodegen rmw_hardened_max_height
        #{poke CodeGenOptions, _rmwMemoryFavorArea                  } ptrCodegen rmw_memory_favor_area
        #{poke CodeGenOptions, _strictMemoryReadWrite               } ptrCodegen strict_memory_read_write
        #{poke CodeGenOptions, _inputOutputRegisterDelay            } ptrCodegen io_register_delay
        #{poke CodeGenOptions, _memToRegThreshold                   } ptrCodegen mem_to_reg
        #{poke CodeGenOptions, _fifoToDspThreshold                  } ptrCodegen fifo_to_dsp
        #{poke CodeGenOptions, _assertNarrowingConversion           } ptrCodegen check_narrowing_assignment
        #{poke CodeGenOptions, _dumpIR                              } ptrCodegen dump_IR
        #{poke CodeGenOptions, _dumpOpt                             } ptrCodegen dump_opt
        #{poke CodeGenOptions, _serializeIRPostOpt                  } ptrCodegen write_ir_post_opt
        #{poke CodeGenOptions, _serializeCIRCTIR                    } ptrCodegen write_circt_ir
        #{poke CodeGenOptions, _fifoWriteDelay                      } ptrCodegen fifo_write_delay
        #{poke CodeGenOptions, _fifoMergeDistance                   } ptrCodegen fifo_merge_distance
        #{poke CodeGenOptions, _noVerilogHeader                     } ptrCodegen no_verilog_header
        #{poke CodeGenOptions, _optimize                            } ptrCodegen optimize
        #{poke CodeGenOptions, _logicRegisterRatio                  } ptrCodegen register_ratio
        #{poke CodeGenOptions, _maxLogicRegisterRatio               } ptrCodegen max_register_ratio
        #{poke CodeGenOptions, _resetCycles                         } ptrCodegen reset_cycles
        #{poke CodeGenOptions, _resetFanOutCycles                   } ptrCodegen reset_fan_out_cycles
        #{poke CodeGenOptions, _clockGatingLevel                    } ptrCodegen clock_gating
        #{poke CodeGenOptions, _clockGatingThreshold                } ptrCodegen clock_gating_threshold
        #{poke CodeGenOptions, _allowFifoStutter                    } ptrCodegen $ not no_fifo_stutter
        #{poke CodeGenOptions, _stallablePipelines                  } ptrCodegen stallable_pipelines
        #{poke CodeGenOptions, _conditionalMemoryReads              } ptrCodegen conditional_memory_reads
        #{poke CodeGenOptions, _semaphoreDelay                      } ptrCodegen semaphore_delay
        #{poke CodeGenOptions, _releaseAssert                       } ptrCodegen $ not no_asserts
        #{poke CodeGenOptions, _suppressDebugView                   } ptrCodegen no_debug_view
        #{poke CodeGenOptions, _codeCoverage                        } ptrCodegen code_coverage
        #{poke CodeGenOptions, _codeCoverageMuxThreshold            } ptrCodegen code_coverage_mux_threshold
        #{poke CodeGenOptions, _dumpMemoryMetadata                  } ptrCodegen dump_memory_metadata
        #{poke CodeGenOptions, _maxStringLength                     } ptrCodegen identifier_length
        #{poke CodeGenOptions, _logWorkListSize                     } ptrCodegen work_list_size
        #{poke CodeGenOptions, _returnStages                        } ptrCodegen return_stages
        #{poke PlacementOptions, _numIterations                     } ptrPlacement place_iterations
        #{poke PlacementOptions, _seed                              } ptrPlacement place_seed
        #{poke PlacementOptions, _pullStrength                      } ptrPlacement place_pull
        #{poke PlacementOptions, _pushStrength                      } ptrPlacement place_push
        #{poke PlacementOptions, _wallStrength                      } ptrPlacement place_wall
        #{poke PlacementOptions, _updateRate                        } ptrPlacement place_update_rate
        #{poke PlacementOptions, _display                           } ptrPlacement place_display
        #{poke CodeGenOptions, _debug                               } ptrCodegen debug
        #{poke CodeGenOptions, _stripDebugSymbols                   } ptrCodegen strip_debug_symbols
        #{poke CodeGenOptions, _stall                               } ptrCodegen stall
        #{poke CodeGenOptions, _forceMemToArray                     } ptrCodegen force_mem_to_array
        #{poke CodeGenOptions, _sparseRegOpt                        } ptrCodegen sparse_reg_opt
        #{poke CodeGenOptions, _carrySelect                         } ptrCodegen carry_select
        #{poke CodeGenOptions, _maxStackDepth                       } ptrCodegen max_stack_depth
        #{poke CodeGenOptions, _resetFanout                         } ptrCodegen $ not no_reset_fanout
        #{poke CodeGenOptions, _enableTransactionSizeWarning        } ptrCodegen $ Set.member TransactionSizeMissing $ enabledWarnings warnings_state
        #{poke CodeGenOptions, _enableDeprecationWarnings           } ptrCodegen $ Set.member Deprecated $ enabledWarnings warnings_state
        #{poke CodeGenOptions, _warningsAsErrors                    } ptrCodegen warnings_as_errors
    poke _ _ = undefined
