{-# LANGUAGE DeriveDataTypeable #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module Options
    ( Layout(..)
    , Options(..)
    , Backend(..)
    ) where

import Data.Data
import Language.Kanagawa.Parser.Options
import Language.Kanagawa.Warning

data Layout = Smart | Pretty
      deriving (Show, Data, Typeable)

data Options
    = Options
    | PrettyPrint
        { base_library :: FilePath
        , define :: [String]
        , dump_parse :: Bool
        , files :: [FilePath]
        , import_dir :: [FilePath]
        , indent :: Int
        , layout :: Layout
        , line_width :: Int
        , no_implicit_base :: Bool
        , parse_docs :: Bool
        , ribbon :: Double
        , target_device :: String
        , using :: [String]
        }
    | Compile
        { additional_latency :: Int
        , auto_pipeline_cross_region :: Int
        , auto_pipeline_scale :: Int
        , backend :: Backend
        , base_library :: FilePath
        , block_ram_util_threshold :: Int
        , carry_chain_width :: Int
        , carry_select :: Bool
        , clock_gating :: Int
        , clock_gating_threshold :: Int
        , code_coverage :: Bool
        , code_coverage_mux_threshold :: Int
        , conditional_memory_reads :: Bool
        , debug :: Bool
        , deep_ram_util_threshold :: Int
        , define :: [String]
        , dgml :: String
        , dgml_detailed :: String
        , check_narrowing_assignment :: Bool
        , dump_IR :: Bool
        , dump_memory_metadata :: Bool
        , dump_opt :: Bool
        , dump_parse :: Bool
        , dump_program :: Bool
        , dump_source :: Bool
        , dump_types :: Bool
        , fifo_merge_distance :: Int
        , fifo_to_dsp :: Int
        , fifo_write_delay :: Int
        , file_list :: FilePath
        , files :: [FilePath]
        , force_mem_to_array :: Int
        , frequency :: Int
        , identifier_length :: Int
        , import_dir :: [FilePath]
        , io_register_delay :: Int
        , jobs :: Maybe Int
        , max_block_ram :: Int
        , max_deep_ram :: Int
        , max_register_ratio :: Int
        , max_mul_src_width :: Int
        , max_mux_sources :: Int
        , max_stack_depth :: Int
        , max_threads_default :: Int
        , max_threads_limit :: Int
        , mem_to_reg :: Float
        , no_asserts :: Bool
        , no_debug_view :: Bool
        , no_fifo_stutter :: Bool
        , no_implicit_base :: Bool
        , no_reset_fanout :: Bool
        , no_verilog_header :: Bool
        , optimize :: Int
        , output :: String
        , parse_docs :: Bool
        , passes :: Int
        , place_display :: Bool
        , place_iterations :: Int
        , place_pull :: Float
        , place_push :: Float
        , place_seed :: Int
        , place_update_rate :: Float
        , place_wall :: Float
        , register_ratio :: Int
        , reset_cycles :: Int
        , reset_fan_out_cycles :: Int
        , reset_duration_cycles :: Int
        , resource_usage :: String
        , return_stages :: Int
        , rmw_hardened_max_height :: Int
        , rmw_hardened_max_width :: Int
        , rmw_hardened_write_delay :: Int
        , rmw_memory_favor_area :: Bool
        , rmw_memory_read_delay :: Int
        , rmw_memory_write_delay :: Int
        , schedule_limit :: Int
        , semaphore_delay :: Int
        , sparse_reg_opt :: Bool
        , stall :: Int
        , stallable_pipelines :: Bool
        , strict_memory_read_write :: Bool
        , strip_debug_symbols :: Bool
        , target_device :: String
        , template_iterations :: Int
        , template_passes :: Int
        , using :: [String]
        , warnings_state :: [WarningState WarningKind]
        , warnings_as_errors :: Bool
        , work_list_size :: Int
        , write_circt_ir :: Bool
        , write_ir_post_opt :: Bool
        }
    | LangServer
        { verbose :: Bool
        , log_file :: Maybe String
        }
      deriving (Show, Data, Typeable)

