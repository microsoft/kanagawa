# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# kanagawac - The Kanagawa compiler back-end as a shared library.
# This library is linked with the front-end code to produce the final
# kanagawa executable.

set(CPP_SOURCES
    backend_common.cpp
    branch_and_bound.cpp
    circt_util.cpp
    compiler.cpp
    conditional_local_updates.cpp
    config.cpp
    control_flow_graph.cpp
    data_flow_work_list.cpp
    debug_symbols.cpp
    device_config_visitor.cpp
    dgml.cpp
    enumerate_function_instances.cpp
    internal_tests.cpp
    ir.cpp
    kanagawa.cpp
    local_data_propagation.cpp
    lower.cpp
    object_path.cpp
    optimize.cpp
    optimize_lut.cpp
    parse_tree.cpp
    path_report.cpp
    place.cpp
    platform.cpp
    power_report.cpp
    resource_usage.cpp
    schedule.cpp
    serialize_ir.cpp
    stack_trace.cpp
    symbols.cpp
    thread_pool.cpp
    verilog.cpp
)

add_library(kanagawac SHARED ${CPP_SOURCES})

# target_compile_definitions(kanagawac PRIVATE KANAGAWA_SKIP_CONSISTENCY_CHECKS=1)

target_include_directories(kanagawac PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Enable pre-compiled headers for faster builds
target_precompile_headers(kanagawac PRIVATE [["pch.h"]])
set_source_files_properties(platform.cpp PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
set_source_files_properties(stack_trace.cpp PROPERTIES SKIP_PRECOMPILE_HEADERS ON)

# Despite the misleading name, this adds Boost to the include path using the information
# gleaned from a call to find_package
target_link_libraries(kanagawac PRIVATE Boost::headers)

# Make boost stack trace print function names in addition to addresses
target_compile_definitions(kanagawac PRIVATE "BOOST_STACKTRACE_USE_ADDR2LINE")

# Link with pthread library for threading support
target_link_libraries(kanagawac PRIVATE Threads::Threads)

# Add CIRCT targets/library dependencies
target_link_libraries(kanagawac PRIVATE
    CIRCTComb
    CIRCTCombTransforms
    CIRCTExportVerilog
    CIRCTHW
    CIRCTHWToSV
    CIRCTHWArith
    CIRCTHWArithToHW
    CIRCTHWTransforms
    CIRCTKanagawa
    CIRCTKanagawaTransforms
    CIRCTMSFT
    CIRCTMSFTTransforms
    CIRCTSeq
    CIRCTSeqTransforms
    CIRCTSeqToSV
    CIRCTPipelineOps
    CIRCTPipelineToHW
    CIRCTPipelineTransforms
    CIRCTSV
    CIRCTSVTransforms
    CIRCTHWArith
    CIRCTTransforms
    MLIRIR
    MLIRMemRefDialect
    MLIROptLib
    MLIRParser
    MLIRFuncDialect
    MLIRSupport
    MLIRTransforms
    CIRCTOM
    CIRCTVerif
    CIRCTLTL
    MLIREmitCDialect
    CIRCTKanagawa
    CIRCTCFToHandshake
    MLIRMaskableOpInterface
    MLIRMaskingOpInterface
    CIRCTDC
    CIRCTTransforms
    MLIRSCFDialect
    MLIRParallelCombiningOpInterface
    MLIRMemRefDialect
    MLIRDialectUtils
    MLIRArithUtils
    LLVMBitWriter
    LLVMProfileData
    LLVMSymbolize
    LLVMObject
    LLVMIRReader
    LLVMAsmParser
    LLVMBitReader
    LLVMTextAPI
    MLIRShapedOpInterfaces
    MLIRValueBoundsOpInterface
    MLIRDestinationStyleOpInterface
    CIRCTPipelineOps
    CIRCTESI
    MLIRControlFlowDialect
    MLIRTranslateLib
    MLIRArithDialect
    MLIRCastInterfaces
    MLIRInferIntRangeCommon
    MLIRUBDialect
    CIRCTSSP
    LLVMCore
    LLVMBinaryFormat
    LLVMRemarks
    LLVMBitstreamReader
    LLVMTargetParser
    CIRCTScheduling
    MLIRFuncDialect
    CIRCTSeq
    CIRCTSV
    CIRCTComb
    CIRCTHW
    CIRCTSupport
    CIRCTDebug
    CIRCTEmit
    MLIRTransforms
    MLIRMemorySlotInterfaces
    MLIRRuntimeVerifiableOpInterface
    MLIRTransformUtils
    MLIRRewrite
    MLIRPDLToPDLInterp
    MLIRPDLInterpDialect
    MLIRPDLDialect
    MLIRRewritePDL
    MLIRParser
    MLIRBytecodeReader
    MLIRAsmParser
    MLIRBytecodeWriter
    MLIRBytecodeOpInterface
    MLIRDebug
    MLIRObservers
    MLIRPluginsLib
    MLIRPass
    MLIRAnalysis
    MLIRControlFlowInterfaces
    MLIRViewLikeInterface
    MLIRCallInterfaces
    MLIRLoopLikeInterface
    MLIRFunctionInterfaces
    MLIRInferIntRangeInterface
    MLIRDataLayoutInterfaces
    MLIRPresburger
    MLIRInferTypeOpInterface
    MLIRSideEffectInterfaces
    MLIRDialect
    MLIRIR
    MLIRSupport
    LLVMSupport
    LLVMDemangle
)

# Add CIRCT include directories
get_target_property(CIRCT_INCLUDES CIRCTHW INCLUDE_DIRECTORIES)
target_include_directories(kanagawac PUBLIC "${CIRCT_INCLUDES}")

file(MAKE_DIRECTORY ${KANAGAWA_BIN_DIR})

add_custom_command(
    TARGET kanagawac
    POST_BUILD
    COMMAND
        ${CMAKE_COMMAND} -E copy_if_different
        $<TARGET_FILE:kanagawac>
        "${KANAGAWA_BIN_DIR}/"
)
