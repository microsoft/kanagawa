# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# kanagawa_lib - The Kanagawa compiler back-end as a shared library.
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

# Compiler settings for building Kanagawa C++

# Stop CMake from auto-including pch.h
# to ensure that source files are always valid C++
set(CMAKE_CXX_COMPILE_OPTIONS_USE_PCH "")

# Required to link a dynamic library with a static library
if(UNIX)
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Default / common MSVC settings
include(${CMAKE_SOURCE_DIR}/build/cmake/msvc.cmake)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /DNOMINMAX /DWIN32_LEAN_AND_MEAN /W3 /WX /wd4244 /wd4624 /wd4146 /w15032 /wd4319 /wd4297 /wd4091 /wd4834 /wd4018 /wd4996 /wd4267 /wd4244 /DBOOST_INTERPROCESS_DETAIL_STD_FWD_HPP /DBOOST_ALL_NO_LIB /fp:strict" )
    set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} /DNOMINMAX /DWIN32_LEAN_AND_MEAN /W3 /WX /wd4244 /wd4624 /wd4146 /w15032 /wd4319 /wd4297 /wd4091 /wd4834 /wd4018 /wd4996 /wd4267 /wd4244 /DBOOST_INTERPROCESS_DETAIL_STD_FWD_HPP /DBOOST_ALL_NO_LIB /fp:strict" )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /GR /EHsc /bigobj /MP4")
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
    set(CMAKE_CXX_FLAGS "-fstandalone-debug -DNDEBUG -DBOOST_USES_XMM")
endif()

add_library(kanagawa_lib SHARED ${CPP_SOURCES})

# Change the actual library filename to "kanagawa.dll" on Windows, "libkanagawa.so" (on Linux), etc.
set_target_properties(kanagawa_lib PROPERTIES
    OUTPUT_NAME ${KANAGAWA_SHARED_LIBRARY_NAME}
)

# target_compile_definitions(kanagawa_lib PRIVATE KANAGAWA_SKIP_CONSISTENCY_CHECKS=1)

target_include_directories(kanagawa_lib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Enable pre-compiled headers for faster builds
target_precompile_headers(kanagawa_lib PRIVATE [["pch.h"]])
set_source_files_properties(platform.cpp PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
set_source_files_properties(stack_trace.cpp PROPERTIES SKIP_PRECOMPILE_HEADERS ON)

# Adds Boost to the include path
target_link_libraries(kanagawa_lib PRIVATE Boost::headers)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    # Ensure that interface functions are exported
    target_compile_definitions(kanagawa_lib PRIVATE "EXPORT_API")
endif()

if(WIN32)
    target_link_libraries(kanagawa_lib PUBLIC ntdll.lib)
endif()

# Make boost stack trace print function names in addition to addresses
target_compile_definitions(kanagawa_lib PRIVATE "BOOST_STACKTRACE_USE_ADDR2LINE")

# Link with pthread library for threading support
target_link_libraries(kanagawa_lib PRIVATE Threads::Threads)

# Add CIRCT targets/library dependencies
target_link_libraries(kanagawa_lib PRIVATE
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
target_include_directories(kanagawa_lib PUBLIC "${CIRCT_INCLUDES}")

# This is needed to avoid generator expressions in OUTPUT
set(KANAGAWA_LIB_NAME "${CMAKE_SHARED_LIBRARY_PREFIX}${KANAGAWA_SHARED_LIBRARY_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(KANAGAWA_LIB_DEST "${BUILD_OUTPUT_BIN_DIR}/${KANAGAWA_LIB_NAME}")

# Copy the shared library into the dest/bin directory
add_custom_command(TARGET kanagawa_lib POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "$<SHELL_PATH:${BUILD_OUTPUT_BIN_DIR}>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:kanagawa_lib>"
        "${BUILD_OUTPUT_BIN_DIR}/$<TARGET_FILE_NAME:kanagawa_lib>"
  # Re-run when the actual built lib changes, and only after the exe exists (dir ready)
    VERBATIM
)

if (MSVC)
    add_custom_command(TARGET kanagawa_lib POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E make_directory "$<SHELL_PATH:${BUILD_OUTPUT_BIN_DIR}>"
        # If there is a PDB, copy it. Otherwise, do a no-op.
        COMMAND ${CMAKE_COMMAND} -E $<IF:$<BOOL:$<TARGET_PDB_FILE:kanagawa_lib>>,copy_if_different,true>
                "$<TARGET_PDB_FILE:kanagawa_lib>"
                "${BUILD_OUTPUT_BIN_DIR}/$<TARGET_FILE_BASE_NAME:kanagawa_lib>.pdb"
        VERBATIM
    )
endif()
