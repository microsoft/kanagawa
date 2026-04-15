# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Default / common MSVC settings
include(${CMAKE_SOURCE_DIR}/build/cmake/msvc.cmake)

# Builds and creates a ctest to run Kanagawa compiler internal tests
add_executable(compiler_internal_test main.cpp)

target_include_directories(compiler_internal_test PRIVATE "${CMAKE_SOURCE_DIR}/compiler/cpp")

target_link_libraries(compiler_internal_test PRIVATE kanagawa_lib Threads::Threads ${CMAKE_DL_LIBS})

# Copy the kanagawa_lib shared library to the test directory so the test can find it at runtime
add_custom_command(TARGET compiler_internal_test POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:kanagawa_lib>"
        "$<TARGET_FILE_DIR:compiler_internal_test>/$<TARGET_FILE_NAME:kanagawa_lib>"
    COMMENT "Copying kanagawa_lib DLL to test directory"
    VERBATIM
)

# Copy the PDB file if present (Windows debug builds)
if (MSVC)
    add_custom_command(TARGET compiler_internal_test POST_BUILD
        # If there is a PDB, copy it. Otherwise, do a no-op.
        COMMAND ${CMAKE_COMMAND} -E $<IF:$<BOOL:$<TARGET_PDB_FILE:kanagawa_lib>>,copy_if_different,true>
                "$<TARGET_PDB_FILE:kanagawa_lib>"
                "$<TARGET_FILE_DIR:compiler_internal_test>/$<TARGET_FILE_BASE_NAME:kanagawa_lib>.pdb"
        COMMENT "Copying kanagawa_lib PDB to test directory"
        VERBATIM
    )
endif()

add_test(
    NAME compiler.internal
    COMMAND "${CMAKE_CURRENT_BINARY_DIR}/compiler_internal_test"
)

# Phoney target to ensure compiler test dependencies have been built
add_custom_target(compiler_tests DEPENDS compiler_internal_test)

# End-to-end test: compile a .k file with named types and verify the CIRCT IR
# output contains hw.typedecl and hw.typealias entries.
# Uses add_kanagawa() to compile, then cmake -P to grep the MLIR output.
set(_typealias_outdir "${CMAKE_CURRENT_BINARY_DIR}/typealias_test_output")

add_kanagawa(typealias_test_hls
    SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/typealias_test.k"
    OPTIONS --backend=sv --write-circt-ir --import-dir=${CMAKE_SOURCE_DIR}/library
    OUTPUT_DIR "${_typealias_outdir}"
    CLEAN_OUTPUT_DIR
)

# CMake script that checks the generated MLIR file for expected patterns
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/check_typealias.cmake" [=[
# Find the .mlir file in the output directory
file(GLOB MLIR_FILES "${OUTDIR}/*.mlir")
list(SORT MLIR_FILES)
list(LENGTH MLIR_FILES NUM_MLIR)
if(NUM_MLIR EQUAL 0)
    message(FATAL_ERROR "No .mlir file found in ${OUTDIR}")
endif()
if(NOT NUM_MLIR EQUAL 1)
    string(JOIN "\n  " MLIR_FILE_LIST ${MLIR_FILES})
    message(FATAL_ERROR
        "Expected exactly one .mlir file in ${OUTDIR}, found ${NUM_MLIR}:\n  ${MLIR_FILE_LIST}")
endif()
list(GET MLIR_FILES 0 MLIR_FILE)
file(READ "${MLIR_FILE}" MLIR_CONTENT)
message(STATUS "Checking ${MLIR_FILE} for type aliases...")

macro(check_pattern DESC PATTERN)
    string(REGEX MATCH "${PATTERN}" _match "${MLIR_CONTENT}")
    if(_match)
        message(STATUS "  PASS: ${DESC}")
    else()
        message(FATAL_ERROR "  FAIL: ${DESC} (pattern: ${PATTERN})")
    endif()
endmacro()

check_pattern("TypeScope contains Point typedecl"  "hw[.]typedecl @Point")
check_pattern("TypeScope contains Line typedecl"   "hw[.]typedecl @Line")
check_pattern("TypeScope contains Color typedecl"  "hw[.]typedecl @Color")
check_pattern("Core port uses Point type alias"     "typealias<@CoreModuleTypeScope::@Point")
check_pattern("Core port uses Line type alias"      "typealias<@CoreModuleTypeScope::@Line")
check_pattern("EsiWrapper container exists"         "EsiWrapper")
check_pattern("ESI channel uses Color type alias"   "typealias<@CoreModuleTypeScope::@Color")
message(STATUS "All type alias checks passed.")
]=])

add_test(
    NAME compiler.typealias
    COMMAND ${CMAKE_COMMAND}
            -DOUTDIR=${_typealias_outdir}
            -P "${CMAKE_CURRENT_BINARY_DIR}/check_typealias.cmake"
)
# The .k must be compiled before the test can run
set_tests_properties(compiler.typealias PROPERTIES
    FIXTURES_REQUIRED typealias_test_hls_fixture
)
# Register the HLS compilation as a fixture-setup test
add_test(
    NAME compiler.typealias_setup
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target typealias_test_hls
)
set_tests_properties(compiler.typealias_setup PROPERTIES FIXTURES_SETUP typealias_test_hls_fixture)

# Add a target to run all compiler tests
add_custom_target(run_compiler_tests
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR} -R "^compiler\\." --output-on-failure --verbose
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS compiler_internal_test typealias_test_hls
    COMMENT "Run all compiler internal tests"
    USES_TERMINAL
)
