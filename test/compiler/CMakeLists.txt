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

# Add a target to run all compiler tests
add_custom_target(run_compiler_tests
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR} -R "^compiler\\." --output-on-failure --verbose
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS compiler_internal_test
    COMMENT "Run all compiler internal tests"
    USES_TERMINAL
)
