# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Builds and creates a ctest to run Kanagawa compiler internal tests
add_executable(compiler_internal_test main.cpp)

target_include_directories(compiler_internal_test PRIVATE "${CMAKE_SOURCE_DIR}/compiler/cpp")

target_link_libraries(compiler_internal_test kanagawa_lib)
target_link_libraries(compiler_internal_test pthread ${CMAKE_DL_LIBS})

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
