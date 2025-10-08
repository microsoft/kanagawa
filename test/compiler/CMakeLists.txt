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