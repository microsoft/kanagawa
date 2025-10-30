# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

add_subdirectory(syntax)
add_subdirectory(compiler)

if (VERILATOR_EXE)
    add_subdirectory(interface)
    add_subdirectory(library)
    add_subdirectory(logic)
    add_subdirectory(runtime/rtl)
endif()