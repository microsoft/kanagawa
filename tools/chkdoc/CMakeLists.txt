# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file configures the build for a program, chkdoc, that checks embedded Kanagawa code within markdown documents.
# To add a document, add a call to `add_chkdoc_test` at the bottom of the file. A ctest will be created with a name with the
# prefix "chkdoc."

# Build chkdoc executable
add_executable(chkdoc chkdoc.cpp)

# chkdoc is dependent on the kanagawa compiler so that compiler changes
# will cause the compiler to be rebuilt before running tests
add_dependencies(chkdoc kanagawa_runtime)


function(add_chkdoc_test test_name document)

    set(DOC_DIR "${CMAKE_SOURCE_DIR}/doc")

    if (NOT IS_ABSOLUTE "${document}")
        set(document "${DOC_DIR}/${document}")
    endif()

    add_test(
        NAME "chkdoc.${test_name}"
        COMMAND
            $<TARGET_FILE:chkdoc> $<TARGET_FILE:kanagawa::exe>
            "${CMAKE_CURRENT_SOURCE_DIR}/contexts" "${CMAKE_CURRENT_SOURCE_DIR}/lib" "${CMAKE_SOURCE_DIR}/library"
            emptyfile.k emptyfunction.k
            ${document}
    )

endfunction()

# Phoney target to ensure chkdoc test dependencies have been built
add_custom_target(chkdoc_tests DEPENDS chkdoc)

# Add a target to run all chkdoc tests
add_custom_target(run_chkdoc_tests
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR} -R "^chkdoc\\." --output-on-failure --verbose
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS chkdoc
    COMMENT "Run all chkdoc tests"
    USES_TERMINAL
)

add_chkdoc_test(effective_kanagawa effective-kanagawa.md)
add_chkdoc_test(mapping_to_hardware "mapping-to-hardware.md")
add_chkdoc_test(sandcastle sandcastle.md)
add_chkdoc_test(programming_guide programming-guide.md)

