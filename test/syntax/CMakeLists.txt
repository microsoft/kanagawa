# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Add a phoney target to "build" syntax tests. Because these tests
# do not require a separate build step, all this does it validate
# that the kanagawa compiler has been built.

add_custom_target(build_syntax_tests
    DEPENDS kanagawa::exe
)

# Add a target to run all syntax tests
add_custom_target(run_syntax_tests
    COMMAND ${CMAKE_CTEST_COMMAND} --test-dir ${CMAKE_CURRENT_BINARY_DIR} -R "^syntax_test\\." --output-on-failure --verbose
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Run all syntax tests"
    USES_TERMINAL
)

function(add_syntax_test TEST_NAME)
    set(options "")
    set(oneValueArgs HELPER TEST_FILE)
    set(multiValueArgs OPTIONS)

    cmake_parse_arguments(_ARG "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    set(TEST_NAME_WITH_PREFIX "syntax_test.${TEST_NAME}")

    if (_ARG_TEST_FILE)
        set(TEST_FILE ${_ARG_TEST_FILE})
    else()
        set(TEST_FILE ${TEST_NAME}.k)
    endif()

    # Default options for the Kanagawa compiler
    set(OPTIONS
        --jobs=1
        --Wno-redundant-import
        --base-library=${CMAKE_SOURCE_DIR}/library/mini-base.k
        --import-dir=${CMAKE_SOURCE_DIR}/library
    )

    if(_ARG_OPTIONS)
        list(APPEND OPTIONS ${_ARG_OPTIONS})
    endif()

    # Join options into a single string
    string(REPLACE ";" " " OPTIONS_STRING "${OPTIONS}")

    set(CMD
        python3
        ${CMAKE_CURRENT_SOURCE_DIR}/syntax-test.py
        -kanagawa "$<TARGET_FILE:kanagawa::exe>"
        -testfile ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_FILE}
        -helperdir ${CMAKE_CURRENT_SOURCE_DIR}
        -options "${OPTIONS_STRING}"
    )

    if (_ARG_HELPER)
        list(APPEND CMD -helper ${_ARG_HELPER})
    endif()

    # Create the test
    add_test(
        NAME ${TEST_NAME_WITH_PREFIX}
        COMMAND ${CMD}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )

endfunction()

add_syntax_test("base-test" OPTIONS --no-implicit-base=false --template-iterations=1)
add_syntax_test("shadow-warning")
add_syntax_test("interpolated-string" HELPER interpolated_string)
add_syntax_test("string" HELPER string)
add_syntax_test("return-type-polymorphism" HELPER return_type_polymorphism OPTIONS --Wconversion)
add_syntax_test("assignment")
add_syntax_test("literal-operand" HELPER literal_operand OPTIONS --Wconversion "--define=ELEVEN#11")
add_syntax_test("type-constexpr" HELPER type_constexpr)
add_syntax_test("type-identifier" HELPER type_identifier)
add_syntax_test("empty-struct")
add_syntax_test("extern-class" HELPER extern_class)
add_syntax_test("function-type")
add_syntax_test("dependent-template" HELPER dependent_template)
add_syntax_test("designated-initializer" HELPER designated_initializer)
add_syntax_test("name-attr")
add_syntax_test("custom-base" OPTIONS --no-implicit-base=false --base-library helper/custom_base.k)
add_syntax_test("statement")
add_syntax_test("options" OPTIONS --Wredundant-import --backend=sv --stall=3 --target-device=ultrascaleplus-xcu250)
add_syntax_test("function-call")
add_syntax_test("declaration-order" HELPER declaration_order)
add_syntax_test("doc-comments" OPTIONS --parse-docs)
add_syntax_test("all-warnings-as-errors" OPTIONS --Wall --Werror)
add_syntax_test("memory" HELPER memory OPTIONS --target-device=mock)
add_syntax_test("conversion-warning" OPTIONS --Wconversion)
add_syntax_test("no-warning" OPTIONS --Wno-shadow)
add_syntax_test("callbacks" HELPER callbacks OPTIONS --Wconversion --Werror)
add_syntax_test("template-template-parameter" HELPER template_template_parameter)
add_syntax_test("template-param-default" HELPER template_param_default)
add_syntax_test("quad-port-memory")
add_syntax_test("export-class" HELPER export_class)
add_syntax_test("scheduling")
add_syntax_test("static-for")
add_syntax_test("static-if" HELPER static_if)
add_syntax_test("loops")
add_syntax_test("this-capture")
add_syntax_test("literals")
add_syntax_test("decltype")
add_syntax_test("auto-placeholder" HELPER auto_placeholder)
add_syntax_test("template-argument-deduction" HELPER template_argument_deduction)
add_syntax_test("template-argument-deduction-from-instance" HELPER template_argument_deduction_from_instance)
add_syntax_test("function-auto-return" HELPER function_auto_return)
add_syntax_test("struct-constexpr" HELPER struct_constexpr)
add_syntax_test("higher-order-functions" HELPER higher_order_functions)
add_syntax_test("lambdas")
add_syntax_test("closures")
add_syntax_test("local-functions" HELPER local_functions)
add_syntax_test("function-constants" HELPER function_constants)
add_syntax_test("imports" OPTIONS --Wredundant-import --import-dir=${CMAKE_CURRENT_SOURCE_DIR}/imports)
add_syntax_test("removed-syntax")
add_syntax_test("initializer-list" HELPER initializer_list)
add_syntax_test("pipelined")
add_syntax_test("template")
add_syntax_test("type-check" HELPER type_check)
add_syntax_test("struct-template" HELPER struct_template)
add_syntax_test("constexpr" HELPER constexpr OPTIONS "--define=FOO_CONST#10")
add_syntax_test("using" HELPER aliases)
add_syntax_test("member-function-template" HELPER member_function_template)
add_syntax_test("function-template" HELPER function_template)
add_syntax_test("function-template-param" HELPER function_template_param)
add_syntax_test("enum" HELPER enum)
add_syntax_test("const-correctness" HELPER const_correctness)
add_syntax_test("nested-templates" HELPER nested_templates)
add_syntax_test("template-instance-identifiers" HELPER template_instance_identifiers)
add_syntax_test("struct" HELPER structs)
add_syntax_test("union")
add_syntax_test("union-template" HELPER union_template)
add_syntax_test("local-classes")
add_syntax_test("local-structs" HELPER local_structs)
add_syntax_test("static-locals" HELPER static_locals)
add_syntax_test("function-attributes")
add_syntax_test("atomic")
add_syntax_test("last")
add_syntax_test("atomic_do" "atomic_do.k")
