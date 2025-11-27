# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file configures the build for a program, chkdoc, that checks embedded Kanagawa code within markdown documents.
# To add a document, add a call to `add_chkdoc_test` at the bottom of the file. A ctest will be created with a name with the
# prefix "chkdoc."

# Build chkdoc executable
add_executable(chkdoc chkdoc.cpp)

# Link against Boost headers (required for boost/algorithm/string.hpp)
target_link_libraries(chkdoc PRIVATE Boost::headers)

# chkdoc is dependent on the kanagawa compiler so that compiler changes
# will cause the compiler to be rebuilt before running tests
add_dependencies(chkdoc kanagawa_runtime)

set(DOC_DIR "${CMAKE_SOURCE_DIR}/doc")

function(add_chkdoc_test test_name document)
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

function(add_md_to_html document)
  set(_opts SKIP_TOC)
  set(_one OUTPUT_LIST_VAR)
  set(_multi)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})


  # If necessary, convert document to an absolute path
  if (NOT IS_ABSOLUTE "${document}")
    set(document "${DOC_DIR}/${document}")
  endif()

  set(OUTPUT_DIR "${BUILD_OUTPUT_DIR}/doc")

  # Get just the base part of the document filename
  get_filename_component(DOC_BASENAME "${document}" NAME_WE)

  set(OUTPUT_DOC_PATH "${OUTPUT_DIR}/${DOC_BASENAME}.html")

  if (_ARG_SKIP_TOC)
    set(PANDOC_TOC)
  else()
    set(PANDOC_TOC --toc --toc-depth=5)
  endif()

  set(PANDOC_HTML_VARS -V monobackgroundcolor=\#dddddd)

  add_custom_command(
    OUTPUT ${OUTPUT_DOC_PATH}
    COMMAND
      pandoc ${PANDOC_HTML_VARS} ${PANDOC_TOC} --strip-comments
      --standalone -f markdown -t html ${document} -o ${OUTPUT_DOC_PATH}
    DEPENDS sandcastle_exe ${document}
    VERBATIM
  )

  if (_ARG_OUTPUT_LIST_VAR)
    list(APPEND ${_ARG_OUTPUT_LIST_VAR} "${OUTPUT_DOC_PATH}")
    set(${_ARG_OUTPUT_LIST_VAR} "${${_ARG_OUTPUT_LIST_VAR}}" PARENT_SCOPE)
  endif()

endfunction()

# Generate docs for the standard library
if (DEFINED CURRENT_COMMIT)
    set(SOURCE_URL "--source-url=https://github.com/microsoft/kanagawa/blob/${CURRENT_COMMIT}/library")
endif()

add_custom_target(libdoc
  COMMAND
    $<TARGET_FILE:sandcastle::exe> --import-dir "${CMAKE_SOURCE_DIR}/library"
    --format html --svgbob --target-device mock --output ${BUILD_OUTPUT_DIR}/doc/library ${SOURCE_URL} "${CMAKE_SOURCE_DIR}/library/**/*.k"
  DEPENDS sandcastle_exe
  VERBATIM
)

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

# Verify that embedded code in markdown documentation is syntactically valid
add_chkdoc_test(effective_kanagawa effective-kanagawa.md)
add_chkdoc_test(mapping_to_hardware "mapping-to-hardware.md")
add_chkdoc_test(sandcastle sandcastle.md)
add_chkdoc_test(programming_guide programming-guide.md)

# Convert markdown docs to HTML
set(GEN_HTML_DOCS)

add_md_to_html(effective-kanagawa.md OUTPUT_LIST_VAR "GEN_HTML_DOCS")
add_md_to_html(mapping-to-hardware.md OUTPUT_LIST_VAR "GEN_HTML_DOCS")
add_md_to_html(sandcastle.md SKIP_TOC OUTPUT_LIST_VAR "GEN_HTML_DOCS")
add_md_to_html(programming-guide.md OUTPUT_LIST_VAR "GEN_HTML_DOCS")

# Phoney target for conversion of markdown docs to html
add_custom_target(doc_md_to_html DEPENDS ${GEN_HTML_DOCS})

# Phony target for all document generation
add_custom_target(gendoc DEPENDS doc_md_to_html libdoc)
