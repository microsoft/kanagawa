# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# Helper function for running the Kanagawa compiler to generate RTL
#
# Usage:
#   add_kanagawa(<target>
#     [OUTPUT_DIR <dir>]
#     [OUTPUT_PREFIX <prefix>]
#     [CLEAN_OUTPUT_DIR]
#     [WORKING_DIRECTORY <dir>]
#     SOURCES <src1> [<src2> ...]
#     [OPTIONS <opt1> <opt2> ...]
#   )
#
# Downstream can depend on <target> to ensure the HLS step has run:
#   add_dependencies(my_rtl_consumer <target>)
#
function(add_kanagawa target)
  set(_opts CLEAN_OUTPUT_DIR)
  set(_one  OUTPUT_DIR WORKING_DIRECTORY OUTPUT_PREFIX)
  set(_multi SOURCES OPTIONS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if(NOT target)
    message(FATAL_ERROR "add_kanagawa: missing <target> name as first argument.")
  endif()

  # Default OUTPUT_DIR if not provided
  if(NOT _ARG_OUTPUT_DIR)
    set(_ARG_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")
  endif()

  if(NOT _ARG_SOURCES)
    message(FATAL_ERROR "add_kanagawa(${target}): SOURCES is required.")
  endif()

  if(NOT _ARG_WORKING_DIRECTORY)
    set(_ARG_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
  endif()

  # Ensure the output directory exists at configure time
  file(MAKE_DIRECTORY "${_ARG_OUTPUT_DIR}")

  # Normalize paths for reproducible content
  file(TO_CMAKE_PATH "${_ARG_OUTPUT_DIR}" _out_dir_norm)
  file(TO_CMAKE_PATH "${_ARG_WORKING_DIRECTORY}" _work_dir_norm)

  # Stable inputs summary (lives in binary dir; triggers rebuilds when changed)
  set(_inputs_file "${CMAKE_CURRENT_BINARY_DIR}/${target}.kanagawa.inputs")
  set(_inputs_text "OUTPUT_DIR=${_out_dir_norm}\nWORKING_DIRECTORY=${_work_dir_norm}\nSOURCES=\n")
  foreach(_s IN LISTS _ARG_SOURCES)
    file(TO_CMAKE_PATH "${_s}" _s_norm)
    string(APPEND _inputs_text "  ${_s_norm}\n")
  endforeach()
  string(APPEND _inputs_text "OPTIONS=\n")
  foreach(_o IN LISTS _ARG_OPTIONS)
    string(APPEND _inputs_text "  ${_o}\n")
  endforeach()
  file(GENERATE OUTPUT "${_inputs_file}" CONTENT "${_inputs_text}")

  # These files that live inside OUTPUT_DIR and are used to help CMake / Ninja understand when something changed
  set(_args_in_outdir "${_ARG_OUTPUT_DIR}/${target}.kanagawa.args")
  set(_stamp         "${_ARG_OUTPUT_DIR}/${target}.kanagawa.stamp")

  # Pre-commands (optional clean, ensure dir, copy inputs -> args in OUTPUT_DIR)
  set(_pre_commands)
  if(_ARG_CLEAN_OUTPUT_DIR)
    list(APPEND _pre_commands
      COMMAND ${CMAKE_COMMAND} -E rm -rf "${_ARG_OUTPUT_DIR}"
    )
  endif()
  list(APPEND _pre_commands
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_ARG_OUTPUT_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_inputs_file}" "${_args_in_outdir}"
  )

  set(KANAGAWA_OUTPUT "${_ARG_OUTPUT_DIR}/${_ARG_OUTPUT_PREFIX}")

  add_custom_command(
    OUTPUT "${_stamp}"
    BYPRODUCTS
      "${_args_in_outdir}"
    ${_pre_commands}
    COMMAND "$<TARGET_FILE:kanagawa::exe>"
            ${_ARG_OPTIONS}
            --output "${KANAGAWA_OUTPUT}"
            ${_ARG_SOURCES}
    COMMAND ${CMAKE_COMMAND} -E echo "Kanagawa HLS completed for '${target}'" > "${_stamp}"
    DEPENDS
      "$<TARGET_FILE:kanagawa::exe>"
      ${_ARG_SOURCES}
      "${_inputs_file}"
    WORKING_DIRECTORY "${_ARG_WORKING_DIRECTORY}"
    VERBATIM
    COMMAND_EXPAND_LISTS
  )

  # Phony target others can depend on
  add_custom_target(${target} DEPENDS "${_stamp}")
  add_dependencies(${target} kanagawa::exe)

  # Expose OUTPUT_DIR to caller
  set(${target}_OUTPUT_DIR "${_ARG_OUTPUT_DIR}" PARENT_SCOPE)

  # Optional: include OUTPUT_DIR in 'clean'
  set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${_ARG_OUTPUT_DIR}")
endfunction()
