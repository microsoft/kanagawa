# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file contains CMake helper functions for cross-compiling RISC-V 64 code
# using the RISCV64 GCC toolchain.

set(RISCV_COREMARK_SUPPORTED_COMPILER_VERSION "SiFive GCC 10.1.0")

find_program(RISCV64_GCC_EXE 
  NAMES riscv64-unknown-elf-gcc riscv64-elf-gcc
  HINTS "${RISCV64_GCC}/bin" "${RISCV64_GCC}"
)

if (RISCV64_GCC_EXE)

  message(STATUS "Found RISCV-64 GCC compiler: ${RISCV64_GCC_EXE}")

  execute_process(
    COMMAND ${RISCV64_GCC_EXE} --version
    OUTPUT_VARIABLE RISCV_GCC_VERSION_OUTPUT
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE _version_result
  )

  if(_version_result EQUAL 0)
    string(FIND "${RISCV_GCC_VERSION_OUTPUT}" "${RISCV_COREMARK_SUPPORTED_COMPILER_VERSION}" _version_match)
    if(NOT _version_match EQUAL -1)
      set(RISCV_COREMARK_SCORE_ENABLED 1)
    endif()
  endif()

  if (NOT DEFINED RISCV_COREMARK_SCORE_ENABLED)
    string(REGEX MATCH "^[^\n\r]*" _riscv_gcc_version_first_line "${RISCV_GCC_VERSION_OUTPUT}")
    string(STRIP "${_riscv_gcc_version_first_line}" _riscv_gcc_version_first_line)
    message(STATUS "RISC-V CoreMark score check disabled. Unsupported compiler version: ${_riscv_gcc_version_first_line}. Supported version: ${RISCV_COREMARK_SUPPORTED_COMPILER_VERSION}")
  endif()

else()
  message(STATUS "Could not find RISCV-64 GCC toolchain. RISC-V unit tests disabled.")
endif()

# This executable creates .mem files from an executable in elf format
add_executable(risc-v-elf2mem "${CMAKE_SOURCE_DIR}/library/processor/risc_v/util/elf2mem.cpp")
set_target_properties(risc-v-elf2mem PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${BUILD_OUTPUT_BIN_DIR}"
)

# Helper function for running the RISCV64-GCC compiler to generate an elf binary
#
# Usage:
#   add_riscv_executable(<target>
#     [ARCH <risc-v-architecture>]
#     [LD_SCRIPT <linker-script>]
#     [OUTPUT_DIR <dir>]
#     [OPTIONS <opt1> <opt2> ...]
#     [DEFINITIONS <def1> <def2> ...]
#     [INCLUDE_DIRS <dir1> <dir2> ...]
#     [DEPENDS <dep1> <dep2> ...]
#   )
#
# target
#   A custom target that will be created and which can be used to build the ELF file or
#   as a dependency to another target.
#
# ARCH (optional)
#   Sets RISC-V architecture. Default: rv32i
#
# LD_SCRIPT (optional)
#   Specifies a script to pass to the linker to determine memory layout, entry point, etc.
#   Default: ${CMAKE_SOURCE_DIR}/library/processor/risc_v/util/ld-script
#
# ELF (optional)
#   The output elf file. Default: ${target}.elf in ${OUTPUT_DIR}
#
# OUTPUT_DIR (optional)
#   The directory in which the compiler will be run and to which byproduct (like .o) files will be
#   written. Default ${CMAKE_CURRENT_BINARY_DIR}
#
# SOURCES
#   Source files to compile
#
# OPTIONS (optional)
#   Compiler options.
#
# DEFINITIONS (optional)
#   A list of pre-processor definitions, for example `HAS_NO_STDOUT=1` (define and set value)
#   or `FOO` (define but set no value)
#
# INCLUDE_DIRS (optional)
#   Directories to add to the include path
#
# DEPENDS (optional)
#   Additional files to add to the DEPENDS for the CMake command that runs the RISCV64_GCC
#   compiler. In other words, additional dependencies. Note that these can not be CMake
#   targets (i.e. must be files). Note that the files in SOURCES are automatically added to
#   the list of dependencies.
#
function(add_riscv_executable target)
  set(_opts)
  set(_one  ARCH LD_SCRIPT ELF OUTPUT_DIR)
  set(_multi SOURCES OPTIONS DEFINITIONS INCLUDE_DIRS DEPENDS)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT target)
    message(FATAL_ERROR "add_riscv_executable: missing <target> as first argument.")
  endif()

  if (_ARG_ELF)
    if (IS_ABSOLUTE "${_ARG_ELF}")
      set(ELF_FILE ${_ARG_ELF})
    else()
      get_filename_component(ELF_FILE "${CMAKE_CURRENT_BINARY_DIR}/${_ARG_ELF}" ABSOLUTE)
    endif()
  else()
    set(ELF_FILE ${CMAKE_CURRENT_BINARY_DIR}/${target}.elf)
  endif()

  if (NOT _ARG_ARCH)
    set(_ARG_ARCH rv32i_zicsr)
  endif()

  if (NOT _ARG_LD_SCRIPT)
    set(_ARG_LD_SCRIPT "${CMAKE_SOURCE_DIR}/library/processor/risc_v/util/ld-script")
  endif()

  if (NOT _ARG_OUTPUT_DIR)
    set(_ARG_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}")
  endif()

  if (NOT _ARG_SOURCES)
    message(FATAL_ERROR "add_riscv_executable: missing SOURCES.")
  endif()

  if (NOT RISCV64_GCC_EXE)
      message(FATAL_ERROR "Could not find RISCV-64 GCC toolchain. Please set RISCV64_GCC to the toolchain root directory or the GCC executable.")
  endif()

  set(RISCV_MABI ilp32)

  execute_process(
    COMMAND ${RISCV64_GCC_EXE} -march=rv32i -mabi=${RISCV_MABI} -print-libgcc-file-name
    OUTPUT_VARIABLE RISCV_LIBGCC
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE _r
  )

  if(NOT _r EQUAL 0 OR RISCV_LIBGCC STREQUAL "" OR RISCV_LIBGCC MATCHES "^libgcc\\.a$")
    message(FATAL_ERROR "Failed to query libgcc path from ${RISCV64_GCC_EXE}. Got: '${RISCV_LIBGCC}'")
  endif()

  get_filename_component(RISCV_GCC_MULTILIB_DIR "${RISCV_LIBGCC}" DIRECTORY)

  set(COREMARK_DIR "${CMAKE_SOURCE_DIR}/thirdparty/coremark")

  list(FIND _ARG_OPTIONS "-nostartfiles" no_start)
  list(FIND _ARG_OPTIONS "-nostdlib" no_stdlib)
  list(FIND _ARG_DEFINITIONS "HAS_NO_STDOUT=1" no_stdout)

  if(
    ${no_stdlib} EQUAL -1
    AND ${no_start} EQUAL -1
    AND ${no_stdout} EQUAL -1
  )
    set(SOURCES
      "${COREMARK_DIR}/barebones/ee_printf.c"
      "${COREMARK_DIR}/barebones/cvt.c"
    )
  endif()

  set(GCC_ARGS
    "-march=${_ARG_ARCH}"
    "-mno-div"
    "-mabi=${RISCV_MABI}"
    "-mbranch-cost=1"
    "-mno-strict-align"
    "-I${COREMARK_DIR}"
    "-I${COREMARK_DIR}/barebones"
    "-L${RISCV_GCC_MULTILIB_DIR}"
    "-Wl,--script=${_ARG_LD_SCRIPT}"
    "-mtune=sifive-3-series"
  )

  # Append caller-supplied options so they can override our defaults if needed
  if (_ARG_OPTIONS)
    list(APPEND GCC_ARGS ${_ARG_OPTIONS})
  endif()

  foreach(def IN LISTS _ARG_DEFINITIONS)
    list(APPEND GCC_ARGS "-D${def}")
  endforeach()

  foreach(inc IN LISTS _ARG_INCLUDE_DIRS)
    # Turn relative paths into absolute (leaves alone if already absolute)
    get_filename_component(abs_inc ${inc} ABSOLUTE)

    list(APPEND GCC_ARGS "-I${abs_inc}")
  endforeach()

  foreach(src IN LISTS _ARG_SOURCES)
    get_filename_component(abs_src ${src} ABSOLUTE)
    list(APPEND SOURCES ${abs_src})
    list(APPEND _ARG_DEPENDS ${abs_src})
  endforeach()

  # Run the compiler + linker
  add_custom_command(
    OUTPUT ${ELF_FILE}
    COMMAND ${RISCV64_GCC_EXE} ${GCC_ARGS} -o ${ELF_FILE} ${SOURCES}
    COMMENT "${RISCV64_GCC_EXE} ${GCC_ARGS} -o ${ELF_FILE} ${SOURCES}"
    DEPENDS ${_ARGS_DEPENDS}
  )

  add_custom_target(${target}
      DEPENDS "${ELF_FILE}"
  )

  set_target_properties(${target}
    PROPERTIES
      ELF_FILE "${ELF_FILE}"
  )

endfunction()

# Runs risc-v-elf2mem to convert an ELF file into IMEM and DMEM mems files
# Usage:
#   add_riscv_executable_mem <target> <elf-file>
#
# elf
#   The elf file that will be converted to mems
#
# IMEM_OUT_VAR (optional)
#   Name of a variable in parent scope that will be populated with the
#   path to the generated IMEM .mem file.
#
# DMEM_OUT_VAR (optional)
#   Name of a variable in parent scope that will be populated with the
#   path to the generated DMEM .mem file.
#
# OUTPUT_DIR (optional)
#   The directory to which the .mem files will be written.
#   Default: ${CMAKE_CURRENT_BINARY_DIR}
#
# TARGET (optional)
#   If provided, a custom target will be created with a dependency on the
#   generated .mem files. For convenience, the function also sets IMEM and
#   DMEM properties on this target.
#
function(add_riscv_elf_to_mem elf)
  set(_opts)
  set(_one  OUTPUT_DIR TARGET IMEM_OUT_VAR DMEM_OUT_VAR)
  set(_multi)
  cmake_parse_arguments(_ARG "${_opts}" "${_one}" "${_multi}" ${ARGN})

  if (NOT elf)
    message(FATAL_ERROR "add_riscv_elf_to_mem: missing <elf> name as first argument.")
  endif()

  get_filename_component(elf_basename "${elf}" NAME)

  if (NOT _ARG_OUTPUT_DIR)
    set(_ARG_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
  endif()

  SET(imem_file "${_ARG_OUTPUT_DIR}/${elf_basename}.imem.mem")
  SET(dmem_file "${_ARG_OUTPUT_DIR}/${elf_basename}.dmem.mem")

  add_custom_command(
    OUTPUT ${imem_file} ${dmem_file}
    COMMAND
      $<TARGET_FILE:risc-v-elf2mem> ${elf} 2147483648 2149580800
    COMMENT "$<TARGET_FILE:risc-v-elf2mem> ${elf} 2147483648 2149580800"
    WORKING_DIRECTORY ${_ARG_OUTPUT_DIR}
    DEPENDS ${elf}
  )

  # Return the paths to the caller.
  if (_ARG_IMEM_OUT_VAR)
    set(${_ARG_IMEM_OUT_VAR} "${imem_file}" PARENT_SCOPE)
  endif()
  if (_ARG_DMEM_OUT_VAR)
    set(${_ARG_DMEM_OUT_VAR} "${dmem_file}" PARENT_SCOPE)
  endif()

  if (_ARG_TARGET)
    add_custom_target(${_ARG_TARGET} DEPENDS ${imem_file} ${dmem_file})

    set_target_properties(${_ARG_TARGET} PROPERTIES
      IMEM_FILE "${imem_file}"
      DMEM_FILE "${dmem_file}"
    )
  endif()
endfunction()