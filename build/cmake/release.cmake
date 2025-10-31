# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This file contains CMake configuration to build a release

install(
  PROGRAMS $<TARGET_FILE:kanagawa::exe>
  DESTINATION bin
  COMPONENT kanagawa
)
install(
  TARGETS kanagawa_lib risc-v-elf2mem
  DESTINATION bin
  COMPONENT kanagawa
)
install(
  PROGRAMS $<TARGET_FILE:sandcastle::exe>
  DESTINATION bin
  COMPONENT kanagawa
)
install(
  DIRECTORY "${CMAKE_SOURCE_DIR}/library/"
  DESTINATION library
  COMPONENT kanagawa
)
install(
  DIRECTORY ${BUILD_OUTPUT_DIR}/doc/
  DESTINATION doc
  COMPONENT kanagawa
)
install(
  FILES
    ${CMAKE_SOURCE_DIR}/LICENSE
    ${CMAKE_SOURCE_DIR}/VERSION
    ${CMAKE_SOURCE_DIR}/NOTICE.md
    ${CMAKE_SOURCE_DIR}/README.md
    ${CMAKE_SOURCE_DIR}/SECURITY.md
    ${CMAKE_SOURCE_DIR}/SUPPORT.md
    ${CMAKE_SOURCE_DIR}/CHANGELOG.md
  DESTINATION "."
  COMPONENT kanagawa
)

function(normalize_system_name_for_release INPUT_VALUE DEFAULT_VALUE OUT_VAR)
  set(_value "${INPUT_VALUE}")
  if(_value STREQUAL "")
    set(_value "${DEFAULT_VALUE}")
  endif()
  string(TOLOWER "${_value}" _value)
  string(REGEX REPLACE "[^a-z0-9]+" "-" _value "${_value}")
  string(REGEX REPLACE "^-+" "" _value "${_value}")
  string(REGEX REPLACE "-+$" "" _value "${_value}")
  if(_value STREQUAL "")
    set(_value "${DEFAULT_VALUE}")
  endif()
  set(${OUT_VAR} "${_value}" PARENT_SCOPE)
endfunction()

# Normalize platform identifiers for release naming
normalize_system_name_for_release("${CMAKE_SYSTEM_NAME}" "unknown-os" _release_system)
normalize_system_name_for_release("${CMAKE_SYSTEM_PROCESSOR}" "unknown-arch" _release_arch)

set(RELEASE_PLATFORM "${_release_system}-${_release_arch}")

set(RELEASE_NAME "${PROJECT_NAME}-${VERSION}-${RELEASE_PLATFORM}")

if(CMAKE_SYSTEM_NAME MATCHES "Windows")
  set(RELEASE_ARCHIVE_EXT "zip")
  set(RELEASE_ARCHIVE_FLAGS "cfv")
  set(RELEASE_ARCHIVE_FORMAT "--format=zip")
else()
  set(RELEASE_ARCHIVE_EXT "tar.gz")
  set(RELEASE_ARCHIVE_FLAGS "cfvz")
  set(RELEASE_ARCHIVE_FORMAT "--format=gnutar")
endif()

set(RELEASE_DIR "${CMAKE_BINARY_DIR}/${RELEASE_NAME}")
set(TARBALL_PATH "${CMAKE_BINARY_DIR}/${RELEASE_NAME}.${RELEASE_ARCHIVE_EXT}")

add_custom_target(prepare_release
  # Ensure everything that produces artifacts is built first
  DEPENDS kanagawa_runtime sandcastle_exe risc-v-elf2mem gendoc

  # Clean out anything left in the release directory
  COMMAND ${CMAKE_COMMAND} -E rm -rf "${RELEASE_DIR}"

  # Run install into the release directory
  COMMAND ${CMAKE_COMMAND} --install ${CMAKE_BINARY_DIR}
          --prefix "${RELEASE_DIR}"
          --component kanagawa
          --config $<CONFIG>

  # Create a platform-appropriate archive
  COMMAND ${CMAKE_COMMAND} -E tar "${RELEASE_ARCHIVE_FLAGS}"
    "${TARBALL_PATH}"
    ${RELEASE_ARCHIVE_FORMAT}
    "${RELEASE_NAME}"

  USES_TERMINAL
  COMMENT "Building prerequisites, installing to ${STAGE_DIR}, and creating a release package in ${TARBALL_PATH}"
)



