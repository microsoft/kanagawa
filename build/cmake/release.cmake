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

set(RELEASE_NAME "${PROJECT_NAME}-${VERSION}")
set(RELEASE_DIR "${CMAKE_BINARY_DIR}/${RELEASE_NAME}")
set(TARBALL_PATH "${CMAKE_BINARY_DIR}/${RELEASE_NAME}.tar.gz")

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

  # Create a tar.gz archive
  COMMAND ${CMAKE_COMMAND} -E tar "cfvz"
          "${TARBALL_PATH}"
          --format=gnutar
          "${RELEASE_NAME}"

  USES_TERMINAL
  COMMENT "Building prerequisites, installing to ${STAGE_DIR}, and creating a release package in ${TARBALL_PATH}"
)



