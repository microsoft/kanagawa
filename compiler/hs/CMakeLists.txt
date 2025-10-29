# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# kanagawa - The Kanagawa compiler executable.
# This executable is built by building the front-end code using the Haskell build
# tool cabal. It copies this executable and the dependent shared library into
# a common folder.
#
# Projects that need to run the Kanagawa compiler can do this:
#
# # Ensure Kanagawa is built
# add_dependencies(my_target kanagawa_runtime)
#
# # Get the path to the Kanagawa executable and use it, with a dependency to make sure it's built
# add_custom_command(
#   COMMAND "$<TARGET_FILE:kanagawa::exe>" ...
#   DEPENDS ${KANAGAWA_RUNTIME_TARGETS}
#   ...
# )

# Set the directory where cabal will be store intermediate files
set(CABAL_WORKING_DIR ${CMAKE_CURRENT_BINARY_DIR}/temp)
file(MAKE_DIRECTORY ${CABAL_WORKING_DIR})

#
# Build the kanagawa exe using cabal

# Where cabal will output the executable (a stable path)
set(KANAGAWA_EXE_DEST "${BUILD_OUTPUT_BIN_DIR}/kanagawa${CMAKE_EXECUTABLE_SUFFIX}")

# This is needed to avoid generator expressions in OUTPUT
set(KANAGAWA_LIB_NAME "${CMAKE_SHARED_LIBRARY_PREFIX}${KANAGAWA_SHARED_LIBRARY_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}")
set(KANAGAWA_LIB_DEST "${BUILD_OUTPUT_BIN_DIR}/${KANAGAWA_LIB_NAME}")

set(CABAL_OPTIONS
    "--with-compiler=${GHC_EXE}"
    "--extra-lib-dirs=$<TARGET_FILE_DIR:kanagawa_lib>"
    "--builddir=${CABAL_WORKING_DIR}/dist"
    "--logs-dir=${CABAL_WORKING_DIR}/logs"
)

# Get the Boost include directory path
get_target_property(BOOST_INCLUDE_DIRS Boost::headers INTERFACE_INCLUDE_DIRECTORIES)

foreach(_inc ${BOOST_INCLUDE_DIRS})
    list(APPEND CABAL_OPTIONS "--extra-include-dirs=${_inc}")
endforeach()

# Tell CMake about the source file dependencies of the cabal build

file(GLOB_RECURSE KANAGAWA_HS_SOURCES CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/*.hs"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.lhs"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.hsc"
)

set(KANAGAWA_COMPILER_DIR "${CMAKE_SOURCE_DIR}/compiler")

file(GLOB KANAGAWA_CABAL_FILES CONFIGURE_DEPENDS
    "${KANAGAWA_COMPILER_DIR}*.cabal"
    "${KANAGAWA_COMPILER_DIR}/cabal.project"
    "${KANAGAWA_COMPILER_DIR}/cabal.project.local"
    "${KANAGAWA_COMPILER_DIR}/cabal.project.freeze"
)

list(APPEND KANAGAWA_HS_SOURCES ${KANAGAWA_CABAL_FILES})

# Determine platform-specific architecture string for Cabal
if(WIN32)
    set(CABAL_ARCH_STRING "x86_64-windows")
else()
    set(CABAL_ARCH_STRING "x86_64-linux")
endif()

# Build the kanagawa executable and copy it to the dist/bin directory
add_custom_command(
    OUTPUT ${KANAGAWA_EXE_DEST}
    COMMAND ${CMAKE_COMMAND} -E echo "Building kanagawa exe with Cabal"
    COMMAND ${CMAKE_COMMAND} -E env
        TMP=${CABAL_WORKING_DIR}
        TEMP=${CABAL_WORKING_DIR}
        ${CABAL_EXE} ${CABAL_OPTIONS} v2-build exe:kanagawa
    COMMAND ${CMAKE_COMMAND} -E make_directory "${BUILD_OUTPUT_BIN_DIR}"
    COMMAND
        ${CMAKE_COMMAND} -E copy_if_different
        "${CABAL_WORKING_DIR}/dist/build/${CABAL_ARCH_STRING}/${GHC_NAME}/kanagawa-1.0.0/x/kanagawa/build/kanagawa/kanagawa${CMAKE_EXECUTABLE_SUFFIX}"
        "${KANAGAWA_EXE_DEST}"
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/compiler
    DEPENDS ${KANAGAWA_HS_SOURCES} kanagawa_lib
    VERBATIM
    USES_TERMINAL
)

# Phony target representing presence of the exe file and the shared library
add_custom_target(kanagawa_exe DEPENDS "${KANAGAWA_EXE_DEST}")

# Copy the shared library next to the exe; re-runs whenever the lib changes
add_custom_command(
    OUTPUT "${KANAGAWA_LIB_DEST}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "$<SHELL_PATH:${BUILD_OUTPUT_BIN_DIR}>"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "$<TARGET_FILE:kanagawa_lib>"
        "$<SHELL_PATH:${KANAGAWA_LIB_DEST}>"
  # Re-run when the actual built lib changes, and only after the exe exists (dir ready)
    DEPENDS "$<TARGET_FILE:kanagawa_lib>" kanagawa_exe
    VERBATIM
    USES_TERMINAL
)

add_custom_target(kanagawa_lib_inplace
    DEPENDS "${KANAGAWA_LIB_DEST}"
)

# Phony target that represents both the exe and the shared library
add_custom_target(kanagawa_runtime)
add_dependencies(kanagawa_runtime ${KANAGAWA_RUNTIME_TARGETS})

# IMPORTED executable for convenient references. You can get the path to the exe with
# $<TARGET_FILE:kanagawa::exe>
# Note that a target should never depend on kanagawa::exe
# because CMake does not know how to rebuild it
# Instead, depend on kanagawa_runtime (for target level dependencies)
# or ${KANAGAWA_RUNTIME_TARGETS} (for add_custom_command)
add_executable(kanagawa::exe IMPORTED GLOBAL)
set_target_properties(kanagawa::exe PROPERTIES
    IMPORTED_LOCATION "${KANAGAWA_EXE_DEST}"
)
