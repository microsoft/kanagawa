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

set(CABAL_OPTIONS
    "--with-compiler=${GHC_EXE}"
    "--extra-lib-dirs=$<TARGET_FILE_DIR:kanagawa_lib>"
    "--builddir=${CABAL_WORKING_DIR}/dist"
    "--logs-dir=${CABAL_WORKING_DIR}/logs"
)

# Conditionally add Haskell defines for CMAKE_PROJECT_VERSION_MAJOR, CMAKE_PROJECT_VERSION_MINOR, CMAKE_PROJECT_VERSION_PATCH
if(CMAKE_PROJECT_VERSION_MAJOR AND NOT CMAKE_PROJECT_VERSION_MAJOR STREQUAL "")
    list(APPEND CABAL_OPTIONS
        "--ghc-options=-DRELEASE_VERSION_MAJOR=${CMAKE_PROJECT_VERSION_MAJOR}")
endif()
if(CMAKE_PROJECT_VERSION_MINOR AND NOT CMAKE_PROJECT_VERSION_MINOR STREQUAL "")
    list(APPEND CABAL_OPTIONS
        "--ghc-options=-DRELEASE_VERSION_MINOR=${CMAKE_PROJECT_VERSION_MINOR}")
endif()
if(CMAKE_PROJECT_VERSION_PATCH AND NOT CMAKE_PROJECT_VERSION_PATCH STREQUAL "")
    list(APPEND CABAL_OPTIONS
        "--ghc-options=-DRELEASE_VERSION_PATCH=${CMAKE_PROJECT_VERSION_PATCH}")
endif()

# Turn BUILD_CHANNEL into a numeric value so we can get it into Haskell via the command-line
set(BUILD_CHANNEL_NUM 0)
if (BUILD_CHANNEL AND NOT BUILD_CHANNEL STREQUAL "")
    if(BUILD_CHANNEL STREQUAL "nightly")
        set(BUILD_CHANNEL_NUM 1)
    elseif(BUILD_CHANNEL STREQUAL "release")
        set(BUILD_CHANNEL_NUM 2)
    endif()
endif()

list(APPEND CABAL_OPTIONS
    "--ghc-options=-DBUILD_CHANNEL=${BUILD_CHANNEL_NUM}")

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

list(APPEND KANAGAWA_HS_SOURCES "${KANAGAWA_COMPILER_DIR}/kanagawa.cabal")

# Include cabal helpers
include(${CMAKE_SOURCE_DIR}/build/cmake/cabal.cmake)

# Build the kanagawa executable and copy it to the dist/bin directory
set(KANAGAWA_BUILD_COMMANDS
    COMMAND ${CMAKE_COMMAND} -E echo "Building kanagawa exe with Cabal"
)

# On macOS, copy the shared library into the cabal build directory before building
if(APPLE)
    list(APPEND KANAGAWA_BUILD_COMMANDS
        COMMAND ${CMAKE_COMMAND} -E copy_if_different "$<TARGET_FILE:kanagawa_lib>" "${CABAL_WORKING_DIR}/dist/build/${CABAL_ARCH_STRING}/${GHC_NAME}/kanagawa-1.0.0/x/kanagawa/build/kanagawa/$<TARGET_FILE_NAME:kanagawa_lib>"
    )
endif()

list(APPEND KANAGAWA_BUILD_COMMANDS
    COMMAND ${CMAKE_COMMAND} -E env
        TMP=${CABAL_WORKING_DIR}
        TEMP=${CABAL_WORKING_DIR}
        ${CABAL_EXE} ${CABAL_OPTIONS} v2-build exe:kanagawa
    COMMAND ${CMAKE_COMMAND} -E make_directory "${BUILD_OUTPUT_BIN_DIR}"
    COMMAND
        ${CMAKE_COMMAND} -E copy_if_different
        "${CABAL_WORKING_DIR}/dist/build/${CABAL_ARCH_STRING}/${GHC_NAME}/kanagawa-1.0.0/x/kanagawa/build/kanagawa/kanagawa${CMAKE_EXECUTABLE_SUFFIX}"
        "${KANAGAWA_EXE_DEST}"
)

add_custom_command(
    OUTPUT ${KANAGAWA_EXE_DEST}
    ${KANAGAWA_BUILD_COMMANDS}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/compiler
    DEPENDS ${KANAGAWA_HS_SOURCES} kanagawa_lib
    VERBATIM
    USES_TERMINAL
    COMMAND_EXPAND_LISTS
)

# Phony target representing presence of the exe file and the shared library
add_custom_target(kanagawa_exe DEPENDS "${KANAGAWA_EXE_DEST}")

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
