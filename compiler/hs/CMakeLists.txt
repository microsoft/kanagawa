# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# kanagawa - The Kanagawa compiler executable.
# This executable is built by building the front-end code using the Haskell build
# tool cabal. It links the compiler back-end shared library (kanagawac).

# Set the directory where cabal will be store intermediate files
set(CABAL_WORKING_DIR ${CMAKE_BINARY_DIR}/kanagawa_temp)
file(MAKE_DIRECTORY ${CABAL_WORKING_DIR})

# Get the Boost include directory path
get_target_property(BOOST_INCLUDE_DIRS Boost::headers INTERFACE_INCLUDE_DIRECTORIES)

set(CABAL_OPTIONS
    "--with-compiler=${GHC_EXE}"
    "--extra-lib-dirs=${CMAKE_BINARY_DIR}/compiler/cpp"
    "--builddir=${CABAL_WORKING_DIR}/dist"
    "--logs-dir=${CABAL_WORKING_DIR}/logs"
    "--extra-include-dirs=${BOOST_INCLUDE_DIRS}"
    "v2-build"
    "exe:kanagawa"
)

add_custom_target(
    kanagawa
    ${CMAKE_COMMAND} -E env TMP=${CABAL_WORKING_DIR}
    TEMP=${CABAL_WORKING_DIR} ${CABAL_EXE} ${CABAL_OPTIONS}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/compiler
    VERBATIM
)

file(MAKE_DIRECTORY ${KANAGAWA_BIN_DIR})

add_custom_command(
    TARGET kanagawa
    POST_BUILD
    COMMAND
        ${CMAKE_COMMAND} -E copy_if_different
        "${CABAL_WORKING_DIR}/dist/build/x86_64-linux/${GHC_NAME}/kanagawa-1.0.0/x/kanagawa/build/kanagawa/kanagawa"
        "${KANAGAWA_BIN_DIR}/"
)

# Ensure that kanagawac is built before kanagawa is built
add_dependencies(kanagawa kanagawac)
