#
# Check for and configure third-party dependencies.
#

cmake_policy(SET CMP0167 NEW)

####
# Boost version 1.74.0 or later
find_package(Boost 1.74 CONFIG REQUIRED)

# Pick the header target name that exists:
if (TARGET Boost::headers)
  set(_boost_hdr_target Boost::headers)
elseif (TARGET Boost::boost)
  set(_boost_hdr_target Boost::boost)
else()
  message(FATAL_ERROR "No Boost header target found in this Boost package.")
endif()

get_target_property(BOOST_INCLUDE_DIR ${_boost_hdr_target} INTERFACE_INCLUDE_DIRECTORIES)
message(STATUS "Dependency: Boost version ${Boost_VERSION} at ${BOOST_INCLUDE_DIR}/boost")

#
# pthreads

find_package(Threads REQUIRED)

####
# Verilator

# Users can override via: -DVERILATOR_EXE=/opt/verilator/bin/verilator
find_program(VERILATOR_EXE
  NAMES verilator
  HINTS
    "$ENV{VERILATOR_ROOT}/bin"
    "$ENV{HOME}/verilator/bin"
    /usr/local/bin
    /usr/bin
)
if (VERILATOR_EXE)
  message(STATUS "Dependency: Verilator at ${VERILATOR_EXE}")
  mark_as_advanced(VERILATOR_EXE)
else()
  message(WARNING "Verilator executable not found. Verilator-based tests will be disabled.")
endif()

# Haskell (cabal and ghc)
# Uses can override on the CMake command line via:
#   -DGHCUP_DIR=/home/user/.ghcup/bin
#   -DCABAL_EXE=/home/user/.ghcup/bin/cabal
#   -DGHC_EXE=/home/user/.ghcup/bin/ghc
if (NOT CABAL_EXE)
  find_program(CABAL_EXE
    NAMES cabal
    HINTS
      "${GHCUP_DIR}"
      "$ENV{CABAL_HOME}/bin"
      "$ENV{GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
      "$ENV{HOME}/.ghcup/bin"
      "/opt/.ghcup/bin"
    DOC "Path to the cabal executable"
  )
endif()

if (CABAL_EXE)
  message(STATUS "Dependency: cabal at ${CABAL_EXE}")
  mark_as_advanced(CABAL_EXE)
else()
  message(FATAL_ERROR "cabal not found. Install via ghcup or set CABAL_EXE.")
endif()

if (NOT GHC_EXE)
  find_program(GHC_EXE
    NAMES ghc
    HINTS
      "${GHCUP_DIR}"
      "$ENV{CABAL_HOME}/bin"
      "$ENV{GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
      "$ENV{HOME}/.ghcup/bin"
      "/opt/.ghcup/bin"
    DOC "Path to the ghc executable"
  )
endif()

if (GHC_EXE)
  message(STATUS "Dependency: ghc at ${GHC_EXE}")
  mark_as_advanced(GHC_EXE)

  # Get the actual GHC version by running ghc --numeric-version
  execute_process(
    COMMAND "${GHC_EXE}" --numeric-version
    OUTPUT_VARIABLE GHC_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE GHC_VERSION_RESULT
  )

  if (GHC_VERSION_RESULT EQUAL 0)
    set(GHC_NAME "ghc-${GHC_VERSION}")
    message(STATUS "GHC version: ${GHC_VERSION}, using GHC_NAME: ${GHC_NAME}")
  else()
    # Fallback to the original method if version detection fails
    get_filename_component(GHC_NAME "${GHC_EXE}" NAME_WE)
    message(WARNING "Could not determine GHC version, falling back to: ${GHC_NAME}")
  endif()
else()
  message(FATAL_ERROR "ghc not found. Install via ghcup or set GHC_EXE.")
endif()
