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
else()
  message(WARNING "Verilator executable not found. Verilator-based tests will be disabled.")
endif()

