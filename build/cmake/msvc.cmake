# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.


# Generally applicable settings for MSVC. It is useful
# to have these as a config file rather than putting them in
# the root CMakeLists.txt, because not every component may want
# these settings.


if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    set(CMAKE_C_FLAGS "/nologo /DWIN32")
    set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS}")

    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

    set(CMAKE_C_FLAGS_DEBUG             "/MTd /Od /D_DEBUG /Zi /RTC1")
    set(CMAKE_C_FLAGS_MINSIZEREL        "/MT /O1 /Ob1 /DNDEBUG")
    set(CMAKE_C_FLAGS_RELEASE           "/MT /O2 /Ob2 /DNDEBUG")
    set(CMAKE_C_FLAGS_RELWITHDEBINFO    "/MT /Zi /O2 /Ob1 /DNDEBUG /Zi")

    set(CMAKE_CXX_FLAGS_DEBUG          ${CMAKE_C_FLAGS_DEBUG})
    set(CMAKE_CXX_FLAGS_MINSIZEREL     ${CMAKE_C_FLAGS_MINSIZEREL})
    set(CMAKE_CXX_FLAGS_RELEASE        ${CMAKE_C_FLAGS_RELEASE})
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO ${CMAKE_C_FLAGS_RELWITHDEBINFO})
endif()