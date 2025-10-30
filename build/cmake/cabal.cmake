# Helpers for cabal builds

# Determine platform-specific architecture string for Cabal
if(WIN32)
    set(CABAL_ARCH_STRING "x86_64-windows")
elseif (APPLE)
    set(CABAL_ARCH_STRING "aarch64-darwin")
else()
    set(CABAL_ARCH_STRING "x86_64-linux")
endif()
