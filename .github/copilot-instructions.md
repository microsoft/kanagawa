# Project Instructions

- Before compiler changes, read the relevant section of the [compiler design reference](../doc/compiler-design-reference.md). Preserve the Haskell frontend -> ParseTree FFI -> C++ middle end -> CIRCT/SystemVerilog backend ownership boundaries.
- Keep Haskell/C++ ParseTree FFI declarations, implementations, and bindings synchronized. Preserve source-location and type metadata when adding or translating nodes.
- Use the [programming guide](../doc/programming-guide.md), [effective Kanagawa](../doc/effective-kanagawa.md), and [hardware mapping guide](../doc/mapping-to-hardware.md) as the authorities for language semantics; do not infer semantics from a single test or implementation detail.
- Follow [BUILDING.md](../BUILDING.md) for dependencies, out-of-tree CMake/Ninja builds, and test targets. Build the smallest relevant target and run the narrowest matching `ctest -R` group or single test.
- Full tests are supported on Linux or WSL. On native Windows, build the compiler and related tools, but do not claim full test coverage.
- Treat `thirdparty/` as vendored code and avoid changing it unless the task explicitly targets a dependency.
