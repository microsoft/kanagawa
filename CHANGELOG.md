# Changelog
All NOTABLE changes to this project will be documented in this file.
Check the release summary for a detailed history based on commits.

## [Unreleased]

## [1.3.0] - 2026-09-21
### Added
- `list-deps` sub-command that enumerates the transitive source files by running parse + import resolution only. Suitable for fast dependency tracking from build systems (e.g. CMake configure-time staleness checks). Writes a plain list (one path per line) to `--file-list <path>` or stdout.
- `--skip-circt-lowering` compiler option to emit CIRCT MLIR without lowering it to SystemVerilog module bodies.
- `--verilator-hier-blocks` compiler option to annotate exported design modules for Verilator hierarchical compilation.
- A Dev Container configuration for a reproducible Kanagawa build environment in VS Code, Codespaces, and GitHub Copilot coding agent.

### Changed
- The CIRCT submodule advances 459 commits to add SystemVerilog package support. This may require updates to downstream integrations and build environments that depend on the previous CIRCT revision.
- `compile --file-list` now refreshes the dependency manifest immediately after parsing succeeds (instead of after codegen). Failed compiles (frontend or backend errors) still update the manifest, so build systems can pick up newly added imports without requiring a successful build.
- Named types in the CIRCT IR are now declared in an `sv.package` named `<ExportedClassName>CoreTypes` instead of a shared `hw.type_scope`. The generated SystemVerilog declares one package per exported class and refers to those types package-qualified.
- ESI wrapper ports now emit callbacks as outputs, represent `[[async]]` functions as channels, and use the ValidOnly signaling standard for `[[no_backpressure]]` functions and callbacks.

### Fixed
- Typedefs are no longer dropped when several exported classes are compiled into the same design. Previously every generated `.sv` file emitted its type declarations behind an identical `` `ifndef _TYPESCOPE_* `` include guard, so only the first file's declarations survived and the others referenced undeclared types (#138).
- `--identifier-length` now applies to generated basic-block module and container instance names, avoiding downstream tool name-length failures (#122).
- Float values are formatted correctly when simulated with Verilator.

## [1.2.0] - 2025-11-09
### Added
- MacOS (ARM64) support

### Fixed
- Add runtime/rtl to release package

## [1.1.0] - 2025-11-05
### Added
- Support for Windows
- Visual Studio Code extension
- Robust test and release automation (CI) via Github Actions
- Enhanced formatting of Kanagawa syntax in Sandcastle
- Auto-publish of library code-docs

### Fixed
- Fix overactive warning about missing transaction_size(N) attribute
- Disallow white spaces within module names
- Various small changes to unit tests that made it into the initial OSS release.

## [1.0.1] - 2025-10-08
### Added
- Add license comment to README.md

## [1.0.0] - 2025-10-08
### Added
- Initial public release of Kanagawa.
