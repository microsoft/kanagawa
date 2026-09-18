# Changelog
All NOTABLE changes to this project will be documented in this file.
Check the release summary for a detailed history based on commits.

## [Unreleased]
### Added
- `list-deps` sub-command that enumerates the transitive source files by running parse + import resolution only. Suitable for fast dependency tracking from build systems (e.g. CMake configure-time staleness checks). Writes a plain list (one path per line) to `--file-list <path>` or stdout.

### Changed
- `compile --file-list` now refreshes the dependency manifest immediately after parsing succeeds (instead of after codegen). Failed compiles (frontend or backend errors) still update the manifest, so build systems can pick up newly added imports without requiring a successful build.
- Named types in the CIRCT IR are now declared in an `sv.package` named `<ExportedClassName>CoreTypes` instead of a shared `hw.type_scope`. The generated SystemVerilog declares one package per exported class and refers to those types package-qualified.

### Fixed
- Typedefs are no longer dropped when several exported classes are compiled into the same design. Previously every generated `.sv` file emitted its type declarations behind an identical `` `ifndef _TYPESCOPE_* `` include guard, so only the first file's declarations survived and the others referenced undeclared types (#138).

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
