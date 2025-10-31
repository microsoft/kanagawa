# Changelog
All notable changes to this project will be documented in this file.

## [1.1.0] - 2025-10-08
### Added
- Support for Windows
- Visual Studio Code extension
- Improve type of RISC-V decode_format function - Declare explicit return type of the decode_format function instead of auto so that generated documentation shows the return type with a link to its description.
- Robust test and release automation (CI) via Github Actions

### Fixed
- Fix overactive warning about missing transaction_size(N) attribute
- Disallow white spaces within module names

## [1.0.1] - 2025-10-08
### Added
- Add license comment to README.md

## [1.0.0] - 2025-10-08
### Added
- Initial public release of Kanagawa.
