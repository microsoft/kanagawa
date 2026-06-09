#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Verify the output of a `--verilator-hier-blocks` compile.

Contract of the flag: every generated module should carry a
`/*verilator hier_block*/` metacomment, which enables hierarchical
Verilation. Verilator requires this metacomment to appear inside the
module body (after the `module name(...);` port list), so we check both
that the metacomment is present and that it follows a `module`
declaration.

Checks:
  1. At least one .sv file exists.
  2. The `/*verilator hier_block*/` metacomment appears in a generated
     .sv file, and it occurs after a `module` declaration (i.e. inside a
     module body, as Verilator requires).

Exits non-zero on failure.
"""
import argparse
import sys
from pathlib import Path

HIER_BLOCK = '/*verilator hier_block*/'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_dir', help='Directory containing compiler outputs')
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    if not output_dir.is_dir():
        print(f"Output directory does not exist: {output_dir}")
        return 1

    sv_files = sorted(output_dir.glob('*.sv'))
    if not sv_files:
        print("--verilator-hier-blocks should produce a .sv file, but none was found.")
        return 1

    for sv in sv_files:
        text = sv.read_text()
        idx = text.find(HIER_BLOCK)
        if idx == -1:
            continue
        # The metacomment must be inside a module body, so a `module`
        # keyword must precede it in the same file.
        if 'module' not in text[:idx]:
            print(
                f"{sv.name} contains {HIER_BLOCK!r} but not after a module declaration."
            )
            return 1
        return 0

    print(
        f"--verilator-hier-blocks must emit {HIER_BLOCK!r} into a generated module, "
        f"but none of {[s.name for s in sv_files]} contain it."
    )
    return 1


if __name__ == '__main__':
    sys.exit(main())
