#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Verify the output of a `--verilator-hier-blocks` compile.

Contract of the flag: the exported design (core) module should carry a
`/*verilator hier_block*/` metacomment, which enables hierarchical
Verilation. Verilator requires this metacomment to appear inside the
module body (after the `module name(...);` port list). The metacomment is
*selective*: it is only emitted on the core design module, not on the
non-exported helper modules that the compiler generates alongside it (the
ESI wrapper, the per-basic-block modules, etc.).

Checks:
  1. At least one .sv file exists.
  2. The `/*verilator hier_block*/` metacomment appears inside at least one
     generated module body (i.e. after that module's `module name(...);`
     declaration, as Verilator requires).
  3. At least one other generated module exists that does *not* contain the
     metacomment, proving the annotation is confined to the exported design
     module and is not blanket-applied to non-exported modules.

Exits non-zero on failure.
"""
import argparse
import re
import sys
from pathlib import Path

HIER_BLOCK = '/*verilator hier_block*/'

# Matches a SystemVerilog module body, capturing the module name. Modules do
# not nest, so a non-greedy match up to the first `endmodule` is sufficient.
MODULE_BODY = re.compile(
    r'(?:^|\s)module\s+(\w+).*?\bendmodule\b', re.DOTALL)


def collect_modules(sv_files):
    """Return a list of (sv_name, module_name, body) for every module."""
    modules = []
    for sv in sv_files:
        text = sv.read_text()
        for match in MODULE_BODY.finditer(text):
            modules.append((sv.name, match.group(1), match.group(0)))
    return modules


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

    modules = collect_modules(sv_files)
    if not modules:
        print(f"No SystemVerilog modules found in {[s.name for s in sv_files]}.")
        return 1

    with_meta = [m for m in modules if HIER_BLOCK in m[2]]
    without_meta = [m for m in modules if HIER_BLOCK not in m[2]]

    # Check 2: the metacomment must appear inside at least one module body.
    if not with_meta:
        print(
            f"--verilator-hier-blocks must emit {HIER_BLOCK!r} into a generated "
            f"module, but none of {[m[1] for m in modules]} contain it."
        )
        return 1

    # Check 3: the metacomment must be selective. The compiler emits several
    # non-exported helper modules (ESI wrapper, per-basic-block modules); these
    # must not carry the metacomment.
    if not without_meta:
        print(
            f"--verilator-hier-blocks should only annotate the exported design "
            f"module, but every generated module "
            f"{[m[1] for m in modules]} contains {HIER_BLOCK!r}."
        )
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
