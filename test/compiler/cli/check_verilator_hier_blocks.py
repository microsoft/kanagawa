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

# Matches a SystemVerilog module declaration, e.g. `module Foo (`, capturing
# the module name.
MODULE_DECL = re.compile(r'(?:^|\s)module\s+(\w+)', re.MULTILINE)


def collect_modules(sv_files):
    """Return a list of (sv_name, module_name, body) for every module.

    Modules do not nest in SystemVerilog, so each module body is the text
    spanning from its `module name` declaration up to the next module
    declaration (or end of file). Slicing on declaration boundaries avoids
    relying on an `endmodule` token, which could otherwise appear inside a
    comment or string and truncate the body prematurely.
    """
    modules = []
    for sv in sv_files:
        text = sv.read_text()
        decls = list(MODULE_DECL.finditer(text))
        for i, decl in enumerate(decls):
            end = decls[i + 1].start() if i + 1 < len(decls) else len(text)
            modules.append((sv.name, decl.group(1), text[decl.start():end]))
    return modules


def has_hier_block_after_port_list(module_body):
    """Return True if HIER_BLOCK appears after the declaration port list."""
    port_list_end = module_body.find(');')
    if port_list_end == -1:
        return False
    return module_body.find(HIER_BLOCK, port_list_end + 2) != -1


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
        names = ', '.join(s.name for s in sv_files)
        print(f"No SystemVerilog modules found in {names}.")
        return 1

    with_meta = [m for m in modules if HIER_BLOCK in m[2]]
    with_meta_after_ports = [m for m in modules if has_hier_block_after_port_list(m[2])]
    without_meta = [m for m in modules if HIER_BLOCK not in m[2]]
    module_names = ', '.join(m[1] for m in modules)

    # Check 2: the metacomment must appear inside at least one module body.
    if not with_meta:
        print(
            f"--verilator-hier-blocks must emit {HIER_BLOCK!r} into a generated "
            f"module, but none of these modules contain it: {module_names}."
        )
        return 1

    if not with_meta_after_ports:
        print(
            f"--verilator-hier-blocks must emit {HIER_BLOCK!r} after a generated "
            "module's port list (after `);`), but no module matches this placement "
            f"requirement: {module_names}."
        )
        return 1

    # Check 3: the metacomment must be selective. The compiler emits several
    # non-exported helper modules (ESI wrapper, per-basic-block modules); these
    # must not carry the metacomment.
    if not without_meta:
        print(
            f"--verilator-hier-blocks should only annotate the exported design "
            f"module, but every generated module contains {HIER_BLOCK!r}: "
            f"{module_names}."
        )
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
