#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Checks that each exported class gets its own SystemVerilog type package.

Regression test for issue #138: the CIRCT type declarations used to be emitted
into every generated .sv file behind a single shared ``ifndef _TYPESCOPE_*``
include guard. Because the guard name was identical in every file but the
contents differed, only the first file processed by a downstream tool kept its
typedefs and every other file ended up referencing undeclared types.

They are now emitted as an ``sv.package`` named after the exported class, so
every file declares its own package and nothing is dropped.

The assertions here are deliberately per-file rather than using
``check_generated_sv_modules.check_modules``: that helper's assertions are
existential ("expect *any* module to match"), which would still pass in exactly
the buggy scenario this test pins down, where one file is correct and the rest
are not. Only the parsing is shared.
"""
import re
import sys
from pathlib import Path

from check_generated_sv_modules import collect_blocks

TYPEDEF_DECL = re.compile(r'^\s*typedef\s+.*?\b(\w+);$', re.MULTILINE)

# Named types each exported class needs declared in its own package, and the
# type on its ports. 'Op' and 'Header' only appear nested inside the payload
# structs, where the package qualifier is not used.
#
# The TemplateSender entries are the shape issue #138 was reported with: each
# instantiation declares a same-named type of a different width, so dropping
# one file's declarations yields a silently wrong-width reference.
EXPECTED_TYPEDEFS = {
    'SmallSender': {'Op', 'Header', 'SmallPayload'},
    'BigSender': {'Op', 'Header', 'BigPayload'},
    'TemplateSender___8__': {'Op', 'Header', 'TemplatePayload___8__'},
    'TemplateSender___64__': {'Op', 'Header', 'TemplatePayload___64__'},
}
PORT_TYPES = {
    'SmallSender': 'SmallPayload',
    'BigSender': 'BigPayload',
    'TemplateSender___8__': 'TemplatePayload___8__',
    'TemplateSender___64__': 'TemplatePayload___64__',
}

# Widths the template instantiations must declare, keyed by exported class.
# Pinned explicitly so that two files declaring the same width (the #138
# failure mode) is caught even though the type names differ.
EXPECTED_WIDTHS = {
    'TemplateSender___8__': '[7:0]',
    'TemplateSender___64__': '[63:0]',
}


def main():
    if len(sys.argv) != 2:
        print("Usage: check_multi_export_type_packages.py <output_dir>")
        return 1

    output_dir = Path(sys.argv[1])
    if not output_dir.is_dir():
        print(f"Output directory does not exist: {output_dir}")
        return 1

    # '<prefix><ExportedClassName>_types.sv' holds the '<ExportedClassName>Types'
    # package, which the SystemVerilog backend writes directly rather than
    # routing through CIRCT. It is a separate, pre-existing feature; only the
    # CIRCT-generated designs are checked here.
    sv_files = sorted(p for p in output_dir.glob('*.sv') if not p.name.endswith('_types.sv'))
    if len(sv_files) != len(EXPECTED_TYPEDEFS):
        raise RuntimeError(
            f"expected {len(EXPECTED_TYPEDEFS)} generated designs, found {[p.name for p in sv_files]}"
        )

    packages_by_file = {}
    for filename, name, body in collect_blocks(sv_files, 'package', end_keyword='endpackage'):
        packages_by_file.setdefault(filename, []).append((name, body))

    seen_packages = {}

    for sv in sv_files:
        text = sv.read_text()

        if '_TYPESCOPE_' in text:
            raise RuntimeError(f"{sv.name} still uses a shared typescope include guard")

        # Require an unambiguous match, so that one exported class name being a
        # suffix of another cannot silently check the wrong expectations.
        matches = [name for name in EXPECTED_TYPEDEFS if sv.name.endswith(f"{name}.sv")]
        if len(matches) != 1:
            raise RuntimeError(f"{sv.name} matched exported classes {matches}, expected exactly one")
        exported = matches[0]

        packages = packages_by_file.get(sv.name, [])
        declared = [name for name, _ in packages]
        if declared != [f"{exported}CoreTypes"]:
            raise RuntimeError(f"{sv.name} declares packages {declared}, expected ['{exported}CoreTypes']")

        package, body = packages[0]
        if package in seen_packages:
            raise RuntimeError(f"package {package} is declared by both {seen_packages[package]} and {sv.name}")
        seen_packages[package] = sv.name

        missing = EXPECTED_TYPEDEFS[exported] - set(TYPEDEF_DECL.findall(body))
        if missing:
            raise RuntimeError(f"{sv.name}: package {package} is missing typedefs {sorted(missing)}")

        # Each template instantiation must declare its own width. Sharing one
        # declaration between instantiations is the #138 failure mode.
        width = EXPECTED_WIDTHS.get(exported)
        if width is not None and width not in body:
            raise RuntimeError(f"{sv.name}: package {package} does not declare a {width} payload:\n{body.strip()}")

        # The port type must be referenced package-qualified from module scope.
        qualified = f"{package}::{PORT_TYPES[exported]}"
        if qualified not in text:
            raise RuntimeError(f"{sv.name}: expected a reference to {qualified}")

    return 0


if __name__ == '__main__':
    sys.exit(main())
