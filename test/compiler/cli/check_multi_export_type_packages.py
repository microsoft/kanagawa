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
"""
import re
import sys
from pathlib import Path

# A generated design file is <prefix><ExportedClassName>.sv; the package it
# declares must be named after that exported class.
PACKAGE_DECL = re.compile(r'^package\s+(\w+);', re.MULTILINE)
TYPEDEF_DECL = re.compile(r'^\s*typedef\s+.*?\b(\w+);$', re.MULTILINE)


def package_body(text, name):
    start = re.search(rf'^package\s+{name};$', text, re.MULTILINE)
    end = re.compile(r'^endpackage', re.MULTILINE).search(text, start.end())
    if end is None:
        raise RuntimeError(f"package {name} has no 'endpackage'")
    return text[start.end():end.start()]


def main():
    output_dir = Path(sys.argv[1])

    # The hand-written '<prefix><ExportedClassName>_types.sv' files are a
    # separate, pre-existing feature; only the generated designs are checked.
    sv_files = sorted(p for p in output_dir.glob('*.sv') if not p.name.endswith('_types.sv'))
    if len(sv_files) != 2:
        raise RuntimeError(f"expected two generated designs, found {[p.name for p in sv_files]}")

    expected = {
        'SmallSender': {'Op', 'Header', 'SmallPayload'},
        'BigSender': {'Op', 'Header', 'BigPayload'},
    }

    # Types on the module ports; references to these from module scope must be
    # qualified with the package name. 'Op' and 'Header' only appear nested
    # inside the payload structs, where the package qualifier is not used.
    port_types = {
        'SmallSender': 'SmallPayload',
        'BigSender': 'BigPayload',
    }

    seen_packages = {}

    for sv in sv_files:
        text = sv.read_text()

        if '_TYPESCOPE_' in text:
            raise RuntimeError(f"{sv.name} still uses a shared typescope include guard")

        exported = next((name for name in expected if sv.name.endswith(f"{name}.sv")), None)
        if exported is None:
            raise RuntimeError(f"unexpected generated design {sv.name}")

        packages = PACKAGE_DECL.findall(text)
        if packages != [f"{exported}CoreTypes"]:
            raise RuntimeError(f"{sv.name} declares packages {packages}, expected ['{exported}CoreTypes']")

        package = packages[0]
        if package in seen_packages:
            raise RuntimeError(f"package {package} is declared by both {seen_packages[package]} and {sv.name}")
        seen_packages[package] = sv.name

        typedefs = set(TYPEDEF_DECL.findall(package_body(text, package)))
        missing = expected[exported] - typedefs
        if missing:
            raise RuntimeError(f"{sv.name}: package {package} is missing typedefs {sorted(missing)}")

        # The port type must be referenced package-qualified from module scope.
        qualified = f"{package}::{port_types[exported]}"
        if qualified not in text:
            raise RuntimeError(f"{sv.name}: expected a reference to {qualified}")

    return 0


if __name__ == '__main__':
    sys.exit(main())
