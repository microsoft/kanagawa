#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Verify the outputs of a `--skip-circt-lowering` compile.

Contract of the flag: the CIRCT MLIR file is emitted, and the CIRCT
lowering passes (which translate that MLIR into SystemVerilog module
bodies) are NOT run. Kanagawa is still free to write other artifacts --
including header comments and directives in the .sv file -- it just must
not append any CIRCT-lowered SystemVerilog into them.

Checks:
  1. A .mlir file exists and contains characteristic CIRCT / Kanagawa
     dialect ops, proving the CIRCT IR was emitted.
  2. No .sv file declares the top-level module that CIRCT lowering
     would have produced for the test design (class `Main`). Other SV
     output (header comments, directives, *_types.sv stubs, etc.) is
     allowed -- we only assert that the CIRCT-lowered top module body
     is absent.

Exits non-zero on failure.
"""
import argparse
import re
import sys
from pathlib import Path

# Name of the top-level class declared in skip_circt_lowering.k. CIRCT
# lowering would emit a SystemVerilog module whose name is derived from
# this identifier; if --skip-circt-lowering is honored we should not see
# any such declaration.
TOP_MODULE_NAME = 'Main'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_dir', help='Directory containing compiler outputs')
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    if not output_dir.is_dir():
        print(f"Output directory does not exist: {output_dir}")
        return 1

    ok = True

    # 1. CIRCT MLIR file must be present and look like CIRCT IR.
    mlir_files = sorted(output_dir.glob('*.mlir'))
    if not mlir_files:
        print("--skip-circt-lowering should produce a .mlir file, but none was found.")
        ok = False
    else:
        mlir_text = mlir_files[0].read_text()
        for marker in ('hw.module', 'kanagawa.'):
            if marker not in mlir_text:
                print(f"Expected CIRCT IR marker {marker!r} not found in {mlir_files[0].name}.")
                ok = False

    # 2. No .sv file may contain the CIRCT-lowered top-level module. We
    #    look specifically for a `module <name>` declaration whose name
    #    is or contains the source class name; this is the artifact that
    #    only the skipped CIRCT lowering passes would produce.
    top_module_decl = re.compile(
        rf'(?:^|\s)module\s+\w*{re.escape(TOP_MODULE_NAME)}\w*\b'
    )
    for sv in sorted(output_dir.glob('*.sv')):
        text = sv.read_text()
        if top_module_decl.search(text):
            print(
                f"--skip-circt-lowering must not emit the CIRCT-lowered top module, "
                f"but {sv.name} declares a module containing {TOP_MODULE_NAME!r}."
            )
            ok = False

    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
