#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""Verify hier_block annotations in generated SystemVerilog output."""
import sys
from pathlib import Path

from check_generated_sv_modules import check_modules

HIER_BLOCK_REGEX = r'/\*verilator hier_block\*/'


def main():
    if len(sys.argv) != 2:
        print("Usage: check_verilator_hier_blocks.py <output_dir>")
        return 1

    output_dir = Path(sys.argv[1])
    if not output_dir.is_dir():
        print(f"Output directory does not exist: {output_dir}")
        return 1

    return check_modules(
        output_dir,
        expect_any_regex=[HIER_BLOCK_REGEX],
        expect_any_regex_after_port_list=[HIER_BLOCK_REGEX],
        expect_any_without_regex=[HIER_BLOCK_REGEX],
    )


if __name__ == '__main__':
    sys.exit(main())
