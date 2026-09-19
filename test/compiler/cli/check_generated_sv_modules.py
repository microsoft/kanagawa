#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Generic checks for generated SystemVerilog declarations.

This script parses declarations (`module`, `package`, ...) in `*.sv` files under
an output directory and applies generic pattern-based assertions.

The parsing helpers are shared with scenario-specific check scripts; the
assertions in `check_modules` are existential ("expect *any* module to ..."),
so checks that must hold for *every* file belong in a scenario script that
drives `collect_blocks` itself.
"""
import argparse
import re
import sys
from pathlib import Path


def block_decl_regex(keyword):
    return re.compile(rf'(?:^|\s){keyword}\s+(\w+)', re.MULTILINE)


MODULE_DECL = block_decl_regex('module')


def collect_blocks(sv_files, keyword='module', end_keyword=None):
    """Split each file into (filename, name, body) triples.

    Each block starts at a `<keyword> <name>` declaration. It ends at
    `end_keyword` when one is given, and otherwise at the next declaration or
    end of file.
    """
    decl_regex = block_decl_regex(keyword)
    end_regex = re.compile(rf'^\s*{end_keyword}\b', re.MULTILINE) if end_keyword else None

    blocks = []
    for sv in sv_files:
        text = sv.read_text()
        decls = list(decl_regex.finditer(text))
        for i, decl in enumerate(decls):
            end = decls[i + 1].start() if i + 1 < len(decls) else len(text)
            if end_regex is not None:
                terminator = end_regex.search(text, decl.end())
                if terminator is None:
                    raise RuntimeError(f"{sv.name}: {keyword} {decl.group(1)} has no '{end_keyword}'")
                end = min(end, terminator.start())
            blocks.append((sv.name, decl.group(1), text[decl.start():end]))
    return blocks


def collect_modules(sv_files):
    return collect_blocks(sv_files, 'module')


def check_modules(
    output_dir,
    expect_any_regex=(),
    expect_any_regex_after_port_list=(),
    expect_any_without_regex=(),
):
    sv_files = sorted(output_dir.glob('*.sv'))
    if not sv_files:
        print("Expected at least one generated .sv file, but none was found.")
        return 1

    modules = collect_modules(sv_files)
    if not modules:
        names = ', '.join(s.name for s in sv_files)
        print(f"No SystemVerilog modules found in {names}.")
        return 1

    module_names = ', '.join(m[1] for m in modules)

    for pattern in expect_any_regex:
        regex = re.compile(pattern, re.MULTILINE)
        if not any(regex.search(m[2]) for m in modules):
            print(f"Expected at least one module to match regex {pattern!r}: {module_names}.")
            return 1

    for pattern in expect_any_regex_after_port_list:
        regex = re.compile(pattern, re.MULTILINE)
        matched = False
        for _, _, body in modules:
            port_list_end = body.find(');')
            if port_list_end != -1 and regex.search(body[port_list_end + 2:]):
                matched = True
                break
        if not matched:
            print(
                f"Expected at least one module to match regex {pattern!r} after "
                f"the port list (`);`): {module_names}."
            )
            return 1

    for pattern in expect_any_without_regex:
        regex = re.compile(pattern, re.MULTILINE)
        if not any(not regex.search(m[2]) for m in modules):
            print(f"Expected at least one module not to match regex {pattern!r}: {module_names}.")
            return 1

    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_dir', help='Directory containing compiler outputs')
    parser.add_argument('--expect-any-regex', action='append', default=[])
    parser.add_argument('--expect-any-regex-after-port-list', action='append', default=[])
    parser.add_argument('--expect-any-without-regex', action='append', default=[])
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    if not output_dir.is_dir():
        print(f"Output directory does not exist: {output_dir}")
        return 1

    return check_modules(
        output_dir,
        expect_any_regex=args.expect_any_regex,
        expect_any_regex_after_port_list=args.expect_any_regex_after_port_list,
        expect_any_without_regex=args.expect_any_without_regex,
    )


if __name__ == '__main__':
    sys.exit(main())
