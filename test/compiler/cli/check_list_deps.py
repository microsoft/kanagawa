#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Verify the outputs of a `kanagawa list-deps` invocation.

Contract of the sub-command: parse + import resolution only. The compiler
must enumerate the transitive set of `.k` source files reachable from the
given input(s) and never invoke the frontend or codegen. Specifically:

  1. The plain-format manifest exists and lists at least the input source
     and one transitive import (the explicit `data.closure` import in
     `list_deps.k` ensures the walker followed at least one edge).
  2. Every line in the manifest resolves to an existing absolute path on
     disk; internal synthetic modules (`.cmdargs.k`, `.options.k`) are
     excluded by the compiler.
  3. The output is sorted, contains no duplicates, and ends with a single
     trailing newline.
  4. No codegen artifacts (.sv / .mlir / metadata) were produced in the
     test output directory; presence of any such file would indicate the
     sub-command did not short-circuit before codegen.

Exits non-zero on failure.
"""
import argparse
import sys
from pathlib import Path


def _read_lines(path: Path):
    text = path.read_text()
    if not text.endswith("\n"):
        print(f"{path.name}: manifest does not end with a newline.")
        return None
    # splitlines() drops the trailing empty element produced by the final \n.
    return text.splitlines()


def _check_plain(manifest: Path, source: Path):
    ok = True
    if not manifest.is_file():
        print(f"plain manifest does not exist: {manifest}")
        return False

    lines = _read_lines(manifest)
    if lines is None:
        return False

    if not lines:
        print(f"plain manifest is empty: {manifest}")
        return False

    if sorted(lines) != lines:
        print(f"plain manifest is not sorted: {manifest}")
        ok = False

    if len(set(lines)) != len(lines):
        print(f"plain manifest contains duplicates: {manifest}")
        ok = False

    for line in lines:
        if line.startswith("."):
            print(
                f"plain manifest contains an internal synthetic module "
                f"(should be filtered): {line}"
            )
            ok = False
        # Each line should resolve to an existing file on disk.
        if not Path(line).is_file():
            print(
                f"plain manifest references a missing file (path may "
                f"not be canonical): {line}"
            )
            ok = False

    # The user's source file must appear (path is canonicalised by the
    # compiler, so compare via Path).
    src_resolved = source.resolve()
    if src_resolved not in {Path(line) for line in lines}:
        print(
            f"plain manifest is missing the input source {src_resolved}; "
            f"got entries: {lines}"
        )
        ok = False

    # At least one transitive dependency must appear (the input file alone
    # is not enough -- mini-base or data.closure must have been followed).
    if len(lines) < 2:
        print(
            f"plain manifest has only {len(lines)} entry; expected the "
            f"transitive walker to pull in at least one import."
        )
        ok = False

    return ok


def _check_no_codegen(out_dir: Path):
    ok = True
    forbidden = []
    for pattern in ("*.sv", "*.mlir", "*.json", "*.dgml"):
        forbidden.extend(out_dir.glob(pattern))
    if forbidden:
        names = ", ".join(p.name for p in forbidden)
        print(
            f"list-deps must not emit codegen artifacts, but found in "
            f"{out_dir}: {names}"
        )
        ok = False
    return ok


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source",
        required=True,
        help="The .k source file passed to list-deps (used for membership check).",
    )
    parser.add_argument(
        "--plain",
        help="Path to the plain-format manifest produced by list-deps.",
    )
    parser.add_argument(
        "--no-codegen-dir",
        help="Directory that must contain no codegen artifacts.",
    )
    args = parser.parse_args()

    ok = True
    source = Path(args.source)

    if args.plain:
        ok &= _check_plain(Path(args.plain), source)

    if args.no_codegen_dir:
        ok &= _check_no_codegen(Path(args.no_codegen_dir))

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
