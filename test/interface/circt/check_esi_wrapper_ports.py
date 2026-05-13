#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.
"""
Verify the EsiWrapper port shapes for the mixed export/callback class
defined in `esi_wrapper_ports.k`. Each combination of {export, callback}
x {regular, async, no_backpressure} must lower to a specific kind of
port (input/output, bundle/bare channel, signaling protocol).

Exits non-zero on failure.
"""
import argparse
import re
import sys
from pathlib import Path

# (port name, list of substrings that must all appear on the port's
# declaration line in the EsiWrapper container).
EXPECTED_PORTS = [
    # Regular export -> input bundle, args FIFO, results ValidReady.
    ("RegExport", [
        'kanagawa.port.input "RegExport" sym @RegExport',
        '!esi.bundle<[',
        ', FIFO> from "result"',  # results channel uses FIFO (rden/empty pull)
        ' to "arg"',              # args channel ValidReady (default)
    ]),

    # [[async]] export -> bare input channel.
    ("AsyncExport", [
        'kanagawa.port.input "AsyncExport" sym @AsyncExport',
        '!esi.channel<',
    ]),

    # [[no_backpressure]] export -> input bundle with ValidOnly channels.
    ("NbpExport", [
        'kanagawa.port.input "NbpExport" sym @NbpExport',
        '!esi.bundle<[',
        ', ValidOnly> from "result"',
        ', ValidOnly> to "arg"',
    ]),

    # Regular callback -> output bundle, args FIFO, results ValidReady.
    ("reg_cb", [
        'kanagawa.port.output "reg_cb" sym @reg_cb',
        '!esi.bundle<[',
        ', FIFO> to "arg"',
        ' from "result"',
    ]),

    # [[async]] callback -> bare output channel.
    ("async_cb", [
        'kanagawa.port.output "async_cb" sym @async_cb',
        '!esi.channel<',
        ', FIFO>',
    ]),

    # [[no_backpressure]] [[async]] callback -> bare ValidOnly output channel.
    ("nbp_cb", [
        'kanagawa.port.output "nbp_cb" sym @nbp_cb',
        '!esi.channel<',
        ', ValidOnly>',
    ]),
]

# Forbid the no-longer-emitted shapes so silent regressions don't slip past.
FORBIDDEN_LINES = [
    # Pre-Phase-1 callback bundles were `kanagawa.port.input` for any *_cb.
    re.compile(r'kanagawa\.port\.input\s+"\w*_cb"'),
    # Pre-Phase-2 single-channel async ports were single-element bundles.
    re.compile(r'"AsyncExport"[^\n]*!esi\.bundle<'),
    re.compile(r'"async_cb"[^\n]*!esi\.bundle<'),
    re.compile(r'"nbp_cb"[^\n]*!esi\.bundle<'),
]


def find_port_line(mlir_text, port_name):
    pattern = re.compile(
        rf'^[^\n]*"{re.escape(port_name)}"\s+sym\s+@{re.escape(port_name)}[^\n]*$',
        re.MULTILINE)
    match = pattern.search(mlir_text)
    return match.group(0) if match else None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_dir', help='Directory containing compiler outputs')
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    if not output_dir.is_dir():
        print(f"Output directory does not exist: {output_dir}", file=sys.stderr)
        return 1

    mlir_files = sorted(output_dir.glob('*.mlir'))
    if not mlir_files:
        print("No .mlir output found.", file=sys.stderr)
        return 1

    mlir_text = mlir_files[0].read_text()

    failures = []
    for name, required in EXPECTED_PORTS:
        line = find_port_line(mlir_text, name)
        if line is None:
            failures.append((name, 'no declaration line found', None))
            continue
        for needle in required:
            if needle not in line:
                failures.append((name, f'missing substring {needle!r}', line))

    for forbidden in FORBIDDEN_LINES:
        match = forbidden.search(mlir_text)
        if match:
            failures.append(('<forbidden>',
                             f'pattern /{forbidden.pattern}/ matched',
                             match.group(0)))

    if failures:
        print(f"EsiWrapper port-shape mismatch in {mlir_files[0].name}:", file=sys.stderr)
        for name, reason, line in failures:
            print(f"  {name}: {reason}", file=sys.stderr)
            if line is not None:
                print(f"    {line.strip()}", file=sys.stderr)
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
