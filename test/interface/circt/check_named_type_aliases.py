#!/usr/bin/env python3
# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

import re
import sys
from pathlib import Path


def find_line(lines, marker):
    matches = [line for line in lines if marker in line]
    if len(matches) != 1:
        raise RuntimeError(f"expected one {marker!r} line, found {len(matches)}")
    return matches[0]


def require(line, marker):
    if marker not in line:
        raise RuntimeError(f"expected {marker!r} in:\n  {line.strip()}")


def main():
    mlir_files = sorted(Path(sys.argv[1]).glob("*.mlir"))
    if len(mlir_files) != 1:
        raise RuntimeError(f"expected one MLIR file, found {len(mlir_files)}")

    lines = mlir_files[0].read_text().splitlines()

    state = find_line(lines, "hw.typedecl @E,")
    metadata = find_line(lines, "hw.typedecl @S,")
    payload = find_line(lines, "hw.typedecl @U")
    body = find_line(lines, "hw.typedecl @Foo,")
    packet = find_line(lines, "hw.typedecl @Bar,")

    declarations = [state, metadata, payload, body, packet]
    if [lines.index(line) for line in declarations] != sorted(lines.index(line) for line in declarations):
        raise RuntimeError("named types were not emitted in dependency order")

    require(state, ": ui2")

    require(metadata, "!hw.struct<x: ui6")
    require(metadata, "e: !hw.typealias<@CoreModuleTypeScope::@E,")

    require(payload, "!hw.union<y: si8, x: ui8>")
    match = re.search(r"hw\.typedecl @([^,]+),", payload)
    if match is None:
        raise RuntimeError(f"could not parse template union alias from:\n  {payload.strip()}")
    payload_alias = f"!hw.typealias<@CoreModuleTypeScope::@{match.group(1)},"

    require(body, "!hw.union<")
    require(body, f"u: {payload_alias}")
    require(body, "s: !hw.typealias<@CoreModuleTypeScope::@S,")
    require(body, "a: !hw.array<2xui4>")
    require(body, "x: ui8")

    require(packet, "!hw.struct<")
    require(packet, "a: !hw.array<2x")
    require(packet, payload_alias)
    require(packet, "f: !hw.typealias<@CoreModuleTypeScope::@Foo,")
    require(packet, "s: !hw.typealias<@CoreModuleTypeScope::@S,")

    run = find_line(lines, 'kanagawa.port.input "Run" sym @Run')
    require(run, "!hw.typealias<@CoreModuleTypeScope::@Bar,")
    require(run, "!hw.struct<x: !hw.typealias<@CoreModuleTypeScope::@Bar,")
    require(run, "y: !hw.typealias<@CoreModuleTypeScope::@Foo,")
    require(run, "z: !hw.typealias<@CoreModuleTypeScope::@E,")
    require(run, 'from "result"')
    require(run, 'to "arg"')

    callback = find_line(lines, 'kanagawa.port.output "callback" sym @callback')
    require(callback, "_param_0: !hw.typealias<@CoreModuleTypeScope::@Bar,")
    require(callback, "_param_1: !hw.typealias<@CoreModuleTypeScope::@Foo,")
    require(callback, "_param_2: !hw.typealias<@CoreModuleTypeScope::@E,")
    require(callback, "!esi.channel<!hw.typealias<@CoreModuleTypeScope::@Bar,")
    require(callback, 'to "arg"')
    require(callback, 'from "result"')

    return 0


if __name__ == "__main__":
    sys.exit(main())