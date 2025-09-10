// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module processor.risc_v.trace.risc_v_dv
    { print_trace
    }

import processor.risc_v.trace as trace

import processor.risc_v.internal.decoder

//
// Print a CSV line in the riscv-dv format
//
inline void print_trace(auto hid, auto pc, auto instr, Decoded decoded, optional<int32> value)
{
    string update = "";

    if (decoded.format.rd.is_valid && value.is_valid)
    {
        const auto rd = cast<ABI>(decoded.format.rd.value);
        update = "{rd}:{value.value:x}";
    }

    print("{pc << 2:x8},,{update},,{instr:x8},3,\"{trace::disasm(instr, decoded)}\",,\n");
}
