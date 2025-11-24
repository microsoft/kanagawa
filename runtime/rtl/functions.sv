// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Contains functions that are useful across Kanagawa compiler generated RTL and
// the runtime RTL modules.

`timescale 1 ns / 1 ps
`default_nettype none

/* verilator lint_off DECLFILENAME */

package KanagawaFunctions;

    // The shortreal type is not supported by Verilator. While it's possible to just truncate a real to 32-bits, this doesn't
    // handle truncation of the exponent and mantissa correctly. These functions implement the conversion
    // from real to float bits and vice versa, which is necessary to correctly model the DSP modules that
    // support single-precision float operations.

    // Convert 32-bit single-precision float binary to real (double-precision)
    function automatic real bits32_to_real(input logic [31:0] bits32);
        logic sign;
        logic [7:0] exp8;
        logic [22:0] mant23;
        logic [10:0] exp11;
        logic [51:0] mant52;
        logic [63:0] bits64;

        // Extract single-precision components
        sign = bits32[31];
        exp8 = bits32[30:23];
        mant23 = bits32[22:0];

        // Handle special cases
        if (exp8 == 8'hFF) begin
            // Infinity or NaN
            exp11 = 11'h7FF;
            mant52 = {mant23, 29'b0};
        end
        else if (exp8 == 8'h00) begin
            // Zero or denormalized number
            if (mant23 == 23'b0) begin
                // Zero
                exp11 = 11'h000;
                mant52 = 52'b0;
            end
            else begin
                // Denormalized - need to normalize for double precision
                // This is a simplified handling - full denorm support would be more complex
                exp11 = 11'h380; // Minimum exponent offset
                mant52 = {mant23, 29'b0};
            end
        end
        else begin
            // Normal number
            // Convert exponent from bias-127 to bias-1023
            // exp8 - 127 + 1023 = exp8 + 896 = exp8 + 0x380
            exp11 = {3'b0, exp8} + 11'h380;
            // Extend mantissa from 23 to 52 bits
            mant52 = {mant23, 29'b0};
        end

        // Assemble double-precision bits
        bits64 = {sign, exp11, mant52};

        // Convert to real
        return $bitstoreal(bits64);
    endfunction

    // Convert real (double-precision) to 32-bit single-precision float binary
    function automatic logic [31:0] real_to_bits32(input real r);
        logic [63:0] bits64;
        logic sign;
        logic [10:0] exp11;
        logic [51:0] mant52;
        logic [7:0] exp8;
        logic [22:0] mant23;
        integer exp_int;

        // Convert real to bits
        bits64 = $realtobits(r);

        // Extract double-precision components
        sign = bits64[63];
        exp11 = bits64[62:52];
        mant52 = bits64[51:0];

        // Handle special cases
        if (exp11 == 11'h7FF) begin
            // Infinity or NaN
            exp8 = 8'hFF;
            mant23 = mant52[51:29]; // Preserve NaN payload bits
        end
        else if (exp11 == 11'h000) begin
            // Zero or denormalized
            exp8 = 8'h00;
            mant23 = 23'b0; // Simplification: treat all denorms as zero
        end
        else begin
            // Normal number - convert exponent from bias-1023 to bias-127
            exp_int = exp11 - 11'h380;

            if (exp_int >= 255) begin
                // Overflow to infinity
                exp8 = 8'hFF;
                mant23 = 23'b0;
            end
            else if (exp_int <= 0) begin
                // Underflow to zero (simplified - could implement gradual underflow)
                exp8 = 8'h00;
                mant23 = 23'b0;
            end
            else begin
                // Normal range
                exp8 = exp_int[7:0];
                // Round mantissa from 52 to 23 bits
                // Simple truncation - could implement proper rounding
                mant23 = mant52[51:29];

                // Basic round-to-nearest-even
                if (mant52[28] && (mant52[27:0] != 28'b0 || mant52[29])) begin
                    // Round up
                    {exp8, mant23} = {exp8, mant23} + 1;
                end
            end
        end

        // Assemble single-precision bits
        real_to_bits32 = {sign, exp8, mant23};
    endfunction

`ifdef VERILATOR
    function automatic string shortreal_bits_to_string(input logic [31:0] bits32);
        real r;
        r = bits32_to_real(bits32);
        return $sformatf("%f", r);
    endfunction
`else
    function automatic string shortreal_bits_to_string(input logic [31:0] bits32);
        shortreal sr;
        sr = $bitstoshortreal(bits32);
        return $sformatf("%f", sr);
    endfunction
`endif

endpackage // KanagawaFunctions
