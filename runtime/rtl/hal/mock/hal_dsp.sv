//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

//
// Modules that implement core DSP operations for the mock target
//

`timescale 1 ns / 1 ps
`default_nettype none

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

/* verilator lint_off DECLFILENAME */

// The shortreal type is not supported by Verilator. While it's possible to just truncate a real to 32-bits, this doesn't
// handle truncation of the exponent and mantissa correctly. These functions implement the conversion
// from real to float bits and vice versa, which is necessary to correctly model the DSP modules that
// support single-precision float operations.
package VerilatorShortrealHelpers;

    // Convert 32-bit single-precision float binary to real (double-precision)
    function real bits32_to_real(input logic [31:0] bits32);
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
    function logic [31:0] real_to_bits32(input real r);
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

endpackage

module KanagawaLutMul
# (
    parameter int unsigned  DW_X = 8,
    parameter int unsigned  DW_Y = 8,
    parameter bit           X_SIGNED = 1'b0,
    parameter bit           Y_SIGNED = 1'b0,
    parameter int unsigned  DW_Z = 16
)
(
    input wire  [DW_X-1:0]  x_in,
    input wire  [DW_Y-1:0]  y_in,
    output logic [DW_Z-1:0] z_out
);

    localparam bit SIGNED = X_SIGNED || Y_SIGNED;

    // LutMul has 0-cycle latency for now
    generate
        if (SIGNED) begin: gen_signed_mul
            assign z_out = $signed({X_SIGNED && x_in[DW_X-1], x_in}) * $signed({Y_SIGNED && y_in[DW_Y-1], y_in});
        end
        else begin: gen_unsigned_mul
            assign z_out = x_in * y_in;
        end
    endgenerate

endmodule

module _hardware_dsp__umul27
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [26:0] op_x_in,
    input wire [26:0] op_y_in,

    output logic [53:0] op_result_out,

    input wire op_valid_in
);
    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [53:0] result_ff;

    always_ff @(posedge clk) result_ff <= op_x_in * op_y_in;

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(result_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (result_ff),
        .data_out   (op_result_out)
    );

endmodule

module _hardware_dsp__imul27
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [26:0] op_x_in,
    input wire [26:0] op_y_in,
    input wire op_negate_in,

    output logic [53:0] op_result_out,

    input wire op_valid_in
);

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [53:0] result_ff;

    always_ff @(posedge clk) result_ff <= op_negate_in ?  (-($signed(op_x_in) * $signed(op_y_in))) : ($signed(op_x_in) * $signed(op_y_in));

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(result_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (result_ff),
        .data_out   (op_result_out)
    );

endmodule

module _hardware_dsp__fmul32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);

    import VerilatorShortrealHelpers::*;

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= real_to_bits32(bits32_to_real(op_x_in) * bits32_to_real(op_y_in));
        end
    end

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(result_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (result_ff),
        .data_out   (op_result_out)
    );

endmodule

module _hardware_dsp__fmac32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,
    input wire op_accumulate_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    import VerilatorShortrealHelpers::*;

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] accum_ff;
    logic [31:0] accum_next;

    real op_x_real, op_y_real, accum_real, accum_real_next;

    localparam logic [31:0] ZERO = 32'd0; // Single-precision floating point zero is also binary zero

    always_comb begin
        op_x_real = bits32_to_real(op_x_in);
        op_y_real = bits32_to_real(op_y_in);
        accum_real = bits32_to_real(accum_ff);
        accum_real_next = op_accumulate_in ? (accum_real + (op_x_real * op_y_real)) : (op_x_real * op_y_real);
        accum_next = real_to_bits32(accum_real_next);
    end

    always_ff @(posedge clk) begin
        if (op_valid_in) begin
            accum_ff <= accum_next;
        end

        if (rst) begin
            accum_ff <= ZERO;
        end
    end

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(accum_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (accum_ff),
        .data_out   (op_result_out)
    );

endmodule

module _hardware_dsp__fadd32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    import VerilatorShortrealHelpers::*;

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= real_to_bits32(bits32_to_real(op_x_in) + bits32_to_real(op_y_in));
        end
    end

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(result_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (result_ff),
        .data_out   (op_result_out)
    );

endmodule

module _hardware_dsp__fsub32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    import VerilatorShortrealHelpers::*;

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= real_to_bits32(bits32_to_real(op_x_in) - bits32_to_real(op_y_in));
        end
    end

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(result_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (result_ff),
        .data_out   (op_result_out)
    );

endmodule

module _hardware_dsp__fmad32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,
    input wire [31:0] op_z_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    // X + Y*Z

    import VerilatorShortrealHelpers::*;

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in) || $isunknown(op_z_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= real_to_bits32(bits32_to_real(op_x_in) + (bits32_to_real(op_y_in) * bits32_to_real(op_z_in)));
        end
    end

    KanagawaCascadedFlipFlopsNoReset
    #(
        .WIDTH      ($bits(result_ff)),
        .DEPTH      (LATENCY - 1)
    ) pipeline
    (
        .clk        (clk),

        .data_in    (result_ff),
        .data_out   (op_result_out)
    );

endmodule
/* verilator lint_on DECLFILENAME */
