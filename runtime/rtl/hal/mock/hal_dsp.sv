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

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= KanagawaFunctions::real_to_bits32(KanagawaFunctions::bits32_to_real(op_x_in) * KanagawaFunctions::bits32_to_real(op_y_in));
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
    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] accum_ff;
    logic [31:0] accum_next;

    real op_x_real, op_y_real, accum_real, accum_real_next;

    localparam logic [31:0] ZERO = 32'd0; // Single-precision floating point zero is also binary zero

    always_comb begin
        op_x_real = KanagawaFunctions::bits32_to_real(op_x_in);
        op_y_real = KanagawaFunctions::bits32_to_real(op_y_in);
        accum_real = KanagawaFunctions::bits32_to_real(accum_ff);
        accum_real_next = op_accumulate_in ? (accum_real + (op_x_real * op_y_real)) : (op_x_real * op_y_real);
        accum_next = KanagawaFunctions::real_to_bits32(accum_real_next);
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
    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= KanagawaFunctions::real_to_bits32(KanagawaFunctions::bits32_to_real(op_x_in) + KanagawaFunctions::bits32_to_real(op_y_in));
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
    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= KanagawaFunctions::real_to_bits32(KanagawaFunctions::bits32_to_real(op_x_in) - KanagawaFunctions::bits32_to_real(op_y_in));
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

    initial begin
        assert(LATENCY >= 1) else $fatal("Expected LATENCY >= 1");
    end

    logic [31:0] result_ff;

    always_ff @(posedge clk) begin
        if ($isunknown(op_x_in) || $isunknown(op_y_in) || $isunknown(op_z_in)) begin
            result_ff <= 'x;
        end
        else begin
            result_ff <= KanagawaFunctions::real_to_bits32(KanagawaFunctions::bits32_to_real(op_x_in) + (KanagawaFunctions::bits32_to_real(op_y_in) * KanagawaFunctions::bits32_to_real(op_z_in)));
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
