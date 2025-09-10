// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Modules that implements DSP intrinsics for Xilinx FPGAs
//
`default_nettype none

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

    (* use_dsp = "no" *) logic [DW_Z-1:0] product;

    // LutMul has 0-cycle latency for now
    generate
        if (SIGNED) begin: gen_signed_mul
            assign product = $signed({X_SIGNED && x_in[DW_X-1], x_in}) * $signed({Y_SIGNED && y_in[DW_Y-1], y_in});
        end
        else begin: gen_unsigned_mul
            assign product = x_in * y_in;
        end
    endgenerate

    assign z_out = product;

endmodule

(* use_dsp48 = "yes" *) module _hardware_dsp__umul18
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [17:0] op_x_in,
    input wire [17:0] op_y_in,

    output logic [35:0] op_result_out,

    input wire op_valid_in
);
    initial begin
        assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
    end

    // registered inputs
    logic [17:0] x_ff;
    logic [17:0] y_ff;

    always_ff @(posedge clk) begin
        x_ff <= op_x_in;
        y_ff <= op_y_in;

        op_result_out <= x_ff * y_ff;
    end
endmodule

(* use_dsp48 = "yes" *) module _hardware_dsp__imul18
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [17:0] op_x_in,
    input wire [17:0] op_y_in,

    output logic [35:0] op_result_out,

    input wire op_valid_in
);
    initial begin
        assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
    end

    // registered inputs
    logic [17:0] x_ff;
    logic [17:0] y_ff;

    always_ff @(posedge clk) begin
        x_ff <= op_x_in;
        y_ff <= op_y_in;

        op_result_out <= $signed(x_ff) * $signed(y_ff);
    end
endmodule
