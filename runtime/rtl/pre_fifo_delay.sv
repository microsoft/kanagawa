// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module KanagawaPreFifoDelay
#(
    parameter WIDTH = 16,
    parameter FEEDFORWARD_DELAY = 1,
    parameter FEEDBACK_DELAY = FEEDFORWARD_DELAY
)
(
    input   wire                clk,
    input   wire                rst,

    input   wire                wrreq_in,
    input   wire [WIDTH-1:0]    data_in,
    output logic                almost_full_out,

    // Output - to FIFO
    output logic                wrreq_out,
    output logic [WIDTH-1:0]    data_out,
    input   wire                almost_full_in
);

    // Feed-forward path
    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (FEEDFORWARD_DELAY)
    ) wrreq_sr
    (
        .clk        (clk),
        .clr        (rst),
        .data_in    (wrreq_in),
        .data_out   (wrreq_out)
    );

    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH      (WIDTH),
        .DEPTH      (FEEDFORWARD_DELAY)
    ) data_sr
    (
        .clk        (clk),
        .data_in    (data_in),
        .data_out   (data_out)
    );

    // Feed-back path
    wire not_almost_full_delayed;
    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (FEEDBACK_DELAY)
    ) full_sr
    (
        .clk        (clk),
        .clr        (rst),
        .data_in    (~almost_full_in),  // Inverted so that on reset the FIFO appears as full
        .data_out   (not_almost_full_delayed)
    );

    assign almost_full_out = ~not_almost_full_delayed;

endmodule
