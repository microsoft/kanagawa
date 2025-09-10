// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Module that wraps two registers in a FIFO interface
//

`default_nettype none
module KanagawaTwoRegisterFifo
#(
    parameter WIDTH = 32,

    // Unused parameters, just there to maintain the interface
    parameter LOG_DEPTH = 0,
    parameter DEPTH = 0,
    parameter ALMOSTFULL_ENTRIES = 0,
    parameter USE_LUTRAM = 0
)
(
    input wire                 clock,
    input wire                 rst,

    input wire                 wrreq,
    input wire [WIDTH-1:0]     data,
    output logic               full,
    output logic               almost_full,
    output logic [LOG_DEPTH:0] usedw,
    output logic               overflow_out,

    input wire                 rdreq,
    output logic               empty,
    output logic               almost_empty,
    output logic [WIDTH-1:0]   q,
    output logic               underflow_out
);
    logic reset;

    KanagawaUnknownReset unknown_reset
    (
        .reset_in(rst),
        .reset_out(reset)
    );

    KanagawaRegisterFifoDual
    #(
        .WIDTH(WIDTH)
    )
    fifo
    (
        .clock(clock),
        .rst(rst),

        .wrreq(wrreq),
        .data(data),
        .full(full),

        .rdreq(rdreq),
        .empty(empty),
        .q(q)
    );

    assign almost_full = full;
    assign usedw = 'x;
    assign almost_empty = empty;

    // Sticky bits that track overflow/underflow
    KanagawaSingleClockFifoDebug debug
    (
        .clk(clock),
        .rst(rst),

        .full_in(full),
        .wren_in(wrreq),
        .overflow_out(overflow_out),

        .empty_in(empty),
        .rden_in(rdreq),
        .underflow_out(underflow_out)
    );

endmodule
