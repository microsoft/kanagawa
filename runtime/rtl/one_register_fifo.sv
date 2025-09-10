// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Module that wraps a register in a FIFO interface. Note
// this FIFO will stutter so it should only be used in cases
// where you need to write at most every other cycle.
//

`default_nettype none

module KanagawaOneRegisterFifo
#(
    parameter WIDTH = 32,

    // Unused parameters, just there to maintain the interface
    parameter DEPTH = 0,
    parameter LOG_DEPTH = 0,
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

    input wire                 rdreq,
    output logic               empty,
    output logic               almost_empty,
    output logic [WIDTH-1:0]   q
);
    logic reset;

    KanagawaUnknownReset unknown_reset
    (
        .reset_in(rst),
        .reset_out(reset)
    );

    KanagawaRegisterFifo
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
    assign usedw = '0;
    assign almost_empty = empty;

endmodule
