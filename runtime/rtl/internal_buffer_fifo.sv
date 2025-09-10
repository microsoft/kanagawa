// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// memory fifo followed by a register fifo, to help with timing in cases
// where rden is on the critical path
//

`default_nettype none

module KanagawaInternalBufferFifo
#(
    parameter DEPTH,
    parameter WRITE_DELAY = 0,
    parameter WIDTH,
    parameter ALMOSTFULL_ENTRIES = 0,
    parameter USE_LUTRAM,
    parameter IS_TRANSACTIONAL = 0,
    parameter END_TRANSACTION_OFFSET = 0
)
(
    input wire                       clock,
    input wire                       rst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     full,
    output logic                     overflow_out,

    input wire                       rdreq,
    output logic                     empty,
    output logic [WIDTH-1:0]         q,
    output logic                     underflow_out
);
    logic [WIDTH-1:0] internal_data;

    logic memory_fifo_empty;
    logic register_fifo_full;
    logic internal_transfer;

    assign internal_transfer = ~memory_fifo_empty & ~register_fifo_full;

    KanagawaWriteDelayFifo
    #(
        .DEPTH(DEPTH),
        .WRITE_DELAY(WRITE_DELAY),
        .WIDTH(WIDTH),
        .ALMOSTFULL_ENTRIES(ALMOSTFULL_ENTRIES),
        .USE_LUTRAM(USE_LUTRAM),
        .IS_TRANSACTIONAL(IS_TRANSACTIONAL),
        .END_TRANSACTION_OFFSET(END_TRANSACTION_OFFSET)
    )
    memory_fifo
    (
        .clock(clock),
        .rst(rst),

        .wrreq(wrreq),
        .data(data),
        .full(full),
        .overflow_out(overflow_out),

        .rdreq(internal_transfer),
        .empty(memory_fifo_empty),
        .q(internal_data),
        .underflow_out()
    );

    KanagawaRegisterFifoSkid
    #(
        .WIDTH(WIDTH)
    )
    register_fifo
    (
        .clock(clock),
        .rst(rst),

        .wrreq(internal_transfer),
        .data(internal_data),
        .full(register_fifo_full),

        .rdreq(rdreq),
        .empty(empty),
        .q(q)
    );

    KanagawaFifoDebug underflow_check
    (
        .clk(clock),
        .rst(rst),

        .state_in(empty),
        .req_in(rdreq),

        .error_out(underflow_out)
    );

endmodule
