// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// This FIFO is used on entry points to export class (i.e. floor-plannable portions of the generated hierarchy).
// Currently it is just a simple wrapper over KanagawaWriteDelayFifo. The separate module allows it to more easily
// be identified in floor-planning constraints.
//

`default_nettype none

module KanagawaCrossRegionFifo
#(
    parameter DEPTH = 32,
    parameter LOG_DEPTH = $clog2(DEPTH),    // The true depth of the FIFO, full will be reported WRITE_DELAY cycles early
    parameter WRITE_DELAY = 0,
    parameter MIN_WRITE_DELAY = WRITE_DELAY,    // If this value is different than WRITE_DELAY,
                                                // this will trigger auto-pipelining on the write-side
                                                // interface with auto-pipelining depth in
                                                // [MIN_WRITE_DELAY, WRITE_DELAY]
    parameter WIDTH = 16,
    parameter PORT_WIDTH = (WIDTH == 0 ? 1 : WIDTH),
    parameter ALMOSTFULL_ENTRIES = 0,
    parameter USE_LUTRAM = 0, // 0 to implement in BRAM
    parameter ALMOSTEMPTY_VAL = 0,
    parameter AUTO_PIPELINE_GROUP = "UNUSED"
)
(
    input  wire                 clock,
    input  wire                 rst,

    input  wire                     wrreq,
    input  wire  [PORT_WIDTH-1:0]   data,
    output logic                    full,
    output logic                    overflow_out,

    input  wire                     rdreq,
    output logic                    empty,
    output logic [PORT_WIDTH-1:0]   q,
    output logic                    underflow_out
);

    KanagawaWriteDelayFifo
    #(
        .DEPTH                  (DEPTH),
        .WRITE_DELAY            (WRITE_DELAY),
        .MIN_WRITE_DELAY        (MIN_WRITE_DELAY),
        .WIDTH                  (WIDTH),
        .ALMOSTFULL_ENTRIES     (ALMOSTFULL_ENTRIES),
        .USE_LUTRAM             (USE_LUTRAM),
        .ALMOSTEMPTY_VAL        (ALMOSTEMPTY_VAL),
        .AUTO_PIPELINE_GROUP    (AUTO_PIPELINE_GROUP),
        .PORT_WIDTH             (PORT_WIDTH)
    ) cross_region_fifo
    (
        .clock          (clock),
        .rst            (rst),
        .wrreq          (wrreq),
        .data           (data),
        .full           (full),
        .overflow_out   (overflow_out),
        .rdreq          (rdreq),
        .empty          (empty),
        .q              (q),
        .underflow_out  (underflow_out)
    );

endmodule
