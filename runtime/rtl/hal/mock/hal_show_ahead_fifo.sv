//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

// Generic FIFO module
// Output has show-ahead behavior
module KanagawaHALShowAheadFifo
#(
    parameter DEPTH = 16,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH = 32,
    parameter ALMOSTFULL_ENTRIES = 1,
    parameter ALMOSTEMPTY_VAL = 1,

/* verilator lint_off UNUSEDPARAM */
    parameter USE_LUTRAM = 0,
/* verilator lint_on UNUSEDPARAM */

    // On underflow (read when empty) or overflow (write when full)
    //  0 - Hardware: FIFO behavior undefined
    //  1 - Hardware: FIFO will ignore read (underflow) or write (overflow)
    // This allows the inverted versions of full and empty to be used as input ready and output valid, respectively
    parameter OVER_UNDER_FLOW_PROTECTION = 0
)
(
    input wire                       clock,
    input wire                       rst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     full,
    output logic                     almost_full,
    output logic [LOG_DEPTH:0]       usedw,

    input wire                       rdreq,
    output logic                     empty,
    output logic                     almost_empty,
    output logic [WIDTH-1:0]         q
);

    KanagawaShowAheadRegisterFifo
    #(
        .LOG_DEPTH                  (LOG_DEPTH),
        .DEPTH                      (DEPTH),
        .WIDTH                      (WIDTH),
        .ALMOSTFULL_ENTRIES         (ALMOSTFULL_ENTRIES),
        .ALMOSTEMPTY_VAL            (ALMOSTEMPTY_VAL),
        .OVER_UNDER_FLOW_PROTECTION (OVER_UNDER_FLOW_PROTECTION),
        .ASSERT_ON_OVER_UNDER_FLOW  (OVER_UNDER_FLOW_PROTECTION != 0 ? 0 : 1),
        .MUX_ON_READ                (1)
    ) fifo_inst
    (
        .clock              (clock),
        .rst                (rst),
        .wrreq              (wrreq),
        .data               (data),
        .full               (full),
        .almost_full        (almost_full),
        .usedw              (usedw),
        .rdreq              (rdreq),
        .empty              (empty),
        .almost_empty       (almost_empty),
        .q                  (q)
    );

endmodule
