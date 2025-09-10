// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Platform-specific FIFO module
// Output has show-ahead behavior
module KanagawaHALShowAheadFifo
#(
    parameter DEPTH,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH,
    parameter ALMOSTFULL_ENTRIES,
    parameter ALMOSTEMPTY_VAL,
    parameter USE_LUTRAM,                        // 0 to implement in BRAM
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
    initial begin
        if (LOG_DEPTH != $clog2(DEPTH)) begin
            $error("%m: LOG_DEPTH not set correctly");
        end

        if (DEPTH != (2**LOG_DEPTH)) begin
            $error("%m: DEPTH must be a power of 2");
        end

        if (ALMOSTFULL_ENTRIES >= DEPTH) begin
            $error("%m: ALMOSTFULL_ENTRIES/DEPTH not set correctly");
        end
    end

    // Ensure that almost_full and full are high the starting 1 cycle after rst is asserted
    // and lasting until 1 cycle after rst is de-asserted.
    // This is used to prevent writes to the FIFO during the startup sequence.
    logic delayed_rst_ff;
    always_ff @(posedge clock) begin
        delayed_rst_ff <= rst;
    end

    logic full_no_reset;
    assign full = full_no_reset | delayed_rst_ff;

    logic almost_full_no_reset;
    assign almost_full = almost_full_no_reset | delayed_rst_ff;

    scfifo #(
        .add_ram_output_register("ON"),
        .almost_full_value(DEPTH - ALMOSTFULL_ENTRIES),
        .almost_empty_value(ALMOSTEMPTY_VAL),
        .intended_device_family("Stratix V"),
        .lpm_hint(USE_LUTRAM ? "RAM_BLOCK_TYPE=MLAB" : "RAM_BLOCK_TYPE=M20K"),
        .lpm_numwords(DEPTH),
        .lpm_showahead("ON"),
        .lpm_type("scfifo"),
        .lpm_width(WIDTH),
        .lpm_widthu(LOG_DEPTH),
        .overflow_checking(OVER_UNDER_FLOW_PROTECTION ? "ON" : "OFF"),
        .underflow_checking(OVER_UNDER_FLOW_PROTECTION ? "ON" : "OFF"),
        .use_eab("ON")
    )FIFO_Inst
    (
        .clock          (clock),
        .sclr           (rst),
        .aclr           (1'b0),

        .wrreq          (wrreq),
        .data           (data),
        .full           (full_no_reset),
        .almost_full    (almost_full_no_reset),
        .usedw          (usedw[LOG_DEPTH-1:0]),

        .rdreq          (rdreq),
        .empty          (empty),
        .almost_empty   (almost_empty),
        .q              (q),

        .eccstatus()
    );

    assign usedw[LOG_DEPTH] = full;

endmodule
