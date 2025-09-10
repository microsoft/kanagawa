// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Platform-specific dual-clock FIFO module
// Output has show-ahead behavior
module KanagawaHALDualClockFifo
#(
    parameter DEPTH,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH,
    parameter USEDW_WIDTH = LOG_DEPTH + 1,
    parameter ALMOSTFULL_ENTRIES,
    parameter USE_LUTRAM
)
(
    input wire                       wrclk,
    input wire                       wrrst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     almost_full,
    output logic                     full,
    output logic [USEDW_WIDTH-1:0]   usedw,

    input wire                       rdclk,
    input wire                       rdrst,

    input wire                       rdreq,
    output logic                     empty,
    output logic [WIDTH-1:0]         q
);
    localparam ALMOSTFULL_VAL = DEPTH - ALMOSTFULL_ENTRIES;

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

    // Xilinx requires that PROG_FULL_THRESH must be no greater than this
    localparam MAX_ALMOSTFULL = DEPTH - 5;

    // Report full & empty while the FIFO is being reset
    logic write_rst_busy;
    logic read_rst_busy;
    logic internal_empty;
    logic internal_almost_full;
    logic prog_full;

    // Special cases for ALMOSTFULL_ENTRIES = 0 or 1
    // prog_full is not used in these cases to avoiding the PROG_FULL_THRESH
    // upper bound
    generate
        if (ALMOSTFULL_VAL == DEPTH) begin
            assign almost_full = full;
        end
        else if (ALMOSTFULL_VAL == DEPTH-1) begin
            assign almost_full = internal_almost_full;
        end
        else begin
            assign almost_full = prog_full;
        end
    endgenerate

    assign empty = internal_empty | read_rst_busy;

    // All of these constants have bits 0 and 1 set, which enable overflow and *_data_count checking
    // "D" enables the almost_full output.  "7" enables the prog_full output.
    localparam FULL_FEATURES = (ALMOSTFULL_VAL == DEPTH) ? "5" : (ALMOSTFULL_VAL == DEPTH-1) ? "D" : "7";
    localparam EMPTY_FEATURES = "5";

    xpm_fifo_async
    #(
        .FIFO_MEMORY_TYPE(USE_LUTRAM ? "distributed" : "block"),
        .ECC_MODE("no_ecc"),

        .FIFO_WRITE_DEPTH(DEPTH),
        .WRITE_DATA_WIDTH(WIDTH),
        .WR_DATA_COUNT_WIDTH(LOG_DEPTH+1),
        .PROG_FULL_THRESH(ALMOSTFULL_VAL > MAX_ALMOSTFULL ? MAX_ALMOSTFULL : ALMOSTFULL_VAL),
        .FULL_RESET_VALUE(1),   // Ensures that full, prog_full and almost_full are asserted during the extended reset (wr_rst_busy and rd_rst_busy)

        .READ_MODE("fwft"),
        .FIFO_READ_LATENCY(0),
        .READ_DATA_WIDTH(WIDTH),
        .RD_DATA_COUNT_WIDTH(LOG_DEPTH+1),

        .USE_ADV_FEATURES({"0", EMPTY_FEATURES,"0",FULL_FEATURES})
    )
    fifo
    (
        .sleep(1'b0),
        .rst(wrrst),

        .wr_clk(wrclk),
        .wr_en(wrreq),
        .din(data),
        .full(full),
        .prog_full(prog_full),
        .wr_data_count(usedw),
        .overflow(),
        .wr_rst_busy(write_rst_busy),
        .almost_full(internal_almost_full),
        .wr_ack(),

        .rd_en(rdreq),
        .rd_clk(rdclk),
        .dout(q),
        .empty(internal_empty),
        .prog_empty(),
        .rd_data_count(),
        .underflow(),
        .rd_rst_busy(read_rst_busy),
        .almost_empty(),
        .data_valid(),
        .injectsbiterr(1'b0),
        .injectdbiterr(1'b0),
        .sbiterr(),
        .dbiterr()
    );

endmodule
