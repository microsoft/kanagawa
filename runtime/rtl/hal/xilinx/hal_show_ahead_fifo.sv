// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Platform-specific FIFO module
// Output has show-ahead behavior
module KanagawaHALShowAheadFifo
#(
    parameter DEPTH,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH,
    parameter ALMOSTFULL_ENTRIES = DEPTH/2,
    parameter ALMOSTEMPTY_VAL = 0,
    parameter USE_LUTRAM,                        // 0 to implement in BRAM
    // On underflow (read when empty) or overflow (write when full)
    //  0 - Hardware: FIFO behavior undefined
    //  1 - Hardware: FIFO will ignore read (underflow) or write (overflow)
    // This allows the inverted versions of full and empty to be used as input ready and output valid, respectively
    //  Overflow/underflow protection is left enabled in Xilinx xpm_fifo_sync,
    //    however, if OVER_UNDER_FLOW_PROTECTION == 0, assertions are enabled here to detect this and emit an error
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
    localparam MIN_ALMOSTEMPTY = 5;

    // Report full & empty while the FIFO is being reset
    logic read_rst_busy;
    logic prog_full;
    logic internal_empty;
    logic prog_empty;
    logic internal_almost_full;
    logic internal_almost_empty;

    assign empty = internal_empty | read_rst_busy;

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

    generate
        if (ALMOSTFULL_VAL == DEPTH) begin
            assign almost_full_no_reset = full_no_reset;
        end
        else if (ALMOSTFULL_VAL == DEPTH-1) begin
            assign almost_full_no_reset = internal_almost_full;
        end
        else begin
            assign almost_full_no_reset = prog_full;
        end

        if (ALMOSTEMPTY_VAL == 0) begin
            assign almost_empty = internal_empty | read_rst_busy;
        end
        else if (ALMOSTEMPTY_VAL == 1) begin
            assign almost_empty = internal_almost_empty | read_rst_busy;
        end
        else begin
            assign almost_empty = prog_empty | read_rst_busy;
        end
    endgenerate

`ifndef NO_DYNAMIC_ASSERTS
// synopsys translate_off
    // Overflow / underflow checks

    // Delay overflow/underflow checks until we see a valid reset assertion. This works around issue with clients that route reset through a flip-flop chain.
    assert property (@(posedge clock)  wrreq |-> ((OVER_UNDER_FLOW_PROTECTION != 0) || rst || !full))
        else $error ("%m overflow");
    assert property (@(posedge clock)  rdreq |-> ((OVER_UNDER_FLOW_PROTECTION != 0) || rst || !internal_empty))
        else $error ("%m underflow");
// synopsys translate_on
`endif

    localparam EMPTY_FEATURES = (ALMOSTEMPTY_VAL == 0) ? "5" : (ALMOSTEMPTY_VAL == 1) ? "D" : "7";
    localparam FULL_FEATURES = (ALMOSTFULL_VAL == DEPTH) ? "5" : (ALMOSTFULL_VAL == DEPTH-1) ? "D" : "7";

    xpm_fifo_sync
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
        .PROG_EMPTY_THRESH(ALMOSTEMPTY_VAL < MIN_ALMOSTEMPTY ? MIN_ALMOSTEMPTY : ALMOSTEMPTY_VAL),

        .USE_ADV_FEATURES({"0", EMPTY_FEATURES,"0",FULL_FEATURES})
    )
    fifo
    (
        .sleep(1'b0),
        .rst(rst),

        .wr_clk(clock),
        .wr_en(wrreq),
        .din(data),
        .full(full_no_reset),
        .prog_full(prog_full),
        .wr_data_count(usedw),
        .overflow(),
        .wr_rst_busy(),
        .almost_full(internal_almost_full),
        .wr_ack(),

        .rd_en(rdreq),
        .dout(q),
        .empty(internal_empty),
        .prog_empty(prog_empty),
        .rd_data_count(),
        .underflow(),
        .rd_rst_busy(read_rst_busy),
        .almost_empty(internal_almost_empty),
        .data_valid(),
        .injectsbiterr(1'b0),
        .injectdbiterr(1'b0),
        .sbiterr(),
        .dbiterr()
    );

endmodule
