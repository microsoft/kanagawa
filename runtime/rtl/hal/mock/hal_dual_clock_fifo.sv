//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

module KanagawaHALDualClockFifo
#(
    parameter DEPTH = 32,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH = 32,
    parameter USEDW_WIDTH = LOG_DEPTH + 1,
    parameter ALMOSTFULL_ENTRIES = 1,
/* verilator lint_off UNUSEDPARAM */
    parameter USE_LUTRAM = 0,
/* verilator lint_on UNUSEDPARAM */
    parameter OVER_UNDER_FLOW_PROTECTION = 0
)
(
    input wire                       wrclk,
    input wire                       wrrst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     full,
    output logic                     almost_full,
    output logic [USEDW_WIDTH-1:0]   usedw,

    input wire                       rdclk,
    input wire                       rdrst,

    input wire                       rdreq,
    output logic                     empty,
    output logic [WIDTH-1:0]         q
);
    // Note: This cross-clock FIFO has no timing constraints because it is designed for simulation use only

    localparam int unsigned ALMOST_FULL_THRESHOLD = DEPTH - ALMOSTFULL_ENTRIES;
    localparam int unsigned ALMOST_EMPTY_MARGIN = 1;

    initial begin
        if (LOG_DEPTH != $clog2(DEPTH)) begin
            $error("%m: LOG_DEPTH not set correctly");
        end
        if (USEDW_WIDTH != (LOG_DEPTH+1)) begin
            $error("%m: USEDW_WIDTH not set correctly. Must equal LOG_DEPTH+1");
        end
    end

    // This simulation only module requires depth be a power of two. There are corner cases in the Kanagawa compiler
    // where that isn't the case, so we quietly force it here.
    localparam POW2_DEPTH = 2**LOG_DEPTH;

    typedef logic [LOG_DEPTH:0] used_t;

    logic pop, push;

    struct packed {
        logic                 rst;
        logic [LOG_DEPTH:0]   wrptr;
        logic [LOG_DEPTH:0]   rdptr;
        logic [LOG_DEPTH:0]   used;
        logic                 almost_full;
    } wr_ff, wr_nxt;

    struct packed {
        logic                 rst;
        logic [LOG_DEPTH:0]   rdptr;
        logic [LOG_DEPTH:0]   wrptr;
        logic [LOG_DEPTH:0]   used;
        logic                 empty;
        logic                 almost_empty;
    } rd_ff, rd_nxt;

    logic [WIDTH-1:0] mem[POW2_DEPTH-1:0];

    /////////////////////////////
    // Over/underflow protection

    assign pop  = rdreq & ((OVER_UNDER_FLOW_PROTECTION == 0) | ~empty);
    assign push = wrreq & ((OVER_UNDER_FLOW_PROTECTION == 0) | ~full);

    /////////////////////////////
    // Write side of FIFO (wrclk)

    always_ff @(posedge wrclk) begin
        wr_ff <= wr_nxt;

        if (push) begin
            mem[wr_ff.wrptr[LOG_DEPTH-1:0]] <= data;
        end

        if (wr_ff.rst) begin
            wr_ff.wrptr <= '0;
            wr_ff.rdptr <= '0;
            wr_ff.almost_full <= 1'b1;
            wr_ff.used <= '0;
        end
    end

    always_comb begin
        wr_nxt = wr_ff;

        if (push) begin
            wr_nxt.wrptr = wr_ff.wrptr + 1'b1;
            wr_nxt.used = wr_ff.used + 1'b1;
        end

        wr_nxt.used = wr_nxt.wrptr - wr_ff.rdptr;
        wr_nxt.almost_full = (wr_nxt.used >= used_t'(ALMOST_FULL_THRESHOLD));

        wr_nxt.rst = wrrst | rdrst;
        wr_nxt.rdptr = rd_ff.rdptr;
    end

    /////////////////////////////
    // Read side of FIFO (rdclk)

    always_ff @(posedge rdclk) begin
        rd_ff <= rd_nxt;

        if (rd_ff.rst) begin
            rd_ff.rdptr <= '0;
            rd_ff.wrptr <= '0;
            rd_ff.empty <= 1'b1;
            rd_ff.almost_empty <= 1'b1;
            rd_ff.used <= '0;
        end
    end

    always_comb begin
        rd_nxt = rd_ff;

        if (pop) begin
            rd_nxt.rdptr = rd_ff.rdptr + 1'b1;
        end

        rd_nxt.used = rd_ff.wrptr - rd_nxt.rdptr;
        rd_nxt.empty = (rd_nxt.used == '0);
        rd_nxt.almost_empty = (rd_nxt.used <= used_t'(ALMOST_EMPTY_MARGIN));

        rd_nxt.rst = wrrst | rdrst;
        rd_nxt.wrptr = wr_ff.wrptr;
    end

    /////////////////////////////
    // Output ports

    always_comb begin
        usedw = wr_ff.used;
        almost_full = wr_ff.almost_full;
        full = wr_ff.used[LOG_DEPTH];
        empty = rd_ff.empty;
        q = mem[rd_ff.rdptr[LOG_DEPTH-1:0]];
    end

endmodule
