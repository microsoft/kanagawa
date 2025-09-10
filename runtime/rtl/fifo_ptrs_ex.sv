// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaFifoPtrsEx

    Implements the mechanics of a FIFO, absent the RAM buffer. This can be
    useful when you want to track the number of items or empty/full status but
    do not need to actually queue/dequeue data, or when you wish to handle the
    storage separate from the FIFO for some reason.

Parameters:
    LOG_DEPTH           - LOG2 of the FIFO depth.
    ALMOST_FULL_MARGIN  - Number of entries before full on which almost_full_out will be asserted.
    ALMOST_EMPTY_MARGIN - Number of entries before empty on which almost_empty_out will be asserted.

Ports:
    clk                 - Clock for both read and write side interfaces.
    rst                 - Reset, synchronous to clk.

    (Write Interface)

    wrreq_in            - If asserted, an entry will be consumed ("written")
    full_out            - Indicates that is no more space to write additional entries.
    almost_full_out     - Asserted when the "FIFO" is almost full (the threshold is
                          determined by the ALMOST_FULL_MARGIN parameter)
    wrptr_out           - The write pointer, for use with an external buffer.

    (Read Interface)

    rdreq_in            - If asserted, an entry will be returned ("read")
    empty_out           - Indicates that there are no entries to be consumed.
    almost_empty_out    - Asserted when the "FIFO" is almost empty (the threshold is
                          determined by the ALMOST_EMPTY_MARGIN parameter)
    rdused_out          - Number of entries available to be consumed ("read"). Due to synchronization,
                          the actual value may be more than this, but is guaranteed
                          not to be less than this.
    rdptr_out           - THe read point, for use with an external buffer.

    (Read and Write Interfaces)
    usedw_out           - Indicates the number of used entries in the FIFO.

Authors:
    - Matthew Humphrey (mhumphr@microsoft.com)
*/

`timescale 1 ns / 1 ps

`ifndef QUESTA
    `default_nettype none
`endif

module KanagawaFifoPtrsEx
#(
    parameter int unsigned DEPTH,
    parameter int unsigned LOG_DEPTH = $clog2(DEPTH),
    parameter int unsigned ALMOST_FULL_MARGIN = 1,
    parameter int unsigned ALMOST_EMPTY_MARGIN = 1
)
(
    input  wire                     clk,
    input  wire                     rst,

    input  wire                     wrreq_in,
    output logic                    full_out,
    output logic                    almost_full_out,
    output logic [LOG_DEPTH-1:0]    wrptr_out,

    output logic [LOG_DEPTH:0]      usedw_out,

    input  wire                     rdreq_in,
    output logic                    empty_out,
    output logic                    almost_empty_out,
    output logic [LOG_DEPTH-1:0]    rdptr_out
);

    typedef logic [LOG_DEPTH:0] used_t;

    logic [LOG_DEPTH-1:0] wrptr_nxt, wrptr_ff;
    logic [LOG_DEPTH:0]   usedw_nxt, usedw_ff;
    logic                 full_nxt, full_ff;
    logic                 almost_full_nxt, almost_full_ff;

    logic [LOG_DEPTH-1:0] rdptr_nxt, rdptr_ff;
    logic                 empty_nxt, empty_ff;
    logic                 almost_empty_nxt, almost_empty_ff;

    logic                 reg_enable;
    logic                 rst_d1;

    // Write side

    localparam int unsigned WRALMOST_FULL_THRESHOLD = DEPTH - ALMOST_FULL_MARGIN;
    localparam int unsigned POW_LOG_DEPTH = (32'h00000002)**LOG_DEPTH;

    always @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            wrptr_ff <= '0;
            full_ff <= 1'b1;
            almost_full_ff <= 1'b1;

            usedw_ff <= '0;

            rdptr_ff <= '0;
            empty_ff <= 1'b1;
            almost_empty_ff <= 1'b1;
        end
		else if (reg_enable) begin
            wrptr_ff <= wrptr_nxt;
            full_ff <= full_nxt;
            almost_full_ff <= almost_full_nxt;

            usedw_ff <= usedw_nxt;

            rdptr_ff <= rdptr_nxt;
            almost_empty_ff <= almost_empty_nxt;
            empty_ff <= empty_nxt;
        end
    end

	always @(posedge clk) begin
        rst_d1 <= rst;
	end

    always_comb begin
        // Defaults
        wrptr_nxt = wrptr_ff;
        rdptr_nxt = rdptr_ff;
        usedw_nxt = usedw_ff;
        almost_full_nxt = almost_full_ff;
        almost_empty_nxt = almost_empty_ff;
        reg_enable = (wrreq_in | rdreq_in | rst_d1);

        if (wrreq_in) begin
            wrptr_nxt = (wrptr_ff == LOG_DEPTH'(DEPTH-1)) ? LOG_DEPTH'(0) : LOG_DEPTH'(wrptr_ff + 1'b1);

            if (!rdreq_in) begin
                usedw_nxt = usedw_ff + 1'b1;
            end
        end

        if (rdreq_in) begin
            rdptr_nxt = (rdptr_ff == LOG_DEPTH'(DEPTH-1)) ? LOG_DEPTH'(0) : LOG_DEPTH'(rdptr_ff + 1'b1);

            if (!wrreq_in) begin
                usedw_nxt = usedw_ff - 1'b1;
            end
        end

        almost_full_nxt = (usedw_nxt >= used_t'(WRALMOST_FULL_THRESHOLD));

        empty_nxt = (usedw_nxt == '0);
        almost_empty_nxt = (usedw_nxt <= used_t'(ALMOST_EMPTY_MARGIN));

        usedw_out = usedw_ff;
        full_out = full_ff;
        almost_full_out = almost_full_ff;
        wrptr_out = wrptr_ff;
        empty_out = empty_ff;
        almost_empty_out = almost_empty_ff;
        rdptr_out = rdptr_ff;
    end

	// Use generate-if to improve branch coverage
	generate
        if (DEPTH == POW_LOG_DEPTH) begin: gen_full_nxt1
            assign full_nxt = usedw_nxt[LOG_DEPTH];
        end
        else begin: gen_full_nxt2
            assign full_nxt = (usedw_nxt == DEPTH);
        end
	endgenerate

    // The assertion is updated to skip the first cycle before checking the assertion due to the
    // initialized FF chain
`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    bit ready = 1'b0;
    int skip_cycles = 1;

    always @(posedge clk) begin
        if (skip_cycles != 0) begin
            skip_cycles <= skip_cycles - 1;
        end
        else begin
            ready <= 1'b1;
        end

        if (rst) begin
            ready <= 1'b0;
            skip_cycles <= 1;
        end
    end

    // Xilinx CoSim doesn't support this feature
    // default disable iff(rst || !ready || skip_cycles);
    bit assert_check = !(rst || !ready || (skip_cycles != 0));
    assert property (@(posedge clk) (assert_check && full_out)  |-> !wrreq_in) else $error ("%m overflow");
    assert property (@(posedge clk) (assert_check && empty_out) |-> !rdreq_in) else $error ("%m underflow");
    // synopsys translate_on
`endif

endmodule // KanagawaFifoPtrsEx

`ifndef QUESTA
    `default_nettype wire
`endif
