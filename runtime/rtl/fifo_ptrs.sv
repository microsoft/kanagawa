// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Read/Write pointers (like a fifo) with write delay to ease timing
`default_nettype none

module KanagawaFifoPtrs
#(
    parameter LOG_DEPTH,
    parameter WRITE_DELAY,
    parameter MAX_SIZE = 2 ** LOG_DEPTH
)
(
    input  wire                     clk,
    input  wire                     rst,

    input  wire                     wrreq,
    output logic                    almost_full,

    input  wire                     rdreq,
    output logic                    empty
);

    logic wrreq_delayed;
    logic almost_full_raw;

	// When WRITE_DELAY = 0,use bypassing logic instead of flip-flop chain
	generate
		if(WRITE_DELAY > 0) begin : gen_ff_chain
	    	// Delay wrreq coming in
    		KanagawaFlipFlopChainNoEnable
    		#(
        		.WIDTH(1),
        		.DEPTH(WRITE_DELAY)
    		)
    		delay_wrreq
    		(
        		.clk(clk),
        		.data_in(wrreq),
        		.data_out(wrreq_delayed)
    		);
    		// Delay almost_full going out
    		KanagawaFlipFlopChainNoEnable
    		#(
        		.WIDTH(1),
        		.DEPTH(WRITE_DELAY)
    		)
    		delay_almost_full
    		(
        		.clk(clk),
        		.data_in(almost_full_raw),
        		.data_out(almost_full)
    		);
		end
		else begin : gen_passthrough
			assign wrreq_delayed = wrreq;
			assign almost_full = almost_full_raw;
		end
	endgenerate

    localparam int unsigned ALMOST_FULL_MARGIN = WRITE_DELAY * 2; // WRITE_DELAY because wrreq is delayed (in), WRITE_DELAY because almost_full is delayed (out)
    localparam int unsigned ALMOST_FULL_THRESHOLD = MAX_SIZE - ALMOST_FULL_MARGIN;

`ifndef EMULATION_AND_FPGA
    // synopsys translate_off
    initial begin
        if (ALMOST_FULL_THRESHOLD < 1) begin
            $error("%m: Depth is insufficient");
        end
    end
    // synopsys translate_on
`endif

    logic [LOG_DEPTH:0] usedw_ff, usedw_next;

    // Lookup table that maps { wrreq_delayed, rdreq } to value to add to usedw
    logic [LOG_DEPTH:0] add_table[3:0];

    assign add_table =
    '{
        0,  // table_index = 3 : write and read
        1,  // table_index = 2 : write
        -1, // table_index = 1 : read
        0   // table_index = 0 : nop
    };

    logic [1:0] table_index;
    assign table_index = { wrreq_delayed, rdreq };

    logic [LOG_DEPTH:0] value_to_add;
    assign value_to_add = add_table[table_index];

    assign usedw_next = (LOG_DEPTH+1)'(usedw_ff + value_to_add);

    always_ff @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            usedw_ff <= '0;
            almost_full_raw <= 1'b0;
            empty <= 1'b0;
        end
        else begin
            usedw_ff <= usedw_next;
            almost_full_raw <= (usedw_next >= ALMOST_FULL_THRESHOLD);
            empty <= (usedw_next == '0);
        end
    end

`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clk) (!rst && almost_full)  |-> !wrreq) else $error ("%m overflow");
    assert property (@(posedge clk) (!rst && empty) |-> !rdreq) else $error ("%m underflow");
    // synopsys translate_on
`endif

endmodule
