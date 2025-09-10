// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// FIFO with a flip-flop-chain at the front
//

`default_nettype none

module KanagawaWriteDelayFifo
#(
    parameter DEPTH = 16,                        // Minimim depth value.  Depth may be rounded up to the next power of 2
    parameter WRITE_DELAY = 0,
    parameter MIN_WRITE_DELAY = WRITE_DELAY,    // If this value is different than WRITE_DELAY,
                                                // this will trigger auto-pipelining on the write-side
                                                // interface with auto-pipelining depth in
                                                // [MIN_WRITE_DELAY, WRITE_DELAY]
    parameter WIDTH = 8,
    parameter ALMOSTFULL_ENTRIES = 0, // almost_full threshold (assuming write_delay = 0) = DEPTH - ALMOSTFULL_ENTRIES
    parameter USE_LUTRAM = 0, // 0 to implement in BRAM
    parameter ALMOSTEMPTY_VAL = 0,
    parameter AUTO_PIPELINE_GROUP = "UNUSED",
    // Quartus 17 does not support localparam-s here
    // This needs to be defined and used for the data ports below to allow "zero-width" FIFOs.
    // Defining the ports below as [WIDTH-1:0] doesn't work in Design Compiler, since this creates a 2-bit, not a 1-bit port
    parameter PORT_WIDTH = (WIDTH == 0 ? 1 : WIDTH),
    // If true, then empty should only be de-asserted when a full transaction is in the FIFO
    parameter IS_TRANSACTIONAL = 0,
    // Offset for the bit which indicates the last input in a transaction
    parameter END_TRANSACTION_OFFSET = 0
)
(
    input  wire                         clock,
    input  wire                         rst,

    input  wire                         wrreq,
    input  wire  [PORT_WIDTH-1:0]       data,
    output logic                        full,
    output logic                        overflow_out,

    input  wire                         rdreq,
    output logic                        empty,
    output logic [PORT_WIDTH-1:0]       q,
    output logic                        underflow_out
);
    localparam ALMOSTFULL_ENTRIES_INTERNAL = ALMOSTFULL_ENTRIES + (2*WRITE_DELAY); // (2 * WRITE_DELAY) because almost_full is pipelined backwards
    localparam LOG_DEPTH = DEPTH > 1 ? $clog2(DEPTH) : 1;

    logic [PORT_WIDTH-1:0] q_internal;

`ifndef EMULATION_AND_FPGA
    // synopsys translate_off
    initial begin
        if (ALMOSTFULL_ENTRIES_INTERNAL >= DEPTH) begin
            $error("%m: Depth is insufficient for configured ALMOSTFULL_ENTRIES and WRITE_DELAY");
        end
    end
    // synopsys translate_on
`endif

    // Allow update to WRITE_DELAY cycles for the data to get to the FIFO
    // If WRITE_DELAY is 0, then wrreq_delayed is directly connected to wrreq_delayed
    // If WRITE_DELAY is > 0, then the delay is between [1, and WRITE_DELAY] cycles (the synthesis tool can choose)
    logic wrreq_delayed;

    // Allow WRITE_DELAY cycles for almost_full signal to return back
    logic almost_full_internal;

    // Verify that rst causes almost_full to go high on the next cycle
    // Interface FIFOs are held in reset during the startup sequence
    // to disallow incoming requests during the startup sequence.
`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clock) (rst) |=> (almost_full_internal)) else $error ("Almost full not asserted the cycle after rst is asserted");
    // synopsys translate_on
`endif

    logic full_internal;
    logic empty_internal;
    logic almost_empty_internal;

    generate
        if (WIDTH > 0) begin : genblk1
            logic [WIDTH-1:0] data_delayed;

            // If auto-pipeline enabled
            if (WRITE_DELAY > MIN_WRITE_DELAY) begin : ap_enabled_wrreq_data
`ifdef SIMULATION
                KanagawaSimRandomLatencyChain
                #(
                    .WIDTH      (WIDTH),
                    .MIN_DEPTH  (MIN_WRITE_DELAY),
                    .MAX_DEPTH  (WRITE_DELAY)
                ) delay_chain
                (
                    .clk            (clock),
                    .enable_in      (wrreq),
                    .data_in        (data),
                    .enable_out     (wrreq_delayed),
                    .data_out       (data_delayed)
                );
`else
                KanagawaHALAutoPipelineChain
                #(
                    .WIDTH                  (WIDTH),
                    .MIN_DEPTH              (MIN_WRITE_DELAY),
                    .MAX_DEPTH              (WRITE_DELAY),
                    .AUTO_PIPELINE_GROUP    (AUTO_PIPELINE_GROUP)
                ) delay_wrreq_data
                (
                    .clk            (clock),
                    .enable_in      (wrreq),
                    .data_in        (data),
                    .enable_out     (wrreq_delayed),
                    .data_out       (data_delayed)
                );
`endif
            end // if (WRITE_DELAY > MIN_WRITE_DELAY)
            // Else, we skip instantiation of KanagawaHALAutoPipelineChain because the
            //   Quartus version used for Stratix5 builds does not like the pragma within it
            else begin : ap_disabled_wrreq_data
            // Only instantiate flip-flop chain when WRITE_DELAY is larger than 0 to avoid Lint warning
				if(WRITE_DELAY > 0) begin : gen_ff_chain
                	KanagawaFlipFlopChainNoEnable
                	#(
                    	.WIDTH      	(WIDTH + 1),
                    	.DEPTH       	(WRITE_DELAY)
                	) delay_wrreq_data
                	(
                    	.clk            (clock),

                    	.data_in        ({wrreq, data}),
                    	.data_out       ({wrreq_delayed, data_delayed})
                	);
				end
				else begin : gen_pass_through
					assign {wrreq_delayed, data_delayed} = {wrreq, data};
				end
            end

            KanagawaHALShowAheadFifo
            #(
                .LOG_DEPTH(LOG_DEPTH),
                .DEPTH(DEPTH),
                .WIDTH(WIDTH),
                .ALMOSTFULL_ENTRIES(ALMOSTFULL_ENTRIES_INTERNAL),
                .ALMOSTEMPTY_VAL(ALMOSTEMPTY_VAL),
                .USE_LUTRAM(USE_LUTRAM)
            )
            fifo
            (
                .clock(clock),
                .rst(rst),

                .wrreq(wrreq_delayed),
                .data(data_delayed),
                .full(full_internal),
                .almost_full(almost_full_internal),
                .usedw(),

                .rdreq(rdreq),
                .empty(empty_internal),
                .almost_empty(almost_empty_internal),
                .q(q_internal)
            );
        end // if (WIDTH > 0)
        else begin: gen_zero_width_fifo // (WIDTH == 0)
            assign q_internal = '0;
			// Only instantiate flip-flop chain when WRITE_DELAY is larger than 0 to avoid Lint warning/LEC mismatch
			if(WRITE_DELAY > 0) begin : gen_ff_chain_zero_width
            	KanagawaFlipFlopChainNoEnable
	            #(
                	.WIDTH(1),
                	.DEPTH(WRITE_DELAY)
            	)
            	delay_wrreq
            	(
                	.clk(clock),
                	.data_in(wrreq),
                	.data_out(wrreq_delayed)
            	);
			end
			else begin : gen_pass_through_zero_width
				assign wrreq_delayed = wrreq;
			end

            KanagawaFifoPtrsEx
            #(
                .DEPTH(DEPTH),
                .LOG_DEPTH(LOG_DEPTH),
                .ALMOST_FULL_MARGIN(ALMOSTFULL_ENTRIES_INTERNAL),
                .ALMOST_EMPTY_MARGIN(ALMOSTEMPTY_VAL)
            )
            fifo_ptrs
            (
                .clk(clock),
                .rst(rst),

                .wrreq_in(wrreq_delayed),
                .full_out(full_internal),
                .almost_full_out(almost_full_internal),
                .wrptr_out(),

                .usedw_out(),

                .rdreq_in(rdreq),
                .empty_out(empty_internal),
                .almost_empty_out(almost_empty_internal),
                .rdptr_out()
            );
        end

        // If auto-pipeline enabled
        if (WRITE_DELAY > MIN_WRITE_DELAY) begin : ap_enabled_afull
`ifdef QUESTA
            KanagawaSimRandomLatencyChain
            #(
                .WIDTH      (1),
                .MIN_DEPTH  (MIN_WRITE_DELAY),
                .MAX_DEPTH  (WRITE_DELAY)
            ) delay_afull
            (
                .clk            (clock),
                .enable_in      (almost_full_internal),
                .data_in        (almost_full_internal),
                .enable_out     (),
                .data_out       (full)
            );
`else
            KanagawaHALAutoPipelineChain
            #(
                .WIDTH                  (1),
                .MIN_DEPTH              (MIN_WRITE_DELAY),
                .MAX_DEPTH              (WRITE_DELAY),
                .AUTO_PIPELINE_GROUP    (AUTO_PIPELINE_GROUP)
            ) delay_afull
            (
                .clk            (clock),
                .enable_in      (1'b0),
                .data_in        (almost_full_internal),
                .enable_out     (),
                .data_out       (full)
            );
`endif
        end
        // Else, we skip instantiation of KanagawaHALAutoPipelineChain because the
        //   Quartus version used for Stratix5 builds does not like the pragma within it
        else begin : ap_disabled_afull
			if(WRITE_DELAY > 0) begin : gen_ap_disabled_afull_ff_chain
            	KanagawaFlipFlopChainNoEnable
            	#(
                	.WIDTH          (1),
                	.DEPTH          (WRITE_DELAY)
            	) delay_afull
            	(
                	.clk            (clock),

                	.data_in        (almost_full_internal),
                	.data_out       (full)
            	);
			end
			else begin : gen_ap_disabled_afull_bypass
				assign full = almost_full_internal;
			end
        end
    endgenerate

    // Sticky bits that track overflow/underflow
    KanagawaSingleClockFifoDebug debug
    (
        .clk(clock),
        .rst(rst),

        .full_in(full_internal),
        .wren_in(wrreq_delayed),
        .overflow_out(overflow_out),

        .empty_in(empty_internal),
        .rden_in(rdreq),
        .underflow_out(underflow_out)
    );

    generate

        // Track transaction count
        logic transaction_empty;
        if (IS_TRANSACTIONAL)
        begin
            logic [LOG_DEPTH:0] transaction_count_ff;
            logic wr_end_transaction;
            logic rd_end_transaction;

            // Write end of transaction
            assign wr_end_transaction = wrreq & data[END_TRANSACTION_OFFSET];
            // Read end of transaction
            assign rd_end_transaction = rdreq & q[END_TRANSACTION_OFFSET];

            always_ff @(posedge clock)
            begin
                if (rst)
                begin
                    // Store actual count - 1 so we can check sign bit for count == 0
                    transaction_count_ff <= {(LOG_DEPTH + 1){1'b1}};
                end
                else
                begin
                    // Increment transaction count if writing and not reading the end of transaction
                    if (wr_end_transaction && !rd_end_transaction)
                    begin
                        transaction_count_ff <= transaction_count_ff + 1'd1;
                    end
                    // Decrement transaction count if reading and not writing the end of a transaction
                    else if (!wr_end_transaction && rd_end_transaction)
                    begin
                        transaction_count_ff <= transaction_count_ff - 1'd1;
                    end
                end
            end

            assign transaction_empty = transaction_count_ff[LOG_DEPTH];
        end // IS_TRANSACTIONAL

        logic empty_pre_transaction;

        // Intel FIFOs require a minimum ALMOSTEMPTY_VAL to work correctly
        if (ALMOSTEMPTY_VAL < 4) begin: gen_almost_empty
            assign empty_pre_transaction = empty_internal;
        end
        else begin: gen_non_aempty
            // almost empty functionality is used to breakup the feedback loop between empty and rdren
            // by pipelining the empty & almost empty signals
            // When almost_empty = 0, then reads can occur every cycle
            // When (almost_empty = 1) and (empty == 0) then reads can occur if there was no read on the last cycle

            // Compute output empty signal based on almost_empty, empty, rdreq
            // Then pass that signal through a flip-flop chain

            KanagawaFlipFlopChainNoEnable
            #(
                .WIDTH(1),
                .DEPTH(1) // if a larger value is used, then reads must occur less frequently than every other cycle
            ) empty_ffc
            (
                .clk(clock),

                .data_in(almost_empty_internal && (empty_internal || rdreq)),
                .data_out(empty_pre_transaction)
            );
        end

        // Apply transaction count
        if (IS_TRANSACTIONAL)
        begin
            // Empty if 0 transactions
            assign empty = empty_pre_transaction || transaction_empty;
        end
        else
        begin
            assign empty = empty_pre_transaction;
        end
    endgenerate

    always_comb begin
        q = q_internal;

        // Set data output to unknown
        // when the FIFO reports empty
        // To help catch bugs
        // and to enable assertions related to bits being constant or unknown

        //synopsys translate_off
        if (empty) begin
            q = 'x;
        end
        //synopsys translate_on
    end

endmodule
