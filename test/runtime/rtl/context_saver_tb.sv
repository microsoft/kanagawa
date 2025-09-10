// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

// Test for output thread filtering functionality (void pipelined function)
module KanagawaContextSaverPipelinedFiltering_tb
#(
    parameter TB_BUFFER_FIFO_OPTIMIZATION = 1
)
(
);
    KanagawaContextSaverPipelinedFiltering_tb_inst
    #(
        .TB_BUFFER_FIFO_OPTIMIZATION(TB_BUFFER_FIFO_OPTIMIZATION)
    ) inst
    ();
endmodule

module KanagawaContextSaverPipelinedFiltering_tb_inst
#(
    parameter TB_BUFFER_FIFO_OPTIMIZATION = 1
)
();
	bit clk;
	bit rst = 1'b1;

    localparam DST_WIDTH = 32;
    localparam CALLEE_IN_WIDTH = 256;
    localparam CALLEE_OUT_WIDTH = 24;
    localparam CALLER_WIDTH = 64;
    localparam LOG_DEPTH = 9;
    localparam USE_LUTRAM = 0;
    localparam IS_PIPELINED = 1;
    localparam LOOP_COUNT_WIDTH = 16;
    localparam RETURN_ELEMENT_WIDTH = CALLEE_OUT_WIDTH;
    localparam RETURN_ELEMENT_COUNT = 1;
    localparam TOTAL_CALL_COUNT = 500;

    typedef struct packed
    {
        logic [LOOP_COUNT_WIDTH-1:0] loop_count;
        logic [CALLER_WIDTH-1:0] data;
    } caller_data_in_t;

    typedef logic [CALLEE_IN_WIDTH-1:0] callee_data_in_t;
    typedef logic [CALLEE_OUT_WIDTH-1:0] callee_data_out_t;
    typedef logic [CALLER_WIDTH-1:0] caller_data_out_t;

    // interface with DUT
    logic caller_wren_in;
    caller_data_in_t caller_data_in;

    logic callee_empty_in;
    callee_data_in_t callee_data_in;
    logic callee_rden_out;

    logic output_rdy_in;
    logic valid_out;

    typedef struct packed
    {
        callee_data_out_t callee_data;
        caller_data_out_t caller_data;
    } context_saver_result_t;

    context_saver_result_t context_saver_result_out;

    clocking cb @(posedge clk);
        // Inputs from DUT, outputs to DUT
        input callee_rden_out;
        input valid_out;
        input context_saver_result_out;

        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int thread_count;
        int reconstructed_i;
        caller_data_in_t caller_data;
        callee_data_in_t callee_data;
        context_saver_result_t context_saver_result;

        $info("Starting ordered context saver test");

        // wait for reset to propagate
        for(int i = 0; i < 50; i++) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        // Generate Inputs
        for (int i = 0; i < TOTAL_CALL_COUNT; i++) begin
            // thread_count==0 case also tests predication
            thread_count = $urandom_range(0, 100);

            // generate caller_data
            caller_data.loop_count = thread_count;
            caller_data.data = '0;

            // Low bits hold "i"
            caller_data.data[LOOP_COUNT_WIDTH-1:0] = i;

            caller_data_in_fifo.put(caller_data);

            // Generate return values from callee
            for (int j = 0; j < thread_count; j++) begin
                callee_data = (i << 16) | j;

                callee_out_fifo.put(callee_data);
            end
        end

        // Wait a while
        #1000000

        $display("Result count: %d", context_saver_result_fifo.num());
        assert(context_saver_result_fifo.num() == 500) else $error("Result count was incorrect");

        for (int i = 0; i < TOTAL_CALL_COUNT; i++) begin
            context_saver_result_fifo.get(context_saver_result);

            reconstructed_i = context_saver_result.caller_data;

            assert(reconstructed_i == i) else $error("reconstructed_i was incorrect");
        end

        $info("SUCCESS");
        $finish;
    end

    // Fifo that writes caller data (live variables) & thread count into the context saver
    KanagawaSimMailboxToFifoWrite
    #(
        .T(caller_data_in_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(1)
    )
    caller_data_in_fifo
    (
        .clk(clk),
        .rst(rst),

        .wrreq_out(caller_wren_in),
        .wrdata_out(caller_data_in),
        .wrfull_in(1'b0)
    );

    // Fifo that holds return values from the callee_data_in
    KanagawaSimMailboxToFifoRead
    #(
        .T(callee_data_in_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(2)
    )
    callee_out_fifo
    (
        .clk(clk),
        .rst(rst),

        .rdreq_in(callee_rden_out),
        .rddata_out(callee_data_in),
        .rdempty_out(callee_empty_in)
    );

    // Fifo that holds results from the context saver
    KanagawaSimReadyValidToMailbox
    #(
        .T(context_saver_result_t),
        .DEPTH(1024),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(3)
    )
    context_saver_result_fifo
    (
        .clk(clk),
        .rst(rst),

        .ready_out(output_rdy_in),
        .valid_in(valid_out),
        .data_in(context_saver_result_out)
    );

	ContextSaverOrdered
    #(
        .DST_WIDTH(DST_WIDTH),
        .CALLEE_IN_WIDTH(CALLEE_IN_WIDTH),
        .CALLEE_OUT_WIDTH(CALLEE_OUT_WIDTH),
        .CALLER_WIDTH(CALLER_WIDTH),
        .DEPTH(2**LOG_DEPTH),
        .LOG_DEPTH(LOG_DEPTH),
        .USE_LUTRAM(USE_LUTRAM),
        .ALMOSTFULL_ENTRIES(0),
        .IS_PIPELINED(IS_PIPELINED),
        .LOOP_COUNT_WIDTH(LOOP_COUNT_WIDTH),
        .RETURN_ELEMENT_WIDTH(RETURN_ELEMENT_WIDTH),
        .RETURN_ELEMENT_COUNT(RETURN_ELEMENT_COUNT),
        .PENDING_COUNT_WIDTH(LOOP_COUNT_WIDTH + $clog2(TOTAL_CALL_COUNT)),
        .CALLEE_VOID_RETURN_TYPE(1),
        .ALMOSTEMPTY_VAL(0),
        .WRITE_DELAY(0),
        .BUFFER_FIFO_OPTIMIZATION(TB_BUFFER_FIFO_OPTIMIZATION)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .caller_wren_in(caller_wren_in),
        .caller_loop_count_in(caller_data_in.loop_count),
        .caller_data_in(caller_data_in.data),
        .caller_almost_full_out(),

        .callee_empty_in(callee_empty_in),
        .callee_data_in(callee_data_in),
        .callee_rden_out(callee_rden_out),

        .output_rdy_in(output_rdy_in),
        .valid_out(valid_out),
        .callee_data_out(context_saver_result_out.callee_data),
        .caller_data_out(context_saver_result_out.caller_data)
    );
endmodule
