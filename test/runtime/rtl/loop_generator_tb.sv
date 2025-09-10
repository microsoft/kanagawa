// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

module KanagawaLoopGenerator_tb
#(
)
(
);
    KanagawaLoopGenerator_tb_inst
    inst
    ();
endmodule

module KanagawaLoopGenerator_tb_inst
#(
)
();
    localparam TOTAL_WIDTH = 128;
    localparam COUNTER_WIDTH = 32;
    localparam OFFSET = 0;
    localparam ONLY_ONE_THREAD_OFFSET = 32;

    typedef struct packed
    {
        logic [TOTAL_WIDTH-COUNTER_WIDTH-2:0] data;
        logic only_one_thread;
        logic [COUNTER_WIDTH-1:0] max_thread_id;
    } data_t;

	bit clk;
	bit rst = 1'b1;

    logic empty_in;
    data_t data_in;
    logic rden_out;

    data_t data_out;
    logic empty_out;
    logic rden_in;

    clocking cb @(posedge clk);
        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int total_count;
        int expected_count;
        data_t input_data;
        data_t output_data;

        $info("Starting KanagawaLoopGenerator test");

        total_count = 100;
        expected_count = 0;

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        // Generate input
        for (int i = 0; i < total_count; i++) begin
            input_data.max_thread_id = i;
            input_data.only_one_thread = (i == 0) ? 1'b1 : 1'b0;
            input_data.data = i + 3;

            // +1 because counter value is max_thread_id, (which is thread_count - 1)
            expected_count += (input_data.max_thread_id + 1);

            input_fifo.put(input_data);
        end

        // Wait a while
        #1000000

        $display("output_fifo count: %d", output_fifo.num());

        // Check output
        assert(output_fifo.num() == expected_count) else $error("Not all results were received");

        for (int i = 0; i < total_count; i++) begin
            for (int j = 0; j < (i + 1); j++) begin
                output_fifo.get(output_data);

                assert(output_data.max_thread_id == j) else $error("Incorrect thread id received");
                assert(output_data.data == (i + 3)) else $error("Incorrect data received");
            end
        end

        $info("SUCCESS");
        $finish;
    end

    KanagawaSimMailboxToFifoRead
    #(
        .T(data_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(1)
    )
    input_fifo
    (
        .clk(clk),
        .rst(rst),

        .rdreq_in(rden_out),
        .rddata_out(data_in),
        .rdempty_out(empty_in)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T(data_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(2)
    )
    output_fifo
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(rden_in),
        .empty_in(empty_out),
        .data_in(data_out)
    );

	KanagawaLoopGenerator
    #(
        .TOTAL_WIDTH(TOTAL_WIDTH),
        .COUNTER_WIDTH(COUNTER_WIDTH),
        .HAS_LITERAL_MAX_THREAD_ID(0),
        .LITERAL_MAX_THREAD_ID(0),
        .OFFSET(OFFSET),
        .ONLY_ONE_THREAD_OFFSET(ONLY_ONE_THREAD_OFFSET)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .empty_in(empty_in),
        .data_in(data_in),
        .rden_out(rden_out),
        .underflow_out(),

        .data_out(data_out),
        .empty_out(empty_out),
        .rden_in(rden_in)
    );
endmodule
