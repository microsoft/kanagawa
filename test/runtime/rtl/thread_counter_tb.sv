// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaThreadCounter_tb;
    KanagawaThreadCounter_tb_inst inst
    ();
endmodule

module KanagawaThreadCounter_tb_inst;

	bit clk;
	bit rst = 1'b1;

    localparam THREAD_COUNT_WIDTH = 32;

    logic incr_in;
    logic only_one_thread_in;
    logic [THREAD_COUNT_WIDTH-1:0] max_thread_id_in;
    logic [THREAD_COUNT_WIDTH-1:0] thread_id_out;

    logic count_reached_out;

    clocking cb @(posedge clk);
        // Inputs from DUT, outputs to DUT
        input count_reached_out;
        input thread_id_out;
        input in_incr_in = incr_in;

        output rst;
        output incr_in;
        output only_one_thread_in;
        output max_thread_id_in;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int threads_counted;

        $info("Starting ThreadCounter test");

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        // Thread count > 1 test
        for (int thread_count = 1; thread_count < 100; thread_count++) begin
            threads_counted = 0;

            cb.only_one_thread_in <= (thread_count == 1);
            cb.max_thread_id_in <= thread_count - 1;

            while (1'b1) begin
                cb.incr_in <= $urandom();

                @(cb);

                assert(cb.thread_id_out == threads_counted) else $error("thread_id_out had incorrect value");

                if (cb.in_incr_in) begin
                    threads_counted++;

                    if (threads_counted == thread_count) begin
                        assert(cb.count_reached_out) else $error("count_reached_out not set when it should have been");

                        break;
                    end
                    else begin
                        assert(~cb.count_reached_out) else $error("count_reached_out set when it should not have been");
                    end
                end
                else begin
                    assert(~cb.count_reached_out) else $error("count_reached_out = 1 when incr_in = 0");
                end
            end
        end

        $info("SUCCESS");
        $finish;
    end

    KanagawaThreadCounter
    #(
        .THREAD_COUNT_WIDTH(THREAD_COUNT_WIDTH),
        .HAS_LITERAL_MAX_THREAD_ID(0),
        .LITERAL_MAX_THREAD_ID(0)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .incr_in(incr_in),
        .only_one_thread_in(only_one_thread_in),
        .max_thread_id_in(max_thread_id_in),

        .count_reached_out(count_reached_out),
        .thread_id_out(thread_id_out)
    );
endmodule
