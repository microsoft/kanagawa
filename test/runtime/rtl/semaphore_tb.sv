// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaSemaphore_tb
(
);
    KanagawaSemaphore_tb_inst inst
    ();
endmodule

module KanagawaSemaphore_tb_inst
();
	bit clk;
	bit rst = 1'b1;

    localparam MAX_VALUE = 128;
    localparam SEM_DELAY = 3;

    logic incr_count_in;
    logic decr_count_in;
    logic full_out;

    clocking cb @(posedge clk);
        // Inputs from DUT, outputs to DUT
        input #0 full_out;

        output rst;
        output incr_count_in;
        output decr_count_in;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int actual_count;

        $info("Starting Semaphore test");

        cb.incr_count_in <= 1'b0;
        cb.decr_count_in <= 1'b0;

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        actual_count = 0;

        for (int iteration = 0; iteration < (1000 * 10); iteration++) begin
            if ((($urandom() & 3) != 0) & !cb.full_out) begin
                // Increment the count
                cb.incr_count_in <= 1'b1;
                actual_count++;
            end
            else begin
                cb.incr_count_in <= 1'b0;
            end

            if (($urandom() & 1) & (actual_count > 0)) begin
                // Decrement the count
                cb.decr_count_in <= 1'b1;
                actual_count--;
            end
            else begin
                cb.decr_count_in <= 1'b0;
            end

            assert(actual_count <= MAX_VALUE) else $error("semaphore allowed count to be exceeded");

            @(cb);

            $display("actual_count: %d.  full: %d", actual_count, cb.full_out);
        end

        $info("SUCCESS");
        $finish;
    end

    KanagawaSemaphore
    #(
        .MAX_VALUE(MAX_VALUE),
        .SEM_DELAY(SEM_DELAY)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .incr_count_in(incr_count_in),
        .decr_count_in(decr_count_in),
        .full_out(full_out)
    );
endmodule
