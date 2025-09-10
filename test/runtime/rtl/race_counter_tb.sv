// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaRaceCounter_tb
(
);
    KanagawaRaceCounter_tb_inst inst
    ();
endmodule

module KanagawaRaceCounter_tb_inst
();
	bit clk;
	bit rst = 1'b1;

    localparam COUNTER_WIDTH = 32;
    localparam INPUT_WIDTH = 256;
    localparam REDUCTION_SIZE = 4;
    localparam DEPTH = 4;

    // interface with DUT
    logic [INPUT_WIDTH-1:0] race_occured = '0;
    logic [COUNTER_WIDTH-1:0] race_count;

    clocking cb @(posedge clk);
        // Inputs from DUT, outputs to DUT
        input race_count;

        output rst;
        output race_occured;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int i;
        int j;

        $info("Starting RaceCounter test");

        // wait for reset to propagate
        for(i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        assert(race_count == 32'd0) else $error("Initial race count was not 0");

        // Simulate 1 race occuring (1 test for each bit)
        for (j = 0; j < INPUT_WIDTH; j++) begin

            race_occured[j] = 1'b1;
            @(cb);
            race_occured = '0;

            // Wait for signal to propagate
            for(i=0; i < DEPTH; i=i+1) @(cb);

            assert(race_count == (j + 1)) else $error("Race count was not j when expected");
        end

        // All races occuring at the same time
        race_occured = '1;
        @(cb);
        race_occured = '0;

        // Wait for signal to propagate
        for(i=0; i < DEPTH; i=i+1) @(cb);

        assert(race_count == (INPUT_WIDTH + 1)) else $error("Race count was not 2 when expected");

        // Ensure race counter stays constant
        for(i=0; i < 10; i=i+1) @(cb);

        assert(race_count == (INPUT_WIDTH + 1)) else $error("Race count was not held constant");

        // Multiple races on back-to-back cycles (testing pipelining)
        for (j = 0; j < 20; j++) begin
            race_occured = '0;
            race_occured[j] = 1'b1;
            @(cb);
        end

        race_occured = '0;

        // Wait for signal to propagate
        for(i=0; i < DEPTH; i=i+1) @(cb);

        assert(race_count == (INPUT_WIDTH + 21)) else $error("Race count pipelining bug");

        $info("SUCCESS");
        $finish;
    end

	KanagawaRaceCounter
    #(
        .COUNTER_WIDTH(32),
        .INPUT_WIDTH(INPUT_WIDTH),
        .REDUCTION_SIZE(REDUCTION_SIZE),
        .DEPTH(DEPTH)
    )
    dut
    (
        .clk(clk),
        .rst(rst),
        .race_occured_in(race_occured),
        .race_count_out(race_count)
    );
endmodule
