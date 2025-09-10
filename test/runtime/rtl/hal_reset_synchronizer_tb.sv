// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

`ifdef QUESTA
`default_nettype none
`define WT wire
`else
`define WT
`endif

module KanagawaHALResetSynchronizer_tb;

    logic                   clk = 1'b0;
    logic                   arst;
    logic                   rst;

    localparam CLOCK_PERIOD_DIV_2 = 5;
    always #CLOCK_PERIOD_DIV_2 clk = ~clk;

    clocking cb @(posedge clk);
        output arst_out = arst;

        input rst;
    endclocking

    KanagawaHALResetSynchronizer dut
    (
        .clk        (clk),
        .arst       (arst),
        .rst        (rst)
    );

    initial begin
        arst = 1'b1;

        // Validate that initial value is asserted
        repeat (3) @(cb);
        assert(cb.rst)
        else $error("Initial state of synchronized reset should have been asserted but was not");

        // Validate that after two clocks, reset is de-asserted
        arst = 1'b0;
        repeat (3) @(cb);
        assert(!cb.rst)
        else $error("Synchronized reset not de-asserted after three clocks");

        // Assert asynchronous reset and validate that synchronous reset asserts
        #1 arst = 1'b1;
        @(cb);
        assert(cb.rst)
        else $error("Synchronous reset should have been asserted after asynchronous reset was asserted, but it was not");

        // Now de-assert reset asynchronously, and validate that it asserts synchronously
        #1 arst = 1'b0;
        @(cb);
        #(CLOCK_PERIOD_DIV_2-1) assert(rst == 1'b1)
        else $error("Synchronous reset was de-asserted asynchronously!");
        @(cb);
        assert(cb.rst)
        else $error("Synchronous reset was not de-asserted synchronously two cycles after asynchronous reset was de-asserted");

        $finish;
    end

endmodule : KanagawaHALResetSynchronizer_tb
