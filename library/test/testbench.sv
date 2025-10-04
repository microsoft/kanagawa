//
//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

module Testbench;

    integer i, fid;

    logic clk = 1'b0;
    logic rst = 1'b1;
    logic rst_and_startup_done_out;

    logic run_valid = 1'b0;
    logic run_rdy;
    logic run_rden = 1'b0;
    logic run_empty;

    logic stall_rate_supported;

    _test_runner_main kanagawa
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done_out),

        .run_valid_in(run_valid),
        .run_rdy_out(run_rdy),
        .run_rden_in(run_rden),
        .run_empty_out(run_empty),

        .stall_rate_supported_out(stall_rate_supported),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    always #2 clk = ~clk;

    clocking cb @(posedge clk);
        input run_rdy;
        input #0 run_empty;
        output run_valid, run_rden;
        output rst;
        input rst_and_startup_done_out;
    endclocking

    initial begin
        $display("Resetting");

        // Reset
        cb.rst <= 1'b1;
        cb.run_valid <= 1'b0;
        cb.run_rden <= 1'b0;

        repeat(10) @(cb);

        cb.rst <= 1'b0;

        // Wait for the reset cycle to complete
        // Note that a while loop is used instead of wait to avoid a bug with Verilator and wait with a clocking block variable in the predicate
        while (!cb.rst_and_startup_done_out) begin
            @(cb);
        end

        $display("Start testing");

        // Assert run_valid and wait for a cycle when run_rdy was also asserted
        // Note that clocking block behavior for run_rdy is that it was sampled at the end of the previous clock cycle
        cb.run_valid <= 1'b1;
        @(cb);
        while (!cb.run_rdy) begin
            @(cb);
        end
        cb.run_valid <= 1'b0;

        // Wait for test to finish
        while (cb.run_empty) begin
            @(cb);
        end

        cb.run_rden <= 1'b1;
        @(cb);
        cb.run_rden <= 1'b0;

        $display("Done testing");

        fid = $fopen("VerilogResult.txt","w");
        $fwrite(fid, "finished\n");
        $fclose(fid);
        $finish;
    end
endmodule
