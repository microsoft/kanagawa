// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

import KanagawaTypes::*;
import KanagawaSimTaskHelpers::*;
import KanagawaSimStallerPolicies::InclusiveRangeStallPolicy;

module Testbench;

    logic rst = 1'b1;
    logic clk = 1'b0;
    logic rst_and_startup_done;

    always #2 clk = ~clk;

    localparam OVERFLOW_TEST_SIZE = 32768;

    typedef logic [4:0] uint5_t;
    typedef logic [31:0] uint32_t;

    // SimpleExportedClass::TimesFive connections
    // SimpleExportedClass connections
    logic TimesFive_valid_in;
    uint32_t TimesFive_x_in;
    logic TimesFive_rdy_out;
    logic TimesFive_rden_in;
    logic TimesFive_empty_out;
    uint32_t TimesFive_result_out;

    KanagawaSimMailboxToReadyValid
    #(
        .T(uint32_t)
    )
    times_five_input
    (
        .clk(clk),
        .rst(rst),

        .valid_out(TimesFive_valid_in),
        .data_out(TimesFive_x_in),
        .ready_in(TimesFive_rdy_out)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T(uint32_t)
    )
    times_five_output
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(TimesFive_rden_in),
        .empty_in(TimesFive_empty_out),
        .data_in(TimesFive_result_out)
    );

    SimpleExportedClass SimpleExportedClass_dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done),

        .TimesFive_valid_in(TimesFive_valid_in),
        .TimesFive_x_in(TimesFive_x_in),
        .TimesFive_rdy_out(TimesFive_rdy_out),

        .TimesFive_rden_in(TimesFive_rden_in),
        .TimesFive_empty_out(TimesFive_empty_out),
        .TimesFive_result_out(TimesFive_result_out),

        .stall_rate_supported_out(),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    task TestTimesFiveClass();
        $display("TimesFive Test");

        for (int i = 0; i < 10; i++) times_five_input.put(i);

        for (int i = 0; i < 10; i++) begin
            uint32_t actual;
            uint32_t expected;

            while(times_five_output.is_empty()) @(posedge clk);

            times_five_output.get(actual);

            expected = i * 5;

            assert(actual == expected) else $error(1, "exporttestclass::TimesFive returned the wrong result.  Actual: %d Expected: %d", actual, expected);
        end
    endtask

    initial begin
        $display("Resetting");

        // Reset
        for(int i=0; i < 10; i++) @(posedge clk);
        rst = 1'b0;
        wait(rst_and_startup_done);

        // Wait for reset to complete
        @(posedge clk)

        TestTimesFiveClass();

        $finish;
    end


endmodule
