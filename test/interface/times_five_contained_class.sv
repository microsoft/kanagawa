// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

import KanagawaTypes::*;
import KanagawaSimTaskHelpers::*;
import KanagawaSimStallerPolicies::InclusiveRangeStallPolicy;

module main;

    logic rst = 1'b1;
    logic clk = 1'b0;
    logic rst_and_startup_done;

    always #2 clk = ~clk;

    localparam OVERFLOW_TEST_SIZE = 32768;

    typedef logic [4:0] uint5_t;
    typedef logic [31:0] uint32_t;

    // TimesFiveContainedClass() connections
    logic TimesFiveContained_valid_in;
    uint32_t TimesFiveContained_a_in;
    logic TimesFiveContained_rdy_out;
    logic TimesFiveContained_rden_in;
    logic TimesFiveContained_empty_out;
    uint32_t TimesFiveContained_result_out;

    KanagawaSimMailboxToReadyValid
    #(
        .T(uint32_t)
    )
    times_five_contained_input
    (
        .clk(clk),
        .rst(rst),

        .valid_out(TimesFiveContained_valid_in),
        .data_out(TimesFiveContained_a_in),
        .ready_in(TimesFiveContained_rdy_out)
    );

    // The stall policy here is used to ensure that TestTimesFiveContainedOverflow
    // has interesting backpressure cases
    KanagawaSimFifoReadToMailbox
    #(
        .T(uint32_t),
        .DEPTH(OVERFLOW_TEST_SIZE),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,64,1,16))
    )
    times_five_contained_output
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(TimesFiveContained_rden_in),
        .empty_in(TimesFiveContained_empty_out),
        .data_in(TimesFiveContained_result_out)
    );

    TimesFiveContainedClass TimesFiveContainedClass_dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done),

        .TimesFiveContained_valid_in(TimesFiveContained_valid_in),
        .TimesFiveContained_a_in(TimesFiveContained_a_in),
        .TimesFiveContained_rdy_out(TimesFiveContained_rdy_out),

        .TimesFiveContained_empty_out(TimesFiveContained_empty_out),
        .TimesFiveContained_result_out(TimesFiveContained_result_out),
        .TimesFiveContained_rden_in(TimesFiveContained_rden_in),

        .stall_rate_supported_out(),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    task TestTimesFiveContained();
        $display("TimesFiveContained Test");

        for (int i = 0; i < 10; i++) times_five_contained_input.put(i);

        for (int i = 0; i < 10; i++) begin
            uint32_t actual;
            uint32_t expected;

            while(times_five_contained_output.is_empty()) @(posedge clk);

            times_five_contained_output.get(actual);

            expected = i * 5;

            assert(actual == expected) else $error(1, "TimesFiveContained returned the wrong result.  Actual: %d Expected: %d", actual, expected);
        end
    endtask

    // Used to ensure that internal param/return value fifos do not overflow
    // when calling a method on an exported class instance
    task TestTimesFiveContainedOverflow();
        $display("TestTimesFiveContainedOverflow Test");

        for (int i = 0; i < OVERFLOW_TEST_SIZE; i++) times_five_contained_input.put(i);

        for (int i = 0; i < OVERFLOW_TEST_SIZE; i++) begin
            uint32_t actual;
            uint32_t expected;

            while(times_five_contained_output.is_empty()) @(posedge clk);

            times_five_contained_output.get(actual);

            expected = i * 5;

            assert(actual == expected) else $error(1, "TimesFiveContained returned the wrong result.  Actual: %d Expected: %d", actual, expected);
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

        TestTimesFiveContained();

        TestTimesFiveContainedOverflow();

        $finish;
    end


endmodule
