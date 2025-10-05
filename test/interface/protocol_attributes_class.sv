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

    // ProtocolAttributesClass connections
    logic PlusFour_valid_in;
    uint32_t PlusFour_x_in;
    logic PlusFour_valid_out;
    uint32_t PlusFour_result_out;

    logic OneMore_valid_out;
    uint32_t OneMore_a_out;

    logic ShiftLeftOne_valid_in;
    uint32_t ShiftLeftOne_x_in;
    uint32_t ShiftLeftOne_result_out;

    logic ShiftLeftCallback_valid_out;
    uint32_t ShiftLeftCallback_a_out;
    uint5_t ShiftLeftCallback_amt_out;
    uint32_t ShiftLeftCallback_result_in;

    // Implementation of ShiftLeftCallback
    always_comb begin
        ShiftLeftCallback_result_in = 'x;

        if (ShiftLeftCallback_valid_out) begin
            ShiftLeftCallback_result_in = ShiftLeftCallback_a_out << ShiftLeftCallback_amt_out;
        end
    end

    KanagawaSimMailboxToValid
    #(
        .T(uint32_t)
    )
    plus_four_input
    (
        .clk(clk),
        .rst(rst),

        .valid_out(PlusFour_valid_in),
        .data_out(PlusFour_x_in)
    );

    KanagawaSimValidToMailbox
    #(
        .T(uint32_t)
    )
    plus_four_output
    (
        .clk(clk),
        .rst(rst),

        .valid_in(PlusFour_valid_out),
        .data_in(PlusFour_result_out)
    );

    KanagawaSimValidToMailbox
    #(
        .T(uint32_t)
    )
    one_more_output
    (
        .clk(clk),
        .rst(rst),

        .valid_in(OneMore_valid_out),
        .data_in(OneMore_a_out)
    );

    ProtocolAttributesClass ProtocolAttributesClass_dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done),

        .PlusFour_valid_in(PlusFour_valid_in),
        .PlusFour_x_in(PlusFour_x_in),
        .PlusFour_valid_out(PlusFour_valid_out),
        .PlusFour_result_out(PlusFour_result_out),

        .OneMore_valid_out(OneMore_valid_out),
        .OneMore_a_out(OneMore_a_out),

        .ShiftLeftOne_valid_in(ShiftLeftOne_valid_in),
        .ShiftLeftOne_x_in(ShiftLeftOne_x_in),
        .ShiftLeftOne_result_out(ShiftLeftOne_result_out),

        .ShiftLeftCallback_result_in(ShiftLeftCallback_result_in),
        .ShiftLeftCallback_valid_out(ShiftLeftCallback_valid_out),
        .ShiftLeftCallback_a_out(ShiftLeftCallback_a_out),
        .ShiftLeftCallback_amt_out(ShiftLeftCallback_amt_out),

        .stall_rate_supported_out(),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    task TestNoBackpresureClass();
        uint32_t expected;

        $display("TestNoBackpresureClass Test");

        for (int i = 0; i < 10; i++) plus_four_input.put(i);

        for (int i = 0; i < 10; i++) begin
            uint32_t actual;

            while(!plus_four_output.try_get(actual)) @(posedge clk);
            expected = i + 4;
            assert(actual == expected) else $error(1, "ProtocolAttributesClass::PlusFour returned the wrong result.  Actual: %d Expected: %d", actual, expected);

            while(!one_more_output.try_get(actual)) @(posedge clk);
            expected = i + 1;
            assert(actual == expected) else $error(1, "ProtocolAttributesClass::PlusFour passed incorrect value to callback.  Actual: %d Expected: %d", actual, expected);
        end

        for (int i = 0; i < 10; i++) begin
            ShiftLeftOne_valid_in <= 1'b1;
            ShiftLeftOne_x_in <= i;

            @(posedge clk)

            expected = i << 1;

            assert(ShiftLeftOne_result_out == expected) else $error(1, "ProtocolAttributesClass::ShiftLeftOne returned incorrect result.  Actual: %d Expected: %d", ShiftLeftOne_result_out, expected);
        end
    endtask

    initial begin
        $display("Resetting");

        // Reset
        for(int i=0; i < 10; i++) @(posedge clk);
        rst = 1'b0;
        wait(rst_and_startup_done);

        // Wait for reset to complete
        @(posedge clk);

        TestNoBackpresureClass();

        $finish;
    end


endmodule
