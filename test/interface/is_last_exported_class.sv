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

    // IsLastExportedClass connections
    logic IsLast_Go_valid_in;
    logic IsLast_Go_rdy_out;

    logic IsLast_cb_rden_in;
    uint32_t IsLast_cb_a_out;
    logic IsLast_cb_is_last_out;
    logic IsLast_cb_empty_out;

    KanagawaSimMailboxToReadyValid IsLastExportedClass_go_input
    (
        .clk(clk),
        .rst(rst),

        .valid_out(IsLast_Go_valid_in),
        .data_out(),
        .ready_in(IsLast_Go_rdy_out)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T(uint32_t)
    )
    IsLastExportedClass_cb_output
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(IsLast_cb_rden_in),
        .empty_in(IsLast_cb_empty_out),
        .data_in(IsLast_cb_a_out)
    );

    IsLastExportedClass IsLastExportedClass_dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done),

        .Go_valid_in(IsLast_Go_valid_in),
        .Go_rdy_out(IsLast_Go_rdy_out),

        .cb_rden_in(IsLast_cb_rden_in),
        .cb_a_out(IsLast_cb_a_out),
        .cb_is_last_out(IsLast_cb_is_last_out),
        .cb_empty_out(IsLast_cb_empty_out),

        .stall_rate_supported_out(),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    task TestIsLast();
        $display("IsLast Test");

        IsLastExportedClass_go_input.put(1'b1);

        for (int i = 0; i < 32; i++) begin
            uint32_t actual;
            uint32_t expected;

            while(IsLastExportedClass_cb_output.is_empty()) @(posedge clk);

            IsLastExportedClass_cb_output.get(actual);

            // 16 calls from 1 call site, then 16 calls from another
            expected = i % 16;

            assert(actual == expected) else $error(1, "IsLastExportedClass callback called with incorrect value.  Actual: %d Expected: %d", actual, expected);
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

        TestIsLast();

        $finish;
    end


endmodule
