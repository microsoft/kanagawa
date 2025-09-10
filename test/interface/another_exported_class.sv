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

    // AnotherExportedClass connections
    logic PlusThree_valid_in;
    uint32_t PlusThree_x_in;
    logic PlusThree_rdy_out;
    logic PlusThree_rden_in;
    logic PlusThree_empty_out;
    uint32_t PlusThree_result_out;
    logic CallAsyncNoBackPressureCallback_valid;
    uint32_t CallAsyncNoBackPressureCallback_value;
    logic AsyncNoBackPressureCallback_valid;
    uint32_t AsyncNoBackPressureCallback_value;

    KanagawaSimMailboxToReadyValid
    #(
        .T(uint32_t)
    )
    plus_three_input
    (
        .clk(clk),
        .rst(rst),

        .valid_out(PlusThree_valid_in),
        .data_out(PlusThree_x_in),
        .ready_in(PlusThree_rdy_out)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T(uint32_t)
    )
    plus_three_output
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(PlusThree_rden_in),
        .empty_in(PlusThree_empty_out),
        .data_in(PlusThree_result_out)
    );

    KanagawaSimMailboxToValid
    #(
        .T(uint32_t)
    ) mb_CallAsyncNoBackPressureCallback
    (
        .clk(clk),
        .rst(rst),

        .valid_out(CallAsyncNoBackPressureCallback_valid),
        .data_out(CallAsyncNoBackPressureCallback_value)
    );

    KanagawaSimValidToMailbox
    #(
        .T(uint32_t)
    ) mb_AsyncNoBackPressureCallback
    (
        .clk(clk),
        .rst(rst),

        .valid_in(AsyncNoBackPressureCallback_valid),
        .data_in(AsyncNoBackPressureCallback_value)
    );

    logic Add_rden_in;
    logic Add_empty_out;
    uint32_t Add_a_out;
    uint32_t Add_b_out;
    logic Add_valid_in;
    logic Add_rdy_out;
    uint32_t Add_result_in;
    // Implementation of Add callback
    always_comb begin
        Add_rden_in = 1'b0;
        Add_valid_in = 1'b0;
        Add_result_in = 'x;

        if (!Add_empty_out && Add_rdy_out) begin
            Add_rden_in = 1'b1;
            Add_valid_in = 1'b1;
            Add_result_in = Add_a_out + Add_b_out;
        end
    end

    AnotherExportedClass AnotherExportedClass_dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done),

        .PlusThree_valid_in(PlusThree_valid_in),
        .PlusThree_x_in(PlusThree_x_in),
        .PlusThree_rdy_out(PlusThree_rdy_out),

        .PlusThree_rden_in(PlusThree_rden_in),
        .PlusThree_empty_out(PlusThree_empty_out),
        .PlusThree_result_out(PlusThree_result_out),

        .Add_rden_in(Add_rden_in),
        .Add_empty_out(Add_empty_out),
        .Add_a_out(Add_a_out),
        .Add_b_out(Add_b_out),

        .Add_valid_in(Add_valid_in),
        .Add_rdy_out(Add_rdy_out),
        .Add_result_in(Add_result_in),

        .CallAsyncNoBackPressureCallback_valid_in(CallAsyncNoBackPressureCallback_valid),
        .CallAsyncNoBackPressureCallback_value_in(CallAsyncNoBackPressureCallback_value),

        .AsyncNoBackPressureCallback_valid_out(AsyncNoBackPressureCallback_valid),
        .AsyncNoBackPressureCallback_value_out(AsyncNoBackPressureCallback_value),

        .stall_rate_supported_out(),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    task TestPlusThreeClass();
        $display("PlusThree Test");

        for (int i = 0; i < 10; i++) plus_three_input.put(i);

        for (int i = 0; i < 10; i++) begin
            uint32_t actual;
            uint32_t expected;

            while(plus_three_output.is_empty()) @(posedge clk);

            plus_three_output.get(actual);

            expected = i + 3;

            assert(actual == expected) else $error(1, "AnotherExportedClass::PlusThree returned the wrong result.  Actual: %d Expected: %d", actual, expected);
        end
    endtask

    task automatic TestAsyncNoBackPressureCallback;
        bit timed_out;
        uint32_t actual;
        uint32_t expected = 1234;

        $display("AsyncNoBackPressureCallback Test");

        mb_CallAsyncNoBackPressureCallback.put(expected);

        mb_AsyncNoBackPressureCallback.get_with_timeout(10, actual, timed_out);
        assert(!timed_out)
        else $error("Timed out waiting for output from AsyncNoBackPressureCallback");
        assert(actual === expected)
        else $error("AsyncNoBackPressureCallback returned %0d but expected %0d", actual, expected);
    endtask


    initial begin
        $display("Resetting");

        // Reset
        for(int i=0; i < 10; i++) @(posedge clk);
        rst = 1'b0;
        wait(rst_and_startup_done);

        // Wait for reset to complete
        @(posedge clk)

        TestPlusThreeClass();

        TestAsyncNoBackPressureCallback();

        $finish;
    end


endmodule
