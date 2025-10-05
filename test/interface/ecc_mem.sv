// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

import KanagawaTypes::*;
import EccMemMainTypes::*;
import KanagawaSimTaskHelpers::*;
import KanagawaSimStallerPolicies::InclusiveRangeStallPolicy;

module Testbench;

    logic rst = 1'b1;
    logic clk = 1'b0;

    always #2 clk = ~clk;

    logic rst_and_startup_done;

    logic main_valid;
    logic main_uram;
    logic main_rdy;

    logic main_rden;
    logic main_empty;

    KanagawaSimMailboxToReadyValid
    #(
        .T(bit)
    )
    main_input
    (
        .clk(clk),
        .rst(rst),

        .valid_out(main_valid),
        .data_out(main_uram),
        .ready_in(main_rdy)
    );

    Result main_result;

    KanagawaSimFifoReadToMailbox
    #(
        .T(Result)
    )
    main_output
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(main_rden),
        .empty_in(main_empty),
        .data_in(main_result)
    );

    EccMemMain dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done),

        .main_valid_in(main_valid),
        .main_testURAM_in(main_uram),
        .main_rdy_out(main_rdy),

        .main_rden_in(main_rden),
        .main_empty_out(main_empty),
        .main_result_out(main_result),

        .stall_rate_supported_out(),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x)
    );

    task NoErrors(logic uram);
        Result result;

        $display("Starting NoErrors uram: %b", uram);

        main_input.put(uram);

        main_output.get(result);

        $display("errors: %d invalid: %d", result._num_error, result._num_invalid);

        assert(result._num_error == 0) else $error("Errors encountered when not expected");
        assert(result._num_invalid == 0) else $error("Invalids encountered when not expected");

        $display("Ending NoErrors");
    endtask

    task CorrectableErrors(logic uram);
        automatic int upper_bound = uram ? 8192 : 512;
        Result result;

        $display("Starting CorrectableErrors uram: %b", uram);

        if (uram) begin
            dut._testURAM__mem.__testURAM__mem_0.correctable_errors_to_inject_ff = 32'd100;
        end
        else begin
            dut._testBRAM__mem.__testBRAM__mem_0.correctable_errors_to_inject_ff = 32'd100;
        end

        main_input.put(uram);

        main_output.get(result);

        $display("errors: %d invalid: %d", result._num_error, result._num_invalid);

        assert(result._num_error > 0) else $error("Errors not encountered when expected");
        assert(result._num_invalid == 0) else $error("Invalids encountered when not expected");

        // Some reads should not see any errors
        assert(result._num_error < upper_bound) else $error("All reads detected an error");

        dut._testBRAM__mem.__testBRAM__mem_0.correctable_errors_to_inject_ff = '0;
        dut._testURAM__mem.__testURAM__mem_0.correctable_errors_to_inject_ff = '0;

        $display("Ending CorrectableErrors");
    endtask

    task UncorrectableErrors(logic uram);
        automatic int upper_bound = uram ? 8192 : 512;
        Result result;

        $display("Starting UncorrectableErrors uram: %b", uram);

        if (uram) begin
            dut._testURAM__mem.__testURAM__mem_0.uncorrectable_errors_to_inject_ff = 32'd100;
        end
        else begin
            dut._testBRAM__mem.__testBRAM__mem_0.uncorrectable_errors_to_inject_ff = 32'd100;
        end

        main_input.put(uram);

        main_output.get(result);

        $display("errors: %d invalid: %d", result._num_error, result._num_invalid);

        assert(result._num_error > 0) else $error("Errors not encountered when expected");
        assert(result._num_invalid == result._num_error) else $error("All errors should have been uncorrectable");

        // Some reads should not see any errors
        assert(result._num_error < upper_bound) else $error("All reads detected an error");

        dut._testBRAM__mem.__testBRAM__mem_0.uncorrectable_errors_to_inject_ff = '0;
        dut._testURAM__mem.__testURAM__mem_0.uncorrectable_errors_to_inject_ff = '0;

        $display("Ending UncorrectableErrors");
    endtask

    initial begin
        $display("Resetting");

        // Reset
        for(int i=0; i < 10; i++) @(posedge clk);
        rst = 1'b0;

        $display("Begin waiting for reset sequence to complete");
        wait(rst_and_startup_done);
        $display("End waiting for reset sequence to complete");

        // Test both URAM and BRAM
        for(int uram = 0; uram < 2; uram++) begin
            NoErrors(uram);

            CorrectableErrors(uram);

            UncorrectableErrors(uram);
        end

        $finish;
    end


endmodule
