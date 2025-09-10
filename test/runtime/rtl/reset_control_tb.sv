// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Testbench for KanagawaResetControl
`timescale 1 ns / 1 ps

module KanagawaResetControl_tb
#(
    parameter TB_FAN_OUT_LEVELS = 0
);
    KanagawaResetControl_tb_inst
    #(
        .TB_FAN_OUT_LEVELS(TB_FAN_OUT_LEVELS)
    )
    inst
    ();
endmodule

module KanagawaResetControl_tb_inst
#(
    parameter TB_FAN_OUT_LEVELS = 0
)
();
	bit clk;
	bit rst = 1'b1;

    localparam WIDTH = 20;
    localparam DELAY_CYCLES = 11;
    localparam HOLD_CYCLES = 17;

    // interface with DUT
    logic rst_out;
    logic reset_sequence_finished_this_cycle_out;
    logic [WIDTH-1:0] rst_delayed_out;

    clocking cb @(posedge clk);
        // Inputs from DUT, outputs to DUT
        input rst_out;
        input reset_sequence_finished_this_cycle_out;
        input rst_delayed_out;

        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        $info("Starting KanagawaResetControl test");

        @(cb);

        // On power-up, rst_delayed_out should be 1.  It should stay this way until input reset goes to 0, and (DELAY_CYCLES + HOLD_CYCLES) cycles have passed
        // This is important on Stratix 10 where power up values of registers are random (hyper registers power up to 1)
        // Users of KanagawaResetControl assume that the output reset values start as 1
        for (int i = 0; i < 100; i++) begin
            $display("Checking initial value on cycle: %d", i);

            for (int x = 0; x < WIDTH; x++) begin
                assert(rst_delayed_out[x] == 1'b1) else $error("Initial rst_delayed_out was not 1");
            end

            @(cb);
        end

        // Clear reset signal and wait for it to propagate out
        cb.rst <= 1'b0;
        @(cb);

        for (int i = 0; i < (DELAY_CYCLES + HOLD_CYCLES); i++) begin
            $display("Waiting for clear, cycle: %d, %d", i, rst_delayed_out[0]);
            @(cb);
        end

        // Test the whole process a few times
        for (int test_index = 0; test_index < 5; test_index++) begin
            $display("Iteration: %d", test_index);

            // Output reset values should be 0 at this point
            for (int x = 0; x < WIDTH; x++) begin
                assert(rst_delayed_out[x] == 1'b0) else $error("Initial rst_delayed_out was not 0");
            end

            // Signal a reset
            cb.rst <= 1'b1;
            @(cb);

            cb.rst <= 1'b0;

            // rst_delayed_out should stay low for DELAY_CYCLES cycles total (the cycle where rst is high counts as one)
            for (int i = 0; i < (DELAY_CYCLES - 1); i++) begin

                // Allow these signals to be 'x on the first test iteration
                if (test_index > 0) begin
                    assert(reset_sequence_finished_this_cycle_out == 1'b0) else $error("reset_sequence_finished_this_cycle_out was not 0 during delay period");

                    for (int x = 0; x < WIDTH; x++) begin
                        assert(rst_delayed_out[x] == 1'b0) else $error("rst_delayed_out was not 0 during delay period");
                    end
                end

                @(cb);
            end

            // rst_delayed_out should go high for HOLD_CYCLES
            for (int i = 0; i < HOLD_CYCLES; i++) begin

                $display("i: %d, reset_sequence_finished_this_cycle_out: %d", i, reset_sequence_finished_this_cycle_out);

                assert(reset_sequence_finished_this_cycle_out == 1'b0) else $error("reset_sequence_finished_this_cycle_out was not 0 during hold period");

                for (int x = 0; x < WIDTH; x++) begin
                    assert(rst_delayed_out[x] == 1'b1) else $error("rst_delayed_out was not 1 during hold period");
                end

                @(cb);
            end

            // reset_sequence_finished_this_cycle_out should be high on this cycle only
            assert(reset_sequence_finished_this_cycle_out == 1'b1) else $error("reset_sequence_finished_this_cycle_out was not 0 during hold period");

            // rst_delayed_out should go low and stay low after that
            for (int i = 0; i < 1000; i++) begin
                @(cb);

                assert(reset_sequence_finished_this_cycle_out == 1'b0) else $error("reset_sequence_finished_this_cycle_out was not 0 after hold period");

                for (int x = 0; x < WIDTH; x++) begin
                    assert(rst_delayed_out[x] == 1'b0) else $error("rst_delayed_out was not 0 after hold period");
                end
            end
        end

        $info("SUCCESS");
        $finish;
    end

    KanagawaResetControl
    #(
        .WIDTH(WIDTH),
        .DELAY_CYCLES(DELAY_CYCLES),
        .HOLD_CYCLES(HOLD_CYCLES),
        .FAN_OUT_LEVELS(TB_FAN_OUT_LEVELS),
        .INIT_VAL(1'b1)
    )
    dut
    (
        .clk(clk),
        .rst_in(rst),
        .has_others_completed_in(1'b1),
        .rst_and_startup_done_out(),
        .rst_delayed_out(rst_delayed_out),
        .reset_sequence_finished_this_cycle_out(reset_sequence_finished_this_cycle_out)
    );
endmodule
