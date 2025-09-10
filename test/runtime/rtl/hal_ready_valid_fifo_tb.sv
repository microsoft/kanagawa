// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Regression test for a case that occured when a pipelined command processor would consume too many input values
`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;
import KanagawaSimTaskHelpers::*;
import KanagawaTypes::*;

module KanagawaHALReadyValidFifo_tb
#(
    parameter DUAL_CLOCK = 0,
    parameter ALMOSTFULL_ENTRIES = 256
)
();

    localparam NUM_SIMULATED_TRANSACTIONS = 50000;
    localparam LOG_DEPTH = 9;
    localparam DATA_WIDTH = 16;
    localparam USE_LUTRAM = 0;

    localparam DEPTH = (1 << LOG_DEPTH);
    localparam ALMOST_FULL_THRESHOLD = DEPTH - ALMOSTFULL_ENTRIES;

    bit input_clk, output_clk;

    // Generate clocks
    always #5 input_clk = ~input_clk;
    generate
        if (DUAL_CLOCK)
            always #7 output_clk = ~output_clk;
        else
            assign output_clk = input_clk;
    endgenerate

    bit input_rst;
    initial begin
        // Assert reset signal
        input_rst <= 1'b1;
        // Clear reset signal
        input_rst <= #(5*50 + 2.5) 1'b0;
    end


    pd_fifo_intf #( .DATA_WIDTH(DATA_WIDTH) ) fifo_input_intf();
    pd_fifo_intf #( .DATA_WIDTH(DATA_WIDTH) ) fifo_output_intf();
    wire [LOG_DEPTH:0] actual_input_usedw;
    wire input_almost_full;


    default clocking input_cb @(posedge input_clk);
        default input #1ns output #1ns;

        // Inputs from DUT
        input actual_input_usedw;
        input input_almost_full;
        input fifo_output_intf_valid = fifo_input_intf.valid;
        input fifo_output_intf_ready = fifo_input_intf.ready;
        input fifo_output_intf_data = fifo_input_intf.data;

        // Outputs to DUT
        output input_rst;
    endclocking

    clocking output_cb @(posedge output_clk);
        default input #1ns output #1ns;

        // Inputs from DUT
        input fifo_output_intf_valid = fifo_output_intf.valid;
        input fifo_output_intf_data = fifo_output_intf.data;

        // Outputs to DUT
        output fifo_output_intf_ready = fifo_output_intf.ready;
    endclocking


    // The staller policy format is: INTERVAL_MIN_CYCLES, INTERVAL_MAX_CYCLES, DURATION_MIN_CYCLES, DURATION_MAX_CYCLES
    typedef logic [DATA_WIDTH-1:0] data_t;
    // Sends fifo input data
    KanagawaSimMailboxToReadyValid
    #(
        .T( data_t ),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,3,1,3)),
        .STALLER_SEED(5)
    )
    input_FIFO
    (
        .clk(input_clk),
        .rst(input_rst),

        .valid_out(fifo_input_intf.valid),
        .ready_in(fifo_input_intf.ready),
        .data_out(fifo_input_intf.data)
    );
    KanagawaSimMailboxWithShutdown #( .T( data_t ) ) output_expected_FIFO = new();


    KanagawaHALReadyValidFifo
    #(
        .DEPTH(DEPTH),
        .LOG_DEPTH(LOG_DEPTH),
        .WIDTH(DATA_WIDTH),
        .USE_LUTRAM(USE_LUTRAM),
        .ALMOSTFULL_ENTRIES(ALMOSTFULL_ENTRIES),
        .DUAL_CLOCK(DUAL_CLOCK)
    )
    dut
    (
        .input_clk(input_clk),
        .input_rst(input_rst),
        .input_intf(fifo_input_intf),
        .input_usedw(actual_input_usedw),
        .input_almost_full(input_almost_full),   // Does not apply for dual clock

        .output_clk(output_clk),
        .output_intf(fifo_output_intf)
    );

    initial $timeformat(-9, 2, " ns", 15);

    // Generate test input vectors and expected output vectors
    data_t input_data, output_data;
    data_t output_expected_data;
    int transaction_counter;
    bit [0:0] shutdown_bits;
    initial begin

        $display( "Time %t: KanagawaHALReadyValidFifo_tb Test Starting", $time );

        // Wait for reset to occur
        while (input_rst != 1'b1) begin
            @(input_cb);
        end
        while (input_rst == 1'b1) begin
            @(input_cb);
        end

        $display( "Time %t: Reset released", $time );

        // First test the ready/valid flow control behavior by loading up the input and output
        //  FIFOs with data and letting it run

        for( int iTransaction = 0; iTransaction < NUM_SIMULATED_TRANSACTIONS; iTransaction++ ) begin

            input_data = iTransaction;
            input_FIFO.put( input_data );

            output_data = input_data;
            output_expected_FIFO.put( output_data );

        end

        // Wait until all data has percolated through, checking for correctness on the way
        $display( "Time %t: Long flow control test starting", $time );
        while (!output_expected_FIFO.is_empty()) begin

            output_cb.fifo_output_intf_ready <= $urandom_range(2);

            @(output_cb);

            if (output_cb.fifo_output_intf_valid == 1'b1 && output_cb.fifo_output_intf_ready == 1'b1) begin
                // $display( "Time %t: Actual %X", $time, output_cb.fifo_output_intf_data );
                output_expected_FIFO.get(output_expected_data, shutdown_bits[0]);
                assert(output_expected_data == output_cb.fifo_output_intf_data) else begin
                    $error("Output Expected: %X, Actual %X", output_expected_data, output_cb.fifo_output_intf_data );
                    $stop;
                end
            end

        end
        output_cb.fifo_output_intf_ready <= 1'b0;

        // Check FIFO empty
        @(output_cb);
        assert(output_cb.fifo_output_intf_valid == 1'b0) else begin
            $error("Expected FIFO empty" );
            $stop;
        end

        // input_almost_full testing below
        //  input_almost_full is only partially supported for DUAL_CLOCK mode, since nothing uses it, nothing will be tested.
        //  It could be made to work, but no need right now.
        if (!DUAL_CLOCK) begin

            $display( "Time %t: Fill FIFO up until just before ALMOST_FULL_THRESHOLD", $time );
            for( int i = 0; i < ALMOST_FULL_THRESHOLD-1; i++ ) begin
                input_data = i;
                input_FIFO.put( input_data );
            end
            // Wait until all transactions go through (variable timing due to input_FIFO)
            transaction_counter = 0;
            while(transaction_counter < ALMOST_FULL_THRESHOLD-1) begin
                @(input_cb);
                transaction_counter += (input_cb.fifo_output_intf_valid == 1'b1 && input_cb.fifo_output_intf_ready == 1'b1);
            end

            @(input_cb);
            $display( "Time %t: Check almost full flag", $time );
            assert(input_cb.input_almost_full == 1'b0) else begin
                $error("Expected input_almost_full to be 0" );
                $stop;
            end

            $display( "Time %t: Add one more entry to the FIFO to hit the ALMOST_FULL_THRESHOLD", $time );
            input_data = ALMOST_FULL_THRESHOLD - 1;
            input_FIFO.put( input_data );
            // Wait until the transaction hits (variable timing due to input_FIFO)
            while (input_cb.fifo_output_intf_valid == 1'b0 || input_cb.fifo_output_intf_ready == 1'b0)
                @(input_cb);

            $display( "Time %t: Check almost full flag", $time );
            // And on the NEXT cycle, the almost full flag should be set
            // Possibly longer in the DUAL_CLOCK case due to clock crossing
            for( int i = 0; i < (DUAL_CLOCK ? 1 : 1); i++ ) begin
                @(input_cb);
            end
            assert(input_cb.input_almost_full == 1'b1) else begin
                $error("Expected input_almost_full to be 1" );
                $stop;
            end

            $display( "Time %t: Now fill it up all the way", $time );
            for( int i = 0; i < DEPTH - ALMOST_FULL_THRESHOLD; i++ ) begin
                input_data = i + ALMOST_FULL_THRESHOLD;
                input_FIFO.put( input_data );
            end
            // Wait until all transactions have gone through (variable timing due to input_FIFO)
            transaction_counter = 0;
            while(transaction_counter < DEPTH - ALMOST_FULL_THRESHOLD) begin
                @(input_cb);
                transaction_counter += (input_cb.fifo_output_intf_valid == 1'b1 && input_cb.fifo_output_intf_ready == 1'b1);
                // Check almost full flag on the way
                assert(input_cb.input_almost_full == 1'b1) else begin
                    $error("Expected input_almost_full to be 1" );
                    $stop;
                end
            end

            // Could check to be sure that it's actually full here (input_cb.fifo_output_intf_ready == 1'b0),
            //  but Xilinx gives a free +2 bonus entries, so would have to special case that.  Few people are
            //  going to complain about extra space in the FIFO.

            $display( "Time %t: Drain it until right at ALMOST_FULL_THRESHOLD", $time );
            for( int i = 0; i < DEPTH-ALMOST_FULL_THRESHOLD; i++ ) begin
                output_cb.fifo_output_intf_ready <= 1'b1;

                @(output_cb);

                // Check almost full flag on the way down
                assert(input_cb.input_almost_full == 1'b1) else begin
                    $error("Expected input_almost_full to be 1" );
                    $stop;
                end
            end
            $display( "Time %t: Take two more out (for Xilinx) and see if input_almost_full goes away", $time );
            for( int i = 0; i < 2; i++ ) begin
                output_cb.fifo_output_intf_ready <= 1'b1;
                @(output_cb);
            end
            output_cb.fifo_output_intf_ready <= 1'b0;
            // And wait for the Xilinx prog_full flag to update
            for( int i = 0; i < (DUAL_CLOCK ? 7 : 2); i++ ) begin
                @(input_cb);
            end
            // Check almost full flag
            assert(input_cb.input_almost_full == 1'b0) else begin
                $error("Expected input_almost_full to be 0" );
                $stop;
            end

            $display( "Time %t: Drain it all the way down and check input_almost_full on the way", $time );
            for( int i = 0; i < ALMOST_FULL_THRESHOLD-2; i++ ) begin
                output_cb.fifo_output_intf_ready <= 1'b1;

                @(output_cb);

                // Check almost full flag on the way down
                assert(input_cb.input_almost_full == 1'b0) else begin
                    $error("Expected input_almost_full to be 0" );
                    $stop;
                end
            end

            $display( "Time %t: Make sure that there is nothing else in the FIFO", $time );
            for( int i = 0; i < (DUAL_CLOCK ? 2 : 1); i++ ) begin
                @(output_cb);
            end
            assert(output_cb.fifo_output_intf_valid == 1'b0) else begin
                $error("Test ended, FIFO should be empty" );
                $stop;
            end

        end // if (!DUAL_CLOCK)

        $display( "Time %t: KanagawaHALReadyValidFifo_tb Test Successful", $time );
        $finish;

    end

endmodule
