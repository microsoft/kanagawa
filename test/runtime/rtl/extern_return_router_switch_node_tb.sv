// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;
import KanagawaSimTaskHelpers::*;

module KanagawaExternReturnRouter_switch_node_tb
();

	bit clk;
	bit rst = 1'b1;

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        // Assert reset signal
        rst <= 1'b1;
        // Clear reset signal
        rst <= #(5*50 + 2.5) 1'b0;
    end

    localparam NUM_OUTPUT_PORTS = 3;
    localparam DATA_WIDTH = 16;

    pd_extern_return_router_switch_intf #( .NUM_OUTPUT_PORTS(NUM_OUTPUT_PORTS), .DATA_WIDTH(DATA_WIDTH) ) switch_input_intf();
    pd_extern_return_router_switch_intf #( .NUM_OUTPUT_PORTS(NUM_OUTPUT_PORTS), .DATA_WIDTH(DATA_WIDTH) ) switch_output_intf();
    pd_fifo_intf #( .DATA_WIDTH(DATA_WIDTH) ) fifo_output_intf();

    // Sends switch input data
    typedef struct packed {
        logic [$clog2(NUM_OUTPUT_PORTS):0] match_counter;
        logic [DATA_WIDTH-1:0] data;
    } switch_payload_t;
    switch_payload_t switch_input_payload_translator;
    assign switch_input_intf.match_counter = switch_input_payload_translator.match_counter;
    assign switch_input_intf.data = switch_input_payload_translator.data;
    KanagawaSimMailboxToReadyValid
    #(
        .T( switch_payload_t ),
        .STALL_POLICY(InclusiveRangeStallPolicy#(0,1,0,1)),
        .STALLER_SEED(5)
    )
    switch_input_FIFO
    (
        .clk(clk),
        .rst(rst),

        .valid_out( switch_input_intf.valid ),
        .ready_in( switch_input_intf.ready ),
        .data_out( switch_input_payload_translator )
    );

    // Receives switch passthrough output data
    switch_payload_t switch_passthrough_payload_translator;
    assign switch_passthrough_payload_translator.match_counter = switch_output_intf.match_counter;
    assign switch_passthrough_payload_translator.data = switch_output_intf.data;
    KanagawaSimReadyValidToMailbox
    #(
        .T( switch_payload_t ),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,2,1,2)),
        .STALLER_SEED(5)
    )
    switch_passthrough_actual_FIFO
    (
        .clk(clk),
        .rst(rst),

        .valid_in( switch_output_intf.valid ),
        .ready_out( switch_output_intf.ready ),
        .data_in( switch_passthrough_payload_translator )
    );
    KanagawaSimMailboxWithShutdown #( .T( switch_payload_t ) ) switch_passthrough_expected_FIFO = new();

    // Receives switch match output data
    typedef struct packed {
        logic [DATA_WIDTH-1:0] data;
    } match_payload_t;
    match_payload_t switch_match_payload_translator;
    assign switch_match_payload_translator.data = fifo_output_intf.data;
    KanagawaSimReadyValidToMailbox
    #(
        .T( match_payload_t ),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,3,1,3)),
        .STALLER_SEED(5)
    )
    switch_match_actual_FIFO
    (
        .clk(clk),
        .rst(rst),

        .valid_in( fifo_output_intf.valid ),
        .ready_out( fifo_output_intf.ready ),
        .data_in( switch_match_payload_translator )
    );
    KanagawaSimMailboxWithShutdown #( .T( switch_payload_t ) ) switch_match_expected_FIFO = new();

    KanagawaExternReturnRouter_switch_node #(.DATA_WIDTH(DATA_WIDTH)) dut
    (
        .clk(clk),
        .rst(rst),

        .switch_input_intf(switch_input_intf),
        .switch_output_intf(switch_output_intf),

        .fifo_output_intf(fifo_output_intf)
    );

    // Generate test input vectors and expected output vectors
    localparam NUM_SIMULATED_TRANSACTIONS = 10000;
    switch_payload_t switch_input_payload, switch_output_payload;
    match_payload_t match_payload;
    logic matches_target;
    initial begin

        // Wait for reset to occur
        wait (rst == 1'b1);
        wait (rst == 1'b0);

        $display( "KanagawaExternReturnRouter_switch_node_tb Test Starting" );

        for( int iTransaction = 0; iTransaction < NUM_SIMULATED_TRANSACTIONS; iTransaction++ ) begin

            // Make about 1/2 of the return values match
            matches_target = ($urandom_range(1) == 0);

            // Match by setting the upper bit of the match_counter
            switch_input_payload.match_counter = (matches_target) ? -1 : 0;
            switch_input_payload.data = iTransaction;
            switch_input_FIFO.put(switch_input_payload);

            if (matches_target) begin
                match_payload.data = switch_input_payload.data;
                switch_match_expected_FIFO.put(match_payload);
            end
            else begin
                switch_output_payload = switch_input_payload;
                switch_output_payload.match_counter--;
                switch_passthrough_expected_FIFO.put(switch_output_payload);
            end

        end

        switch_match_expected_FIFO.shutdown();
        switch_passthrough_expected_FIFO.shutdown();

    end

    // Check actual outputs against expected outputs on all interfaces
    switch_payload_t passthrough_expected_payload, passthrough_actual_payload;
    match_payload_t match_expected_payload, match_actual_payload;
    bit [1:0] shutdown_bits;
    always @(posedge clk) begin
        if (rst) begin
        end
        else begin

            // Access match interface
            if (!switch_match_expected_FIFO.is_empty() && switch_match_actual_FIFO.num() > 0) begin
                switch_match_expected_FIFO.get(match_expected_payload, shutdown_bits[0]);
                assert(!shutdown_bits[0]) else begin
                    $error( "Switch Match Extra Data" );
                    $stop;
                end
                switch_match_actual_FIFO.get(match_actual_payload);

                assert(match_expected_payload === match_actual_payload) else begin
                    $error( "Match Expected: data %X", match_expected_payload.data );
                    $error( "Match Actual  : data %X", match_actual_payload.data );
                    $stop;
                end
            end

            // Access passthrough interface
            if (!switch_passthrough_expected_FIFO.is_empty() && switch_passthrough_actual_FIFO.num() > 0) begin
                switch_passthrough_expected_FIFO.get(passthrough_expected_payload, shutdown_bits[1]);
                assert(!shutdown_bits[1]) else begin
                    $error( "Switch Passthrough Extra Data" );
                    $stop;
                end
                switch_passthrough_actual_FIFO.get(passthrough_actual_payload);

                assert(passthrough_expected_payload === passthrough_actual_payload) else begin
                    $error( "Passthrough Expected: match_counter %X data %X", passthrough_expected_payload.match_counter, passthrough_expected_payload.data );
                    $error( "Passthrough Actual  : match_counter %X data %X", passthrough_actual_payload.match_counter, passthrough_actual_payload.data );
                    $stop;
                end
            end

            // Check for termination condition
            switch_match_expected_FIFO.peek_shutdown( shutdown_bits[0] );
            switch_passthrough_expected_FIFO.peek_shutdown( shutdown_bits[1] );
            if (&shutdown_bits == 1'b1) begin
                $display( "KanagawaExternReturnRouter_switch_node_tb Test Successful" );
                $finish;
            end

        end

    end

endmodule
