// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

module KanagawaReorderBuffer_tb
(
);
    KanagawaReorderBuffer_tb_inst inst
    ();
endmodule

module KanagawaReorderBuffer_tb_inst
();
	bit clk;
	bit rst = 1'b1;

    localparam LOG_DEPTH = 5;
    localparam SLOT_ID_OFFSET = 0;
    localparam SLOT_ID_WIDTH = LOG_DEPTH + 1;
    localparam DEPTH = 2**LOG_DEPTH;
    localparam DEVICE_FAMILY = "Stratix10";

    typedef struct packed
    {
        logic [7:0] other_data;
        logic [1:0] padding; // to make it easier to read in simulator
        logic which_iteration; // alternates between 0 and 1
        logic [SLOT_ID_WIDTH-2:0] slot_id;
    } data_t;

    // interface with DUT
    logic wrreq;
    logic rdreq;
    data_t data;

    logic full;
    logic empty;
    data_t q;

    clocking cb @(posedge clk);
        // Inputs from DUT, outputs to DUT
        input full;
        input empty;
        input q;

        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        data_t input_data[DEPTH];
        data_t result;

        $info("Starting KanagawaReorderBuffer test");

        // Generate input data
        for (int i = 0; i < DEPTH; i++) begin
            input_data[i].slot_id = i;
            input_data[i].which_iteration = 1'b0;
            input_data[i].padding = '0;
            input_data[i].other_data = i + 3;
        end

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        for (int iteration = 0; iteration < 1000; iteration++) begin
            // Randomize the input data
            input_data.shuffle();

            // Set other_data to unique values for each iteration
            for (int i = 0; i < DEPTH; i++) begin
                input_data[i].other_data = input_data[i].slot_id + (iteration % 15);
            end

            // Write input data into writer
            for (int i = 0; i < DEPTH; i++) begin
                writer.put(input_data[i]);
            end

            // Wait a while
            #10000

            // Validate output count
            assert(reader.num() == DEPTH) else $error("Failed get all results");

            for (int i = 0; i < DEPTH; i++) begin
                reader.get(result);

                assert(result.slot_id == i) else $error("Wrong slot id");
                assert(result.other_data == (i + (iteration % 15))) else $error("Wrong slot other_data");
            end

            // Flip the which_iteration bits for the next round
            for (int i = 0; i < DEPTH; i++) begin
                input_data[i].which_iteration = ~input_data[i].which_iteration;
            end
        end

        $info("SUCCESS");
        $finish;
    end

    // Module that writes into reorder buffer
    KanagawaSimMailboxToFifoWrite
    #(
        .T(data_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(1)
    )
    writer
    (
        .clk(clk),
        .rst(rst),

        .wrreq_out(wrreq),
        .wrdata_out(data),
        .wrfull_in(full)
    );

    // Module that reads from reorder buffer
    KanagawaSimFifoReadToMailbox
    #(
        .T(data_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(2)
    )
    reader
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(rdreq),
        .empty_in(empty),
        .data_in(q)
    );

	KanagawaReorderBuffer
    #(
        .WIDTH($bits(data_t)),
        .DEPTH(DEPTH),
        .SLOT_ID_OFFSET(SLOT_ID_OFFSET),
        .SLOT_ID_WIDTH(SLOT_ID_WIDTH),
        .DEVICE_FAMILY(DEVICE_FAMILY)
    )
    dut
    (
        .clock(clk),
        .rst(rst),

        .wrreq(wrreq),
        .data(data),
        .full(full),
        .almost_full(),
        .usedw(),
        .overflow_out(),

        .rdreq(rdreq),
        .empty(empty),
        .almost_empty(),
        .q(q),
        .underflow_out()
    );
endmodule
