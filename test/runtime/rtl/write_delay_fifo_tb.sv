// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Randomized test
// Mostly test to test almost_empty handling
`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

module KanagawaWriteDelayFifo_tb
#(
    parameter ALMOSTEMPTY_VAL = 0
)
(

);
    bit clk;
    bit rst = 1'b1;

    typedef logic [31:0] data_t;

    logic wrreq;
    data_t data;
    logic full;

    logic rdreq;
    logic empty;
    data_t q;

    clocking cb @(posedge clk);
        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int item_count;
        data_t item;

        item_count = 10000;

        $info("Starting write delay fifo test");

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        // Create input data
        for (int i = 0; i < item_count; i++) begin
            item = i + 5;

            writer.put(item);
        end

        // wait a while
        #1000000

        // Validate that all items made it out
        $display("Num output items: %d", reader.num());
        assert(reader.num() == item_count) else $error("Unexpected item count");

        // Validate output data
        for (int i = 0; i < item_count; i++) begin
            reader.get(item);

            assert(item == (i + 5)) else $error("Unexpected output data");
        end

        $info("SUCCESS");
        $finish;
    end

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

    KanagawaWriteDelayFifo
    #(
        .DEPTH(32),
        .WRITE_DELAY(3),
        .WIDTH($bits(data_t)),
        .ALMOSTFULL_ENTRIES(12),
        .USE_LUTRAM(0),
        .ALMOSTEMPTY_VAL(ALMOSTEMPTY_VAL)
    )
    dut
    (
        .clock(clk),
        .rst(rst),

        .wrreq(wrreq),
        .data(data),
        .full(full),
        .overflow_out(),

        .rdreq(rdreq),
        .empty(empty),
        .q(q),
        .underflow_out()
    );
endmodule
