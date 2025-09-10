// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

module KanagawaFifoPtrs_tb;
    localparam LOG_DEPTH = 3;
    localparam WRITE_DELAY = 2;
    localparam WRITE_COUNT = 1000 * 3;

	bit clk;
	bit rst = 1'b1;

    clocking cb @(posedge clk);
        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        $info("Starting FifoPtrs test");

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        // Generate input
        for (int i = 0; i < WRITE_COUNT; i++) begin
            write_fifo.put(1'b1);
        end

        // Wait a while
        #100000

        $display("read_fifo count: %d", read_fifo.num());

        // Check output
        assert(read_fifo.num() == WRITE_COUNT) else $error("Not all writes were received");

        $info("SUCCESS");
        $finish;
    end

    logic wrreq;
    logic full;

    logic rdreq;
    logic empty;

    // Writes into fifo ptrs
    KanagawaSimMailboxToFifoWrite
    #(
        .T(bit),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(1)
    )
    write_fifo
    (
        .clk(clk),
        .rst(rst),

        .wrreq_out(wrreq),
        .wrdata_out(),
        .wrfull_in(full)
    );

    // Reads from fifo ptrs
    KanagawaSimFifoReadToMailbox
    #(
        .T(bit),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(2)
    )
    read_fifo
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(rdreq),
        .empty_in(empty),
        .data_in(1'b0)
    );

	KanagawaFifoPtrs
    #(
        .LOG_DEPTH(LOG_DEPTH),
        .WRITE_DELAY(WRITE_DELAY)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .wrreq(wrreq),
        .almost_full(full),

        .rdreq(rdreq),
        .empty(empty)
    );
endmodule
