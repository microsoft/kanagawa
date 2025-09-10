// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Directed testing for handling of almost_empty
`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

module KanagawaWriteDelayFifoDirected_tb
#(
    parameter ALMOSTEMPTY_VAL = 4
)
(
);
    localparam LOG_DEPTH = 4;
    localparam DEPTH = 2**LOG_DEPTH;
    localparam WIDTH = 32;
    localparam ALMOSTFULL_ENTRIES = 2;

    typedef logic [WIDTH-1:0] Data_t;

    logic clk = 1'b0;
    logic rst = 1'b1;
    logic wren;
    logic almost_full;
    logic rden;
    logic empty;

    always #5 clk = ~clk;

    clocking cb @(posedge clk);
        output rst_out = rst;
        input rst_in = rst;

        output wren, rden;
        input almost_full, empty;
        input #0 almost_full_now = almost_full, empty_now = empty, internal_empty_now = dut.genblk1.fifo.empty, internal_full_now = dut.genblk1.fifo.full;
    endclocking

    KanagawaWriteDelayFifo
    #(
        .DEPTH                  (DEPTH),
        .WIDTH                      (WIDTH),
        .WRITE_DELAY                (0),
        .ALMOSTFULL_ENTRIES           (ALMOSTFULL_ENTRIES),
        .USE_LUTRAM                 (0),
        .ALMOSTEMPTY_VAL            (ALMOSTEMPTY_VAL)
    ) dut
    (
        .clock              (clk),
        .rst                (rst),

        .wrreq              (wren),
        .data               ('0),
        .full               (almost_full),
        .overflow_out       (),

        .rdreq              (rden),
        .q                  (),
        .empty              (empty),
        .underflow_out      ()
    );

    task automatic push_pop(input bit push, input bit pop);
        if (push) begin
            assert(!cb.internal_full_now) else $error("Attempting to push entry on FIFO when internal FIFO state indicates it is full");
            cb.wren <= 1'b1;
        end
        if (pop) begin
            assert(!cb.internal_empty_now) else $error("Attempting to pop entry on FIFO when internal FIFO state indicates it is empty");
            cb.rden <= 1'b1;
        end
        @(cb);
        cb.wren <= 1'b0;
        cb.rden <= 1'b0;
    endtask

    task automatic push();
        push_pop(1'b1, 1'b0);
    endtask

    task automatic pop();
        push_pop(1'b0, 1'b1);
    endtask

    task automatic fill();
        int num_pushed = 0;
        int timeout = 0;

        while (num_pushed < DEPTH - ALMOSTFULL_ENTRIES) begin
            if (!cb.internal_full_now) begin
                push();
                num_pushed++;
                timeout = 0;
            end
            else begin
                ++timeout;
                if (timeout > 1000) begin
                    $error("Timed out waiting for FIFO to become not full");
                    timeout = 0;
                end
                @(cb);
            end
        end
        $display("fill pushed %0d out of expected %0d entries", num_pushed, DEPTH-ALMOSTFULL_ENTRIES);
    endtask

    task automatic drain(input int usedw);
        int num_popped = 0;
        int timeout = 0;

        timeout = 0;
        while (num_popped < usedw) begin
            if (!cb.empty_now) begin
                pop();
                ++num_popped;
                timeout = 0;
            end
            else begin
                ++timeout;
                if (timeout > 1000) begin
                    $error("Timed out waiting for FIFO to become not empty");
                    timeout = 0;
                end
                @(cb);
            end
        end

        $display("drain popped %0d out of expected %0d entries", num_popped, usedw);
    endtask

    // Drive reset
    initial begin
        cb.rst_out <= 1'b1;
        repeat (3) @(cb);
        cb.rst_out <= 1'b0;
    end

    initial $timeformat(-9, 2, " ns", 15);

    // Main entry point
    initial begin
        automatic int usedw;
        automatic int timeout;
        automatic int write_to_not_empty_latency;

        cb.rden <= 1'b0;
        cb.wren <= 1'b0;

        while (cb.rst_in) @(cb);

        // Give the dut a few cycles to recover from reset.
        for (int i = 0; i < 10; i++) begin
            @(cb);
        end

        // Determine the write to !empty latency
        assert(cb.empty_now) else $error("Expected FIFO to be empty but it is not");
        push();

        write_to_not_empty_latency = 1;
        while (cb.empty_now) begin
            ++write_to_not_empty_latency;
            @(cb);
        end

        // Pop the entry we just put on there
        pop();

        $display("Write to !empty latency is %0d", write_to_not_empty_latency);

        fill();
        usedw = DEPTH-ALMOSTFULL_ENTRIES;

        // Now drain the FIFO
        drain(usedw);

        // Empty should stay asserted now because the FIFO really is empty
        repeat (3) begin
            @(cb);
        end

        // Now push write_to_not_empty_latency-1 entries, and then pop them off, ensuring we don't underflow due to the write-to-empty latency of the FIFO
        usedw = 0;
        repeat (write_to_not_empty_latency-1) begin
            push();
            ++usedw;
        end

        drain(usedw);

        // Empty should stay asserted now because the FIFO really is empty
        repeat (10) begin
            assert(cb.empty_now) else $error("Expected empty to assert and stay asserted because FIFO should be empty, but it did not");
            @(cb);
        end

        @(cb);

        // Repeatedly fill, then drain to some level, and then push a single entry to the FIFO and try and pop as soon as it asserts !empty
        // This catches a corner case regarding write-to-empty latency and the logic using almost_empty internal to the DUT
        for (int threshold = 0; threshold < write_to_not_empty_latency+1; ++threshold) begin
            $display("Time %t:Beginning fill/drain/push test for threshold: %0d", $time, threshold);
            fill();
            usedw = DEPTH-ALMOSTFULL_ENTRIES;
            timeout = 0;
            while (usedw > threshold) begin
                if (!cb.empty_now) begin
                    pop();
                    usedw--;
                    timeout = 0;
                end
                else begin
                    ++timeout;
                    if (timeout > 1000) begin
                        $error("Timed out waiting for FIFO to become not empty");
                        timeout = 0;
                    end
                    @(cb);
                end
            end
            push();
            timeout = 0;
            while (cb.empty_now) begin
                ++timeout;
                if (timeout > 1000) begin
                    $error("Timed out waiting for FIFO to become not empty");
                    timeout = 0;
                end
                @(cb);
            end
            while (!cb.empty_now)
                pop();
        end

        $info("SUCCESS");
        $finish;
    end

`ifndef NO_DYNAMIC_ASSERTS
// synopsys translate_off
    assert property (@(posedge clk) dut.genblk1.fifo.empty |-> (dut.empty || rst )) else $error ("Internal empty is asserted, but external empty is not");
    assert property (@(posedge clk) dut.genblk1.fifo.full  |-> (!dut.wrreq || rst )) else $error ("Internal full is asserted, but wrreq_in is asserted");
// synopsys translate_on
`endif

endmodule
