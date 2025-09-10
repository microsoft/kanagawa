// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

// Randomized test
// Mostly to test almost_empty handling
module KanagawaFifoPtrsEx_tb
#(
    parameter TB_DEPTH = 27,
    parameter TB_ALMOST_FULL_MARGIN = 5,
    parameter TB_ALMOST_EMPTY_MARGIN = 3,
    parameter TB_ITERATION_COUNT = 100
);
    import KanagawaSimStallerPolicies::*;

    bit clk;
    logic rst;

    localparam LOG_DEPTH = $clog2(TB_DEPTH);
    localparam logic [LOG_DEPTH:0] WRALMOST_FULL_THRESHOLD = TB_DEPTH - TB_ALMOST_FULL_MARGIN;

    typedef logic [0:0] bogus_data_t;

    logic wrreq;
    logic full;
    logic afull;
    logic [LOG_DEPTH-1:0] wrptr;

    logic [LOG_DEPTH:0] usedw;

    logic rdreq;
    logic empty;
    logic aempty;
    logic [LOG_DEPTH-1:0] rdptr;

    // Generate clock
    always #5 clk = ~clk;

	KanagawaFifoPtrsEx
    #(
        .DEPTH(TB_DEPTH),
        .ALMOST_FULL_MARGIN(TB_ALMOST_FULL_MARGIN),
        .ALMOST_EMPTY_MARGIN(TB_ALMOST_EMPTY_MARGIN)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .wrreq_in(wrreq),
        .full_out(full),
        .almost_full_out(afull),
        .wrptr_out(wrptr),

        .usedw_out(usedw),

        .rdreq_in(rdreq),
        .empty_out(empty),
        .almost_empty_out(aempty),
        .rdptr_out(rdptr)
    );

    KanagawaSimMailboxToFifoWrite
    #(
        .T(bogus_data_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(1)
    )
    writer
    (
        .clk(clk),
        .rst(rst),

        .wrreq_out(wrreq),
        .wrdata_out(),
        .wrfull_in(full)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T(bogus_data_t),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,10,1,10)),
        .STALLER_SEED(2)
    )
    reader
    (
        .clk(clk),
        .rst(rst),

        .rdreq_out(rdreq),
        .empty_in(empty),
        .data_in(1'b0)
    );

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        input rst_last_cycle = rst;
        output rst_out = rst;

        input #0 full;
        input #0 afull;
        input #0 wrptr;
        input #0 wrreq;

        input #0 usedw;

        input #0 empty;
        input #0 aempty;
        input #0 rdptr;
        input #0 rdreq;
    endclocking

    task automatic monitor_control;
        bit reset_at_least_once = 1'b0;
        logic expected_full;
        logic expected_afull;
        logic [LOG_DEPTH-1:0] expected_wrptr;

        logic [LOG_DEPTH:0] expected_usedw;

        logic expected_empty;
        logic expected_aempty;
        logic [LOG_DEPTH-1:0] expected_rdptr;

        expected_full = 1'bx;
        expected_afull = 1'bx;
        expected_wrptr = 'x;
        expected_usedw = 'x;
        expected_empty = 1'bx;
        expected_aempty = 1'bx;
        expected_rdptr = 'x;

        forever begin
            @(cb);

            // Verify against expected values for this cycle
            if (reset_at_least_once) begin
                assert(cb.full === expected_full) else $error("Mismatch on full: expected %x but received %x", expected_full, cb.full);
                assert(cb.afull === expected_afull) else $error("Mismatch on afull: expected %x but received %x", expected_afull, cb.afull);
                assert(cb.full === expected_full) else $error("Mismatch on full: expected %x but received %x", expected_full, cb.full);
                assert(cb.empty === expected_empty) else $error("Mismatch on empty: expected %x but received %x", expected_empty, cb.empty);
                assert(cb.aempty === expected_aempty) else $error("Mismatch on aempty: expected %x but received %x", expected_aempty, cb.aempty);
                assert(cb.wrptr === expected_wrptr) else $error("Mismatch on wrptr: expected %x but received %x", expected_wrptr, cb.wrptr);
                assert(cb.rdptr === expected_rdptr) else $error("Mismatch on rdptr: expected %x but received %x", expected_rdptr, cb.rdptr);
            end

            // Calculate expected values for next cycle
            if (cb.rst_in) begin
                // In reset for at least 1 cycle
                expected_full = 1'b1;
                expected_afull = 1'b1;
                expected_wrptr = '0;
                expected_usedw = '0;
                expected_empty = 1'b1;
                expected_aempty = 1'b1;
                expected_rdptr = '0;
                reset_at_least_once = 1'b1;
            end
            else if (cb.rst_last_cycle) begin
                // Just coming out of reset
                expected_full = 1'b0;
                expected_afull = 1'b0;
                expected_wrptr = '0;
                expected_usedw = '0;
                expected_empty = 1'b1;
                expected_aempty = 1'b1;
                expected_rdptr = '0;
            end
            else begin
                if (cb.wrreq) begin
                    expected_wrptr = (expected_wrptr == (TB_DEPTH-1)) ? '0 : LOG_DEPTH'(expected_wrptr+1);
                end
                if (cb.rdreq) begin
                    expected_rdptr = (expected_rdptr == (TB_DEPTH-1)) ? '0 : LOG_DEPTH'(expected_rdptr+1);
                end

                if (cb.wrreq && !cb.rdreq) begin
                    assert(!cb.full) else $error("Testbench is attempting to write when DUT is asserting full");

                    expected_usedw++;

                    expected_afull = expected_usedw > WRALMOST_FULL_THRESHOLD;
                    expected_full = expected_usedw == TB_DEPTH;
                end
                else if (!cb.wrreq && cb.rdreq) begin
                    assert(!cb.empty) else $error("Testbench is attempting to read when DUT is asserting empty");

                    expected_usedw--;

                    expected_aempty = expected_usedw <= TB_ALMOST_EMPTY_MARGIN;
                    expected_empty = expected_usedw == 0;
                end


            end
        end
    endtask

    task automatic run_test_iterations;

        repeat (TB_ITERATION_COUNT) begin
            bogus_data_t item;
            bit timed_out;

            repeat (TB_DEPTH) begin
                writer.put(1'b0);
            end

            repeat(TB_DEPTH) begin
                reader.get_with_timeout(1000, item, timed_out);
                assert(!timed_out) else $error("Timed out awaiting output from DUT");
            end
        end

        $finish;
    endtask

    initial begin
        // Align to clock edge
        @(cb);

        // Assert reset
        cb.rst_out <= 1'b1;
        @(cb);

        // De-assert reset
        cb.rst_out <= 1'b0;
        @(cb);

        fork
            monitor_control();
            run_test_iterations();
        join_none
    end

endmodule
