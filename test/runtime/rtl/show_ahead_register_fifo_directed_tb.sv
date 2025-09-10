// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Directed testing for KanagawaHALShowAheadFifo full, empty, almost_full, and almost_empty ports
`timescale 1 ns / 1 ps

module KanagawaShowAheadRegisterFifo_Directed_tb
#(
    parameter integer TB_DEPTH = 7,
    parameter integer TB_WIDTH = 32,
    parameter integer TB_ALMOSTFULL_ENTRIES = 2,
    parameter integer TB_ALMOSTEMPTY_ENTRIES = 3,
    parameter integer TB_MUX_ON_READ = 1
);
    localparam integer LOG_DEPTH = $clog2(TB_DEPTH);
    localparam integer ALMOSTFULL_VAL = TB_DEPTH - TB_ALMOSTFULL_ENTRIES;
    localparam integer ALMOSTEMPTY_VAL = TB_ALMOSTEMPTY_ENTRIES;

    logic rst;
    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst_delayed;
    always_ff @(posedge clk) begin
        rst_delayed <= rst;
    end

    logic wrreq;
    logic [TB_WIDTH-1:0] data;
    logic full;
    logic almost_full;
    logic [LOG_DEPTH:0] usedw;

    logic rdreq;
    logic empty;
    logic almost_empty;
    logic [TB_WIDTH-1:0] q;

    KanagawaShowAheadRegisterFifo
    #(
        .DEPTH          (TB_DEPTH),
        .LOG_DEPTH          (LOG_DEPTH),
        .WIDTH              (TB_WIDTH),
        .ALMOSTFULL_ENTRIES (TB_ALMOSTFULL_ENTRIES),
        .ALMOSTEMPTY_VAL    (ALMOSTEMPTY_VAL),
        .MUX_ON_READ        (TB_MUX_ON_READ)
    ) dut
    (
        .clock          (clk),
        .rst            (rst),

        .wrreq          (wrreq),
        .data           (data),
        .full           (full),
        .almost_full    (almost_full),
        .usedw          (usedw),

        .rdreq          (rdreq),
        .empty          (empty),
        .almost_empty   (almost_empty),
        .q              (q)
    );

    clocking cb @(posedge clk);
        input  rst_in = rst;
        output rst_out = rst;

        output wrreq, data, rdreq;
        input #0 full, almost_full, usedw, empty, almost_empty, q;
    endclocking

    function automatic void check_control_state(input integer expected_count);
        bit expected_empty;
        bit expected_full;
        bit expected_almost_empty;
        bit expected_almost_full;

        expected_empty = (expected_count < 1);
        expected_full = (expected_count >= TB_DEPTH) || rst_delayed;
        expected_almost_full = (expected_count >= ALMOSTFULL_VAL) || rst_delayed;
        expected_almost_empty = (expected_count <= ALMOSTEMPTY_VAL);

        assert(expected_count === cb.usedw)
        else $error("Incorrect usedw value. Expected %0d but found %0d", expected_count, cb.usedw);

        assert(expected_empty === cb.empty)
        else $error("Incorrect empty value for count %0d. Expected %x but found %x", expected_count, expected_empty, cb.empty);

        assert(expected_full === cb.full)
        else $error("Incorrect full value for count %0d. Expected %x but found %x", expected_count, expected_full, cb.full);

        assert(expected_almost_empty === cb.almost_empty)
        else $error("Incorrect almost_empty value for count %0d. Expected %x but found %x", expected_count, expected_almost_empty, cb.almost_empty);

        assert(expected_almost_full === cb.almost_full)
        else $error("Incorrect almost_full value for count %0d. Expected %x but found %x", expected_count, expected_almost_full, cb.almost_full);
    endfunction

    initial begin
        logic [TB_WIDTH-1:0] expected;
        logic [TB_WIDTH-1:0] expected_values[$];
        integer expected_count;

        cb.rst_out <= 1'b1;
        cb.wrreq <= 1'b0;
        cb.rdreq <= 1'b0;

        repeat (3) @(cb);

        cb.rst_out <= 1'b0;
        @(cb);

        // Check initial conditions after reset;
        expected_count = 0;

        check_control_state(expected_count);

        // Push values until full
        for (int i = 0; i < TB_DEPTH; ++i) begin

            expected = $urandom();
            expected_values.push_back(expected);

            cb.data <= expected;
            cb.wrreq <= 1'b1;
            @(cb);
            cb.data <= 'x;
            cb.wrreq <= 1'b0;

            ++expected_count;
            check_control_state(expected_count);
        end

        @(cb);

        // Pop values until empty
        for (int i = 0; i < TB_DEPTH; ++i) begin

            expected = expected_values.pop_front();
            assert(expected === cb.q)
            else $error("Data mismatch on read of value %0d. Expected 0x%x but found 0x%x", i, expected, cb.q);

            cb.rdreq <= 1'b1;
            @(cb);
            cb.rdreq <= 1'b0;

            --expected_count;
            check_control_state(expected_count);
        end

        $finish;
    end
endmodule
