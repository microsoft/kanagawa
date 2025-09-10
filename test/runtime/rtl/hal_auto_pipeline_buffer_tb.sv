// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaHALAutoPipelineBuffer_tb
#(
    parameter integer TB_MIN_AUTO_PIPELINE_DEPTH = 2,
    parameter integer TB_MAX_AUTO_PIPELINE_DEPTH = 8,
    parameter integer TB_ALMOST_FULL_ENTRIES = 0,
    parameter integer TB_SIM_AUTO_PIPELINE_DEPTH = -1,
    parameter integer ITERATIONS = 100000
);

    localparam integer WIDTH = 32;
    localparam AUTO_PIPELINE_GROUP = "KanagawaHALAutoPipelineBuffer_tb";

    localparam integer EXPECTED_DEPTH = 2**$clog2(3*TB_MAX_AUTO_PIPELINE_DEPTH + TB_ALMOST_FULL_ENTRIES + 5);

    localparam DEFAULT_TIMEOUT = 10000;

    typedef logic [WIDTH-1:0] data_t;

    logic   clk = 1'b0;
    logic   rst;
    logic   almost_full;
    logic   wren;
    logic   empty;
    logic   rden;
    data_t  data_in;
    data_t  data_out;

    clocking cb @(posedge clk);
        output rst_out = rst;
        input rst_in = rst;
        output data_in, wren, rden;
        input #0 empty, almost_full, data_out;
    endclocking

    always #5 clk = ~clk;

    initial begin
        cb.rst_out <= 1'b1;
        repeat (TB_MAX_AUTO_PIPELINE_DEPTH*2+1) @(cb);
        cb.rst_out <= 1'b0;
    end

    KanagawaHALAutoPipelineBuffer
    #(
        .WIDTH                      (WIDTH),
        .MIN_AUTO_PIPELINE_DEPTH    (TB_MIN_AUTO_PIPELINE_DEPTH),
        .MAX_AUTO_PIPELINE_DEPTH    (TB_MAX_AUTO_PIPELINE_DEPTH),
        .ALMOST_FULL_ENTRIES        (TB_ALMOST_FULL_ENTRIES),
        .SIM_AUTO_PIPELINE_DEPTH    (TB_SIM_AUTO_PIPELINE_DEPTH),
        .AUTO_PIPELINE_GROUP        (AUTO_PIPELINE_GROUP)
    ) dut
    (
        .clk                (clk),
        .rst                (rst),

        .almost_full_out    (almost_full),
        .wren_in            (wren),
        .data_in            (data_in),

        .empty_out          (empty),
        .rden_in            (rden),
        .data_out           (data_out)
    );

    KanagawaSimTaskHelpers::KanagawaSimMailboxWithShutdown #(.T(data_t)) mb_expected = new;

    typedef enum integer
    {
        FILL = 0,
        DRAIN = 1,
        RANDOM = 2
    } mode_t;

    mode_t mode = FILL;

    task send_inputs;
        data_t data;
        int timeout;
        int num_written;
        int max_stall_cycles;
        int stall_cycles;

        cb.wren <= 1'b0;
        cb.data_in <= 'x;

        wait(!cb.rst_in);

        // First fill the FIFO until almost_full is asserted
        mode = FILL;
        @(cb);

        timeout = DEFAULT_TIMEOUT;
        while (cb.almost_full) begin
            if (timeout <= 0) begin
                $error("Timed out waiting for initial de-assertion of almost_full");
            end
            @(cb);
            --timeout;
        end

        while (!cb.almost_full) begin
            data = $urandom();
            cb.data_in <= data;
            cb.wren <= 1'b1;
            assert(mb_expected.try_put(data));
            @(cb);
            cb.wren <= 1'b0;
            cb.data_in <= 'x;
        end

        // Now enable DRAIN mode and wait until the FIFO is empty
        assert(!cb.empty);
        mode = DRAIN;
        timeout = DEFAULT_TIMEOUT;
        do begin
            if (timeout <= 0) begin
                $error("Timed out waiting for FIFO to drain");
                timeout = DEFAULT_TIMEOUT;
            end
            @(cb);
            --timeout;
        end while (!cb.empty);

        mode = RANDOM;
        max_stall_cycles = $urandom_range(0, 2*EXPECTED_DEPTH);
        @(cb);

        repeat (ITERATIONS) begin

            if (cb.empty || cb.almost_full) begin
                max_stall_cycles = $urandom_range(0, 2*EXPECTED_DEPTH);
            end

            stall_cycles = ($urandom_range(0, 3) == 0) ? $urandom_range(0, max_stall_cycles) : 0; // 25% chance of stall

            timeout = DEFAULT_TIMEOUT;
            while (cb.almost_full || stall_cycles != 0) begin
                if (timeout <= 0) begin
                    $error("Timed out waiting for almost_full to deassert");
                    timeout = DEFAULT_TIMEOUT;
                end
                @(cb);
                --timeout;
                if (stall_cycles > 0) begin
                    stall_cycles--;
                end
            end
            assert(!cb.almost_full);

            data = $urandom();
            cb.data_in <= data;
            cb.wren <= 1'b1;
            assert(mb_expected.try_put(data));
            @(cb);
            cb.wren <= 1'b0;
            cb.data_in <= 'x;
        end

        mb_expected.shutdown();
    endtask

    task check_outputs;
        data_t expected;
        bit shutdown;
        int timeout;
        int max_stall_cycles;
        int stall_cycles;

        cb.rden <= 1'b0;

        // Fill
        timeout = DEFAULT_TIMEOUT;
        while (mode == FILL) begin
            if (timeout <= 0) begin
                $error("Timed out waiting for FILL mode to end");
            end
            @(cb);
            --timeout;
        end

        assert(!cb.empty) else $error("empty asserted at an unexpected time");

        // Drain
        while (!cb.empty) begin
            mb_expected.get(expected, shutdown);
            assert(!shutdown) else $error("Shutdown indicated before initial DRAIN operation complete");
            assert(expected === cb.data_out) else $error("Data mismatch.\nExpected: %0d'h%x\nActual  : %0d'h%x", WIDTH, expected, WIDTH, cb.data_out);
            cb.rden <= 1'b1;
            @(cb);
            cb.rden <= 1'b0;
        end

        // Random
        max_stall_cycles = $urandom_range(0, 2*EXPECTED_DEPTH);

        forever begin
            mb_expected.get(expected, shutdown);
            if (shutdown) begin
                return;
            end

            if (cb.empty || cb.almost_full) begin
                max_stall_cycles = $urandom_range(0, 2*EXPECTED_DEPTH);
            end

            stall_cycles = ($urandom_range(0, 3) == 0) ? $urandom_range(0, max_stall_cycles) : 0; // 25% chance of stall

            timeout = DEFAULT_TIMEOUT;
            while (cb.empty) begin
                if (timeout <= 0) begin
                    $error("Timed out waiting for empty to deassert");
                    timeout = DEFAULT_TIMEOUT;
                end
                @(cb);
                --timeout;
                if (stall_cycles > 0) begin
                    stall_cycles--;
                end
            end
            repeat (stall_cycles) begin
                @(cb);
            end
            assert(!cb.empty) else $error("empty asserted at an unexpected time");

            assert(expected === cb.data_out) else $error("Data mismatch.\nExpected: %0d'h%x\nActual  : %0d'h%x", WIDTH, expected, WIDTH, cb.data_out);
            cb.rden <= 1'b1;
            @(cb);
            cb.rden <= 1'b0;
        end
    endtask

    initial begin
        fork
            send_inputs();
            check_outputs();
        join

        $finish;
    end

endmodule
