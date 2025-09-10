// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module KanagawaSimRandomLatencyChain_tb
#(
    parameter integer TB_DEPTH = 0,
    parameter integer TB_MAX_DEPTH = 0,
    parameter integer TB_ITERATIONS = 10000
);

    initial begin
        $display("KanagawaSimRandomLatencyChain_tb testing with:\n  TB_DEPTH = %0d\n  TB_MAX_DEPTH = %0d\n  TB_ITERATIONS = %0d",
                    TB_DEPTH, TB_MAX_DEPTH, TB_ITERATIONS);
    end

    localparam integer WIDTH = 32;
    localparam integer DEFAULT_TIMEOUT = 1000;

    typedef logic [WIDTH-1:0] Data_t;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic enable_in;
    logic enable_out;
    Data_t data_in;
    Data_t data_out;

    typedef bit [31:0] Cycle_t;

    Cycle_t cycle = 0;
    always @(posedge clk) cycle = cycle + 1'b1;

    typedef struct packed {
        Cycle_t cycle;
        Data_t data;
    } DataAndCycle_t;

    clocking cb @(posedge clk);
        output data_in, enable_in;
        input enable_out, data_out;
        input #0 cycle;
    endclocking

    KanagawaSimRandomLatencyChain
    #(
        .WIDTH      (WIDTH),
        .MIN_DEPTH  (TB_DEPTH),
        .MAX_DEPTH  (TB_MAX_DEPTH)
    ) dut
    (
        .clk        (clk),

        .enable_in  (enable_in),
        .data_in    (data_in),

        .enable_out (enable_out),
        .data_out   (data_out)
    );

    KanagawaSimTaskHelpers::KanagawaSimMailboxWithShutdown #(.T(DataAndCycle_t)) mb_expected = new;

    integer counts [TB_MAX_DEPTH:TB_DEPTH];

    task send_inputs;
        bit should_stall;
        Data_t data;
        DataAndCycle_t expected;
        int stall_cycles;

        cb.enable_in <= 1'b0;
        cb.data_in <= 'x;
        @(cb);

        repeat (TB_ITERATIONS) begin
            // Delay a random number of cycles one in 4 times
            should_stall = $urandom_range(0, 3) == 0;
            if (should_stall) begin
                stall_cycles = $urandom_range(1, TB_MAX_DEPTH+1);
                repeat (stall_cycles) begin
                    @(cb);
                end
            end

            data = $urandom();
            expected.data = data;
            expected.cycle = cb.cycle;
            assert(mb_expected.try_put(expected));

            cb.enable_in <= 1'b1;
            cb.data_in <= data;
            @(cb);
            cb.enable_in <= 1'b0;
            cb.data_in <= 'x;
        end
        mb_expected.shutdown();
    endtask

    int output_number;

    task check_outputs;
        DataAndCycle_t expected;
        int timeout;
        int delay;
        bit shutdown;

        output_number = 0;

        forever begin
            while (!mb_expected.try_get(expected, shutdown)) begin
                @(cb);
            end

            if (shutdown) begin
                return;
            end

            timeout = DEFAULT_TIMEOUT;
            while (!cb.enable_out) begin
                if (timeout == 0) begin
                    $error("Timed out waiting enable_out");
                    timeout = DEFAULT_TIMEOUT;
                end
                @(cb);
            end

            assert(cb.data_out == expected.data)
            else $error("Data mismatch on output number %0d. Expected 0x%x but received 0x%x", output_number, expected.data, cb.data_out);

            delay = cb.cycle - expected.cycle - 1;
            if (delay >= TB_DEPTH && delay <= TB_MAX_DEPTH) begin
                counts[delay] = counts[delay] + 1;
            end
            else begin
                $error("Output %0d received at %0d cycles after input which is outside of the expected range [%0d, %0d]", output_number, delay, TB_DEPTH, TB_MAX_DEPTH);
            end

            @(cb);
            output_number += 1;
        end
    endtask

    initial begin
        cb.enable_in <= 1'b0;
        cb.data_in <= '0;
        @(cb)

        // Allow buffer to clear
        repeat (TB_MAX_DEPTH) @(cb);

        // Verify buffer stays clear
        repeat (TB_MAX_DEPTH + 1) begin
            @(cb);
            assert(cb.data_out == '0) else $error("Expected zero output when pipeline reached steady state, but instead received 0x%x", cb.data_out);
        end

        for (int i = TB_DEPTH; i <= TB_MAX_DEPTH; ++i) begin
            counts[i] = 0;
        end

        // Now push a stream of random inputs and validate that the outputs show up, in order, within the expected latency range.
        fork
            send_inputs();
            check_outputs();
        join

        // TODO: Check distribution in counts array
        $display("Counts:");
        for (int i = TB_DEPTH; i <= TB_MAX_DEPTH; ++i) begin
            $display("   %0d cycles: %0d", i, counts[i]);
            assert(counts[i] != 0) else $warning("No outputs were received with a delay of %0d. Check to ensure iteration count is high enough to see all delays", i);
        end

        $finish;
    end

endmodule
