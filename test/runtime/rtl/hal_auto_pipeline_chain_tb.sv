// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Testbench for KanagawaHALAutoPipelineChain
//
// Note that it isn't possible to fully test KanagawaHALAutoPipelineChain due to its
// dependence on EDA tool auto-pipelining. This testbench is designed to ensure that
// at least the minimal connectivity and fixed-length pipeline aspects are working
// correctly.
module KanagawaHALAutoPipelineChain_tb
#(
    parameter integer TB_ITERATIONS = 1000
);

    localparam integer WIDTH = 32;
    localparam integer FIXED_DEPTH = 4;
    localparam integer DEFAULT_TIMEOUT = 1000;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    typedef bit [31:0] Cycle_t;
    Cycle_t cycle = 0;
    always @(posedge clk) cycle = cycle + 1'b1;

    typedef logic [WIDTH-1:0] Data_t;

    typedef struct packed {
        logic enable;
        Data_t data;
    } EnableAndData_t;

    EnableAndData_t en_data_in;
    EnableAndData_t en_data_out;

    typedef struct packed {
        Cycle_t cycle;
        Data_t data;
    } DataAndCycle_t;

    KanagawaHALAutoPipelineChain
    #(
        .WIDTH                  (WIDTH),
        .MIN_DEPTH              (FIXED_DEPTH),
        .MAX_DEPTH              (FIXED_DEPTH),
        .AUTO_PIPELINE_GROUP    ("KanagawaHALAutoPipelineChain_tb")
    ) dut
    (
        .clk        (clk),

        .enable_in  (en_data_in.enable),
        .data_in    (en_data_in.data),

        .enable_out (en_data_out.enable),
        .data_out   (en_data_out.data)
    );

    KanagawaSimTaskHelpers::KanagawaSimMailboxWithShutdown #(.T(DataAndCycle_t)) mb_expected = new;

    clocking cb @(posedge clk);
        output en_data_in;
        input en_data_out;
        input #0 cycle;
    endclocking

    function automatic string format_en_data(input EnableAndData_t en_data);
        return $sformatf("enable: %x, data: %0d'h%x", en_data.enable, WIDTH, en_data.data);
    endfunction

    task send_inputs;
        bit should_stall;
        Data_t data;
        DataAndCycle_t expected;
        int stall_cycles;

        cb.en_data_in <= '{enable:1'b0, data:{WIDTH{1'bx}}};
        @(cb);

        repeat (TB_ITERATIONS) begin
            // Delay a random number of cycles one in 4 times
            should_stall = $urandom_range(0, 3) == 0;
            if (should_stall) begin
                stall_cycles = $urandom_range(1, FIXED_DEPTH+1);
                repeat (stall_cycles) begin
                    @(cb);
                end
            end

            data = $urandom();
            expected.data = data;
            expected.cycle = cb.cycle;
            assert(mb_expected.try_put(expected));

            cb.en_data_in <= '{enable:1'b1, data:data};
            @(cb);
            cb.en_data_in <= '{enable:1'b0, data:{WIDTH{1'bx}}};
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
            while (!cb.en_data_out.enable) begin
                if (timeout == 0) begin
                    $error("Timed out waiting enable_out");
                    timeout = DEFAULT_TIMEOUT;
                end
                @(cb);
            end

            assert(cb.en_data_out.data == expected.data)
            else $error("Data mismatch on output number %0d. Expected 0x%x but received 0x%x", output_number, expected.data, cb.en_data_out.data);

            delay = cb.cycle - expected.cycle - 1;

            assert(delay == FIXED_DEPTH)
            else $error("Output %0d received at %0d cycles after input which is outside of the expected range [%0d, %0d]", output_number, delay, FIXED_DEPTH, FIXED_DEPTH);

            @(cb);
            output_number += 1;
        end
    endtask

    initial begin
        cb.en_data_in <= '0;
        @(cb)

        // Allow buffer to clear
        repeat (FIXED_DEPTH) @(cb);

        // Verify buffer stays clear
        repeat (FIXED_DEPTH + 1) begin
            @(cb);
            assert(cb.en_data_out === '{enable:1'b0, data:0})
            else $error("Expected zero output when pipeline reached steady state, but instead received: %s", format_en_data(cb.en_data_out));
        end

        // Now push a stream of random inputs and validate that the outputs show up, in order, within the expected latency range.
        fork
            send_inputs();
            check_outputs();
        join

        $finish;
    end

endmodule
