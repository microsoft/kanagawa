// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaHALSimpleDualPortRAM_tb
#(
    parameter TB_DATA_WIDTH = 1,
    parameter TB_ADDR_WIDTH = 0,
    parameter TB_USE_LUTRAM = 0,
    parameter TB_USE_BRAM = 0,
    parameter TB_USE_OUTPUT_REG = 0,
    parameter string TB_DEVICE_FAMILY = "",
    parameter TB_DEPTH = (1 << TB_ADDR_WIDTH)
);
    localparam MAX_VALUE = (1 << TB_DATA_WIDTH) - 1;

	bit clk;
    logic rst;

    logic [TB_ADDR_WIDTH-1:0] readaddr_in = '0;
    logic [TB_DATA_WIDTH-1:0] data_out;

    logic wren_in = 1'b0;
    logic [TB_ADDR_WIDTH-1:0] writeaddr_in = '0;
    logic [TB_DATA_WIDTH-1:0] data_in = '0;

    clocking cb @(posedge clk);
        output readaddr_in;
        output wren_in;
        output writeaddr_in;
        output data_in;
        output rst_out = rst;

        input data_out;
    endclocking

    logic [TB_DATA_WIDTH-1:0] reference_data [TB_DEPTH];
    logic [TB_ADDR_WIDTH-1:0] write_addr = '0;
    logic [TB_ADDR_WIDTH-1:0] read_addr = '0;

    logic [TB_DATA_WIDTH-1:0] expected_data;

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        $info("Starting KanagawaHALSimpleDualPortRAM test");

        cb.rst_out <= 1'b1;
        repeat (3) @(cb);
        cb.rst_out <= 1'b0;

        // Initialize RAM contents
        for (int i = 0; i < TB_DEPTH; i++) begin
            reference_data[i] = $urandom_range(0, MAX_VALUE);

            cb.wren_in <= 1'b1;
            cb.writeaddr_in <= i;
            cb.data_in <= reference_data[i];
            @(cb);
        end

        cb.wren_in <= 1'b0;
        @(cb);

        // Verify initial RAM contents
        for (int i = 0; i < TB_DEPTH; i++) begin
            $display("Read from %0d", i);
            cb.readaddr_in <= i;
            @(cb);
            @(cb);

            // Account for delay of output register
            if (TB_USE_OUTPUT_REG)  @(cb);

            assert(cb.data_out == reference_data[i]) else $error("Initial contents were not as expected");
        end

        // Concurrent read and write - should return old data
        for (int i = 0; i < TB_DEPTH; i++) begin
            expected_data = reference_data[i];
            reference_data[i] = $urandom_range(0, MAX_VALUE);

            cb.wren_in <= 1'b1;
            cb.writeaddr_in <= i;
            cb.data_in <= reference_data[i];

            cb.readaddr_in <= i;

            @(cb);
            @(cb);

            // Account for delay of output register
            if (TB_USE_OUTPUT_REG)  @(cb);

            $display("[%0d]: Actual: %x Expected: %x", i, cb.data_out, expected_data);
            assert(cb.data_out == expected_data) else $error("Concurrent read/write contents were not as expected");
        end

        // Verify RAM contents after concurrent read/write test
        for (int i = 0; i < TB_DEPTH; i++) begin
            cb.readaddr_in <= i;
            @(cb);
            @(cb);

            // Account for delay of output register
            if (TB_USE_OUTPUT_REG)  @(cb);

            assert(cb.data_out == reference_data[i]) else $error("Final contents were not as expected");
        end

        // Random read and write
        for (int i = 0; i < 1000; i++) begin

            write_addr = $urandom_range(0, TB_DEPTH - 1);
            read_addr = $urandom_range(0, TB_DEPTH - 1);

            expected_data = reference_data[read_addr];

            reference_data[write_addr] = $urandom_range(0, MAX_VALUE);

            cb.wren_in <= 1'b1;
            cb.writeaddr_in <= write_addr;
            cb.data_in <= reference_data[write_addr];

            cb.readaddr_in <= read_addr;

            @(cb);
            @(cb);

            // Account for delay of output register
            if (TB_USE_OUTPUT_REG)  @(cb);

            assert(cb.data_out == expected_data) else $error("Concurrent read/write contents were not as expected");
        end

        // Verify RAM contents after random read and write
        for (int i = 0; i < TB_DEPTH; i++) begin
            cb.readaddr_in <= i;
            @(cb);
            @(cb);

            // Account for delay of output register
            if (TB_USE_OUTPUT_REG)  @(cb);

            assert(cb.data_out == reference_data[i]) else $error("Final contents were not as expected");
        end

        $info("SUCCESS");
        $finish;
    end

	KanagawaSimpleDualPortRAM
    #(
        .DATA_WIDTH(TB_DATA_WIDTH),
        .ADDR_WIDTH(TB_ADDR_WIDTH),
        .USE_LUTRAM(TB_USE_LUTRAM),
        .USE_BRAM(TB_USE_BRAM),
        .USE_OUTPUT_REG(TB_USE_OUTPUT_REG),
        .DEVICE_FAMILY(TB_DEVICE_FAMILY)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .rden_in(1'b1),
        .readaddr_in(readaddr_in),
        .data_out(data_out),

        .wren_in(wren_in),
        .writeaddr_in(writeaddr_in),
        .data_in(data_in)
    );
endmodule
