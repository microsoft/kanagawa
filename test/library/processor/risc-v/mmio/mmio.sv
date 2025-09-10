//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

module main;

    integer i;

    logic clk = 1'b0;
    logic rst = 1'b1;
    logic rst_and_startup_done_out;

    logic stall_rate_supported;

    logic StartRISCV_valid = 1'b0;
    logic StartRISCV_rdy;
    logic StartRISCV_rden = 1'b0;
    logic StartRISCV_empty;

    logic LoadBlocking_rden;
    logic [31:0] LoadBlocking_addr;
    logic LoadBlocking_empty;
    logic LoadBlocking_valid;
    logic [31:0] LoadBlocking_result;
    logic LoadBlocking_rdy;

    logic StoreBlocking_rden;
    logic [31:0] StoreBlocking_addr;
    logic [31:0] StoreBlocking_value;
    logic StoreBlocking_empty;

    logic LoadLatency1_valid;
    logic [31:0] LoadLatency1_addr;
    logic [31:0] LoadLatency1_result;

    logic StoreLatency1_valid;
    logic [31:0] StoreLatency1_addr;
    logic [31:0] StoreLatency1_value;

    RISCVMMIOWrapper dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done_out),

        .stall_rate_supported_out(stall_rate_supported),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x),

        .bogus_valid_in(1'b0),
        .bogus_rden_in(1'b0),
        .bogus_empty_out(),
        .bogus_rdy_out(),

        .StartRISCV_valid_in(StartRISCV_valid),
        .StartRISCV_rdy_out(StartRISCV_rdy),
        .StartRISCV_rden_in(StartRISCV_rden),
        .StartRISCV_empty_out(StartRISCV_empty),

        .LoadBlocking_rden_in(LoadBlocking_rden),
        .LoadBlocking_addr_out(LoadBlocking_addr),
        .LoadBlocking_empty_out(LoadBlocking_empty),
        .LoadBlocking_valid_in(LoadBlocking_valid),
        .LoadBlocking_result_in(LoadBlocking_result),
        .LoadBlocking_rdy_out(LoadBlocking_rdy),

        .StoreBlocking_rden_in(StoreBlocking_rden),
        .StoreBlocking_addr_out(StoreBlocking_addr),
        .StoreBlocking_value_out(StoreBlocking_value),
        .StoreBlocking_empty_out(StoreBlocking_empty),

        .LoadLatency1_valid_out(LoadLatency1_valid),
        .LoadLatency1_addr_out(LoadLatency1_addr),
        .LoadLatency1_result_in(LoadLatency1_result),

        .StoreLatency1_valid_out(StoreLatency1_valid),
        .StoreLatency1_addr_out(StoreLatency1_addr),
        .StoreLatency1_value_out(StoreLatency1_value)
    );

    always #2 clk = ~clk;

    KanagawaSimpleDualPortRAM
    #(
        .DATA_WIDTH(32),
        .ADDR_WIDTH(10),
        .USE_LUTRAM(0),
        .USE_BRAM(1),
        .USE_OUTPUT_REG(0),
        .NUM_BYPASS_SLOTS(1),
        .DEVICE_FAMILY("Stratix10")
    )
    mem
    (
        .clk(clk),
        .rst(rst),

        .rden_in('1),

        .readaddr_in(10'(LoadLatency1_addr)),
        .data_out(LoadLatency1_result),

        .wren_in(StoreLatency1_valid),
        .writeaddr_in(10'(StoreLatency1_addr)),
        .data_in(StoreLatency1_value)
    );

    always_comb begin
        LoadBlocking_rden = 1'b0;
        LoadBlocking_result = 32'hdeadbeef;
        LoadBlocking_valid = 1'b0;

        if (!LoadBlocking_empty && LoadBlocking_rdy) begin
            LoadBlocking_rden = 1'b1;
            LoadBlocking_result = LoadBlocking_addr + 13;
            LoadBlocking_valid = 1'b1;
        end else begin
        end


        StoreBlocking_rden = 1'b0;

        if (!StoreBlocking_empty) begin
            StoreBlocking_rden = 1'b1;
            if (StoreBlocking_addr == 32'h10000040) begin
                assert(StoreBlocking_value == 32'h10000140 + 13 + (10 * (10 - 1) / 2));
            end
        end
    end

    task TestRISCV();
        $display("Testing RISCV");

        // Wait for input fifo to not be full
        wait(StartRISCV_rdy);

        @(negedge clk);
        StartRISCV_valid = 1'b1;

        @(negedge clk);
        StartRISCV_valid = 1'b0;

        // Wait for test to finish
        wait(~StartRISCV_empty);

        StartRISCV_rden = 1'b1;

        @(negedge clk);
        StartRISCV_rden = 1'b0;
    endtask

    initial begin

        // Reset
        for(i=0; i < 10; i=i+1) @(negedge clk);

        rst = 1'b0;

        TestRISCV();

        $display("Done testing");
        $finish;
    end
endmodule

