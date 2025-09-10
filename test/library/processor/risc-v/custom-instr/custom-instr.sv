//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

module main;
    import RISCVCustomInstrWrapperTypes::*;

    integer i;

    logic clk = 1'b0;
    logic rst = 1'b1;
    logic rst_and_startup_done_out;

    logic stall_rate_supported;

    logic StartRISCV_valid = 1'b0;
    logic StartRISCV_rdy;
    logic StartRISCV_rden = 1'b0;
    logic StartRISCV_empty;

    logic CustomInstr_valid;
    logic CustomInstr_hid;
    logic [4:0] CustomInstr_major_opcode;
    logic [2:0] CustomInstr_minor_opcode;
    logic [31:0] CustomInstr_op1;
    logic [31:0] CustomInstr_op2;
    logic [31:0] CustomInstr_imm;
    logic [6:0] CustomInstr_funct7;
    logic [31:0] CustomInstr_result;

    RISCVCustomInstrWrapper dut
    (
        .clk(clk),
        .rst(rst),
        .rst_and_startup_done_out(rst_and_startup_done_out),

        .stall_rate_supported_out(stall_rate_supported),
        .stall_rate_valid_in(1'b0),
        .stall_rate_in('x),

        .StartRISCV_valid_in(StartRISCV_valid),
        .StartRISCV_rdy_out(StartRISCV_rdy),
        .StartRISCV_rden_in(StartRISCV_rden),
        .StartRISCV_empty_out(StartRISCV_empty),

        .CustomInstr_valid_out(CustomInstr_valid),
        .CustomInstr_hid_out(CustomInstr_hid),
        .CustomInstr_major_opcode_out(CustomInstr_major_opcode),
        .CustomInstr_minor_opcode_out(CustomInstr_minor_opcode),
        .CustomInstr_op1_out(CustomInstr_op1),
        .CustomInstr_op2_out(CustomInstr_op2),
        .CustomInstr_imm_out(CustomInstr_imm),
        .CustomInstr_funct7_out(CustomInstr_funct7),
        .CustomInstr_result_in(CustomInstr_result)
    );

    always #2 clk = ~clk;

    logic [31:0] custom_instr_state[1:0];

    always @(posedge clk) begin
        CustomInstr_result <= 32'hdeadbeef;

        if (CustomInstr_valid) begin
            if (CustomInstr_major_opcode == _processor_risc_v_isa__RVG_custom_0) begin
                if (CustomInstr_minor_opcode == 0) begin
                    custom_instr_state[CustomInstr_hid] <= CustomInstr_op1;
                end
                if (CustomInstr_minor_opcode == 1) begin
                    custom_instr_state[CustomInstr_hid] <= custom_instr_state[CustomInstr_hid] + CustomInstr_op2;
                end
                if (CustomInstr_minor_opcode == 2) begin
                    CustomInstr_result <= custom_instr_state[CustomInstr_hid];
                end
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

        wait(rst_and_startup_done_out);

        TestRISCV();

        $display("Done testing");
        $finish;
    end
endmodule
