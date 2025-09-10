// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

module Adder
(
    input wire clk,
    input wire rst,

    input wire SetBase_valid_in,
    input wire [31:0] SetBase_base_in,

    input wire Add___uint_32___valid_in,
    input wire [31:0] Add___uint_32___a_in,
    input wire [31:0] Add___uint_32___b_in,
    output logic [31:0] Add___uint_32___result_out
);
    // Add and then accumulate, fixed latency of 3
    logic [31:0] add_sum;

    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(32),
        .DEPTH(3)
    )
    add_ffc
    (
        .clk(clk),

        .data_in(Add___uint_32___valid_in ? 32'(Add___uint_32___a_in + Add___uint_32___b_in) : '0),
        .data_out(add_sum)
    );

    logic [31:0] acc_ff, acc_next;

    always_comb begin
        acc_next = SetBase_valid_in ? SetBase_base_in : acc_ff + add_sum;

        Add___uint_32___result_out = acc_next;
    end

    always_ff @(posedge clk) begin
        acc_ff <= rst ? '0 : acc_next;
    end

endmodule

module ModuleCallbackWrapper__AsyncCounter
(
    input wire clk,
    input wire rst,

    input wire UpdateCount_rdy_in,
    output logic [31:0] UpdateCount_newCount_out,
    output logic UpdateCount_valid_out,

    output logic Increment_rden_out,
    input wire [31:0] Increment_amt_in,
    input wire Increment_empty_in
);
    logic [31:0] count_ff, count_next;

    always_comb begin
        count_next = count_ff;

        Increment_rden_out = 1'b0;
        UpdateCount_valid_out = 1'b0;

        UpdateCount_newCount_out = 'x;

        if (~Increment_empty_in & UpdateCount_rdy_in) begin
            Increment_rden_out = 1'b1;
            UpdateCount_valid_out = 1'b1;

            count_next += Increment_amt_in;

            UpdateCount_newCount_out = count_next;
        end
    end

    always_ff @(posedge clk) begin
        count_ff <= rst ? '0 : count_next;
    end
endmodule

module _helper_async_counter__AsyncCounter
(
    input wire clk,
    input wire rst,

    input wire UpdateCount_rdy_in,
    output logic [31:0] UpdateCount_newCount_out,
    output logic UpdateCount_valid_out,

    output logic Increment_rden_out,
    input wire [31:0] Increment_amt_in,
    input wire Increment_empty_in
);
    logic [31:0] count_ff, count_next;

    always_comb begin
        count_next = count_ff;

        Increment_rden_out = 1'b0;
        UpdateCount_valid_out = 1'b0;

        UpdateCount_newCount_out = 'x;

        if (~Increment_empty_in & UpdateCount_rdy_in) begin
            Increment_rden_out = 1'b1;
            UpdateCount_valid_out = 1'b1;

            count_next += Increment_amt_in;

            UpdateCount_newCount_out = count_next;
        end
    end

    always_ff @(posedge clk) begin
        count_ff <= rst ? '0 : count_next;
    end
endmodule

module _helper_external_class__Adder
(
    input wire clk,
    input wire rst,

    input wire Add_valid_in,
    input wire [15:0] Add_a_in,
    input wire [15:0] Add_b_in,
    output logic [15:0] Add_result_out
);
    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(16),
        .DEPTH(4)
    )
    add_ffc
    (
        .clk(clk),

        .data_in(Add_valid_in ? 16'(Add_a_in + Add_b_in) : '0),
        .data_out(Add_result_out)
    );

endmodule

module AluTemplate
#(
    parameter Width,
    parameter C0,
    parameter C1,
    parameter Operation
)
(
    input wire clk,
    input wire rst,

    input wire Exec_rdy_in,
    output logic [Width-1:0] Exec_result_out,
    output logic Exec_valid_out,

    output logic Exec_rden_out,
    input wire [Width-1:0] Exec_a_in,
    input wire Exec_empty_in,

    input wire ExecFixed_valid_in,
    input wire [Width-1:0] ExecFixed_a_in,
    output logic [Width-1:0] ExecFixed_result_out
);
    always_comb begin
        Exec_result_out = 'x;
        Exec_valid_out = 1'b0;
        Exec_rden_out = 1'b0;

        if (!Exec_empty_in && Exec_rdy_in) begin
            if (Operation == "Add") begin
                Exec_result_out = Exec_a_in + C0;
            end else if (Operation == "Mul") begin
                Exec_result_out = Exec_a_in * C0;
            end else begin
                assert(Operation == "Mad") else $error("Unsupported operation: %s", Operation);
                Exec_result_out = (Exec_a_in * C0) + C1;
            end
            Exec_rden_out = 1'b1;
            Exec_valid_out = 1'b1;
        end
    end

    always_ff @(posedge clk) begin
        ExecFixed_result_out <= ExecFixed_valid_in ? (ExecFixed_a_in + C0) : '0;
    end
endmodule

module _helper_external_class_template__Inverter
#(
    parameter Mask
)
(
    input wire clk,
    input wire rst,

    input wire Invert_valid_in,
    input wire [15:0] Invert_a_in,
    output logic [15:0] Invert_result_out
);
    always_ff @(posedge clk) begin
        Invert_result_out <= Invert_valid_in ? ~(Invert_a_in & Mask) : '0;
    end
endmodule

module ClassWithLatencyCallback
(
    input wire clk,
    input wire rst,

    input wire entry_valid_in,
    input wire [31:0] entry_x_in,
    output logic [31:0] entry_result_out,

    output logic cb_valid_out,
    output logic [31:0] cb_x_out,
    input wire [31:0] cb_result_in
);
    assign cb_valid_out = entry_valid_in;
    assign cb_x_out = entry_x_in;

    // Delay result by 1 cycle
    always_ff @(posedge clk) begin
        entry_result_out <= cb_result_in;
    end
endmodule

module ClassWithNoBackpressureCallback
(
    input wire clk,
    input wire rst,

    input wire entry_valid_in,
    input wire [31:0] entry_x_in,

    output logic cb_valid_out,
    output logic [31:0] cb_x_out
);
    // Delay parameters by 1 cycle
    always_ff @(posedge clk) begin
        cb_valid_out <= rst ? 1'b0 : entry_valid_in;
        cb_x_out <= entry_x_in;
    end
endmodule

module EMTest
(
    input wire clk,
    input wire rst,

    input wire Callback_rdy_in,
    output logic Callback_valid_out,

    output logic XOR_rden_out,
    input wire [16:0] XOR_a_in,
    input wire [16:0] XOR_b_in,
    input wire XOR_empty_in,
    output logic XOR_valid_out,
    output logic [16:0] XOR_result_out,
    input wire XOR_rdy_in,

    input wire Sub_valid_in,
    input wire [31:0] Sub_a_in,
    input wire [31:0] Sub_b_in,

    input wire Add_valid_in,
    input wire [31:0] Add_a_in,
    input wire [31:0] Add_b_in,
    output logic [31:0] Add_result_out
);
    // cycle counter, to only accept data 1/16 cycles
    // This tests CDC backpressure handling
    logic [3:0] counter_ff;

    always_ff @(posedge clk) begin
        counter_ff <= rst ? '0 : (counter_ff + 1);
    end

    always_comb begin
        Callback_valid_out = 1'b0;

        XOR_rden_out = 1'b0;
        XOR_valid_out = 1'b0;
        XOR_result_out = 'x;

        if (Callback_rdy_in & XOR_rdy_in & ~XOR_empty_in & (counter_ff == '0)) begin
            // Call the callback
            Callback_valid_out = 1'b1;

            // Compute a XOR b
            XOR_result_out = XOR_a_in ^ XOR_b_in;
            XOR_valid_out = 1'b1;
            XOR_rden_out = 1'b1;
        end
    end

    // Add and then accumulate, fixed latency of 3
    logic [31:0] add_sum;

    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(32),
        .DEPTH(3)
    )
    add_ffc
    (
        .clk(clk),

        .data_in(Add_valid_in ? 32'(Add_a_in + Add_b_in) : '0),
        .data_out(add_sum)
    );

    logic [31:0] acc_ff, acc_next;

    always_comb begin
        acc_next = acc_ff + add_sum;

        Add_result_out = acc_next;
    end

    always_ff @(posedge clk) begin
        acc_ff <= rst ? '0 : acc_next;
    end

endmodule

module ComplexReturns
(
    input wire clk,
    input wire rst,

    output logic ReturnStruct_rden_out,
    input wire ReturnStruct_empty_in,
    input wire [7:0] ReturnStruct_v_in,
    output logic ReturnStruct_valid_out,
    output logic [31:0] ReturnStruct_result_out,
    input wire ReturnStruct_rdy_in,

    output logic ReturnArray_rden_out,
    input wire [7:0] ReturnArray_v_in,
    input wire ReturnArray_empty_in,
    output logic ReturnArray_valid_out,
    output logic [15:0] ReturnArray_result_out,
    input wire ReturnArray_rdy_in,

    output logic ReturnArrayOfStruct_rden_out,
    input wire ReturnArrayOfStruct_empty_in,
    input wire [7:0] ReturnArrayOfStruct_v_in,
    output logic ReturnArrayOfStruct_valid_out,
    output logic [63:0] ReturnArrayOfStruct_result_out,
    input wire ReturnArrayOfStruct_rdy_in
);
    always_comb begin
        ReturnStruct_rden_out = 1'b0;
        ReturnStruct_valid_out = 1'b0;
        ReturnStruct_result_out = 32'b0;

        ReturnArray_rden_out = 1'b0;
        ReturnArray_valid_out = 1'b0;
        ReturnArray_result_out = 16'b0;

        ReturnArrayOfStruct_rden_out = 1'b0;
        ReturnArrayOfStruct_valid_out = 1'b0;
        ReturnArrayOfStruct_result_out = 64'b0;

        if (~ReturnStruct_empty_in & ReturnStruct_rdy_in) begin
            ReturnStruct_rden_out = 1'b1;
            ReturnStruct_valid_out = 1'b1;
            ReturnStruct_result_out = { ~ReturnStruct_v_in, ReturnStruct_v_in, 8'b0, ReturnStruct_v_in};
        end

        if (~ReturnArray_empty_in & ReturnArray_rdy_in) begin
            ReturnArray_rden_out = 1'b1;
            ReturnArray_valid_out = 1'b1;
            ReturnArray_result_out = { ~ReturnArray_v_in, ReturnArray_v_in & 8'hf0};
        end

        if (~ReturnArrayOfStruct_empty_in & ReturnArrayOfStruct_rdy_in) begin
            ReturnArrayOfStruct_rden_out = 1'b1;
            ReturnArrayOfStruct_valid_out = 1'b1;
            ReturnArrayOfStruct_result_out = {
                                              {ReturnArrayOfStruct_v_in, ~ReturnArrayOfStruct_v_in, 8'b0, ~ReturnArrayOfStruct_v_in},
                                              {~ReturnArrayOfStruct_v_in, ReturnArrayOfStruct_v_in, 8'b0, ReturnArrayOfStruct_v_in}
                                             };
        end
    end

endmodule
