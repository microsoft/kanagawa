//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

module KanagawaHALNonMergeableRegisters
#(
    parameter WIDTH,
    parameter DEPTH = 1,
    parameter logic [DEPTH-1:0][WIDTH-1:0] INIT_VAL = 'x
)
(
    input  wire     clk,
    input  wire [DEPTH-1:0][WIDTH-1:0] data_in,
    output wire [DEPTH-1:0][WIDTH-1:0] data_out
);

    // synopsys translate_off
    initial begin
        assert(DEPTH >= 1) else $error("%m: DEPTH must be not less than 1");
    end
    // synopsys translate_on

    logic [DEPTH-1:0][WIDTH-1:0] data_ff = INIT_VAL;

    assign data_out = data_ff;

    always @(posedge clk) begin
        data_ff <= data_in;
    end

endmodule : KanagawaHALNonMergeableRegisters

/* verilator lint_off DECLFILENAME */
module KanagawaHALNonMergeableRegistersWithClear
#(
    parameter WIDTH,
    parameter DEPTH = 1,
    parameter logic [DEPTH-1:0][WIDTH-1:0] INIT_VAL = 'x
)
(
    input  wire     clk,
    input  wire     clr,
    input  wire [DEPTH-1:0][WIDTH-1:0] data_in,
    output wire [DEPTH-1:0][WIDTH-1:0] data_out
);

    initial begin
        assert(DEPTH >= 1) else $error("%m: DEPTH must be not less than 1");
    end

    logic [DEPTH-1:0][WIDTH-1:0] data_ff = INIT_VAL;

    assign data_out = data_ff;

    always @(posedge clk) begin
        if (clr)
            data_ff <= '0;
        else
            data_ff <= data_in;
    end

endmodule : KanagawaHALNonMergeableRegistersWithClear

module KanagawaHALRegisterWithInitialValue
#(
    parameter WIDTH = 1,
    parameter logic [WIDTH-1:0] INIT_VAL = '0
)
(
    input  wire     clk,
    input  wire [WIDTH-1:0] data_in,
    output wire [WIDTH-1:0] data_out
);
    logic [WIDTH-1:0] data_ff = INIT_VAL;

    assign data_out = data_ff;

    always @(posedge clk) begin
        data_ff <= data_in;
    end

endmodule

/* verilator lint_off DECLFILENAME */
