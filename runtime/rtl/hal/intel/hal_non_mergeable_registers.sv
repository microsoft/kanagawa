// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

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
        assert(DEPTH >= 1) else $error("%m: DEPTH must be not less than 1 for Intel FPGA target!");
    end
    // synopsys translate_on

    // dont_merge is used to ensure that the synthesis tool does not combine registers that
    // have explicitly been duplicated (typically to limit fanout)
    // dont_merge is used instead of preserve because preserve prevents hyper-retiming

    (* dont_merge *) logic [DEPTH-1:0][WIDTH-1:0] data_ff = INIT_VAL /* synthesis dont_merge */;

    assign data_out = data_ff;

    always @(posedge clk) begin
        data_ff <= data_in;
    end

endmodule : KanagawaHALNonMergeableRegisters

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

    // synopsys translate_off
    initial begin
        assert(DEPTH >= 1) else $error("%m: DEPTH must be not less than 1 for Intel FPGA target!");
    end
    // synopsys translate_on

    // dont_merge is used to ensure that the synthesis tool does not combine registers that
    // have explicitly been duplicated (typically to limit fanout)
    // dont_merge is used instead of preserve because preserve prevents hyper-retiming

    (* dont_merge *) logic [DEPTH-1:0][WIDTH-1:0] data_ff = INIT_VAL /* synthesis dont_merge */;

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
