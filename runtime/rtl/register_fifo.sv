// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaRegisterFifo
    to or read from at most every other clock. If the stuttering behavior is
    not tolerable, consider using <KanagawaRegisterFifoSkid> or
    <KanagawaRegisterFifoDual>.
    this module, the port names from the original module were retained.

Authors:
    - (Original author unknown)
    - Matthew Humphrey (mhumphr@microsoft.com)
*/

module KanagawaRegisterFifo
#(
    parameter WIDTH = 32
)
(
    input  wire                 clock,
    input  wire                 rst,

    input  wire                 wrreq,
    input  wire [WIDTH-1:0]     data,
    output wire                 full,

    input  wire                 rdreq,
    output wire                 empty,
    output reg [WIDTH-1:0]      q
);

    reg                         wrptr_ff,
                                rdptr_ff;

    always@(posedge clock) begin
        //synopsys sync_set_reset "rst"
        if(rst) begin
            wrptr_ff <= 1'b0;
            rdptr_ff <= 1'b0;
        end
        else begin
            if(wrreq) begin
                wrptr_ff <= ~wrptr_ff;
            end
            if(rdreq) begin
                rdptr_ff <= ~rdptr_ff;
            end
        end
    end

	always@(posedge clock) begin
		if(wrreq) begin
			q <= data;
		end
	end

    assign empty = rdptr_ff == wrptr_ff;
    assign full  = rdptr_ff != wrptr_ff;

`ifndef NO_DYNAMIC_ASSERTS
//synopsys translate_off
    assert property (@(posedge clock)  full |-> (rst || !wrreq)) else $error ("%m overflow");
    assert property (@(posedge clock) empty |-> (rst || !rdreq)) else $error ("%m underflow");
//synopsys translate_on
`endif

endmodule // KanagawaRegisterFifo
