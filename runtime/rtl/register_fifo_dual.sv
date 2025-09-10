// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaRegisterFifoDual

    This module implements a 2-deep FIFO using only registers.

Authors:
	- (Original author unknown)
	- Matthew Humphrey (mhumphr@microsoft.com)
*/

module KanagawaRegisterFifoDual
#(
    parameter WIDTH = 32
)
(
    input  wire 			clock,
    input  wire             rst,

    input  wire             wrreq,
    input  wire [WIDTH-1:0] data,
    output wire             full,

    input  wire             rdreq,
    output wire             empty,
    output wire [WIDTH-1:0] q
);

    reg in_which;
    reg out_which;

    always@(posedge clock) begin
        //synopsys sync_set_reset "rst"
        if(rst) begin
            in_which  <= 1'b0;
            out_which <= 1'b0;
        end
        else begin
            if(wrreq) in_which  <= ~in_which;
            if(rdreq) out_which <= ~out_which;
        end
    end

    wire full_first,
         full_second;

    wire empty_first,
         empty_second;

    wire [WIDTH-1:0] q_first;
    wire [WIDTH-1:0] q_second;

    KanagawaRegisterFifo#(.WIDTH(WIDTH)) FirstFIFO
    (
        .clock          (clock),
        .rst            (rst),
        .wrreq          (wrreq & ~in_which),
        .rdreq          (rdreq & ~out_which),
        .data           (data),
        .q              (q_first),
        .empty          (empty_first),
        .full           (full_first)
    );

    KanagawaRegisterFifo#(.WIDTH(WIDTH)) secondFIFO
    (
        .clock          (clock),
        .rst            (rst),
        .wrreq          (wrreq & in_which),
        .rdreq          (rdreq & out_which),
        .data           (data),
        .q              (q_second),
        .empty          (empty_second),
        .full           (full_second)
    );

    assign q     = ~out_which ? q_first     : q_second;
    assign empty = ~out_which ? empty_first : empty_second;
    assign full  = ~in_which  ? full_first  : full_second;

`ifndef NO_DYNAMIC_ASSERTS
//synopsys translate_off
    assert property (@(posedge clock)  full |-> (rst || !wrreq)) else $error ("%m overflow");
    assert property (@(posedge clock) empty |-> (rst || !rdreq)) else $error ("%m underflow");
//synopsys translate_on
`endif

endmodule // KanagawaRegisterFifoDual
