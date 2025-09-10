// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Linear feed back shift register that advances on each cycle
//

`default_nettype none

module KanagawaLfsr
#(
    parameter WIDTH = 11
)
(
    input wire clk,
    input wire rst,
    input wire en,

    output logic [WIDTH-1:0] lfsr_out
);
    logic [WIDTH-1:0] lfsr_ff;

    assign lfsr_out = lfsr_ff;

    // synopsys translate_off
    initial begin
	    assert(WIDTH == 11) else $fatal("Only WIDTH of 11 is supported");
    end
    // synopsys translate_on

    logic lfsr_next;
    assign lfsr_next = lfsr_ff[11-1] ^~ lfsr_ff[9-1];

    always_ff@(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst)
            lfsr_ff <= rst;
        else if (en)
            lfsr_ff <= {lfsr_ff, lfsr_next};
        else
            lfsr_ff <= lfsr_ff; // Needed to avoid coverage miss in VCS
    end
endmodule
