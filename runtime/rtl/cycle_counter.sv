// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Counter that increments on each cycle
//

`default_nettype none

module KanagawaCycleCounter
#(
    parameter WIDTH
)
(
    input wire clk,
    input wire rst,

    output logic [WIDTH-1:0] count_out
);
    logic [WIDTH-1:0] counter_ff;

    assign count_out = counter_ff;

    always_ff@(posedge clk) begin
        //synopsys sync_set_reset "rst"
        counter_ff <= rst ? '0 : (counter_ff + 1);
    end
endmodule
