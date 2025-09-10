// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// A module that is used to avoid false overflow errors during simulation
// If the input reset signal is unknown (occurs during first few cycles),
// then it sets the output to 1'b1 (reset asserted)

`default_nettype none

module KanagawaUnknownReset
(
    input wire reset_in,
    output logic reset_out
);
    always_comb begin
        reset_out = reset_in;

`ifndef VERILATOR
        //synopsys translate_off
        if (reset_in === 'x) begin
            reset_out = 1'b1;
        end
        //synopsys translate_on
`endif
    end
endmodule
