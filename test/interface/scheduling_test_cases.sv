// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module RegisteredIncrement
(
    input wire clk,
    input wire rst,
    input wire increment_valid_in,
    input wire [31:0] increment_x_in,
    output logic [31:0] increment_result_out
);
    always_ff @(posedge clk) begin
        increment_result_out <= increment_valid_in ? increment_x_in + 1 : 'x;
    end
endmodule