// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Modules that checks for FIFO overflow/underflow
//

`default_nettype none

//VCS coverage off
// coverage off
module KanagawaFifoDebug
(
    input wire clk,
    input wire rst,

    input wire state_in, // full or empty
    input wire req_in,   // wren or rden

    output logic error_out
);
    logic error_ff;

    assign error_out = error_ff;

    always_ff @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            error_ff <= 1'b0;
        end
        else begin
`ifndef NO_DYNAMIC_ASSERTS
            if (state_in & req_in) begin
                error_ff <= 1'b1;
            end
`endif
        end
    end
endmodule

module KanagawaDualClockFifoDebug
(
    input wire wr_clk,
    input wire wr_rst,
    input wire wr_full_in,
    input wire wr_wren_in,
    output logic wr_overflow_out, // sticky

    input wire rd_clk,
    input wire rd_rst,
    input wire rd_empty_in,
    input wire rd_rden_in,
    output logic rd_underflow_out // sticky
);
    KanagawaFifoDebug write
    (
        .clk(wr_clk),
        .rst(wr_rst),

        .state_in(wr_full_in),
        .req_in(wr_wren_in),
        .error_out(wr_overflow_out)
    );

    KanagawaFifoDebug read
    (
        .clk(rd_clk),
        .rst(rd_rst),

        .state_in(rd_empty_in),
        .req_in(rd_rden_in),
        .error_out(rd_underflow_out)
    );

endmodule

module KanagawaSingleClockFifoDebug
(
    input wire clk,
    input wire rst,

    input wire full_in,
    input wire wren_in,

    input wire empty_in,
    input wire rden_in,

    // These outputs are sticky (once they go high, they stay high until reset)
    output logic overflow_out,
    output logic underflow_out
);
    KanagawaDualClockFifoDebug dc
    (
        .wr_clk(clk),
        .wr_rst(rst),
        .wr_full_in(full_in),
        .wr_wren_in(wren_in),
        .wr_overflow_out(overflow_out),

        .rd_clk(clk),
        .rd_rst(rst),
        .rd_empty_in(empty_in),
        .rd_rden_in(rden_in),
        .rd_underflow_out(underflow_out)
    );
endmodule
// coverage on
//VCS coverage on
