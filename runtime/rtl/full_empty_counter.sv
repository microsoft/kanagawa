// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaFullEmptyCounter

    This module exposes FIFO-like read and write interfaces without data input
    or output, and is useful for tracking the number of events that have been
    queued. You can think of it as a zero bit FIFO. All outputs are
    registered.

Authors:
    - Matthew Humphrey (mhumphr@microsoft.com)
*/

module KanagawaFullEmptyCounter
#(
    parameter LOG_DEPTH = 1,
    parameter logic [LOG_DEPTH:0] ALMOST_FULL_MARGIN = (LOG_DEPTH+1)'('d1)
)
(
    input  wire                     clk,
    input  wire                     rst,

    input  wire                     wrreq_in,
    output logic                    full_out,
    output logic                    almost_full_out,

    output logic [LOG_DEPTH:0]      usedw_out,

    input  wire                     rdreq_in,
    output logic                    empty_out
);

    localparam logic [LOG_DEPTH:0] WRALMOST_FULL_THRESHOLD = (1'b1 << LOG_DEPTH) - ALMOST_FULL_MARGIN;

    struct packed
    {
        logic [LOG_DEPTH:0] usedw;
        logic               afull;
        logic               empty;
    } st_ff, st_nxt;

    always_comb begin
        st_nxt = st_ff;

        st_nxt.usedw = (st_ff.usedw + (wrreq_in ? 1'b1 : 1'b0) - (rdreq_in ? 1'b1 : 1'b0));
        st_nxt.afull = (st_nxt.usedw >= WRALMOST_FULL_THRESHOLD);
        st_nxt.empty = (st_nxt.usedw == '0);

        usedw_out = st_ff.usedw;
        full_out = st_ff.usedw[LOG_DEPTH];
        almost_full_out = st_ff.afull;
        empty_out = st_ff.empty;
    end

    always_ff @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            st_ff <= 'x;
            st_ff.usedw <= '0;
            st_ff.empty <= 1'b1;
            st_ff.afull <= 1'b0;
        end
        else begin
            st_ff <= st_nxt;
        end
    end

endmodule // KanagawaFullEmptyCounter
