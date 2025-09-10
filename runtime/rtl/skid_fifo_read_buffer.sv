// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module KanagawaSkidFifoReadBuffer
#(
    parameter WIDTH = 16,
    parameter DEPTH = 1
)
(
    input  wire             clk,
    input  wire             rst,

    // To existing FIFO we are buffering
    output wire             rdreq_out,
    input  wire             rdempty_in,
    input  wire [WIDTH-1:0] rddata_in,

    // FIFO read output
    input  wire             rdreq_in,
    output wire             rdempty_out,
    output wire [WIDTH-1:0] rddata_out
);

    genvar i;
    generate
        if (DEPTH < 1) begin
            assign rdreq_out = rdreq_in;
            assign rdempty_out = rdempty_in;
            assign rddata_out = rddata_in;
        end
        else begin
            logic               wrreq [DEPTH];
            logic [WIDTH-1:0]   data [DEPTH];
            logic               full [DEPTH];
            logic               rdreq [DEPTH];
            logic               empty [DEPTH];
            logic [WIDTH-1:0]   q [DEPTH];

            assign wrreq[0] = ~rdempty_in & ~full[0];
            assign data[0] = rddata_in;
            assign rdreq_out = wrreq[0];

            assign rdreq[DEPTH-1] = rdreq_in;
            assign rddata_out = q[DEPTH-1];
            assign rdempty_out = empty[DEPTH-1];

            for (i = 0; i < DEPTH; i = i + 1) begin : gen_fifos
                KanagawaRegisterFifoSkid
                #(
                    .WIDTH      (WIDTH)
                ) output_reg_fifo
                (
                    .clock       (clk),
                    .rst         (rst),

                    .wrreq       (wrreq[i]),
                    .data        (data[i]),
                    .full        (full[i]),

                    .rdreq       (rdreq[i]),
                    .empty       (empty[i]),
                    .q           (q[i])
                );

                if (i > 0) begin
                    always @(*) begin
                        wrreq[i] = ~empty[i-1] & ~full[i];
                        data[i] = q[i-1];
                        rdreq[i-1] = wrreq[i];
                    end
                end

            end
        end

    endgenerate

endmodule // KanagawaSkidFifoReadBuffer
