// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Modules that routes a signal through a set of flip flops
`default_nettype none

/* verilator lint_off DECLFILENAME */

// ---------------------------------------------------------
// Single-stage of FFs without reset
// ---------------------------------------------------------
module KanagawaFlipFlopNoReset
#(
    parameter WIDTH = 1
)(
    input   wire                clk,
    input   wire    [WIDTH-1:0] data_in,
    output  logic   [WIDTH-1:0] data_out
);

    always @(posedge clk)
        data_out <= data_in;

endmodule

// ---------------------------------------------------------
// Multi-stage of FFs without reset
// ---------------------------------------------------------
module KanagawaCascadedFlipFlopsNoReset
#(
    parameter WIDTH = 1,
    parameter DEPTH = 1
) (
    input wire clk,

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

    // synopsys translate_off
    initial begin
        assert(DEPTH > 0) else $error("%m: DEPTH must be larger than 0!");
    end
    // synopsys translate_on

    logic [DEPTH-1:0][WIDTH-1:0] data_i, data_o;

    genvar ii;
    generate
        for (ii = 0; ii < DEPTH; ii++) begin: gen_ff_chain
            KanagawaFlipFlopNoReset #(
                .WIDTH      (WIDTH)
            ) ff (
                .clk        (clk),
                .data_in    (data_i[ii]),
                .data_out   (data_o[ii])
            );

            if (ii == 0) begin: gen_first_in
                assign data_i[ii] = data_in;
            end
            else begin: gen_others_in
                // Unnecessary for correctness, but needed for coverage
                if (DEPTH > 1)
                begin: gen_deph_gt_1
                    assign data_i[ii] = data_o[ii-1];
                end
            end
        end
    endgenerate
    assign data_out = data_o[DEPTH-1];

endmodule

// ---------------------------------------------------------
// Multi-stage of FFs with optional power-on value
// ---------------------------------------------------------
module KanagawaCascadedFlipFlopsWithOptionalInitialValue
#(
    parameter WIDTH = 1,
    parameter DEPTH = 1,
    parameter logic [WIDTH-1:0] INIT_VAL = '0,
    parameter DO_INIT = 1
) (
    input wire clk,

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

    logic [DEPTH-1:0][WIDTH-1:0] data_i, data_o;

    genvar ii;
    generate
        if (DEPTH == 0) begin: gen_passthrough
            assign data_out = data_in;
        end
        else begin: depth_gt_zero
            for (ii = 0; ii < DEPTH; ii++) begin: gen_ff_chain
                if (DO_INIT) begin: gen_with_init_val
                    KanagawaHALRegisterWithInitialValue #(
                        .WIDTH      (WIDTH),
                        .INIT_VAL   (INIT_VAL)
                    ) ff (
                        .clk        (clk),
                        .data_in    (data_i[ii]),
                        .data_out   (data_o[ii])
                    );
                end
                else begin: gen_without_init_val
                    KanagawaFlipFlopNoReset #(
                        .WIDTH      (WIDTH)
                    ) ff (
                        .clk        (clk),
                        .data_in    (data_i[ii]),
                        .data_out   (data_o[ii])
                    );
                end

                if (ii == 0) begin: gen_first_in
                    assign data_i[ii] = data_in;
                end
                else begin: gen_others_in
                    assign data_i[ii] = data_o[ii-1];
                end
            end
        end
    endgenerate
    assign data_out = data_o[DEPTH-1];
endmodule

module KanagawaFlipFlopChainNoEnable
#(
    parameter WIDTH = 1,
    parameter DEPTH = 1
)
(
/* verilator lint_off UNUSEDSIGNAL */
    input wire clk,
/* verilator lint_on UNUSEDSIGNAL */

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

    generate
        if (DEPTH == 0) begin: gen_passthrough
            assign data_out = data_in;
        end
        else begin: depth_gt_zero
            KanagawaCascadedFlipFlopsNoReset
            #(
                .WIDTH      (WIDTH),
                .DEPTH      (DEPTH)
            ) ff_chain(
                .clk        (clk),
                .data_in    (data_in),
                .data_out   (data_out)
            );
        end
    endgenerate

endmodule

module KanagawaFlipFlopChainWithClear
#(
    parameter WIDTH = 1,
    parameter DEPTH = 1
)
(
    input wire clk,
    input wire clr,
    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

	genvar ii;
    generate
        if (DEPTH > 0) begin: gen_non_zero_DEPTH

            logic [DEPTH-1:0][WIDTH-1:0] data_nxt, data_ff;

            always @(posedge clk) begin
                data_ff <= data_nxt;

                //synopsys sync_set_reset "clr"
                if (clr) begin
                    data_ff <= '0;
                end
            end

			// no need to detect DEPTH > 1 here since it is handled by the for loop
            for (ii=0; ii < (DEPTH - 1); ii++) begin: gen_data_nxt
              	assign data_nxt[ii] = data_ff[ii+1];
            end

            always @(*) begin
                // Slot[0] is the output slot
                data_out = data_ff[0];

                // Initialize the input slots
                data_nxt[DEPTH-1] = data_in;
            end
        end
        else begin: gen_passthrough
            assign data_out = data_in;
        end
    endgenerate
endmodule

module KanagawaFlipFlopChainWithEnableAndReset
#(
    parameter WIDTH = 1,
    parameter DEPTH = 1
)
(
    input wire clk,
    input wire rst,
    input wire enable,
    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

	genvar ii;
    generate
        if (DEPTH > 0) begin: gen_non_zero_DEPTH

            logic [DEPTH-1:0][WIDTH-1:0] data_nxt, data_ff;

            always @(posedge clk) begin
                data_ff <= enable ? data_nxt : data_ff;

                //synopsys sync_set_reset "rst"
                if (rst) begin
                    data_ff <= '0;
                end
            end

            // Pass values down the chain
			// no need to detect DEPTH > 1 here since it is handled by the for loop
            for (ii=0; ii < (DEPTH - 1); ii++) begin: gen_data_nxt
            	assign data_nxt[ii] = data_ff[ii+1];
            end


            always @(*) begin
                // Slot[0] is the output slot
                data_out = data_ff[0];

                // Initialize the input slots
                data_nxt[DEPTH-1] = data_in;
            end
        end
        else begin: gen_passthrough
            assign data_out = data_in;
        end
    endgenerate
endmodule

/* verilator lint_on DECLFILENAME */
