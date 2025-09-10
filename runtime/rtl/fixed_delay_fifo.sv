// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`default_nettype none

// Emits output data after a fixed number of cycles
// Data is stored in a memory
module KanagawaFixedDelayFifo
#(
    parameter WIDTH,
    parameter DELAY,
    parameter USE_LUTRAM,
    parameter DEVICE_FAMILY,
    parameter USE_DSP = 0
)
(
    input wire clk,
    input wire rst,

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);
    localparam ADDR_WIDTH = $clog2(DELAY);

    initial begin
        assert(DELAY >= 2) else $error("%m: DELAY must be >= 2");
    end

    generate
        if (USE_DSP == 0)
        begin
            logic [ADDR_WIDTH-1:0] read_addr_ff;
            logic [ADDR_WIDTH-1:0] write_addr_ff;

            KanagawaSimpleDualPortRAM
            #(
                .DATA_WIDTH(WIDTH),
                .ADDR_WIDTH(ADDR_WIDTH),
                .USE_LUTRAM(USE_LUTRAM),
                .USE_BRAM(1 - USE_LUTRAM),
                .USE_OUTPUT_REG(1),
                .DEVICE_FAMILY(DEVICE_FAMILY),
                .SUPPORTS_RW_COLLISIONS(0),
                .RW_COLLISIONS_IMPOSSIBLE(1) // Addreses are always offset, collisions are impossible
            )
            ram
            (
                .clk(clk),
                .rst(rst),

                .rden_in(1'b1),
                .readaddr_in(read_addr_ff),
                .data_out(data_out),

                .wren_in(1'b1),
                .writeaddr_in(write_addr_ff),
                .data_in(data_in)
            );

            always_ff @(posedge clk) begin
                write_addr_ff <= ADDR_WIDTH'(rst ? DELAY - 1 : write_addr_ff + 1);
                read_addr_ff <= ADDR_WIDTH'(rst ?  '0 : read_addr_ff + 1);
            end
        end
        else
        begin
            // Implement delay using DSP
            KanagawaHALDSPPipeline
            #(
                .DEVICE_FAMILY(DEVICE_FAMILY),
                .WIDTH(WIDTH),
                .PIPELINE_STAGES(DELAY + 1)
            )
            dsp
            (
                .clk(clk),

                .data_in(data_in),
                .data_out(data_out)
            );
        end
    endgenerate
endmodule
