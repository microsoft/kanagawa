// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaHALSyncRegs

    This module implements a synchronizer chain that can be used to mitigate
    metastability issues when crossing clock domains. Although the module
    supports multiple bits, synchronization of the bits may not occur on the
    same clock cycle, and so must be achieved in some other way (for example,
    with a Gray code).

Authors:
    - (Original author unknown)
    - Matthew Humphrey (mhumphr@microsoft.com)
*/

module KanagawaHALSyncRegs
#(
    parameter integer WIDTH = 32,
    parameter integer DEPTH = 2,     // minimum of 2, maximum of 10
    parameter integer SIM_ASSERT_CHK = 0 // integer; 0=disable simulation messages, 1=enable simulation messages
)
(
    input  wire             clk,
    input  wire [WIDTH-1:0] din,
    output wire [WIDTH-1:0] dout
);

    // Parameter validation
    initial begin
        assert(DEPTH >= 2 && DEPTH <= 10) else $error("Synchronizer chain depth must be in the range [2,10], but DEPTH value is %0d", DEPTH);
    end

    generate
        if (WIDTH > 1) begin : multi_bit_sync
            xpm_cdc_array_single 
            #(
                .DEST_SYNC_FF   (DEPTH), // integer; range: 2-10
                .SIM_ASSERT_CHK (SIM_ASSERT_CHK), // integer; 0=disable simulation messages, 1=enable simulation messages
                .SRC_INPUT_REG  (0), // integer; 0=do not register input, 1=register input
                .WIDTH          (WIDTH) // integer; range: 2-1024
            ) xpm_cdc_array_single_inst 
            (
                .src_clk        (1'b0), // optional; required when SRC_INPUT_REG = 1
                .src_in         (din),
                .dest_clk       (clk),
                .dest_out       (dout)
            );
        end
        else begin : single_bit_sync
            xpm_cdc_single 
            #(
                .DEST_SYNC_FF   (DEPTH), // integer; range: 2-10
                .SIM_ASSERT_CHK (SIM_ASSERT_CHK), // integer; 0=disable simulation messages, 1=enable simulation messages
                .SRC_INPUT_REG  (0) // integer; 0=do not register input, 1=register input
            ) xpm_cdc_single_inst 
            (
                .src_clk        (1'b0), // optional; required when SRC_INPUT_REG = 1
                .src_in         (din),
                .dest_clk       (clk),
                .dest_out       (dout)
            );
        end
    endgenerate

endmodule // KanagawaHALSyncRegs
