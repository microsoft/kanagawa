// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaHALResetSynchronizer

    This module converts an asynchronous reset into a reset synchronous with the provided clock.

Ports: 
    clk             - Clock (input)
    arst            - Asynchronous (relative to clk) reset (input)
    rst             - Synchronous reset (output)

Authors:   
    - Matt Humphrey (mhumphr@microsoft.com)
*/
module KanagawaHALResetSynchronizer
#(
    DEPTH = 2
)
(
    input  wire     clk,
    input  wire     arst,
    output wire     rst
);

    xpm_cdc_async_rst 
    #(
        .DEST_SYNC_FF    (DEPTH), // integer; range: 2-10
        .RST_ACTIVE_HIGH (1) // integer; 0=active low reset, 1=active high reset
    ) xpm_cdc_async_rst_inst 
    (
        .src_arst   (arst),
        .dest_clk   (clk),
        .dest_arst  (rst)
    );

endmodule : KanagawaHALResetSynchronizer
