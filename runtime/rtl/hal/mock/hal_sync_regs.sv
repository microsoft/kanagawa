//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

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

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

module KanagawaHALSyncRegs
#(
    parameter WIDTH = 32,
    parameter DEPTH = 2     // minimum of 2
)
(
    input  wire             clk,
    input  wire [WIDTH-1:0] din,
    output wire [WIDTH-1:0] dout
);

    // Parameter validation
    initial begin
        assert(DEPTH >= 2) else $error("Synchronizer chains should be at least 2 deep, but DEPTH value is %0d", DEPTH);
    end

    reg [WIDTH-1:0] din_meta = 0;

    reg [WIDTH*(DEPTH-1)-1:0] sync_sr = 0;

    always @(posedge clk) begin
        din_meta <= din;
        sync_sr <= (sync_sr << WIDTH) | din_meta;
    end
    assign dout = sync_sr[WIDTH*(DEPTH-1)-1:WIDTH*(DEPTH-2)];

endmodule // KanagawaHALSyncRegs
