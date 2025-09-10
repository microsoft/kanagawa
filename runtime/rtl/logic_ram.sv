// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


/*
Module: KanagawaLogicRam

    This module provides a mechanism for the Kanagawa compiler to create memories
    constructed of discrete (RTL) registers and muxes. Previously an approach was
    used where the compiler just created the registers directly. However, the approach
    used there was creating bogus code coverage misses. This module allows us to work
    around this problem and also provides a clear encapsulation of the memory within
    the RTL.

    The module has zero cycle latency for reads, and a one cycle latency for writes.

Authors:
    - Matthew Humphrey (mhumphr@microsoft.com)
*/

module KanagawaLogicRam
#(
    parameter integer DATA_WIDTH = 32,
    parameter integer ADDR_WIDTH = 16,
    parameter integer DEPTH = 2**ADDR_WIDTH,
    parameter integer NUM_READ_PORTS = 2
)
(
    input  wire clk,
    input  wire rst,

    // Multiple read ports
    input  wire  [NUM_READ_PORTS-1:0][ADDR_WIDTH-1:0]   rdaddr_in,
    output logic [NUM_READ_PORTS-1:0][DATA_WIDTH-1:0]   rddata_out,

    // Single write port
    input  wire                     wren_in,
    input  wire  [ADDR_WIDTH-1:0]   wraddr_in,
    input  wire  [DATA_WIDTH-1:0]   wrdata_in
);

    logic [0:DEPTH-1][DATA_WIDTH-1:0] data_ff;

    always_comb begin
        for (int i = 0; i < NUM_READ_PORTS; ++i) begin
            rddata_out[i] = data_ff[rdaddr_in[i]];
        end
    end

    always_ff @(posedge clk) begin
        if (wren_in) begin
            data_ff[wraddr_in] <= wrdata_in;
        end
    end

endmodule
