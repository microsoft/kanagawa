//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

module KanagawaHALDualClockDualPortRAM
#(
    parameter DATA_WIDTH = 32,
    parameter ADDR_WIDTH = 9,
/* verilator lint_off UNUSEDPARAM */
    parameter DEVICE_FAMILY = "mock"
/* verilator lint_on UNUSEDPARAM */
)
(
    input wire rdclk,
    input wire [(ADDR_WIDTH-1):0] readaddr_in,
    output logic [(DATA_WIDTH-1):0] data_out,

    input wire wrclk,
    input wire wren_in,
    input wire [(ADDR_WIDTH-1):0] writeaddr_in,
    input wire [(DATA_WIDTH-1):0] data_in
);

    localparam DEPTH = 2**ADDR_WIDTH;

    logic [DATA_WIDTH-1:0] mem[DEPTH];

    logic [DATA_WIDTH-1:0] rddata_delay;

    always_ff @(posedge rdclk) begin
        // Some of the Kanagawa modules are expecting 2 cycle read latency, so we mimic that here
        rddata_delay <= mem[readaddr_in];
        data_out <= rddata_delay;
    end

    always_ff @(posedge wrclk) begin
        if (wren_in) begin
            mem[writeaddr_in] <= data_in;
        end
    end

endmodule
