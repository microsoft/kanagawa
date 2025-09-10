// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module KanagawaHALDualClockDualPortRAM
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter DEVICE_FAMILY
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

    xpm_memory_sdpram
    #(
        .MEMORY_SIZE(DATA_WIDTH * DEPTH),
        .MEMORY_PRIMITIVE("block"),
        .MEMORY_INIT_PARAM("0"),
        .MEMORY_INIT_FILE("none"),
        .CLOCKING_MODE("independent_clock"),

        .WRITE_DATA_WIDTH_A(DATA_WIDTH),
        .ADDR_WIDTH_A(ADDR_WIDTH),

        .READ_DATA_WIDTH_B(DATA_WIDTH),
        .ADDR_WIDTH_B(ADDR_WIDTH),

        .READ_LATENCY_B(2),
        .WRITE_MODE_B("read_first")
    )
    ram
    (
        .sleep(1'b0),

        .clka(wrclk),
        .ena(wren_in),
        .wea('1),
        .addra(writeaddr_in),
        .dina(data_in),
        .injectsbiterra(1'b0),
        .injectdbiterra(1'b0),

        .clkb(rdclk),
        .rstb(1'b0),
        .enb(1'b1),
        .regceb(1'b1),
        .addrb(readaddr_in),
        .doutb(data_out),
        .sbiterrb(),
        .dbiterrb()
    );

endmodule
