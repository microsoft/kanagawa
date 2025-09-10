//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

// Platform-specific RAM module for use in tiling memories together to make a larger memory

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

module KanagawaHALSimpleDualPortRamTile
#(
    parameter integer WIDTH = 32,
    parameter integer DEPTH = 32,

/* verilator lint_off UNUSEDPARAM */
    parameter integer MUX = 4,
/* verilator lint_on UNUSEDPARAM */

    parameter integer USE_OUTPUT_REG = 1, // 1 = enable, 0 = disable
    parameter integer SUPPORTS_RW_COLLISIONS = 1,
    parameter integer RW_COLLISIONS_IMPOSSIBLE = 0,
    parameter DEVICE_FAMILY = "mock",

    parameter integer ADDR_WIDTH = $clog2(DEPTH)
)
(
    input  wire clk,

    input  wire                     wr_en_in,
    input  wire [ADDR_WIDTH-1:0]    wr_addr_in,
    input  wire [WIDTH-1:0]         wr_data_in,

    input  wire                     rd_en_in,
    input  wire [ADDR_WIDTH-1:0]    rd_addr_in,
    output wire [WIDTH-1:0]         rd_data_out
);

    wire [1:0][ADDR_WIDTH-1:0] addr_in;
    assign addr_in[0] = wr_addr_in;
    assign addr_in[1] = rd_addr_in;

    wire [1:0] wren_in;
    assign wren_in[0] = wr_en_in;
    assign wren_in[1] = 1'b0;

    wire [1:0][WIDTH-1:0] data_in;
    assign data_in[0] = wr_data_in;
    assign data_in[1] = '0;

    wire [1:0] rden_in;
    assign rden_in[0] = 1'b0;
    assign rden_in[1] = rd_en_in;

/* verilator lint_off UNUSEDSIGNAL */
    wire [1:0][WIDTH-1:0] data_out;
/* verilator lint_on UNUSEDSIGNAL */
    assign rd_data_out = data_out[1];

    KanagawaHALDualPortRAM
    #(
        .DATA_WIDTH             (WIDTH),
        .ADDR_WIDTH             (ADDR_WIDTH),
        .MAX_DEPTH              (DEPTH),
        .USE_LUTRAM             (DEPTH <= 64 || (USE_OUTPUT_REG == 0)),
        .USE_BRAM               (0),
        .USE_OUTPUT_REG         (USE_OUTPUT_REG),
        .DEVICE_FAMILY          (DEVICE_FAMILY),
        .INITIAL_DATA_FILE      ("UNUSED"),
        .TRUE_DUAL_PORT         (1),
        .USE_HARDENED_BYPASS    (0),
        .SUPPORTS_RW_COLLISIONS (SUPPORTS_RW_COLLISIONS),
        .RW_COLLISIONS_IMPOSSIBLE (RW_COLLISIONS_IMPOSSIBLE),
        .ECC                    (0)
    ) pd_dp_ram
    (
        .clk                            (clk),

        .addr_in                        (addr_in),

        .wren_in                        (wren_in),
        .data_in                        (data_in),
        .inject_correctable_error_in    (1'b0),
        .inject_uncorrectable_error_in  (1'b0),

        .rden_in                        (rden_in),
        .data_out                       (data_out),
        .error_detected_out             (),
        .data_valid_out                 ()
    );

endmodule
