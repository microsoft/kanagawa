// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// simple dual-port memory in either BRAM or LUTRAM
// decomposition into smaller memories is done here, because Quartus stops using 40-bit wide mode after depth > 4096
// This version of KanagawaSyncRam intially runs correctly but will eventually
// (at a random point in time) flip one random bit of output at one randomly
// selected address. This module should be used for testing only (e.g.,
// testing parity checks).
module KanagawaSyncRam
#(
     parameter DATA_WIDTH = 40,
     parameter ADDR_WIDTH = 13,
     parameter DEPTH = 2**ADDR_WIDTH,
     parameter MAX_DEPTH = 16384,
     parameter USE_LUTRAM = 0,
     parameter USE_BRAM = 1,
     parameter USE_OUTPUT_REG = 1,
     parameter INITIAL_DATA_FILE = "UNUSED",
     parameter USE_HARDENED_BYPASS = 0,
     parameter NUM_BYPASS_SLOTS = 0,
     parameter WRITE_DELAY = 0,
     parameter TRUE_DUAL_PORT,   // if 0, then port 0 is always write and port 1 is always read
     parameter SUPPORTS_RW_COLLISIONS, // 1 if concurrent reads and writes return old data, 0 for undefined behavior (either new_data or old_data, implementation can choose)
     parameter RW_COLLISIONS_IMPOSSIBLE = 0, // 1 if 'x is an acceptable output for read data on concurrent read and write of the same address
     parameter ECC = 0, // to enable ECC
     parameter DEVICE_FAMILY,
     parameter ERROR = USE_BRAM, // Enable error for BRAM
     parameter ERROR_RATE = 31 // Every cycle, there is a 1 / (2 ^ ERROR_RATE) chance of the error being enabled
)
(
     input wire clk,
     input wire rst,

     input wire [1:0][ADDR_WIDTH-1:0] addr_in,

     // writes
     input wire [1:0] wren_in,
     input wire [1:0][DATA_WIDTH-1:0] data_in,

     // reads
     input wire [1:0] rden_in,
     output logic [1:0][DATA_WIDTH-1:0] data_out,
     output logic [1:0] ecc_status_out
);

    localparam NUM_BYPASS_ENTRIES = 2;

    logic [1:0][ADDR_WIDTH-1:0] addr_ff;
    logic [1:0][(DATA_WIDTH-1):0] ram_read_data;
    logic [1:0][(DATA_WIDTH-1):0] ram_read_data_error;

    typedef struct packed
    {
        logic valid;
        logic [(ADDR_WIDTH-1):0] addr;
        logic [(DATA_WIDTH-1):0] data;
    } BypassRecord;

    initial begin
        if (TRUE_DUAL_PORT > 0) begin
            if (NUM_BYPASS_SLOTS != 0) begin
                $error("%m: Bypass and true dual-port mode are not supported together");
            end

            if (WRITE_DELAY != 0) begin
                $error("%m: Bypass and write delay are not supported together");
            end

            // The following widths are restricted because of the use of $random
            if (ADDR_WIDTH > 32) begin
                $error("%m: Address width > 32 not supported");
            end

            if ($clog2(DATA_WIDTH) > 32) begin
                $error("%m: Data width > 32 not supported");
            end

            if (ERROR_RATE > 32) begin
                $error("%m: Error rate > 32 not supported");
            end
        end
    end

    // Error injection
    generate
        if (ERROR == 0) begin
            assign ram_read_data_error = ram_read_data;
        end else begin
            // Select a random address and bit index where the error will occur
            // % operations ensure that bit flips occur in valid words and bits
            logic [ADDR_WIDTH-1:0] error_addr;
            logic [$clog2(DATA_WIDTH)-1:0] error_bit;
            logic error_enabled = 1'b0;
            logic [ERROR_RATE-1:0] random_value;

            // not always_ff to avoid Questa warning about multiple drivers for error_enabled
            always @(posedge clk) begin
                if (error_enabled == 1'b0) begin
                    error_addr = $urandom_range(DEPTH - 1, 0);
                    error_bit = $urandom_range(DATA_WIDTH - 1, 0);

                    // Every cycle, generate a random value
                    random_value = $random;

                    // If random value is 0, then enable error
                    if (random_value == 0) begin
                        error_enabled = 1'b1;
                        $display("[%0t] %m: Memory bit flip error enabled, address = 0x%x, bit index = %d", $time, error_addr, error_bit);
                    end
                end
            end

            // Delay addr for error application
            KanagawaFlipFlopChainNoEnable
            #(
                .WIDTH(ADDR_WIDTH),
                .DEPTH(USE_OUTPUT_REG ? 2 : 1)
            )
            addr_delay_chain0
            (
                .clk(clk),

                .data_in(addr_in[0]),
                .data_out(addr_ff[0])
            );
            KanagawaFlipFlopChainNoEnable
            #(
                .WIDTH(ADDR_WIDTH),
                .DEPTH(USE_OUTPUT_REG ? 2 : 1)
            )
            addr_delay_chain1
            (
                .clk(clk),

                .data_in(addr_in[1]),
                .data_out(addr_ff[1])
            );

            always_comb begin
                ram_read_data_error = ram_read_data;

                if (error_enabled == 1'b1) begin
                    if (addr_ff[0] == error_addr) begin
                        ram_read_data_error[0][error_bit] ^= 1;
                    end
                    if (addr_ff[1] == error_addr) begin
                        ram_read_data_error[1][error_bit] ^= 1;
                    end
                end
            end
        end
    endgenerate

    generate
        if (NUM_BYPASS_SLOTS == 0) begin
            // Connect ram out directly to output of this module
            assign data_out = ram_read_data_error;
        end
        else begin : genBypass
            assign data_out[0] = 'x;

            KanagawaMemoryBypass
            #(
                .DATA_WIDTH(DATA_WIDTH),
                .ADDR_WIDTH(ADDR_WIDTH),
                .NUM_BYPASS_SLOTS(NUM_BYPASS_SLOTS),
                .READ_LATENCY(USE_OUTPUT_REG ? 2 : 1)
            ) bypass
            (
                .clk(clk),

                .rden_in(rden_in[1]),
                .readaddr_in(addr_in[1]),
                .read_data_in(ram_read_data_error[1]),
                .read_data_out(data_out[1]),

                .wren_in(wren_in[0]),
                .writeaddr_in(addr_in[0]),
                .data_in(data_in[0])
            );
        end
    endgenerate

    generate

        // Work around simulation warnings from EDA tool simlibs for reads from
        // invalid addresses. This workaround would not be needed if read-enables
        // were used. This adjustment only happens in simulation.
        logic [1:0][ADDR_WIDTH-1:0] bounded_addr;

        always @(*) begin
            bounded_addr = addr_in;

            // synopsys translate_off
            for (int i = 0; i < 2; ++i) begin
                if (!wren_in[i] && addr_in[i] >= DEPTH) begin
                    bounded_addr[i] = '0;
                end
            end
            // synopsys translate_on
        end


        if (TRUE_DUAL_PORT == 1'b1) begin

            // ECC status should not be used
            // Always indicate an uncorrectable error
            assign ecc_status_out = 2'b01;

            KanagawaHALDualPortRAM
            #(
                .DATA_WIDTH(DATA_WIDTH),
                .ADDR_WIDTH(ADDR_WIDTH),
                .DEPTH(DEPTH),
                .MAX_DEPTH(MAX_DEPTH),
                .USE_LUTRAM(USE_LUTRAM),
                .USE_BRAM(USE_BRAM),
                .USE_OUTPUT_REG(USE_OUTPUT_REG),
                .DEVICE_FAMILY(DEVICE_FAMILY),
                .INITIAL_DATA_FILE(INITIAL_DATA_FILE),
                .TRUE_DUAL_PORT(1),
                .USE_HARDENED_BYPASS(USE_HARDENED_BYPASS),
                .SUPPORTS_RW_COLLISIONS(SUPPORTS_RW_COLLISIONS),
                .RW_COLLISIONS_IMPOSSIBLE(RW_COLLISIONS_IMPOSSIBLE),
                .ECC(0)
            ) hal_ram
            (
                .clk(clk),

                .addr_in(bounded_addr),

                .wren_in(wren_in),
                .data_in(data_in),
                .inject_correctable_error_in(1'b0),
                .inject_uncorrectable_error_in(1'b0),

                .rden_in(rden_in),
                .data_out(ram_read_data),
                .error_detected_out(),
                .data_valid_out()
            );

        end else begin
            // Delay writes by a few cycles if requested (to help timing)
            // Callers must compenstate with NUM_BYPASS_SLOTS to ensure correctness
            logic [1:0] hal_ram_wren;
            assign hal_ram_wren[1] = 1'b0;

            logic [1:0][ADDR_WIDTH-1:0] hal_ram_addr;
            assign hal_ram_addr[1] = bounded_addr[1];

            logic [1:0][DATA_WIDTH-1:0] hal_ram_data_in;
            assign hal_ram_data_in[1] = 'x;

            struct packed {
                logic                   wren;
                logic [ADDR_WIDTH-1:0]  addr;
                logic [DATA_WIDTH-1:0]  data;
            } pre_delay, post_delay;

            assign pre_delay = '{wren:wren_in[0], addr:bounded_addr[0], data:data_in[0]};
            assign hal_ram_wren[0] = post_delay.wren;
            assign hal_ram_addr[0] = post_delay.addr;
            assign hal_ram_data_in[0] = post_delay.data;

            KanagawaFlipFlopChainNoEnable
            #(
                .WIDTH($bits(pre_delay)),
                .DEPTH(WRITE_DELAY)
            )
            write_delay_chain
            (
                .clk(clk),

                .data_in(pre_delay),
                .data_out(post_delay)
            );

            KanagawaHALDualPortRAM
            #(
                .DATA_WIDTH(DATA_WIDTH),
                .ADDR_WIDTH(ADDR_WIDTH),
                .DEPTH(DEPTH),
                .MAX_DEPTH(MAX_DEPTH),
                .USE_LUTRAM(USE_LUTRAM),
                .USE_BRAM(USE_BRAM),
                .USE_OUTPUT_REG(USE_OUTPUT_REG),
                .DEVICE_FAMILY(DEVICE_FAMILY),
                .INITIAL_DATA_FILE(INITIAL_DATA_FILE),
                .TRUE_DUAL_PORT(0),
                .USE_HARDENED_BYPASS(USE_HARDENED_BYPASS),
                .SUPPORTS_RW_COLLISIONS(SUPPORTS_RW_COLLISIONS),
                .RW_COLLISIONS_IMPOSSIBLE(RW_COLLISIONS_IMPOSSIBLE),
                .ECC(ECC)
            ) hal_ram
            (
                .clk(clk),

                .addr_in(hal_ram_addr),

                .wren_in(hal_ram_wren),
                .data_in(hal_ram_data_in),
                .inject_correctable_error_in(1'b0),
                .inject_uncorrectable_error_in(1'b0),

                .rden_in(rden_in),
                .data_out(ram_read_data),
                .error_detected_out(ecc_status_out[0]),
                .data_valid_out(ecc_status_out[1])
            );
        end
    endgenerate

endmodule

// RAM with 1 read and 1 write port
// Wrapper around KanagawaSyncRam RAM
module KanagawaSimpleDualPortRAM
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter MAX_DEPTH = 8192, // Ignored for Xilinx, controls decomposition on Intel
    parameter USE_LUTRAM,       // 0 to used dedicated BRAM, 1 to use LUT resources
    parameter USE_BRAM,
    parameter USE_OUTPUT_REG,   // 1 to enable RAM output register
    parameter DEVICE_FAMILY,
    parameter NUM_BYPASS_SLOTS = 0,
    parameter WRITE_DELAY = 0,
    parameter INITIAL_DATA_FILE = "UNUSED",
    parameter SUPPORTS_RW_COLLISIONS = 1,
    parameter RW_COLLISIONS_IMPOSSIBLE = 0
)
(
    input wire clk,
    input wire rst,

    input wire rden_in,
    input wire [(ADDR_WIDTH-1):0] readaddr_in,
    output logic [(DATA_WIDTH-1):0] data_out,

    input wire wren_in,
    input wire [(ADDR_WIDTH-1):0] writeaddr_in,
    input wire [(DATA_WIDTH-1):0] data_in
);
    logic [1:0] sync_ram_rden_in;
    logic [1:0][ADDR_WIDTH-1:0] sync_ram_addr_in;
    logic [1:0] sync_ram_wren_in;
    logic [1:0][DATA_WIDTH-1:0] sync_ram_data_in;
    logic [1:0][DATA_WIDTH-1:0] sync_ram_data_out;

    assign sync_ram_addr_in[0] = writeaddr_in;
    assign sync_ram_addr_in[1] = readaddr_in;

    assign sync_ram_rden_in[0] = 1'b0;
    assign sync_ram_rden_in[1] = rden_in;

    assign sync_ram_wren_in[0] = wren_in;
    assign sync_ram_wren_in[1] = 1'b0;

    assign sync_ram_data_in[0] = data_in;
    assign sync_ram_data_in[1] = 'x;

    assign data_out = sync_ram_data_out[1];

    KanagawaSyncRam
    #(
        .DATA_WIDTH(DATA_WIDTH),
        .ADDR_WIDTH(ADDR_WIDTH),
        .DEPTH(DEPTH),
        .MAX_DEPTH(MAX_DEPTH),
        .USE_LUTRAM(USE_LUTRAM),
        .USE_BRAM(USE_BRAM),
        .USE_OUTPUT_REG(USE_OUTPUT_REG),
        .INITIAL_DATA_FILE(INITIAL_DATA_FILE),
        .NUM_BYPASS_SLOTS(NUM_BYPASS_SLOTS),
        .WRITE_DELAY(WRITE_DELAY),
        .TRUE_DUAL_PORT(0),
        .DEVICE_FAMILY(DEVICE_FAMILY),
        .SUPPORTS_RW_COLLISIONS(SUPPORTS_RW_COLLISIONS),
        .RW_COLLISIONS_IMPOSSIBLE(RW_COLLISIONS_IMPOSSIBLE),
        .ECC(0)
    ) sync_ram
    (
        .clk(clk),
        .rst(rst),

        .addr_in(sync_ram_addr_in),

        .wren_in(sync_ram_wren_in),
        .data_in(sync_ram_data_in),

        .rden_in(sync_ram_rden_in),
        .data_out(sync_ram_data_out),
        .ecc_status_out()
    );
endmodule
