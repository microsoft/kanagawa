// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// simple dual-port memory in either BRAM or LUTRAM
// decomposition into smaller memories is done here, because Quartus stops using 40-bit wide mode after depth > 4096
module KanagawaSyncRam
#(
     parameter DATA_WIDTH = 32,
     parameter ADDR_WIDTH = 8,
     parameter DEPTH = 2**ADDR_WIDTH,
     parameter MAX_DEPTH = 16384,
     parameter USE_LUTRAM = 0,
     parameter USE_BRAM = 1,
     parameter USE_OUTPUT_REG = 1,
     parameter INITIAL_DATA_FILE = "UNUSED",
     parameter USE_HARDENED_BYPASS = 0,
     parameter WRITE_DELAY = 0,
     parameter TRUE_DUAL_PORT = 0,   // if 0, then port 0 is always write and port 1 is always read
     parameter SUPPORTS_RW_COLLISIONS = 0, // 1 if concurrent reads and writes return old data, 0 for undefined behavior (either new_data or old_data, implementation can choose)
     parameter RW_COLLISIONS_IMPOSSIBLE = 0, // 1 if 'x is an acceptable output for read data on concurrent read and write of the same address
     parameter ECC = 0, // to enable ECC
     parameter DEVICE_FAMILY
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

    typedef logic [ADDR_WIDTH-1:0] addr_t;
    typedef logic [DATA_WIDTH-1:0] data_t;

    data_t [1:0] ram_read_data;

    typedef struct packed
    {
        logic valid;
        addr_t addr;
        data_t data;
    } BypassRecord;

    // synopsys translate_off
    initial begin
        if (TRUE_DUAL_PORT > 0) begin
            if (WRITE_DELAY != 0) begin
                $error("%m: True dual port and write delay are not supported together");
            end
        end
    end
    // synopsys translate_on

    // Connect ram out directly to output of this module
    assign data_out = ram_read_data;

    // ECC error injection
    // Set these 2 registers to a count of the number of errors to inject
    logic [31:0] correctable_errors_to_inject_ff;
    logic [31:0] uncorrectable_errors_to_inject_ff;

    logic inject_correctable_error_ff;
    logic inject_uncorrectable_error_ff;

    generate

        if (ECC) begin
            // On each clock cycle, determine what errors should be injected
            always_ff @(posedge clk) begin
                inject_correctable_error_ff <= 1'b0;
                inject_uncorrectable_error_ff <= 1'b0;

                if (rst) begin
                    correctable_errors_to_inject_ff <= '0;
                    uncorrectable_errors_to_inject_ff <= '0;
                end
                else begin
                    if (correctable_errors_to_inject_ff > 0) begin
                        correctable_errors_to_inject_ff <= correctable_errors_to_inject_ff - 1;
                        inject_correctable_error_ff <= 1'b1;
                    end

                    if (uncorrectable_errors_to_inject_ff > 0) begin
                        uncorrectable_errors_to_inject_ff <= uncorrectable_errors_to_inject_ff - 1;
                        inject_uncorrectable_error_ff <= 1'b1;
                    end
                end
            end
        end
        else begin
            assign correctable_errors_to_inject_ff = '0;
            assign uncorrectable_errors_to_inject_ff = '0;
            assign inject_correctable_error_ff = 1'b0;
            assign inject_uncorrectable_error_ff = 1'b0;
        end

        // Work around simulation warnings from EDA tool simlibs for reads from
        // invalid addresses. This adjustment only happens in simulation.
        logic [1:0] rden;

        always @(*) begin
            rden = rden_in;

            // synopsys translate_off
            // If not power of two memory
            if (2**$clog2(DEPTH) != DEPTH) begin
                for (int i = 0; i < 2; ++i) begin
                    if (!$isunknown(addr_in[i]) && addr_in[i] >= DEPTH) begin
                        rden[i] = 1'b0;
                    end
                end
            end
            // synopsys translate_on
        end


        if (TRUE_DUAL_PORT == 1'b1) begin: gen_HAL

            // synopsys translate_off
            initial begin
                if (ECC != 0) begin
                    $error("%m: ECC is not supported with true dual-port memories");
                end
            end
            // synopsys translate_on

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

                .addr_in(addr_in),

                .wren_in(wren_in),
                .data_in(data_in),
                .inject_correctable_error_in(1'b0),
                .inject_uncorrectable_error_in(1'b0),

                .rden_in(rden),
                .data_out(ram_read_data),
                .error_detected_out(),
                .data_valid_out()
            );

        end else begin: gen_HAL
            // Delay writes by a few cycles if requested (to help timing)
            // Callers must compensate with external bypass to ensure correctness
            logic [1:0] hal_ram_wren;
            assign hal_ram_wren[1] = 1'b0;

            addr_t [1:0] hal_ram_addr;
            assign hal_ram_addr[1] = addr_in[1];

            data_t [1:0] hal_ram_data_in;
            assign hal_ram_data_in[1] = '0;

            struct packed {
                logic   wren;
                addr_t  addr;
                data_t  data;
            } pre_delay, post_delay;

            assign pre_delay = '{wren:wren_in[0], addr:addr_in[0], data:data_in[0]};
            assign hal_ram_wren[0] = post_delay.wren;
            assign hal_ram_addr[0] = post_delay.addr;
            assign hal_ram_data_in[0] = post_delay.data;

            // conditionalize WRITE_DELAY of 0 to avoid the Lint warning about unused clk port when data passing through
            if(WRITE_DELAY == 0) begin: gen_passthrough
                assign post_delay = pre_delay;
            end
            else begin: gen_ff_chain
                // The initial value here is important for FPGA targets when the memory has initial content
                // The initial value prevents stray writes immediately after power-on
                // The initial value is only used for the write-enable value
                // To allow hyper-retiming of the address and data registers (which are fine if they power-on to undefined values)
                KanagawaCascadedFlipFlopsWithOptionalInitialValue
                #(
                    .WIDTH(1),
                    .DEPTH(WRITE_DELAY),
                    .INIT_VAL('0),
                    .DO_INIT(INITIAL_DATA_FILE != "UNUSED")
                )
                wren_write_delay_chain
                (
                    .clk(clk),

                    .data_in(pre_delay.wren),
                    .data_out(post_delay.wren)
                );

                KanagawaCascadedFlipFlopsWithOptionalInitialValue
                #(
                    .WIDTH($bits(addr_t) + $bits(data_t)),
                    .DEPTH(WRITE_DELAY),
                    .INIT_VAL('0),
                    .DO_INIT(0)
                )
                addr_and_data_write_delay_chain
                (
                    .clk(clk),

                    .data_in({pre_delay.addr, pre_delay.data}),
                    .data_out({post_delay.addr, post_delay.data})
                );
            end

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
                .inject_correctable_error_in(inject_correctable_error_ff),
                .inject_uncorrectable_error_in(inject_uncorrectable_error_ff),

                .rden_in(rden),
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
    parameter DATA_WIDTH = 32,
    parameter ADDR_WIDTH = 8,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter MAX_DEPTH = 8192, // Ignored for Xilinx, controls decomposition on Intel
    parameter USE_LUTRAM = 0,       // 0 to used dedicated BRAM, 1 to use LUT resources
    parameter USE_BRAM = 1,
    parameter USE_OUTPUT_REG,   // 1 to enable RAM output register
    parameter DEVICE_FAMILY,
    parameter NUM_BYPASS_SLOTS = 0,
    parameter WRITE_DELAY = 0,
    parameter INITIAL_DATA_FILE = "UNUSED",
    parameter SUPPORTS_RW_COLLISIONS = 1, // 1 if concurrent reads and writes return old data, 0 for undefined behavior (either new_data or old_data, implementation can choose)
    parameter RW_COLLISIONS_IMPOSSIBLE = 0 // 1 if 'x is an acceptable output for read data on concurrent read and write of the same address
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

    // Bypass support
    logic [ADDR_WIDTH-1:0] read_addr_delayed;

    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(ADDR_WIDTH),
        .DEPTH(USE_OUTPUT_REG ? 2 : 1)
    )
    delay_read_addr
    (
        .clk(clk),

        .data_in(readaddr_in),
        .data_out(read_addr_delayed)
    );

    KanagawaMemoryBypass
    #(
        .DATA_WIDTH(DATA_WIDTH),
        .ADDR_WIDTH(ADDR_WIDTH),
        .NUM_BYPASS_SLOTS(NUM_BYPASS_SLOTS),
        .READ_ADDRESS_EARLY(0),
        .READ_DATA_EARLY(0)
    )
    bypass
    (
        .clk(clk),
        .read_addr_in(read_addr_delayed),
        .read_data_in(sync_ram_data_out[1]),
        .read_data_out(data_out),

        .wren_in(wren_in),
        .write_addr_in(writeaddr_in),
        .data_in(data_in)
    );

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
