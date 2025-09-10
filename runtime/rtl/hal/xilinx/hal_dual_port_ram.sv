// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Platform-specific RAM module
// 1 read and 1 write port
// concurrent reads and writes return the old data

module KanagawaHALDualPortRAM
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter MAX_DEPTH,        // Ignored for Xilinx
    parameter USE_LUTRAM,       // 0 to used dedicated BRAM, 1 to use LUT resources
    parameter USE_BRAM,
    parameter USE_OUTPUT_REG,   // 1 to enable RAM output register
    parameter DEVICE_FAMILY,
    parameter INITIAL_DATA_FILE,
    parameter TRUE_DUAL_PORT,  // if 0, then port 0 is always write and port 1 is always read
    parameter USE_HARDENED_BYPASS, // if 1, then enable hardened coherent read logic
    parameter SUPPORTS_RW_COLLISIONS, // 1 if concurrent reads and writes return old data, 0 for undefined behavior (either new_data or old_data, implementation can choose)
    parameter RW_COLLISIONS_IMPOSSIBLE, // 1 if 'x is an acceptable output for read data on concurrent read and write of the same address
    parameter ECC
)
(
    input wire clk,

    input wire [1:0][ADDR_WIDTH-1:0] addr_in,

    // writes
    input wire [1:0] wren_in,
    input wire [1:0][DATA_WIDTH-1:0] data_in,
    input wire inject_correctable_error_in,
    input wire inject_uncorrectable_error_in,

    // reads
    input wire [1:0] rden_in,
    output logic [1:0][DATA_WIDTH-1:0] data_out,
    output logic error_detected_out,
    output logic data_valid_out
);
    initial begin
        assert(USE_HARDENED_BYPASS == 0) else $fatal(0, "%m: Xilinx KanagawaHALDualPortRAM does not support hardened bypass logic");
        assert(!(USE_LUTRAM && USE_BRAM)) else $fatal("Choose USE_LUTRAM, USE_BRAM, or neither, but not both");
    end

    // Round width up to a multiple of 64 if ECC is enabled
    localparam INTERNAL_DATA_WIDTH = ECC ? (DATA_WIDTH + 63) & 32'hFFFFFFC0 : DATA_WIDTH;

    logic [1:0][INTERNAL_DATA_WIDTH-1:0] internal_data_in;
    logic [1:0][INTERNAL_DATA_WIDTH-1:0] internal_data_out;

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    genvar i;
    generate
        for (i = 0; i < 2; i++) begin: genInternalData
            assign internal_data_in[i] = INTERNAL_DATA_WIDTH'(data_in[i]);
            assign data_out[i] = DATA_WIDTH'(internal_data_out[i]);
        end

        if (!USE_LUTRAM && !USE_BRAM) begin: gen_ram_type
            KanagawaXilinxURAM
            #(
                .DATA_WIDTH             (INTERNAL_DATA_WIDTH),
                .ADDR_WIDTH             (ADDR_WIDTH),
                .USE_OUTPUT_REG         (USE_OUTPUT_REG),
                .DEVICE_FAMILY          (DEVICE_FAMILY),
                .INITIAL_DATA_FILE      (INITIAL_DATA_FILE),
                .TRUE_DUAL_PORT         (TRUE_DUAL_PORT),
                .USE_HARDENED_BYPASS    (USE_HARDENED_BYPASS),
                .SUPPORTS_RW_COLLISIONS (SUPPORTS_RW_COLLISIONS),
                .ECC                    (ECC)
            ) xilinx_uram
            (
                .clk                            (clk),
                .addr_in                        (addr_in),
                .wren_in                        (wren_in),
                .data_in                        (internal_data_in),
                .inject_correctable_error_in    (inject_correctable_error_in),
                .inject_uncorrectable_error_in  (inject_uncorrectable_error_in),
                .rden_in                        (rden_in),
                .data_out                       (internal_data_out),
                .error_detected_out             (error_detected_out),
                .data_valid_out                 (data_valid_out)
            );

        end
        else begin: gen_ram_type
            KanagawaXilinxRAM
            #(
                .DATA_WIDTH             (INTERNAL_DATA_WIDTH),
                .ADDR_WIDTH             (ADDR_WIDTH),
                .DEPTH                  (DEPTH),
                .USE_LUTRAM             (USE_LUTRAM),
                .USE_OUTPUT_REG         (USE_OUTPUT_REG),
                .DEVICE_FAMILY          (DEVICE_FAMILY),
                .INITIAL_DATA_FILE      (INITIAL_DATA_FILE),
                .TRUE_DUAL_PORT         (TRUE_DUAL_PORT),
                .USE_HARDENED_BYPASS    (USE_HARDENED_BYPASS),
                .SUPPORTS_RW_COLLISIONS (SUPPORTS_RW_COLLISIONS),
                .ECC                    (ECC)
            ) xilinx_ram
            (
                .clk                            (clk),
                .addr_in                        (addr_in),
                .wren_in                        (wren_in),
                .data_in                        (internal_data_in),
                .inject_correctable_error_in    (inject_correctable_error_in),
                .inject_uncorrectable_error_in  (inject_uncorrectable_error_in),
                .rden_in                        (rden_in),
                .data_out                       (internal_data_out),
                .error_detected_out             (error_detected_out),
                .data_valid_out                 (data_valid_out)
            );

        end
    endgenerate
endmodule

module KanagawaXilinxRAM
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter USE_LUTRAM,       // 0 to used dedicated BRAM, 1 to use LUT resources
    parameter USE_OUTPUT_REG,   // 1 to enable RAM output register
    parameter DEVICE_FAMILY,
    parameter INITIAL_DATA_FILE,
    parameter TRUE_DUAL_PORT,  // if 0, then port 0 is always write and port 1 is always read
    parameter USE_HARDENED_BYPASS, // if 1, then enable hardened coherent read logic
    parameter SUPPORTS_RW_COLLISIONS, // 1 if concurrent reads and writes return old data, 0 for undefined behavior
    parameter ECC
)
(
    input wire clk,

    input wire [1:0][ADDR_WIDTH-1:0] addr_in,

    // writes
    input wire [1:0] wren_in,
    input wire [1:0][DATA_WIDTH-1:0] data_in,
    input wire inject_correctable_error_in,
    input wire inject_uncorrectable_error_in,

    // reads
    input wire [1:0] rden_in,
    output logic [1:0][DATA_WIDTH-1:0] data_out,
    output logic error_detected_out,
    output logic data_valid_out
);

    localparam MEMORY_PRIMITIVE = USE_LUTRAM ? "distributed" : "block";
    localparam MEMORY_INIT_PARAM = INITIAL_DATA_FILE == "UNUSED" ? "0" : "";
    localparam MEMORY_INIT_FILE = INITIAL_DATA_FILE == "UNUSED" ? "none" : INITIAL_DATA_FILE;
    localparam USE_MEM_INIT = INITIAL_DATA_FILE == "UNUSED" ? 0 : 1;
    localparam READ_LATENCY = USE_OUTPUT_REG ? 2 : 1;
    localparam WRITE_MODE = SUPPORTS_RW_COLLISIONS || USE_LUTRAM ? "read_first" : "no_change";
    localparam MEMORY_SIZE = DATA_WIDTH * DEPTH;

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    generate
        if (TRUE_DUAL_PORT) begin: gen_ram_ports
            initial begin
                assert(ECC == 0) else $fatal(0, "%m: ECC not supported with true dual port ram");
            end

            if(!ECC) begin: gen_ecc
                // Because ECC is not supported, always report an uncorrectable error
                assign error_detected_out = 1'b1;
                assign data_valid_out = 1'b0;
            end

            xpm_memory_tdpram
            #(
                .MEMORY_SIZE(MEMORY_SIZE),
                .MEMORY_PRIMITIVE(MEMORY_PRIMITIVE),
                .MEMORY_INIT_PARAM(MEMORY_INIT_PARAM),
                .MEMORY_INIT_FILE(MEMORY_INIT_FILE),
                .USE_MEM_INIT(USE_MEM_INIT),

                .WRITE_DATA_WIDTH_A(DATA_WIDTH),
                .BYTE_WRITE_WIDTH_A(DATA_WIDTH),
                .READ_DATA_WIDTH_A(DATA_WIDTH),
                .ADDR_WIDTH_A(ADDR_WIDTH),
                .READ_LATENCY_A(READ_LATENCY),
                .WRITE_MODE_A(WRITE_MODE),

                .WRITE_DATA_WIDTH_B(DATA_WIDTH),
                .BYTE_WRITE_WIDTH_B(DATA_WIDTH),
                .READ_DATA_WIDTH_B(DATA_WIDTH),
                .ADDR_WIDTH_B(ADDR_WIDTH),
                .READ_LATENCY_B(READ_LATENCY),
                .WRITE_MODE_B(WRITE_MODE)
            )
            tdpram
            (
                .sleep(1'b0),

                .clka(clk),

                .rsta(1'b0),
                .ena(1'b1),
                .regcea(1'b1),
                .wea(wren_in[0]),
                .addra(addr_in[0]),
                .dina(data_in[0]),
                .injectsbiterra(1'b0),
                .injectdbiterra(1'b0),
                .douta(data_out[0]),
                .sbiterra(),
                .dbiterra(),

                .clkb(clk),
                .rstb(1'b0),
                .enb(1'b1),
                .regceb(1'b1),
                .web(wren_in[1]),
                .addrb(addr_in[1]),
                .dinb(data_in[1]),
                .injectsbiterrb(1'b0),
                .injectdbiterrb(1'b0),
                .doutb(data_out[1]),
                .sbiterrb(),
                .dbiterrb()
            );
        end
        else begin: gen_ram_ports
            assign data_out[0] = 'x;

            logic single_bit_error_detected;
            logic double_bit_error_detected;

            if (ECC) begin: gen_ecc
                assign error_detected_out = single_bit_error_detected | double_bit_error_detected;
                assign data_valid_out = !double_bit_error_detected;
            end
            else begin: gen_ecc
                // ECC status bits should not be used
                // Report uncorrectable errors
                assign error_detected_out = 1'b1;
                assign data_valid_out = 1'b0;
            end

            xpm_memory_sdpram
            #(
                .MEMORY_SIZE(MEMORY_SIZE),
                .MEMORY_PRIMITIVE(MEMORY_PRIMITIVE),
                .MEMORY_INIT_PARAM(MEMORY_INIT_PARAM),
                .MEMORY_INIT_FILE(MEMORY_INIT_FILE),
                .USE_MEM_INIT(USE_MEM_INIT),

                .ECC_MODE(ECC ? "both_encode_and_decode" : "no_ecc"),

                .WRITE_DATA_WIDTH_A(DATA_WIDTH),
                .BYTE_WRITE_WIDTH_A(DATA_WIDTH),
                .ADDR_WIDTH_A(ADDR_WIDTH),

                .READ_DATA_WIDTH_B(DATA_WIDTH),
                .ADDR_WIDTH_B(ADDR_WIDTH),
                .READ_LATENCY_B(READ_LATENCY),
                .WRITE_MODE_B("read_first")
            )
            sdpram
            (
                .sleep(1'b0),

                .clka(clk),
                .ena(1'b1),
                .wea(wren_in[0]),
                .addra(addr_in[0]),
                .dina(data_in[0]),
                .injectsbiterra(inject_correctable_error_in),
                .injectdbiterra(inject_uncorrectable_error_in),

                .clkb(clk),
                .rstb(1'b0),
                .enb(1'b1),
                .regceb(1'b1),
                .addrb(addr_in[1]),
                .doutb(data_out[1]),
                .sbiterrb(single_bit_error_detected),
                .dbiterrb(double_bit_error_detected)
            );
        end
    endgenerate
endmodule

module KanagawaXilinxURAM
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter USE_OUTPUT_REG,   // 1 to enable RAM output register
    parameter DEVICE_FAMILY,
    parameter INITIAL_DATA_FILE,
    parameter TRUE_DUAL_PORT,  // if 0, then port 0 is always write and port 1 is always read
    parameter USE_HARDENED_BYPASS, // if 1, then enable hardened coherent read logic
    parameter SUPPORTS_RW_COLLISIONS, // 1 if concurrent reads and writes return old data, 0 for undefined behavior
    parameter ECC
)
(
    input wire clk,

    input wire [1:0][ADDR_WIDTH-1:0] addr_in,

    // writes
    input wire [1:0] wren_in,
    input wire [1:0][DATA_WIDTH-1:0] data_in,
    input wire inject_correctable_error_in,
    input wire inject_uncorrectable_error_in,

    // reads
    input wire [1:0] rden_in,
    output logic [1:0][DATA_WIDTH-1:0] data_out,
    output logic error_detected_out,
    output logic data_valid_out
);
    localparam DEPTH = 1 << ADDR_WIDTH;

    localparam MEMORY_INIT_PARAM = INITIAL_DATA_FILE == "UNUSED" ? "0" : "";
    localparam MEMORY_INIT_FILE = INITIAL_DATA_FILE == "UNUSED" ? "none" : INITIAL_DATA_FILE;
    localparam USE_MEM_INIT = INITIAL_DATA_FILE == "UNUSED" ? 0 : 1;
    localparam READ_LATENCY = USE_OUTPUT_REG ? 2 : 1;
    localparam WRITE_MODE = SUPPORTS_RW_COLLISIONS ? "read_first" : "no_change";

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    initial begin
        assert(USE_HARDENED_BYPASS == 0) else $fatal(0, "%m: Xilinx KanagawaHALDualPortRAM does not support hardened bypass logic");
    end

    typedef struct { int width, factor; } fold_uram_t;
    function automatic fold_uram_t fold_uram_func(int data_width);
        fold_uram_func.width = data_width;
        fold_uram_func.factor = 1;
        // Consider only 8-bit or 9-bit multiples, since that's the byte enable resolution
        if (data_width == 8 || data_width == 16 || data_width == 32 ||
            data_width == 9 || data_width == 18 || data_width == 36)
        begin
            // Only consider if it will be mapped to more than one URAM
            if (ADDR_WIDTH > 12) begin
                fold_uram_func.width = data_width;
                if (data_width % 9 == 0)
                    fold_uram_func.factor = 72 / data_width;
                else
                    fold_uram_func.factor = 64 / data_width;
                // No point in folding past the minimum necessary for reducing into a single URAM
                if (fold_uram_func.factor > (1 << (ADDR_WIDTH - 12)))
                    fold_uram_func.factor = 1 << (ADDR_WIDTH - 12);
            end
        end
        // For less than one URAM data width, round up to nearest 8-bit or 9-bit multiple
        else if (data_width < 8)
            fold_uram_func = fold_uram_func(8);
        else if (data_width < 16)
            fold_uram_func = fold_uram_func(16);
        else if (data_width < 18)
            fold_uram_func = fold_uram_func(18);
        else if (data_width < 32)
            fold_uram_func = fold_uram_func(32);
        else if (data_width < 36)
            fold_uram_func = fold_uram_func(36);
    endfunction

    localparam fold_uram_t FOLD_URAM = fold_uram_func(DATA_WIDTH);
    localparam MEMORY_ADDR_WIDTH = ADDR_WIDTH - $clog2(FOLD_URAM.factor);
    localparam MEMORY_DEPTH = 1 << MEMORY_ADDR_WIDTH;
    localparam MEMORY_DATA_WIDTH = FOLD_URAM.width * FOLD_URAM.factor;
    localparam MEMORY_SIZE = MEMORY_DATA_WIDTH * MEMORY_DEPTH;
    localparam MEMORY_ENABLE_WIDTH = (FOLD_URAM.factor == 1 ? 1 :
                                      MEMORY_DATA_WIDTH / (MEMORY_DATA_WIDTH % 9 == 0 ? 9 : 8));

    typedef logic [MEMORY_DATA_WIDTH-1:0] sim_access_data_t;
    typedef logic [MEMORY_ADDR_WIDTH-1:0] sim_access_addr_t;

    sim_access_addr_t ram_addr_in[2];
    wire [MEMORY_ENABLE_WIDTH-1:0] ram_wren_in[2];
    sim_access_data_t ram_data_in[2];
    sim_access_data_t ram_data_out[2];

    generate
        for (genvar i = 0; i < 2; i++) begin
            if (FOLD_URAM.factor > 1) begin
                // Pass through high address bits
                assign ram_addr_in[i] = addr_in[i][ADDR_WIDTH-1:$clog2(FOLD_URAM.factor)];
                // Extract low address bits
                wire [$clog2(FOLD_URAM.factor)-1:0] addr_low = addr_in[i][$clog2(FOLD_URAM.factor)-1:0];
                // Replicate data in
                wire [FOLD_URAM.width-1:0] data = data_in[i];
                assign ram_data_in[i] = {FOLD_URAM.factor{data}};
                // Compute byte-enables
                localparam BYTES_PER_DATA = FOLD_URAM.width / (FOLD_URAM.width % 9 == 0 ? 9 : 8);
                assign ram_wren_in[i] = {BYTES_PER_DATA{wren_in[i]}} << (addr_low * BYTES_PER_DATA);

                // Delay addr_low and use it to multiplex data out
                logic [$clog2(FOLD_URAM.factor)-1:0] out_addr_low[READ_LATENCY];
                always @(posedge clk) begin
                    for (integer i = 0; i < READ_LATENCY-1; i++)
                        out_addr_low[i] <= out_addr_low[i+1];
                    out_addr_low[READ_LATENCY-1] <= addr_low;
                end
                assign data_out[i] = ram_data_out[i][out_addr_low[0] * FOLD_URAM.width +: DATA_WIDTH];
            end
            else begin
                // Pass straight through
                assign ram_addr_in[i] = addr_in[i];
                assign ram_wren_in[i] = wren_in[i];
                assign ram_data_in[i] = data_in[i];
                assign data_out[i] = ram_data_out[i];
            end
        end
    endgenerate

    generate
        if (TRUE_DUAL_PORT) begin: gen_ram_ports
            if(!ECC) begin: gen_ecc
                // Because ECC is not supported, always report an uncorrectable error
                assign error_detected_out = 1'b1;
                assign data_valid_out = 1'b0;
            end

            xpm_memory_tdpram
            #(
                .MEMORY_SIZE(MEMORY_SIZE),
                .MEMORY_PRIMITIVE("ultra"),
                .MEMORY_INIT_PARAM(MEMORY_INIT_PARAM),
                .MEMORY_INIT_FILE(MEMORY_INIT_FILE),
                .USE_MEM_INIT(USE_MEM_INIT),

                .WRITE_DATA_WIDTH_A(MEMORY_DATA_WIDTH),
                .BYTE_WRITE_WIDTH_A(FOLD_URAM.factor == 1 ? MEMORY_DATA_WIDTH :
                                    MEMORY_DATA_WIDTH % 9 == 0 ? 9 : 8),
                .READ_DATA_WIDTH_A(MEMORY_DATA_WIDTH),
                .ADDR_WIDTH_A(MEMORY_ADDR_WIDTH),
                .READ_LATENCY_A(READ_LATENCY),
                .WRITE_MODE_A(WRITE_MODE),

                .WRITE_DATA_WIDTH_B(MEMORY_DATA_WIDTH),
                .BYTE_WRITE_WIDTH_B(FOLD_URAM.factor == 1 ? MEMORY_DATA_WIDTH :
                                    MEMORY_DATA_WIDTH % 9 == 0 ? 9 : 8),
                .READ_DATA_WIDTH_B(MEMORY_DATA_WIDTH),
                .ADDR_WIDTH_B(MEMORY_ADDR_WIDTH),
                .READ_LATENCY_B(READ_LATENCY),
                .WRITE_MODE_B(WRITE_MODE)
            )
            tdpram
            (
                .sleep(1'b0),

                .clka(clk),

                .rsta(1'b0),
                .ena(1'b1),
                .regcea(1'b1),
                .wea(ram_wren_in[0]),
                .addra(ram_addr_in[0]),
                .dina(ram_data_in[0]),
                .injectsbiterra(1'b0),
                .injectdbiterra(1'b0),
                .douta(ram_data_out[0]),
                .sbiterra(),
                .dbiterra(),

                .clkb(clk),
                .rstb(1'b0),
                .enb(1'b1),
                .regceb(1'b1),
                .web(ram_wren_in[1]),
                .addrb(ram_addr_in[1]),
                .dinb(ram_data_in[1]),
                .injectsbiterrb(1'b0),
                .injectdbiterrb(1'b0),
                .doutb(ram_data_out[1]),
                .sbiterrb(),
                .dbiterrb()
            );
        end
        else begin: gen_ram_ports
            assign ram_data_out[0] = 'x;

            logic single_bit_error_detected;
            logic double_bit_error_detected;

            if (ECC) begin: gen_ecc
                assign error_detected_out = single_bit_error_detected | double_bit_error_detected;
                assign data_valid_out = !double_bit_error_detected;
            end
            else begin: gen_ecc
                // ECC status bits should not be used
                // Report uncorrectable errors
                assign error_detected_out = 1'b1;
                assign data_valid_out = 1'b0;
            end

            xpm_memory_sdpram
            #(
                .MEMORY_SIZE(MEMORY_SIZE),
                .MEMORY_PRIMITIVE("ultra"),
                .MEMORY_INIT_PARAM(MEMORY_INIT_PARAM),
                .MEMORY_INIT_FILE(MEMORY_INIT_FILE),
                .USE_MEM_INIT(USE_MEM_INIT),

                .ECC_MODE(ECC ? "both_encode_and_decode" : "no_ecc"),

                .WRITE_DATA_WIDTH_A(MEMORY_DATA_WIDTH),
                .BYTE_WRITE_WIDTH_A(FOLD_URAM.factor == 1 ? MEMORY_DATA_WIDTH :
                                    MEMORY_DATA_WIDTH % 9 == 0 ? 9 : 8),
                .ADDR_WIDTH_A(MEMORY_ADDR_WIDTH),

                .READ_DATA_WIDTH_B(MEMORY_DATA_WIDTH),
                .ADDR_WIDTH_B(MEMORY_ADDR_WIDTH),
                .READ_LATENCY_B(READ_LATENCY),
                .WRITE_MODE_B("read_first")
            )
            sdpram
            (
                .sleep(1'b0),

                .clka(clk),
                .ena(1'b1),
                .wea(ram_wren_in[0]),
                .addra(ram_addr_in[0]),
                .dina(ram_data_in[0]),
                .injectsbiterra(inject_correctable_error_in),
                .injectdbiterra(inject_uncorrectable_error_in),

                .clkb(clk),
                .rstb(1'b0),
                .enb(1'b1),
                .regceb(1'b1),
                .addrb(ram_addr_in[1]),
                .doutb(ram_data_out[1]),
                .sbiterrb(single_bit_error_detected),
                .dbiterrb(double_bit_error_detected)
            );
        end
    endgenerate
endmodule
