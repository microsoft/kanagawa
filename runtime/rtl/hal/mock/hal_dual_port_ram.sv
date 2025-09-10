//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

/* verilator lint_off DECLFILENAME */
module KanagawaHALDualPortRAM
#(
    parameter DATA_WIDTH = 32,
    parameter ADDR_WIDTH = 6,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter USE_OUTPUT_REG = 0,   // 1 to enable RAM output register
    parameter INITIAL_DATA_FILE = "UNUSED",
    parameter TRUE_DUAL_PORT = 0,  // if 0, then port 0 is always write and port 1 is always read
    parameter USE_HARDENED_BYPASS = 0, // if 1, then enable hardened coherent read logic
/* verilator lint_off UNUSEDPARAM */
    parameter MAX_DEPTH = 0,
    parameter DEVICE_FAMILY = "mock",
    parameter USE_LUTRAM = 0,
    parameter USE_BRAM = 0,
    parameter SUPPORTS_RW_COLLISIONS = 0, // 1 if concurrent reads and writes return old data, 0 for undefined behavior (either new_data or old_data, implementation can choose)
    parameter RW_COLLISIONS_IMPOSSIBLE = 0, // 1 if 'x is an acceptable output for read data on concurrent read and write of the same address
    parameter ECC
/* verilator lint_on UNUSEDPARAM */
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

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;
    typedef struct packed {
      logic correctable_error;
      logic uncorrectable_error;
    } ecc_status_t;

    data_t mem [0:DEPTH-1];

    ecc_status_t [0:DEPTH-1] ecc_err;
    logic ecc_data_valid_ff;
    logic ecc_error_detected_ff;

    logic [1:0][DATA_WIDTH-1:0] internal_rddata;

    initial begin
        if (USE_HARDENED_BYPASS != 0) begin
            $error("%m: Hardened bypass not supported in mock HAL module");
        end

        if (INITIAL_DATA_FILE != "UNUSED" && INITIAL_DATA_FILE != "") begin
            $display("%m: Initializing memory contents from %s", INITIAL_DATA_FILE);
            $readmemh(INITIAL_DATA_FILE, mem);
        end
        // Initialize ECC error status
        ecc_err = '0;
    end

    generate
        // Writes
        always @(posedge clk) begin
            if (wren_in[0]) begin
                mem[addr_in[0]] <= data_in[0];
                ecc_err[addr_in[0]].correctable_error <= inject_correctable_error_in;
                ecc_err[addr_in[0]].uncorrectable_error <= inject_uncorrectable_error_in;
            end
            else if ((TRUE_DUAL_PORT != 0) && wren_in[1]) begin
                mem[addr_in[1]] <= data_in[1];
                ecc_err[addr_in[1]].correctable_error <= inject_correctable_error_in;
                ecc_err[addr_in[1]].uncorrectable_error <= inject_uncorrectable_error_in;
            end
        end

        // Reads
        always_ff @(posedge clk) begin
            ecc_data_valid_ff <= 1'b1;
            ecc_error_detected_ff <= 1'b0;

            if (rden_in[1]) begin
                internal_rddata[1] <= mem[addr_in[1]];
                ecc_data_valid_ff <= !ecc_err[addr_in[1]].uncorrectable_error;
                ecc_error_detected_ff <= |ecc_err[addr_in[1]];
            end
            if ((TRUE_DUAL_PORT != 0) && rden_in[0]) begin
                internal_rddata[0] <= mem[addr_in[0]];
            end
        end

        if (USE_OUTPUT_REG) begin : gen_use_output_reg
            always_ff @(posedge clk) begin
                data_out <= internal_rddata;
                error_detected_out <= ECC ? ecc_error_detected_ff : 1'b0;
                data_valid_out <= ECC ? ecc_data_valid_ff : 1'b1;
            end
        end
        else begin : gen_no_output_reg
            always_comb begin
                data_out = internal_rddata;
                error_detected_out = ECC ? ecc_error_detected_ff : 1'b0;
                data_valid_out = ECC ? ecc_data_valid_ff : 1'b1;
            end
        end
    endgenerate

    function automatic data_t sim_access_syncram(input bit is_write, input addr_t addr, input data_t wrval = '0);
        if (is_write) begin
            mem[addr] = wrval;
            return 'x;
        end
        else begin
            return mem[addr];
        end
    endfunction

endmodule
/* verilator lint_on DECLFILENAME */
