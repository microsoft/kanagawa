// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Platform-specific RAM module
// 2 modes of operation
// simple: 1 read and 1 write port
// true dual port: 2 ports, either can be read or write
// Note that if this name changes, KanagawaSynthSettings.tcl must change also
module KanagawaHALDualPortRAM
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter MAX_DEPTH,        // Controls how deep each primitive RAM component can be, useful for not wasting bits
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
    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    // For MLAB memories, NEW_DATA/DONT_CARE use fewer ALM resources
    // Use NEW_DATA to avoid 'x during simulation of mixed-read-write
    // For M20K, NEW_DATA is not supported, use OLD_DATA
    // In TRUE_DUAL_PORT mode (only used for ROM) - DONT_CARE is required by Quartus
    // Quartus requires DONT_CARE when USE_HARDENED_BYPASS is enabled
    localparam READ_WRITE_MIXED_PORTS_PARAM =
        TRUE_DUAL_PORT | USE_HARDENED_BYPASS ? "DONT_CARE" :
        RW_COLLISIONS_IMPOSSIBLE ? "DONT_CARE" :
        (USE_LUTRAM & ~SUPPORTS_RW_COLLISIONS) ? "NEW_DATA" : "OLD_DATA";

    // Intel LUTRAM do not support well-defined concurrent read-write to the same address
    // if the output register is disabled
    initial begin
        assert(USE_OUTPUT_REG | USE_BRAM) else $fatal(1, "%m: If output register is disabled, then memory type must be BRAM");
    end

    localparam VALIDATE_KNOWN_WREN = (INITIAL_DATA_FILE != "UNUSED") && (DEVICE_FAMILY != "Arria10");

    // On FPGAs newer than Arria 10, if the memory has initial content then
    // wren enable should never be unknown.
    // The control path that sets wren should power on to a state such that wren will be 0 before reset
    // If wren is 1 before reset, then stray writes can overwrite
    // the initial contents of memories
`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clk) VALIDATE_KNOWN_WREN -> !(wren_in[0] === 1'bx)) else $error("wren[0] signal is unknown");
    assert property (@(posedge clk) VALIDATE_KNOWN_WREN -> !(wren_in[1] === 1'bx)) else $error("wren[1] signal is unknown");
    // synopsys translate_on
`endif

    // The read address can be 'x on cycles when read data is not needed
    // Avoid debug output from the simulator in this case
    addr_t [1:0] addr_clean;

    always_comb begin
        for (int i = 0; i < 2; i++) begin
            addr_clean[i] = addr_in[i];

            //synopsys translate off
            if ((addr_in[i] === 'x) && (wren_in[i] === 1'b0)) addr_clean[i] = '0;
            //synopsys translate on
        end
    end

    data_t ram_data_out_a;

    logic [1:0] ecc_status_internal;

    // Set if an error has been detected (corrected or not)
    assign error_detected_out = ecc_status_internal[1];

    // Set if the output data is error free
    // either no ECC error (2'b00), or the error was corrected (2'b10)
    assign data_valid_out = !ecc_status_internal[0];

    // 2 different instantiations memory
    // altera_syncram has Stratix 10 feature: enable_coherent_read
    generate
        if ((DEVICE_FAMILY == "Stratix10") || (DEVICE_FAMILY == "Stratix10NX") || (DEVICE_FAMILY == "Agilex")) begin : gen_family
            initial begin
                assert(!USE_HARDENED_BYPASS || !USE_LUTRAM) else $fatal(0, "%m: USE_HARDENED_BYPASS requires BRAM (not LUTRAM)");
            end

            // ECC is handled with this conditional code
            // because quartus requires ports like eccstatus to be disconnected when ECC="FALSE"
            if (ECC == 1) begin: gen_ecc
                altera_syncram
                #(
                    .enable_coherent_read(USE_HARDENED_BYPASS ? "TRUE" : "FALSE"),
                    .clock_enable_input_a("BYPASS"),
                    .clock_enable_input_b("BYPASS"),
                    .clock_enable_output_a("BYPASS"),
                    .clock_enable_output_b("BYPASS"),
                    .intended_device_family(DEVICE_FAMILY),
                    .lpm_type("altsyncram"),
                    .maximum_depth(MAX_DEPTH),
                    .numwords_a(DEPTH),
                    .numwords_b(DEPTH),
                    .operation_mode(TRUE_DUAL_PORT ? "BIDIR_DUAL_PORT" : "DUAL_PORT"),
                    .outdata_reg_a(USE_OUTPUT_REG ? "CLOCK0" : "UNREGISTERED"),
                    .outdata_reg_b(USE_OUTPUT_REG ? "CLOCK0" : "UNREGISTERED"),
                    .power_up_uninitialized("FALSE"),
                    .ram_block_type(USE_LUTRAM ? "MLAB" : "M20K"),
                    .read_during_write_mode_mixed_ports(READ_WRITE_MIXED_PORTS_PARAM),
                    .widthad_a(ADDR_WIDTH),
                    .widthad_b(ADDR_WIDTH),
                    .width_a(DATA_WIDTH),
                    .width_b(DATA_WIDTH),
                    .width_byteena_a(1),
                    .width_byteena_b(1),
                    .init_file(INITIAL_DATA_FILE),
                    .address_reg_b("CLOCK0"),
                    .rdcontrol_reg_b("CLOCK0"),
                    .indata_reg_b("CLOCK0"),
                    .wrcontrol_wraddress_reg_b("CLOCK0"),
                    .byteena_reg_b("CLOCK0"),
                    .enable_ecc("TRUE"),
                    .enable_ecc_encoder_bypass("TRUE"),
                    .width_eccstatus(2)
                    )
                altsyncram_component
                (
                    .address_a (addr_clean[0]),
                    .address_b (addr_clean[1]),
                    .clock0 (clk),
                    .data_a (data_in[0]),
                    .data_b (TRUE_DUAL_PORT ? data_in[1] : {DATA_WIDTH{1'b1}}),
                    .wren_a (wren_in[0]),
                    .wren_b (TRUE_DUAL_PORT ? wren_in[1] : 1'b0),
                    .q_a (ram_data_out_a),
                    .q_b (data_out[1]),
                    .aclr0 (1'b0),
                    .aclr1 (1'b0),
                    .addressstall_a (1'b0),
                    .addressstall_b (1'b0),
                    .byteena_a (1'b1),
                    .byteena_b (1'b1),
                    .clock1 (1'b1),
                    .clocken0 (1'b1),
                    .clocken1 (1'b1),
                    .clocken2 (1'b1),
                    .clocken3 (1'b1),
                    .eccstatus (ecc_status_internal),
                    .rden_a (1'b1),
                    .rden_b (1'b1),

                    .address2_a(),
                    .address2_b(),
                    .eccencparity(inject_correctable_error_in ? 8'b1 : (inject_uncorrectable_error_in ? 8'b00001111 : '0)),
                    .eccencbypass(inject_correctable_error_in | inject_uncorrectable_error_in),
                    .sclr(1'b0)
                );

            end else begin: gen_ecc
                // Always report uncorrectable error
                // This signals should not be used
                assign ecc_status_internal = 2'b11;

                altera_syncram
                #(
                    .enable_coherent_read(USE_HARDENED_BYPASS ? "TRUE" : "FALSE"),
                    .clock_enable_input_a("BYPASS"),
                    .clock_enable_input_b("BYPASS"),
                    .clock_enable_output_a("BYPASS"),
                    .clock_enable_output_b("BYPASS"),
                    .intended_device_family(DEVICE_FAMILY),
                    .lpm_type("altsyncram"),
                    .maximum_depth(MAX_DEPTH),
                    .numwords_a(DEPTH),
                    .numwords_b(DEPTH),
                    .operation_mode(TRUE_DUAL_PORT ? "BIDIR_DUAL_PORT" : "DUAL_PORT"),
                    .outdata_reg_a(USE_OUTPUT_REG ? "CLOCK0" : "UNREGISTERED"),
                    .outdata_reg_b(USE_OUTPUT_REG ? "CLOCK0" : "UNREGISTERED"),
                    .power_up_uninitialized("FALSE"),
                    .ram_block_type(USE_LUTRAM ? "MLAB" : "M20K"),
                    .read_during_write_mode_mixed_ports(READ_WRITE_MIXED_PORTS_PARAM),
                    .widthad_a(ADDR_WIDTH),
                    .widthad_b(ADDR_WIDTH),
                    .width_a(DATA_WIDTH),
                    .width_b(DATA_WIDTH),
                    .width_byteena_a(1),
                    .width_byteena_b(1),
                    .init_file(INITIAL_DATA_FILE),
                    .address_reg_b("CLOCK0"),
                    .rdcontrol_reg_b("CLOCK0"),
                    .indata_reg_b("CLOCK0"),
                    .wrcontrol_wraddress_reg_b("CLOCK0"),
                    .byteena_reg_b("CLOCK0"),
                    .enable_ecc("FALSE"),
                    .enable_ecc_encoder_bypass("FALSE")
                    )
                altsyncram_component
                (
                    .address_a (addr_clean[0]),
                    .address_b (addr_clean[1]),
                    .clock0 (clk),
                    .data_a (data_in[0]),
                    .data_b (TRUE_DUAL_PORT ? data_in[1] : {DATA_WIDTH{1'b1}}),
                    .wren_a (wren_in[0]),
                    .wren_b (TRUE_DUAL_PORT ? wren_in[1] : 1'b0),
                    .q_a (ram_data_out_a),
                    .q_b (data_out[1]),
                    .aclr0 (1'b0),
                    .aclr1 (1'b0),
                    .addressstall_a (1'b0),
                    .addressstall_b (1'b0),
                    .byteena_a (1'b1),
                    .byteena_b (1'b1),
                    .clock1 (1'b1),
                    .clocken0 (1'b1),
                    .clocken1 (1'b1),
                    .clocken2 (1'b1),
                    .clocken3 (1'b1),
                    .eccstatus (),
                    .rden_a (1'b1),
                    .rden_b (1'b1),

                    .address2_a(),
                    .address2_b(),
                    .eccencparity(),
                    .eccencbypass(),
                    .sclr(1'b0)
                );

            end
        end
        else begin : gen_family
            initial begin
                assert(USE_HARDENED_BYPASS == 0) else $fatal(0, "%m: Intel KanagawaHALDualPortRAM (before Stratix 10) does not support hardened bypass logic");
                assert(ECC == 0) else $fatal(0, "%m: Intel KanagawaHALDualPortRAM (before Stratix 10) does not support ECC");
            end

            // ECC support on Arria 10 is not yet tested
            // Always report ECC errors
            assign ecc_status_internal = 2'b11;

            if (ECC == 0) begin: gen_ecc
                altsyncram
                #(
                    .clock_enable_input_a("BYPASS"),
                    .clock_enable_input_b("BYPASS"),
                    .clock_enable_output_a("BYPASS"),
                    .clock_enable_output_b("BYPASS"),
                    .intended_device_family(DEVICE_FAMILY),
                    .lpm_type("altsyncram"),
                    .maximum_depth(MAX_DEPTH),
                    .numwords_a(DEPTH),
                    .numwords_b(DEPTH),
                    .operation_mode(TRUE_DUAL_PORT ? "BIDIR_DUAL_PORT" : "DUAL_PORT"),
                    .outdata_reg_a(USE_OUTPUT_REG ? "CLOCK0" : "UNREGISTERED"),
                    .outdata_reg_b(USE_OUTPUT_REG ? "CLOCK0" : "UNREGISTERED"),
                    .power_up_uninitialized("FALSE"),
                    .ram_block_type(USE_LUTRAM ? "MLAB" : "M20K"),
                    .read_during_write_mode_mixed_ports(READ_WRITE_MIXED_PORTS_PARAM),
                    .widthad_a(ADDR_WIDTH),
                    .widthad_b(ADDR_WIDTH),
                    .width_a(DATA_WIDTH),
                    .width_b(DATA_WIDTH),
                    .width_byteena_a(1),
                    .width_byteena_b(1),
                    .init_file(INITIAL_DATA_FILE),
                    .address_reg_b("CLOCK0"),
                    .rdcontrol_reg_b("CLOCK0"),
                    .indata_reg_b("CLOCK0"),
                    .wrcontrol_wraddress_reg_b("CLOCK0"),
                    .byteena_reg_b("CLOCK0")
                    )
                altsyncram_component
                (
                    .address_a (addr_clean[0]),
                    .address_b (addr_clean[1]),
                    .clock0 (clk),
                    .data_a (data_in[0]),
                    .data_b (TRUE_DUAL_PORT ? data_in[1] : {DATA_WIDTH{1'b1}}),
                    .wren_a (wren_in[0]),
                    .wren_b (TRUE_DUAL_PORT ? wren_in[1] : 1'b0),
                    .q_a (ram_data_out_a),
                    .q_b (data_out[1]),
                    .aclr0 (1'b0),
                    .aclr1 (1'b0),
                    .addressstall_a (1'b0),
                    .addressstall_b (1'b0),
                    .byteena_a (1'b1),
                    .byteena_b (1'b1),
                    .clock1 (1'b1),
                    .clocken0 (1'b1),
                    .clocken1 (1'b1),
                    .clocken2 (1'b1),
                    .clocken3 (1'b1),
                    .eccstatus (),
                    .rden_a (1'b1),
                    .rden_b (1'b1)
                );

            end
        end
    endgenerate

    // In simple mode, set port 0 read data to 'x
    assign data_out[0] = TRUE_DUAL_PORT ? ram_data_out_a : 'x;
endmodule
