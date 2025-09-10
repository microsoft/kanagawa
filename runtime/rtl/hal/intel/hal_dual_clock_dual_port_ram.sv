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

    generate
        if ((DEVICE_FAMILY == "Stratix10") | (DEVICE_FAMILY == "Stratix10NX") | (DEVICE_FAMILY == "Agilex")) begin : gen_s10


            altera_syncram
                #(
                    // .enable_coherent_read(USE_HARDENED_BYPASS ? "TRUE" : "FALSE"),
                    .clock_enable_input_a("BYPASS"),
                    .clock_enable_input_b("BYPASS"),
                    .clock_enable_output_a("BYPASS"),
                    .clock_enable_output_b("BYPASS"),
                    .intended_device_family(DEVICE_FAMILY),
                    .lpm_type("altsyncram"),
                    .maximum_depth(DEPTH),
                    .numwords_a(DEPTH),
                    .numwords_b(DEPTH),
                    .operation_mode("DUAL_PORT"),
                    .outdata_reg_a("CLOCK0"),
                    .outdata_reg_b("CLOCK1"),
                    .power_up_uninitialized("FALSE"),
                    .ram_block_type("M20K"),
                    // .read_during_write_mode_mixed_ports(SUPPORTS_RW_COLLISIONS ? "OLD_DATA" : "DONT_CARE"),
                    .widthad_a(ADDR_WIDTH),
                    .widthad_b(ADDR_WIDTH),
                    .width_a(DATA_WIDTH),
                    .width_b(DATA_WIDTH),
                    .width_byteena_a(1),
                    .width_byteena_b(1),
                    .address_reg_b("CLOCK1"),
                    .rdcontrol_reg_b("CLOCK1"),
                    .indata_reg_b("CLOCK1"),
                    .wrcontrol_wraddress_reg_b("CLOCK1"),
                    .byteena_reg_b("CLOCK1")
                    )
                altsyncram_component
                (
                    .address_a (writeaddr_in),
                    .address_b (readaddr_in),
                    .clock0 (wrclk),
                    .data_a (data_in),
                    .data_b ({DATA_WIDTH{1'b0}}),
                    .wren_a (wren_in),
                    .wren_b (1'b0),
                    .q_a (),
                    .q_b (data_out),
                    .aclr0 (1'b0),
                    .aclr1 (1'b0),
                    .addressstall_a (1'b0),
                    .addressstall_b (1'b0),
                    .byteena_a (1'b1),
                    .byteena_b (1'b1),
                    .clock1 (rdclk),
                    .clocken0 (1'b1),
                    .clocken1 (1'b1),
                    .clocken2 (1'b1),
                    .clocken3 (1'b1),
                    .eccstatus (),
                    .rden_a (1'b0),
                    .rden_b (1'b1)
                );
        end
        else begin : gen_pre_s10
            altsyncram #(
                .address_reg_b("CLOCK1"),
                .clock_enable_input_a("BYPASS"),
                .clock_enable_input_b("BYPASS"),
                .clock_enable_output_a("BYPASS"),
                .clock_enable_output_b("BYPASS"),
                .indata_reg_b("CLOCK1"),
                .intended_device_family(DEVICE_FAMILY),
                .lpm_type("altsyncram"),
                .numwords_a(DEPTH),
                .numwords_b(DEPTH),
                .operation_mode("DUAL_PORT"),
                .outdata_aclr_a("NONE"),
                .outdata_aclr_b("NONE"),
                .outdata_reg_a("CLOCK0"),
                .outdata_reg_b("CLOCK1"),
                .power_up_uninitialized("FALSE"),
                .read_during_write_mode_port_a("NEW_DATA_NO_NBE_READ"),
                .read_during_write_mode_port_b("NEW_DATA_NO_NBE_READ"),
                .widthad_a(ADDR_WIDTH),
                .widthad_b(ADDR_WIDTH),
                .width_a(DATA_WIDTH),
                .width_b(DATA_WIDTH),
                .width_byteena_a(1),
                .width_byteena_b(1),
                .wrcontrol_wraddress_reg_b("CLOCK1")
            )
            altsyncram_component (
                .clock0 (wrclk),
                .wren_a (wren_in),
                .rden_a (1'b0),
                .address_a (writeaddr_in),
                .data_a (data_in),
                .q_a (),

                .clock1 (rdclk),
                .wren_b (1'b0),
                .rden_b (1'b1),
                .address_b (readaddr_in),
                .data_b ({DATA_WIDTH{1'b0}}),
                .q_b (data_out),

                .aclr0 (1'b0),
                .aclr1 (1'b0),
                .addressstall_a (1'b0),
                .addressstall_b (1'b0),
                .byteena_a (1'b1),
                .byteena_b (1'b1),
                .clocken0 (1'b1),
                .clocken1 (1'b1),
                .clocken2 (1'b1),
                .clocken3 (1'b1),
                .eccstatus ());
        end
    endgenerate
endmodule
