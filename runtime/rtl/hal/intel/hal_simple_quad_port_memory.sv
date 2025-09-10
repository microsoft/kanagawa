// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module KanagawaHalSimpleQuadPortMemory
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter MAX_DEPTH,        // Controls how deep each primitive RAM component can be, useful for not wasting bits
    parameter READ_LATENCY_0,
    parameter WRITE_DELAY_0,
    parameter READ_LATENCY_1,
    parameter WRITE_DELAY_1,
    parameter DEVICE_FAMILY,
    parameter INITIAL_DATA_FILE = ""
)
(
    input wire clk,
    input wire rst,

    // writes
    input wire [1:0][ADDR_WIDTH-1:0] write_addr_in,
    input wire [1:0] wren_in,
    input wire [1:0][DATA_WIDTH-1:0] data_in,

    // reads
    input wire [1:0] rden_in,
    input wire [1:0][ADDR_WIDTH-1:0] read_addr_in,
    output logic [1:0][DATA_WIDTH-1:0] data_out,
    output logic [1:0] ecc_status_out

);
    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    initial begin
        assert((READ_LATENCY_0 == 1) || (READ_LATENCY_0 == 2)) else $error("%m: READ_LATENCY_0 must be 1 or 2");
        assert((READ_LATENCY_1 == 1) || (READ_LATENCY_1 == 2)) else $error("%m: READ_LATENCY_1 must be 1 or 2");
    end

    localparam VALIDATE_KNOWN_WREN = (INITIAL_DATA_FILE != "UNUSED");

    // If the memory has initial content then
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

    // ECC is not supported with quad-port memories
    // Always report an uncorrectable error
    assign ecc_status_out[0] = 1'b1;
    assign ecc_status_out[1] = 1'b0;

    typedef struct packed
    {
        logic                   wren;
        addr_t  addr;
        data_t  data;
    } write_record;

    write_record [1:0] delayed_write;

    // write delay for each channel
    genvar port_index;
    generate
        for (port_index = 0; port_index < 2; port_index++) begin: gen_write_delay
            write_record pre_delay;
            assign pre_delay = '{wren:wren_in[port_index], addr:write_addr_in[port_index], data:data_in[port_index]};

            KanagawaFlipFlopChainNoEnable
            #(
                .WIDTH($bits(write_record)),
                .DEPTH(port_index == 0 ? WRITE_DELAY_0 : WRITE_DELAY_1)
            )
            write_delay_chain
            (
                .clk(clk),

                .data_in(pre_delay),
                .data_out(delayed_write[port_index])
            );
        end
    endgenerate

    // Work around simulation warnings from EDA tool simlibs for reads from
    // invalid addresses. This adjustment only happens in simulation.
    logic [1:0] rden;

    always @(*) begin
        rden = rden_in;

    // synopsys translate_off
            // If not power of two memory
            if (2**$clog2(DEPTH) != DEPTH) begin
                for (int i = 0; i < 2; ++i) begin
                    if (!$isunknown(read_addr_in[i]) && read_addr_in[i] >= DEPTH) begin
                        rden[i] = 1'b0;
                    end
                end
            end
    // synopsys translate_on
    end

    altera_syncram
    #(
        .enable_coherent_read("FALSE"),
        .clock_enable_input_a("BYPASS"),
        .clock_enable_input_b("BYPASS"),
        .clock_enable_output_a("BYPASS"),
        .clock_enable_output_b("BYPASS"),
        .intended_device_family("Stratix 10"),
        .lpm_type("altera_syncram"),
        .maximum_depth(MAX_DEPTH),
        .numwords_a(DEPTH),
        .numwords_b(DEPTH),
        .operation_mode("QUAD_PORT"),
        .outdata_reg_a(READ_LATENCY_0 == 2 ? "CLOCK0" : "UNREGISTERED"),
        .outdata_reg_b(READ_LATENCY_1 == 2 ? "CLOCK0" : "UNREGISTERED"),
        .power_up_uninitialized("FALSE"),
        .ram_block_type("M20K"),
        .read_during_write_mode_port_a("DONT_CARE"),
        .read_during_write_mode_port_b("DONT_CARE"),
        .read_during_write_mode_mixed_ports("NEW_A_OLD_B"),
        .widthad_a(ADDR_WIDTH),
        .widthad2_a(ADDR_WIDTH),
        .widthad_b(ADDR_WIDTH),
        .widthad2_b(ADDR_WIDTH),
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
        .address_a (delayed_write[0].addr),
        .address_b (delayed_write[1].addr),
        .address2_a(read_addr_in[0]),
        .address2_b(read_addr_in[1]),
        .clock0 (clk),
        .data_a (delayed_write[0].data),
        .data_b (delayed_write[1].data),
        .wren_a (delayed_write[0].wren),
        .wren_b (delayed_write[1].wren),
        .q_a (data_out[0]),
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
        .rden_a (rden[0]),
        .rden_b (rden[1]),

        .eccencparity(8'b0),
        .eccencbypass(1'b0),
        .sclr(1'b0)
    );

    // synopsys translate_off
    function automatic data_t sim_access_syncram(bit is_write, addr_t addr, data_t wrval = '0);
        if (is_write) begin
            altsyncram_component.mem_data[addr] = wrval;
            altsyncram_component.mem_data_b[addr] = wrval;
            return 'x;
        end
        else begin
            return altsyncram_component.mem_data[addr];
        end
    endfunction
    // synopsys translate_on
endmodule
