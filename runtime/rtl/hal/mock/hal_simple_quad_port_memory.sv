//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`ifndef SIMULATION
    `error "This file is only supported in simulation"
`endif

module KanagawaHalSimpleQuadPortMemory
#(
    parameter DATA_WIDTH = 32,
    parameter ADDR_WIDTH = 6,
    parameter DEPTH = 2**ADDR_WIDTH,
    parameter READ_LATENCY_0 = 1,
    parameter WRITE_DELAY_0 = 0,
    parameter READ_LATENCY_1 = 1,
    parameter WRITE_DELAY_1 = 0,
/* verilator lint_off UNUSEDPARAM */
    parameter MAX_DEPTH = 0,
    parameter DEVICE_FAMILY = "mock",
/* verilator lint_on UNUSEDPARAM */
    parameter string INITIAL_DATA_FILE = ""
)
(
    input wire clk,
/* verilator lint_off UNUSEDSIGNAL */
    input wire rst,
/* verilator lint_on UNUSEDSIGNAL */

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

    initial begin
        assert((READ_LATENCY_0 == 1) || (READ_LATENCY_0 == 2)) else $error("%m: READ_LATENCY_0 must be 1 or 2");
        assert((READ_LATENCY_1 == 1) || (READ_LATENCY_1 == 2)) else $error("%m: READ_LATENCY_1 must be 1 or 2");
        assert(WRITE_DELAY_0 >= 0) else $error("%m: WRITE_DELAY_0 must be >= 0");
        assert(WRITE_DELAY_1 >= 0) else $error("%m: WRITE_DELAY_1 must be >= 0");
    end

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    data_t mem [0:DEPTH-1];

    initial begin
        if (INITIAL_DATA_FILE != "UNUSED" && INITIAL_DATA_FILE != "") begin
            $display("%m: Initializing memory contents from %s", INITIAL_DATA_FILE);
            $readmemh(INITIAL_DATA_FILE, mem);
        end
    end

    logic [1:0][DATA_WIDTH-1:0] internal_rddata;

    typedef struct packed
    {
        logic   wren;
        addr_t  addr;
        data_t  data;
    } write_record;

    write_record [1:0] delayed_write;

    // Reads
    always_ff @(posedge clk) begin
        if (rden_in[0]) begin
            internal_rddata[0] <= mem[read_addr_in[0]];
        end

        if (rden_in[1]) begin
            internal_rddata[1] <= mem[read_addr_in[1]];
        end
    end

    // Pipelining
    genvar port_index;
    generate
        for (port_index = 0; port_index <= 1; ++port_index) begin: gen_read_write_delay
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

            KanagawaFlipFlopChainNoEnable
            #(
                .WIDTH(DATA_WIDTH),
                .DEPTH((port_index == 0 ? READ_LATENCY_0 : READ_LATENCY_1) - 1)
            )
            read_delay_chain
            (
                .clk(clk),

                .data_in(internal_rddata[port_index]),
                .data_out(data_out[port_index])
            );
        end
    endgenerate

    // Writes
    always_ff @(posedge clk) begin
        if (delayed_write[0].wren) begin
            mem[delayed_write[0].addr] <= delayed_write[0].data;
        end

        if (delayed_write[1].wren) begin
            mem[delayed_write[1].addr] <= delayed_write[1].data;
        end
    end

    // ECC is not supported with quad-port memories
    // Always report an uncorrectable error
    assign ecc_status_out[0] = 1'b1;
    assign ecc_status_out[1] = 1'b0;

endmodule
