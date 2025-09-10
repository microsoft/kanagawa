// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

`ifdef QUESTA
`default_nettype none
`define WT wire
`else
`define WT
`endif

/*
Module: KanagawaLogicRam_tb

    Testbench for KanagawaLogicRam.

Authors:
    - Matt Humphrey (mhumphr@microsoft.com)
*/

module KanagawaLogicRam_tb;

    localparam integer DEPTH = 217;
    localparam integer ADDR_WIDTH = $clog2(DEPTH);
    localparam integer DATA_WIDTH = 32;
    localparam integer NUM_READ_PORTS = 2;

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    logic [NUM_READ_PORTS-1:0][ADDR_WIDTH-1:0]   rdaddr;
    logic [NUM_READ_PORTS-1:0][DATA_WIDTH-1:0]   rddata;

    logic                    wren;
    logic [ADDR_WIDTH-1:0]   wraddr;
    logic [DATA_WIDTH-1:0]   wrdata;

    KanagawaLogicRam
    #(
        .DATA_WIDTH         (DATA_WIDTH),
        .ADDR_WIDTH         (ADDR_WIDTH),
        .DEPTH              (DEPTH),
        .NUM_READ_PORTS     (NUM_READ_PORTS)
    ) dut
    (
        .clk        (clk),
        .rst        (rst),

        // Multiple read ports
        .rdaddr_in  (rdaddr),
        .rddata_out (rddata),

        // Single write port
        .wren_in    (wren),
        .wraddr_in  (wraddr),
        .wrdata_in  (wrdata)
    );

    clocking cb @(posedge clk);
        input  rst_in = rst;
        output rst_out = rst;

        output #0 rdaddr;
        input #1step rddata;
        output wren, wraddr, wrdata;
    endclocking

    data_t expected_values[DEPTH];

    task automatic reader(input int port_num);
        addr_t rdaddr[DEPTH];

        for (int i = 0; i < DEPTH; ++i) begin
            rdaddr[i] = i;
        end
        rdaddr.shuffle();

        @(cb);
        for (int i = 0; i < DEPTH; ++i) begin
            cb.rdaddr[port_num] <= rdaddr[i];
            @(cb);
            assert(cb.rddata[port_num] == expected_values[rdaddr[i]])
            else $error("rddata mismatch on port %0d at address 0x%x:\nExpected: 0x%x\nActual  : 0x%x",
                port_num, rdaddr[i], expected_values[rdaddr[i]], cb.rddata[port_num]);
        end
    endtask

    task automatic fill_ram_with_random_values;
        data_t wrdata;
        addr_t wraddr[DEPTH];

        for (int i = 0; i < DEPTH; ++i) begin
            wraddr[i] = i;
        end
        wraddr.shuffle();

        for (int i = 0; i < DEPTH; ++i) begin
            wrdata = $urandom();
            cb.wren <= 1'b1;
            cb.wrdata <= wrdata;
            cb.wraddr <= wraddr[i];
            @(cb);
            cb.wren <= 1'b0;
            cb.wrdata <= 'x;
            cb.wraddr <= 'x;
            expected_values[wraddr[i]] = wrdata;
        end
    endtask

    task automatic readers;
        fork
            begin
                for (int i = 0; i < NUM_READ_PORTS; ++i) begin
                    fork
                        automatic int k = i;
                        begin
                            reader(k);
                        end
                    join_none
                end
                wait fork;
            end
        join
    endtask

    initial begin
        cb.wren <= 1'b0;
        cb.rst_out <= 1'b1;
        repeat (5) @(cb);
        cb.rst_out <= 1'b0;

        fill_ram_with_random_values();
        repeat (2) @(cb);
        readers();

        $finish;
    end

endmodule
