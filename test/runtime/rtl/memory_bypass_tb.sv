// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Randomized test
// Mostly test to test almost_empty handling
`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;

module KanagawaMemoryBypass_tb
#(
    parameter NUM_BYPASS_SLOTS = 2,
    parameter ADDRESS_EARLY = 0,
    parameter DATA_EARLY = 0
)
();
    localparam DATA_WIDTH = 32;
    localparam ADDR_WIDTH = 5;
    localparam ELEMENT_COUNT = 2**ADDR_WIDTH;

    bit clk;
    bit rst = 1'b1;

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDR_WIDTH-1:0] addr_t;

    addr_t read_addr;
    addr_t read_addr_prev;
    data_t read_data_in;
    data_t read_data_in_prev;
    data_t read_data_out;

    typedef struct packed
    {
        logic wren;
        addr_t addr;
        data_t data;
    } write_record_t;

    write_record_t write_record;

    // arrays that keep track of values of write port on previous cycles
    write_record_t write_history[NUM_BYPASS_SLOTS];

    always_ff @(posedge clk) begin
        for (int i = 0; i < NUM_BYPASS_SLOTS - 1; i++) begin
            write_history[i] <= write_history[i + 1];
        end

        write_history[NUM_BYPASS_SLOTS - 1] <= write_record;
    end

    // default is used to enable $past to find a clocking event
    default clocking cb @(posedge clk);
        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    // Random inputs
    always_ff @(posedge clk) begin
        read_addr_prev <= read_addr;
        read_data_in_prev <= read_data_in;

        read_addr <= $urandom();
        read_data_in <= $urandom();

        write_record.wren <= $urandom();
        write_record.addr <= $urandom();
        write_record.data <= $urandom();
    end

    initial begin
        data_t expected_output;
        data_t read_addr_to_compare;

        $display("Starting memory bypass test. NUM_BYPASS_SLOTS: %u", NUM_BYPASS_SLOTS);

        // wait for reset
        for (int i=0; i < 50; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        for (int i = 0; i < 10000; i++) begin
            read_addr_to_compare = ADDRESS_EARLY ? read_addr_prev : read_addr;
            expected_output = DATA_EARLY ? read_data_in_prev : read_data_in;

            // search through historical writes
            // prioritize recent writes over older ones
            for (int j = 0; j < NUM_BYPASS_SLOTS; j++) begin
                if (write_history[j].wren &&
                   (read_addr_to_compare == write_history[j].addr)) begin

                    expected_output = write_history[j].data;
                end
            end

            assert(read_data_out == expected_output) else $error("Incorrect result found");

            /*$display(
                "%p, %d, %d, %d, %d, %d, %p, %p, %d",
                write_record,
                read_addr,
                read_data_in,
                read_data_out,
                expected_output,
                read_addr,
                write_history[0],
                write_history[1],
                read_data_out == expected_output);*/

            @(cb);
        end

        $display("SUCCESS");
        $finish;
    end

    KanagawaMemoryBypass
    #(
        .DATA_WIDTH(DATA_WIDTH),
        .ADDR_WIDTH(ADDR_WIDTH),
        .NUM_BYPASS_SLOTS(NUM_BYPASS_SLOTS),
        .READ_ADDRESS_EARLY(ADDRESS_EARLY),
        .READ_DATA_EARLY(DATA_EARLY)
    )
    dut
    (
        .clk(clk),

        .read_addr_in(read_addr),
        .read_data_in(read_data_in),
        .read_data_out(read_data_out),

        .wren_in(write_record.wren),
        .write_addr_in(write_record.addr),
        .data_in(write_record.data)
    );
endmodule
