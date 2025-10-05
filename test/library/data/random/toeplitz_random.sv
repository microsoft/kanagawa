//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

/*
Module: Testbench

    Testbench for data.random.toeplitz toeplitz_random class.

Authors:
    - Matthew Humphrey (mhumphr@microsoft.com)
*/

`timescale 1 ns / 1 ps

`ifndef QUESTA
    `default_nettype none
`endif

class Toeplitz #(parameter DataWidth, parameter HashWidth, parameter KeyWidth);
    local logic [KeyWidth-1:0] _key;

    function new(logic [KeyWidth-1:0] key);
        _key = key;
    endfunction

    function logic [HashWidth-1:0] calc_hash(logic [DataWidth-1:0] data);
        logic [KeyWidth-1:0] key = _key;
        logic [HashWidth-1:0] hash = '0;

        for (int i = DataWidth-1; i >= 0; --i) begin
            if (data[i]) begin
                hash = hash ^ key[KeyWidth-1 -: HashWidth];
            end
            key = {key[0 +: KeyWidth-1], key[KeyWidth-1]}; // Rotate left
        end

        return hash;
    endfunction
endclass

class ToeplitzRandom #(parameter DataWidth, parameter KeyWidth);
    local Toeplitz #(DataWidth, DataWidth, KeyWidth) _hash;
    local logic [DataWidth-1:0] _counter;

    function new (logic [KeyWidth-1:0] key);
        _hash = new(key);
    endfunction

    function logic [DataWidth-1:0] next(bit set_seed, logic [DataWidth-1:0] seed);
        logic [DataWidth-1:0] hash;

        if (set_seed) begin
            _counter = seed;
        end

        hash = _hash.calc_hash(_counter);
        _counter++;

        return hash;
    endfunction
endclass

module Testbench #(parameter NUM_TEST_VECTORS = 100);

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    logic rst_and_startup_done;

    logic stall_rate_supported;

    localparam DataWidth = 32;
    localparam Key = 256'h6D5A1BA6540E36AE7384C94779710E89BAB5778362E9B302C3A2CF202B5615A9;

    typedef logic [DataWidth-1:0] Data_t;

    typedef struct packed
    {
        logic set_seed;
        Data_t seed;
    } Req_t;

    logic nr_req_valid;
    Req_t nr_req;
    logic nr_req_rdy;

    logic  nr_rsp_rden;
    Data_t nr_rsp_result;
    logic  nr_rsp_empty;

    ToeplitzRandomWrapper dut
    (
        .clk                        (clk),
        .rst                        (rst),
        .rst_and_startup_done_out   (rst_and_startup_done),

        .stall_rate_supported_out   (stall_rate_supported),
        .stall_rate_valid_in        (1'b0),
        .stall_rate_in              ('x),

        .NextRandom_valid_in    (nr_req_valid),
        .NextRandom_set_seed_in (nr_req.set_seed),
        .NextRandom_seed_in     (nr_req.seed),
        .NextRandom_rdy_out     (nr_req_rdy),

        .NextRandom_rden_in     (nr_rsp_rden),
        .NextRandom_result_out  (nr_rsp_result),
        .NextRandom_empty_out   (nr_rsp_empty)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T          (Req_t)
    ) mb_nr_req
    (
        .clk        (clk),
        .rst        (rst),

        .valid_out  (nr_req_valid),
        .data_out   (nr_req),
        .ready_in   (nr_req_rdy)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T          (Data_t)
    ) mb_nr_rsp
    (
        .clk        (clk),
        .rst        (rst),

        .rdreq_out  (nr_rsp_rden),
        .empty_in   (nr_rsp_empty),
        .data_in    (nr_rsp_result)
    );

    clocking cb @(posedge clk);
        output out_rst = rst;
    endclocking

    ToeplitzRandom #(DataWidth, $bits(Key)) toeplitz_random_golden = new(Key);

    task send_input_and_check_results(input logic set_seed, input Data_t seed);
        Data_t expected;
        Data_t actual;
        bit timed_out;

        expected = toeplitz_random_golden.next(set_seed, seed);

        mb_nr_req.put('{set_seed:set_seed, seed:seed});
        mb_nr_rsp.get_with_timeout(100, actual, timed_out);
        assert(!timed_out) else $error("Timed out awaiting output on response interface");

        assert(actual === expected)
        else $error("Output did not match expected value\nEXPECTED: %x\nACTUAL  : %x", expected, actual);
    endtask

    initial begin

        cb.out_rst <= 1'b1;
        repeat (32) @(cb);
        cb.out_rst <= 1'b0;

        wait(rst_and_startup_done);

        send_input_and_check_results(1'b1, 1234);
        repeat (NUM_TEST_VECTORS) begin
            send_input_and_check_results(1'b0, 'x);
        end

        // Test wrap
        send_input_and_check_results(1'b1, '1);
        send_input_and_check_results(1'b0, 'x);
        send_input_and_check_results(1'b0, 'x);

        $finish;
    end

endmodule // ToeplitzTest_tb

`ifndef QUESTA
    `default_nettype wire
`endif
