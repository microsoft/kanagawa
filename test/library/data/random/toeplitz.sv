//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

/*
Module: Testbench

    Testbench for data.random.toeplitz toeplitz class.

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

        //$display("|||> hash(%d'h%x) = %d'h%x", $bits(data), data, $bits(hash), hash);
        return hash;
    endfunction
endclass

module Testbench #(parameter NUM_TEST_VECTORS = 100);

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    logic rst_and_startup_done;

    logic stall_rate_supported;
    logic stall_rate_valid_in = 1'b0;
    logic [2:0] stall_rate_in = 0;

    localparam DataWidth = 242;
    localparam AlignedDataWidth = 128;
    localparam BigHashWidth = 512;
    localparam MediumHashWidth = 49;
    localparam SmallHashWidth = 16;
    localparam BigKeyWidth = 512;
    localparam SmallKeyWidth = 16;
    localparam MediumKeyWidth = 64;
    // Adding explicit size of the constant for numbers > 32 bits (Verilog requirement: IEEE 1800-2023 section 5.7.1)
    localparam logic [BigKeyWidth-1:0] BigKey = 512'h6D5A1BA6540E36AE7384C94779710E89BAB5778362E9B302C3A2CF202B5615A9DD5E8EF2E2EF40444F7C23BBB76A508BF48BE900D8A33DAE8829FB3C643771A6;
    localparam logic [SmallKeyWidth-1:0] SmallKey = 'h6d5a;
    // Adding explicit size of the constant for numbers > 32 bits (Verilog requirement: IEEE 1800-2023 section 5.7.1)
    localparam logic [MediumKeyWidth-1:0] MediumKey = 64'h6D5A1BA691A843CF;

    typedef logic [DataWidth-1:0]       Data_t;
    typedef logic [AlignedDataWidth-1:0] AlignedData_t;
    typedef logic [BigHashWidth-1:0]    BigHash_t;
    typedef logic [MediumHashWidth-1:0] MediumHash_t;
    typedef logic [SmallHashWidth-1:0]  SmallHash_t;

    logic           bhbkmc_req_valid;
    Data_t          bhbkmc_req_data;
    logic           bhbkmc_req_ready;

    logic           bhbkmc_rsp_rden;
    BigHash_t       bhbkmc_rsp_data;
    logic           bhbkmc_rsp_empty;

    logic           mhmkmc_req_valid;
    Data_t          mhmkmc_req_data;
    logic           mhmkmc_req_ready;

    logic           mhmkmc_rsp_rden;
    MediumHash_t    mhmkmc_rsp_data;
    logic           mhmkmc_rsp_empty;

    logic           shskmc_req_valid;
    Data_t          shskmc_req_data;
    logic           shskmc_req_ready;

    logic           shskmc_rsp_rden;
    SmallHash_t     shskmc_rsp_data;
    logic           shskmc_rsp_empty;

    logic           shskmc_aligned_req_valid;
    AlignedData_t   shskmc_aligned_req_data;
    logic           shskmc_aligned_req_ready;

    logic           shskmc_aligned_rsp_rden;
    SmallHash_t     shskmc_aligned_rsp_data;
    logic           shskmc_aligned_rsp_empty;

    logic           shskoc_req_valid;
    Data_t          shskoc_req_data;
    logic           shskoc_req_ready;

    logic           shskoc_rsp_rden;
    SmallHash_t     shskoc_rsp_data;
    logic           shskoc_rsp_empty;

    ToeplitzWrapper dut
    (
        .clk                                        (clk),
        .rst                                        (rst),
        .rst_and_startup_done_out                   (rst_and_startup_done),

        .stall_rate_supported_out                   (stall_rate_supported),
        .stall_rate_valid_in                        (stall_rate_valid_in),
        .stall_rate_in                              (stall_rate_in),

        .BigHashBigKeyMultiCycles_valid_in          (bhbkmc_req_valid),
        .BigHashBigKeyMultiCycles_data_in           (bhbkmc_req_data),
        .BigHashBigKeyMultiCycles_rdy_out           (bhbkmc_req_ready),

        .BigHashBigKeyMultiCycles_rden_in           (bhbkmc_rsp_rden),
        .BigHashBigKeyMultiCycles_result_out        (bhbkmc_rsp_data),
        .BigHashBigKeyMultiCycles_empty_out         (bhbkmc_rsp_empty),

        .MediumHashMediumKeyMultiCycles_valid_in    (mhmkmc_req_valid),
        .MediumHashMediumKeyMultiCycles_data_in     (mhmkmc_req_data),
        .MediumHashMediumKeyMultiCycles_rdy_out     (mhmkmc_req_ready),

        .MediumHashMediumKeyMultiCycles_rden_in     (mhmkmc_rsp_rden),
        .MediumHashMediumKeyMultiCycles_result_out  (mhmkmc_rsp_data),
        .MediumHashMediumKeyMultiCycles_empty_out   (mhmkmc_rsp_empty),

        .SmallHashSmallKeyMultiCycles_valid_in      (shskmc_req_valid),
        .SmallHashSmallKeyMultiCycles_data_in       (shskmc_req_data),
        .SmallHashSmallKeyMultiCycles_rdy_out       (shskmc_req_ready),

        .SmallHashSmallKeyMultiCycles_rden_in       (shskmc_rsp_rden),
        .SmallHashSmallKeyMultiCycles_result_out    (shskmc_rsp_data),
        .SmallHashSmallKeyMultiCycles_empty_out     (shskmc_rsp_empty),

        .SmallHashSmallKeyMultiCyclesAligned_valid_in   (shskmc_aligned_req_valid),
        .SmallHashSmallKeyMultiCyclesAligned_data_in    (shskmc_aligned_req_data),
        .SmallHashSmallKeyMultiCyclesAligned_rdy_out    (shskmc_aligned_req_ready),

        .SmallHashSmallKeyMultiCyclesAligned_rden_in    (shskmc_aligned_rsp_rden),
        .SmallHashSmallKeyMultiCyclesAligned_result_out (shskmc_aligned_rsp_data),
        .SmallHashSmallKeyMultiCyclesAligned_empty_out  (shskmc_aligned_rsp_empty),

        .SmallHashSmallKeyOneCycle_valid_in         (shskoc_req_valid),
        .SmallHashSmallKeyOneCycle_data_in          (shskoc_req_data),
        .SmallHashSmallKeyOneCycle_rdy_out          (shskoc_req_ready),

        .SmallHashSmallKeyOneCycle_rden_in          (shskoc_rsp_rden),
        .SmallHashSmallKeyOneCycle_result_out       (shskoc_rsp_data),
        .SmallHashSmallKeyOneCycle_empty_out        (shskoc_rsp_empty)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T          (Data_t)
    ) mb_bhbkmc_req
    (
        .clk        (clk),
        .rst        (rst),

        .valid_out  (bhbkmc_req_valid),
        .data_out   (bhbkmc_req_data),
        .ready_in   (bhbkmc_req_ready)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T          (BigHash_t)
    ) mb_bhbkmc_rsp
    (
        .clk        (clk),
        .rst        (rst),

        .rdreq_out  (bhbkmc_rsp_rden),
        .empty_in   (bhbkmc_rsp_empty),
        .data_in    (bhbkmc_rsp_data)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T          (Data_t)
    ) mb_mhmkmc_req
    (
        .clk        (clk),
        .rst        (rst),

        .valid_out  (mhmkmc_req_valid),
        .data_out   (mhmkmc_req_data),
        .ready_in   (mhmkmc_req_ready)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T          (MediumHash_t)
    ) mb_mhmkmc_rsp
    (
        .clk        (clk),
        .rst        (rst),

        .rdreq_out  (mhmkmc_rsp_rden),
        .empty_in   (mhmkmc_rsp_empty),
        .data_in    (mhmkmc_rsp_data)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T          (Data_t)
    ) mb_shskmc_req
    (
        .clk        (clk),
        .rst        (rst),

        .valid_out  (shskmc_req_valid),
        .data_out   (shskmc_req_data),
        .ready_in   (shskmc_req_ready)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T          (SmallHash_t)
    ) mb_shskmc_rsp
    (
        .clk        (clk),
        .rst        (rst),

        .rdreq_out  (shskmc_rsp_rden),
        .empty_in   (shskmc_rsp_empty),
        .data_in    (shskmc_rsp_data)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T          (AlignedData_t)
    ) mb_shskmc_aligned_req
    (
        .clk        (clk),
        .rst        (rst),

        .valid_out  (shskmc_aligned_req_valid),
        .data_out   (shskmc_aligned_req_data),
        .ready_in   (shskmc_aligned_req_ready)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T          (SmallHash_t)
    ) mb_shskmc_aligned_rsp
    (
        .clk        (clk),
        .rst        (rst),

        .rdreq_out  (shskmc_aligned_rsp_rden),
        .empty_in   (shskmc_aligned_rsp_empty),
        .data_in    (shskmc_aligned_rsp_data)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T          (Data_t)
    ) mb_shskoc_req
    (
        .clk        (clk),
        .rst        (rst),

        .valid_out  (shskoc_req_valid),
        .data_out   (shskoc_req_data),
        .ready_in   (shskoc_req_ready)
    );

    KanagawaSimFifoReadToMailbox
    #(
        .T          (SmallHash_t)
    ) mb_shskoc_rsp
    (
        .clk        (clk),
        .rst        (rst),

        .rdreq_out  (shskoc_rsp_rden),
        .empty_in   (shskoc_rsp_empty),
        .data_in    (shskoc_rsp_data)
    );

    clocking cb @(posedge clk);
        output out_rst = rst;
    endclocking

    Toeplitz #(DataWidth, BigHashWidth, BigKeyWidth) bhbk_golden_toeplitz = new(BigKey);

    task send_input_and_check_results_bhbkmc(input Data_t data);
        BigHash_t actual;
        bit timed_out;
        BigHash_t expected;

        expected = bhbk_golden_toeplitz.calc_hash(data);

        mb_bhbkmc_req.put(data);
        mb_bhbkmc_rsp.get_with_timeout(100, actual, timed_out);
        assert(!timed_out) else $error("BHBKMC: Timed out awaiting output from BHBKMC");

        assert(actual === expected)
        else $error("BHBKMC: Output did not match expected value\nDATA    : %x\nEXPECTED: %x\nACTUAL  : %x", data, expected, actual);
    endtask

    class DataClass;
        rand Data_t data;

        function new();
            data = {1'b1, {(DataWidth-1){1'b0}}};
        endfunction
    endclass

    task automatic test_bhbkmc;
        DataClass data_obj = new();

        $display("Testing Big Hash, Big Key");

        for (int i = 0; i <= DataWidth; ++i) begin
            send_input_and_check_results_bhbkmc(data_obj.data);
            data_obj.data >>= 1;
        end

        repeat (NUM_TEST_VECTORS) begin
            assert(data_obj.randomize());
            send_input_and_check_results_bhbkmc(data_obj.data);
        end
    endtask

    Toeplitz #(DataWidth, MediumHashWidth, MediumKeyWidth) mhmk_golden_toeplitz = new(MediumKey);

    task send_input_and_check_results_mhmkmc(input Data_t data);
        MediumHash_t actual;
        bit timed_out;
        MediumHash_t expected;

        expected = mhmk_golden_toeplitz.calc_hash(data);

        mb_mhmkmc_req.put(data);
        mb_mhmkmc_rsp.get_with_timeout(100, actual, timed_out);
        assert(!timed_out) else $error("MHMKMC: Timed out awaiting output from MHMKMC");

        assert(actual === expected)
        else $error("MHMKMC: Output did not match expected value\nDATA    : %x\nEXPECTED: %x\nACTUAL  : %x", data, expected, actual);
    endtask

    task automatic test_mhmkmc;
        DataClass data_obj = new();

        $display("Testing Medium Hash, Medium Key");

        for (int i = 0; i <= DataWidth; ++i) begin
            send_input_and_check_results_mhmkmc(data_obj.data);
            data_obj.data >>= 1;
        end

        repeat (NUM_TEST_VECTORS) begin
            assert(data_obj.randomize());
            send_input_and_check_results_mhmkmc(data_obj.data);
        end
    endtask

    Toeplitz #(DataWidth, SmallHashWidth, SmallKeyWidth) shsk_golden_toeplitz = new(SmallKey);

    task send_input_and_check_results_shskmc(input Data_t data);
        SmallHash_t actual;
        bit timed_out;
        SmallHash_t expected;

        expected = shsk_golden_toeplitz.calc_hash(data);

        mb_shskmc_req.put(data);
        mb_shskmc_rsp.get_with_timeout(100, actual, timed_out);
        assert(!timed_out) else $error("SHSKMC: Timed out awaiting output from SHSKMC");

        assert(actual === expected)
        else $error("SHSKMC: Output did not match expected value\nDATA    : %x\nEXPECTED: %x\nACTUAL  : %x", data, expected, actual);
    endtask

    task automatic test_shskmc;
        DataClass data_obj = new();

        $display("Testing Small Hash, Small Key");

        for (int i = 0; i <= 0; ++i) begin
            send_input_and_check_results_shskmc(data_obj.data);
            data_obj.data >>= 1;
        end

        repeat (NUM_TEST_VECTORS) begin
            assert(data_obj.randomize());
            send_input_and_check_results_shskmc(data_obj.data);
        end
    endtask

    Toeplitz #(AlignedDataWidth, SmallHashWidth, SmallKeyWidth) shsk_aligned_golden_toeplitz = new(SmallKey);

    task send_input_and_check_results_shskmc_aligned(input AlignedData_t data);
        SmallHash_t actual;
        bit timed_out;
        SmallHash_t expected;

        expected = shsk_aligned_golden_toeplitz.calc_hash(data);

        mb_shskmc_aligned_req.put(data);
        mb_shskmc_aligned_rsp.get_with_timeout(100, actual, timed_out);
        assert(!timed_out) else $error("SHSKMC_ALIGNED: Timed out awaiting output from SHSKMC_ALIGNED");

        assert(actual === expected)
        else $error("SHSKMC_ALIGNED: Output did not match expected value\nDATA    : %x\nEXPECTED: %x\nACTUAL  : %x", data, expected, actual);
    endtask

    task automatic test_shskmc_aligned;
        DataClass data_obj = new();

        $display("Testing Small Hash, Small Key, Aligned Data");

        for (int i = 0; i <= 0; ++i) begin
            send_input_and_check_results_shskmc_aligned(data_obj.data);
            data_obj.data >>= 1;
        end

        repeat (NUM_TEST_VECTORS) begin
            assert(data_obj.randomize());
            send_input_and_check_results_shskmc_aligned(data_obj.data);
        end
    endtask

    task send_input_and_check_results_shskoc(input Data_t data);
        SmallHash_t actual;
        bit timed_out;
        SmallHash_t expected;

        expected = shsk_golden_toeplitz.calc_hash(data);

        mb_shskoc_req.put(data);
        mb_shskoc_rsp.get_with_timeout(100, actual, timed_out);
        assert(!timed_out) else $error("SHSKOC: Timed out awaiting output from SHSKOC");

        assert(actual === expected)
        else $error("SHSKOC: Output did not match expected value\nDATA    : %x\nEXPECTED: %x\nACTUAL  : %x", data, expected, actual);
    endtask

    task automatic test_shskoc;
        DataClass data_obj = new();

        $display("Testing Small Hash, Small Key, Single Pipeline Cycle");

        for (int i = 0; i <= 0; ++i) begin
            send_input_and_check_results_shskoc(data_obj.data);
            data_obj.data >>= 1;
        end

        repeat (NUM_TEST_VECTORS) begin
            assert(data_obj.randomize());
            send_input_and_check_results_shskoc(data_obj.data);
        end
    endtask

    initial begin

        cb.out_rst <= 1'b1;
        repeat (32) @(cb);
        cb.out_rst <= 1'b0;

        for (int i = 0; i <= 1; ++i) begin

            wait(rst_and_startup_done);

            test_bhbkmc();
            test_mhmkmc();
            test_shskmc();
            test_shskmc_aligned();
            test_shskoc();

            // Iff --stall was used try again at --stall=0
            if (stall_rate_supported === 1'b0) begin
                i++;
            end
            else begin
                if (i == 0) begin
                    $display("Re-testing with stall==0");
                end
                cb.out_rst <= 1'b1;
                repeat (32) @(cb);
                cb.out_rst <= 1'b0;

                wait(rst_and_startup_done);
                // Set the new rate and try again
                stall_rate_in       = 0;
                stall_rate_valid_in = 1'b1;
                @(posedge clk);
                stall_rate_valid_in = 1'b0;
            end
        end

        $finish;
    end

endmodule // ToeplitzTest_tb

`ifndef QUESTA
    `default_nettype wire
`endif
