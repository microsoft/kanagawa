// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaHALDSP_mock_umul27_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic [26:0] x;
        logic [26:0] y;
    } op_input_t;

    typedef logic [53:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__umul27
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk            (clk),
        .rst            (rst),

        .op_x_in        (op_inputs.x),
        .op_y_in        (op_inputs.y),
        .op_result_out  (op_result),
        .op_valid_in    (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;

        bit [26:0] test_vectors_x[] = '{
            27'h0000005, // Small numbers
            27'h0000000, // One zero
            27'h0000040, // Powers of 2
            27'h7FFFFFF, // Large numbers
            27'h7FFFFFF, // Max values
            27'h0000001, // One is 1
            27'h0003039, // Medium numbers
            27'h5555555 // Alternating bits
        };

        bit [26:0] test_vectors_y[] = '{
            27'h0000007, // 7
            27'h0003039, // 12345
            27'h0000080, // 128
            27'h0001000, // 4096
            27'h7FFFFFF, // 134217727
            27'h3456789, // 54880137
            27'h0010932, // 67890
            27'h2AAAAAA // 44739242
        };

        bit [53:0] test_vectors_result[] = '{
            54'h00000000000023, // 5 * 7 = 35
            54'h00000000000000, // 0 * 12345 = 0
            54'h00000000002000, // 64 * 128 = 8192
            54'h00007FFFFFF000, // 134217727 * 4096 = 549755809792
            54'h3FFFFFF0000001, // 134217727 * 134217727 = 18014398241046529
            54'h00000003456789, // 1 * 54880137 = 54880137
            54'h00000031F46C22, // 12345 * 67890 = 838102050
            54'h0E38E389C71C72 // 89478485 * 44739242 = 4003199594208370
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors_x); ++i) begin
            mb_inputs.put('{x: test_vectors_x[i], y: test_vectors_y[i]});
        end

        for (int unsigned i = 0; i < $size(test_vectors_x); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, expected result = 0x%h, actual result = 0x%h", i, test_vectors_x[i], test_vectors_y[i], test_vectors_result[i], result);

            assert(result === test_vectors_result[i])
            else $error("%m: Mismatch on result %0d: Expected 0x%x. Actual 0x%x", i, test_vectors_result[i], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule

module KanagawaHALDSP_mock_imul27_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic negate;
        logic [26:0] x;
        logic [26:0] y;
    } op_input_t;

    typedef logic [53:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__imul27
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk            (clk),
        .rst            (rst),

        .op_x_in        (op_inputs.x),
        .op_y_in        (op_inputs.y),
        .op_negate_in   (op_inputs.negate),
        .op_result_out  (op_result),
        .op_valid_in    (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;
        logic sint_mult_test_vectors_negate[];
        logic signed [53:0] sint_mult_test_vectors_result[];

        bit signed [26:0] test_vectors_x[] =  '{
            27'h000007B, // Positive no negate (123)
            27'h0000064, // Positive with negate (100)
            27'h7FFFFCE, // Negative * positive (-50)
            27'h7FFFFE2, // Negative * positive negated (-30)
            27'h7FFFFE7, // Both negative (-25)
            27'h7FFFFF1, // Both negative negated (-15)
            27'h0000000, // Zero multiply (0)
            27'h3FFFFFF, // Max positive (67108863)
            27'h4000000, // Max negative (-67108864)
            27'h0000001 // One multiply (1)
        };

        bit signed [26:0] test_vectors_y[] = '{
            27'h00001C8, // 456
            27'h00000C8, // 200
            27'h0000050, // 80
            27'h0000028, // 40
            27'h7FFFFC4, // -60
            27'h7FFFFEC, // -20
            27'h7FFCFC7, // -12345
            27'h0000002, // 2
            27'h0000002, // 2
            27'h7F0EDFA // -987654
        };

        bit test_vectors_negate[] = '{
            1'b0, // no negate
            1'b1, // negate
            1'b0, // no negate
            1'b1, // negate
            1'b0, // no negate
            1'b1, // negate
            1'b0, // no negate
            1'b0, // no negate
            1'b1, // negate
            1'b0 // no negate
        };

        bit signed [53:0] test_vectors_result[] = '{
            54'h0000000000DB18, // 123 * 456 = 56088
            54'h3FFFFFFFFFB1E0, // 100 * 200 (negated) = -20000
            54'h3FFFFFFFFFF060, // -50 * 80 = -4000
            54'h000000000004B0, // -30 * 40 (negated) = 1200
            54'h000000000005DC, // -25 * -60 = 1500
            54'h3FFFFFFFFFFED4, // -15 * -20 (negated) = -300
            54'h00000000000000, // 0 * -12345 = 0
            54'h00000007FFFFFE, // 67108863 * 2 = 134217726
            54'h00000008000000, // -67108864 * 2 (negated) = 134217728
            54'h3FFFFFFFF0EDFA // 1 * -987654 = -987654
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors_x); ++i) begin
            mb_inputs.put('{x: test_vectors_x[i], y: test_vectors_y[i], negate: test_vectors_negate[i]});
        end

        for (int unsigned i = 0; i < $size(test_vectors_x); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, expected result = 0x%h, actual result = 0x%h", i, test_vectors_x[i], test_vectors_y[i], test_vectors_result[i], result);

            assert(result === test_vectors_result[i])
            else $error("%m: Mismatch on result %0d: Expected 0x%x. Actual 0x%x", i, test_vectors_result[i], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule

module KanagawaHALDSP_mock_fmul32_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic [31:0] x;
        logic [31:0] y;
    } op_input_t;

    typedef logic [31:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__fmul32
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk            (clk),
        .rst            (rst),

        .op_x_in        (op_inputs.x),
        .op_y_in        (op_inputs.y),
        .op_result_out  (op_result),
        .op_valid_in    (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;

        // Format: {op_x, op_y, expected_result}
        logic [31:0] test_vectors[][3] = '{
            '{32'h40200000, 32'h40800000, 32'h41200000}, // 2.5 x 4 = 10
            '{32'h40490FD0, 32'hC02DF84D, 32'hC108A2B3}, // 3.14159 x -2.71828 = -8.539721
            '{32'h0DC79433, 32'h383F42A1, 32'h06951B84}, // 1.230000e-30 x 4.560000e-05 = 5.608800e-35
            '{32'h60D55EF9, 32'h59E03F4E, 32'h7B3AE7DD}, // 1.230000e+20 x 7.890000e+15 = 9.704700e+35
            '{32'h3F800000, 32'hC228C7AE, 32'hC228C7AE}, // 1 x -42.195 = -42.195
            '{32'h00000000, 32'h42F6E979, 32'h00000000}, // 0 x 123.456001 = 0
            '{32'h80000000, 32'h40A00000, 32'h80000000}, // 0 x 5 = 0
            '{32'h41000000, 32'h3E000000, 32'h3F800000}, // 8 x 0.125 = 1
            '{32'hC0C80000, 32'hBFCCCCCD, 32'h41200000}, // -6.25 x -1.6 = 10
            '{32'h7E967699, 32'h3FC00000, 32'h7EE1B1E6}, // 1.000000e+38 x 1.5 = 1.500000e+38
            '{32'h20CA78B2, 32'h1F21FA28, 32'h00801BC1}, // 3.430000e-19 x 3.430000e-20 = 1.176490e-38
            '{32'h3EAAAAAA, 32'h3F2AAAAB, 32'h3E638E38}, // 0.333333 x 0.666667 = 0.222222
            '{32'h40F00000, 32'h00000000, 32'h00000000}, // 7.5 x 0 = 0
            '{32'hEAA34913, 32'h503748C7, 32'hFB69CF50}, // -9.870000e+25 x 1.230000e+10 = -1.214010e+36
            '{32'h1F6C1E4A, 32'h58E35FA9, 32'h38D1B717}, // 5.000000e-20 x 2.000000e+15 = 1.000000e-04
            '{32'h3FB504EE, 32'h3FB504EE, 32'h3FFFFFF1}, // 1.414213 x 1.414213 = 1.999998
            '{32'h1FEC1E4A, 32'h200DABC6, 32'h0082AB1E}, // 1.000000e-19 x 1.200000e-19 = 1.200000e-38
            '{32'h3F9E0652, 32'h411E0652, 32'h4143179B}, // 1.234568 x 9.876543 = 12.193263
            '{32'h7F7FFFFF, 32'h3F7D70A4, 32'h7F7D70A3}, // 3.402823e+38 x 0.99 = 3.368795e+38
            '{32'h41800000, 32'h3D800000, 32'h3F800000} // 16 x 0.0625 = 1
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_inputs.put('{x: test_vectors[i][0], y: test_vectors[i][1]});
        end

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, expected result = 0x%h, actual result = 0x%h", i, test_vectors[i][0], test_vectors[i][1], test_vectors[i][2], result);

            assert(result === test_vectors[i][2])
            else $error("Incorrect output for test vector %0d. Expected 32'h%x, but got 32'h%x", i, test_vectors[i][2], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule

module KanagawaHALDSP_mock_fadd32_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic [31:0] x;
        logic [31:0] y;
    } op_input_t;

    typedef logic [31:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__fadd32
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk            (clk),
        .rst            (rst),

        .op_x_in        (op_inputs.x),
        .op_y_in        (op_inputs.y),
        .op_result_out  (op_result),
        .op_valid_in    (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;

        // Format: {op_x, op_y, expected_result}
        logic [31:0] test_vectors[][3] = '{
            '{32'h40200000, 32'h40800000, 32'h40D00000}, // 2.5 x 4 = 6.5
            '{32'h40490FD0, 32'hC02DF84D, 32'h3ED8BC18}, // 3.14159 x -2.71828 = 0.42331
            '{32'h0DC79433, 32'h383F42A1, 32'h383F42A1}, // 1.230000e-30 x 4.560000e-05 = 4.560000e-05
            '{32'h60D55EF9, 32'h59E03F4E, 32'h60D5627A}, // 1.230000e+20 x 7.890000e+15 = 1.230079e+20
            '{32'h3F800000, 32'hC228C7AE, 32'hC224C7AE}, // 1 x -42.195 = -41.195
            '{32'h00000000, 32'h42F6E979, 32'h42F6E979}, // 0 x 123.456001 = 123.456001
            '{32'h80000000, 32'h40A00000, 32'h40A00000}, // 0 x 5 = 5
            '{32'h41000000, 32'h3E000000, 32'h41020000}, // 8 x 0.125 = 8.125
            '{32'hC0C80000, 32'hBFCCCCCD, 32'hC0FB3333}, // -6.25 x -1.6 = -7.85
            '{32'h7E967699, 32'h3FC00000, 32'h7E967699}, // 1.000000e+38 x 1.5 = 1.000000e+38
            '{32'h20CA78B2, 32'h1F21FA28, 32'h20DEB7F7}, // 3.430000e-19 x 3.430000e-20 = 3.773000e-19
            '{32'h3EAAAAAA, 32'h3F2AAAAB, 32'h3F800000}, // 0.333333 x 0.666667 = 1
            '{32'h40F00000, 32'h00000000, 32'h40F00000}, // 7.5 x 0 = 7.5
            '{32'hEAA34913, 32'h503748C7, 32'hEAA34913}, // -9.870000e+25 x 1.230000e+10 = -9.870000e+25
            '{32'h1F6C1E4A, 32'h58E35FA9, 32'h58E35FA9}, // 5.000000e-20 x 2.000000e+15 = 2.000000e+15
            '{32'h3FB504EE, 32'h3FB504EE, 32'h403504EE}, // 1.414213 x 1.414213 = 2.828426
            '{32'h1FEC1E4A, 32'h200DABC6, 32'h2081DD76}, // 1.000000e-19 x 1.200000e-19 = 2.200000e-19
            '{32'h3F9E0652, 32'h411E0652, 32'h4131C71C}, // 1.234568 x 9.876543 = 11.111111
            '{32'h7F7FFFFF, 32'h3F7D70A4, 32'h7F7FFFFF}, // 3.402823e+38 x 0.99 = 3.402823e+38
            '{32'h41800000, 32'h3D800000, 32'h41808000} // 16 x 0.0625 = 16.0625
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_inputs.put('{x: test_vectors[i][0], y: test_vectors[i][1]});
        end

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, expected result = 0x%h, actual result = 0x%h", i, test_vectors[i][0], test_vectors[i][1], test_vectors[i][2], result);

            assert(result === test_vectors[i][2])
            else $error("Incorrect output for test vector %0d. Expected 32'h%x, but got 32'h%x", i, test_vectors[i][2], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule

module KanagawaHALDSP_mock_fsub32_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic [31:0] x;
        logic [31:0] y;
    } op_input_t;

    typedef logic [31:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__fsub32
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk            (clk),
        .rst            (rst),

        .op_x_in        (op_inputs.x),
        .op_y_in        (op_inputs.y),
        .op_result_out  (op_result),
        .op_valid_in    (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;

        // Format: {op_x, op_y, expected_result}
        logic [31:0] test_vectors[][3] = '{
            '{32'h40200000, 32'h40800000, 32'hBFC00000}, // 2.5 x 4 = -1.5
            '{32'h40490FD0, 32'hC02DF84D, 32'h40BB840E}, // 3.14159 x -2.71828 = 5.85987
            '{32'h0DC79433, 32'h383F42A1, 32'hB83F42A1}, // 1.230000e-30 x 4.560000e-05 = -4.560000e-05
            '{32'h60D55EF9, 32'h59E03F4E, 32'h60D55B78}, // 1.230000e+20 x 7.890000e+15 = 1.229921e+20
            '{32'h3F800000, 32'hC228C7AE, 32'h422CC7AE}, // 1 x -42.195 = 43.195
            '{32'h00000000, 32'h42F6E979, 32'hC2F6E979}, // 0 x 123.456001 = -123.456001
            '{32'h80000000, 32'h40A00000, 32'hC0A00000}, // 0 x 5 = -5
            '{32'h41000000, 32'h3E000000, 32'h40FC0000}, // 8 x 0.125 = 7.875
            '{32'hC0C80000, 32'hBFCCCCCD, 32'hC094CCCD}, // -6.25 x -1.6 = -4.65
            '{32'h7E967699, 32'h3FC00000, 32'h7E967699}, // 1.000000e+38 x 1.5 = 1.000000e+38
            '{32'h20CA78B2, 32'h1F21FA28, 32'h20B6396D}, // 3.430000e-19 x 3.430000e-20 = 3.087000e-19
            '{32'h3EAAAAAA, 32'h3F2AAAAB, 32'hBEAAAAAC}, // 0.333333 x 0.666667 = -0.333333
            '{32'h40F00000, 32'h00000000, 32'h40F00000}, // 7.5 x 0 = 7.5
            '{32'hEAA34913, 32'h503748C7, 32'hEAA34913}, // -9.870000e+25 x 1.230000e+10 = -9.870000e+25
            '{32'h1F6C1E4A, 32'h58E35FA9, 32'hD8E35FA9}, // 5.000000e-20 x 2.000000e+15 = -2.000000e+15
            '{32'h3FB504EE, 32'h3FB504EE, 32'h00000000}, // 1.414213 x 1.414213 = 0
            '{32'h1FEC1E4A, 32'h200DABC6, 32'h9EBCE508}, // 1.000000e-19 x 1.200000e-19 = -2.000000e-20
            '{32'h3F9E0652, 32'h411E0652, 32'hC10A4588}, // 1.234568 x 9.876543 = -8.641975
            '{32'h7F7FFFFF, 32'h3F7D70A4, 32'h7F7FFFFF}, // 3.402823e+38 x 0.99 = 3.402823e+38
            '{32'h41800000, 32'h3D800000, 32'h417F0000} // 16 x 0.0625 = 15.9375
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_inputs.put('{x: test_vectors[i][0], y: test_vectors[i][1]});
        end

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, expected result = 0x%h, actual result = 0x%h", i, test_vectors[i][0], test_vectors[i][1], test_vectors[i][2], result);

            assert(result === test_vectors[i][2])
            else $error("Incorrect output for test vector %0d. Expected 32'h%x, but got 32'h%x", i, test_vectors[i][2], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule

module KanagawaHALDSP_mock_fmac32_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic accumulate;
        logic [31:0] x;
        logic [31:0] y;
    } op_input_t;

    typedef logic [31:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__fmac32
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk                (clk),
        .rst                (rst),

        .op_x_in            (op_inputs.x),
        .op_y_in            (op_inputs.y),
        .op_accumulate_in   (op_inputs.accumulate),
        .op_result_out      (op_result),
        .op_valid_in        (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;

        // Format: {op_x, op_y, accumulate, expected_result}
        logic [31:0] test_vectors[][4] = '{
            '{32'h40000000, 32'h40400000, 1'b0, 32'h40C00000}, // 2 x 3 -> acc = 6
            '{32'h3FC00000, 32'h40000000, 1'b1, 32'h41100000}, // 1.5 x 2 + acc = 9
            '{32'h40800000, 32'h40200000, 1'b0, 32'h41200000}, // 4 x 2.5 -> acc = 10
            '{32'hC0000000, 32'h3F800000, 1'b1, 32'h41000000}, // -2 x 1 + acc = 8
            '{32'h00000000, 32'h40A00000, 1'b1, 32'h41000000}, // 0 x 5 + acc = 8
            '{32'h00000000, 32'h00000000, 1'b0, 32'h00000000}, // 0 x 0 -> acc = 0
            '{32'h2EDBE6FF, 32'h2F5BE6FF, 1'b1, 32'h1EBCE509}, // 1.000000e-10 x 2.000000e-10 + acc = 2.000000e-20
            '{32'h60AD78EC, 32'h58E35FA9, 1'b1, 32'h7A1A130C}, // 1.000000e+20 x 2.000000e+15 + acc = 2.000000e+35
            '{32'h3E800000, 32'h3F400000, 1'b0, 32'h3E400000}, // 0.25 x 0.75 -> acc = 0.1875
            '{32'h41000000, 32'h3E000000, 1'b1, 32'h3F980000}  // 8 x 0.125 + acc = 1.1875
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_inputs.put('{x: test_vectors[i][0], y: test_vectors[i][1], accumulate: test_vectors[i][2]});
        end

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, accumulate = %x, expected result = 0x%h, actual result = 0x%h", i, test_vectors[i][0], test_vectors[i][1], test_vectors[i][2], test_vectors[i][3], result);

            assert(result === test_vectors[i][3])
            else $error("Incorrect output for test vector %0d. Expected 32'h%x, but got 32'h%x", i, test_vectors[i][3], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule


module KanagawaHALDSP_mock_fmad32_tb;

    localparam int LATENCY = 3;

    logic clk = 1'b0;
    always #5 clk = ~clk;

    logic rst = 1'b1;

    typedef struct packed {
        logic [31:0] x;
        logic [31:0] y;
        logic [31:0] z;
    } op_input_t;

    typedef logic [31:0] op_output_t;

    op_input_t op_inputs;
    op_output_t op_result;
    logic op_valid;
    logic output_valid;

    clocking cb @(posedge clk);
        input #0 rst_in = rst;
        output rst_out = rst;
    endclocking

    _hardware_dsp__fmad32
    #(
        .DEVICE_FAMILY  ("UNUSED"),
        .LATENCY        (LATENCY)
    ) dut
    (
        .clk                (clk),
        .rst                (rst),

        .op_x_in            (op_inputs.x),
        .op_y_in            (op_inputs.y),
        .op_z_in            (op_inputs.z),
        .op_result_out      (op_result),
        .op_valid_in        (op_valid)
    );

    KanagawaSimMailboxToValid
    #(
        .T      (op_input_t)
    ) mb_inputs
    (
        .clk            (clk),
        .rst            (rst),

        .valid_out      (op_valid),
        .data_out       (op_inputs)
    );

    KanagawaFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (LATENCY)
    ) delay_output_valid
    (
        .clk            (clk),
        .clr            (rst),
        .data_in        (op_valid),
        .data_out       (output_valid)
    );

    KanagawaSimValidToMailbox
    #(
        .T          (op_output_t)
    ) mb_output
    (
        .clk            (clk),
        .rst            (rst),

        .valid_in       (output_valid),
        .data_in        (op_result)
    );

    task automatic run_test;
        op_output_t result;
        bit timed_out;

        // Format: {op_x, op_y, op_z, expected_result}
        logic [31:0] test_vectors[][4] = '{
            '{32'h3F800000, 32'h40000000, 32'h40400000, 32'h40E00000}, // 1 + 2 x 3 = 7
            '{32'h00000000, 32'h40800000, 32'h40A00000, 32'h41A00000}, // 0 + 4 x 5 = 20
            '{32'h41200000, 32'h00000000, 32'h40E00000, 32'h41200000}, // 10 + 0 x 7 = 10
            '{32'hC0A00000, 32'h40000000, 32'h40800000, 32'h40400000}, // -5 + 2 x 4 = 3
            '{32'h41000000, 32'hC0400000, 32'h40000000, 32'h40000000}, // 8 + -3 x 2 = 2
            '{32'hBF800000, 32'hC0000000, 32'hC0400000, 32'h40A00000}, // -1 + -2 x -3 = 5
            '{32'h3F000000, 32'h3E800000, 32'h3F400000, 32'h3F300000}, // 0.5 + 0.25 x 0.75 = 0.6875
            '{32'h501502F9, 32'h58635FA9, 32'h509502F9, 32'h69845951}, // 1.000000e+10 + 1.000000e+15 x 2.000000e+10 = 2.000000e+25
            '{32'h2EDBE6FF, 32'h27101D7D, 32'h2FA4ED3F, 32'h2EDBE6FF}, // 1.000000e-10 + 2.000000e-15 x 3.000000e-10 = 1.000000e-10
            '{32'h40A00000, 32'hBF800000, 32'h40A00000, 32'h00000000}, // 5 + -1 x 5 = 0
            '{32'h40E00000, 32'h3F800000, 32'h00000000, 32'h40E00000}, // 7 + 1 x 0 = 7
            '{32'h41800000, 32'h41000000, 32'h3E000000, 32'h41880000} // 16 + 8 x 0.125 = 17
        };

        @(cb);
        wait(!cb.rst_in);
        @(cb);

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_inputs.put('{x: test_vectors[i][0], y: test_vectors[i][1], z: test_vectors[i][2]});
        end

        for (int unsigned i = 0; i < $size(test_vectors); ++i) begin
            mb_output.get_with_timeout(LATENCY + 5, result, timed_out);

            assert(!timed_out)
            else $error("Timeout while waiting for output for test vector %0d", i);

            $display("Test vector %0d: x = 0x%h, y = 0x%h, z = 0x%h, expected result = 0x%h, actual result = 0x%h", i, test_vectors[i][0], test_vectors[i][1], test_vectors[i][2], test_vectors[i][3], result);

            assert(result === test_vectors[i][3])
            else $error("Incorrect output for test vector %0d. Expected 32'h%x, but got 32'h%x", i, test_vectors[i][3], result);
        end
    endtask

    initial begin
        cb.rst_out <= 1'b1;
        @(cb);
        repeat (LATENCY) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        run_test();

        $finish;
    end
endmodule

