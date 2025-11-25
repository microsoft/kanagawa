// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

// Directed test for KanagawaFunctions::bits32_to_real function
module bits32_to_real_tb;
    function automatic real fabs(input real value);
        return value < 0.0 ? -value : value;
    endfunction

    task automatic expect_real(string name, logic [31:0] bits, real expected);
        real got = KanagawaFunctions::bits32_to_real(bits);
        longint unsigned got_bits = $realtobits(got);
        longint unsigned expected_bits = $realtobits(expected);
        bit expected_nan = (expected != expected);
        bit expected_inf = (expected_bits[62:52] == 11'h7FF) && (expected_bits[51:0] == 0);

        if (expected_nan) begin
            if (got == got) $fatal(1, "[bits32_to_real_tb] %s: expected NaN, got %f", name, got);
        end else if (expected_inf) begin
            if (got_bits !== expected_bits)
                $fatal(1, "[bits32_to_real_tb] %s: expected infinity, got 0x%016h", name, got_bits);
        end else if (fabs(got - expected) > 1e-18) begin
            $fatal(1, "[bits32_to_real_tb] %s: expected %.12f, got %.12f", name, expected, got);
        end
    endtask

    initial begin
        expect_real("zero",      32'h0000_0000, 0.0);
        expect_real("one",       32'h3F80_0000, 1.0);
        expect_real("negative",  32'hC040_0000, -3.0);
        expect_real("infinity",  32'h7F80_0000, 1.0/0.0);
        expect_real("negInf",    32'hFF80_0000, -1.0/0.0);
        expect_real("denorm",    32'h0000_0001, 2.0**-149);
        expect_real("qNaN",      32'h7FC0_0001, 0.0/0.0);
        $display("bits32_to_real_tb PASSED");
    end
endmodule

// Directed test for KanagawaFunctions::real_to_bits32 function
module real_to_bits32_tb;
    task automatic expect_bits(string name, real value, logic [31:0] expected);
        logic [31:0] got = KanagawaFunctions::real_to_bits32(value);
        if (got !== expected)
            $fatal(1, "[real_to_bits32_tb] %s: expected 0x%08h, got 0x%08h", name, expected, got);
    endtask

    task automatic expect_nan(string name, real value);
        logic [31:0] got = KanagawaFunctions::real_to_bits32(value);
        if (!(got[30:23] == 8'hFF && got[22:0] != 0))
            $fatal(1, "[real_to_bits32_tb] %s: expected NaN encoding, got 0x%08h", name, got);
    endtask

    initial begin
        expect_bits("zero",        0.0,        32'h0000_0000);
        expect_bits("negative",   -2.5,        32'hC020_0000);
        expect_bits("positive",    3.75,       32'h4070_0000);
        expect_bits("overflow",    1.0e40,     32'h7F80_0000);
        expect_bits("negOverflow",-1.0e40,     32'hFF80_0000);
        expect_bits("underflow",   1.0e-50,    32'h0000_0000);
        expect_nan ("nanInput",    0.0/0.0);
        $display("real_to_bits32_tb PASSED");
    end
endmodule

// Directed test for KanagawaFunctions::shortreal_bits_to_string function
module shortreal_bits_to_string_tb;
    task automatic expect_string(string name, logic [31:0] bits, string expected);
        string got = KanagawaFunctions::shortreal_bits_to_string(bits);
        if (got != expected)
            $fatal(1, "[shortreal_bits_to_string_tb] %s: expected \"%s\", got \"%s\"", name, expected, got);
    endtask

    initial begin
        expect_string("one",        32'h3F80_0000, "1.000000");
        expect_string("negHalf",    32'hBF00_0000, "-0.500000");
        expect_string("ten",        32'h4120_0000, "10.000000");
        expect_string("inf",        32'h7F80_0000, "inf");
        $display("shortreal_bits_to_string_tb PASSED");
    end
endmodule