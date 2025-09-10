// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaStringTable_tb
(
);
    KanagawaStringTable_tb_inst inst
    ();
endmodule

module KanagawaStringTable_tb_inst
();
    typedef logic [63:0] string_handle_t;

    bit clk;
    string referenceTable[string_handle_t];

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        int handle;
        int total_strings_allocated;

        $info("Starting String Table Test");

        for (int iteration = 0; iteration < 1000; iteration++) begin
            automatic string_handle_t handle;
            automatic string str;

            str = $sformatf("str_%d_before", iteration);

            handle = iteration + 1;

            dut.allocate(str, handle);

            if ($urandom % 2) begin
                dut.reference_string(handle, -1, 1'b1);
            end
            else begin
                referenceTable[handle] = str;

                if ($urandom % 4) begin
                    dut.reference_string(handle, 1, 1'b1);
                    dut.reference_string(handle, -1, 1'b1);
                end

                if ($urandom % 4) begin
                    // This should have no effect because enable == 0
                    dut.reference_string(handle, 7, 1'b0);
                end
            end

            @(posedge clk);
        end

        foreach (referenceTable[handle]) begin
            automatic string actual;
            automatic string expected;

            actual = dut.get(handle, 1'b1);
            expected = referenceTable[handle];

            assert(actual == expected) else $error("DUT contents did not match expectations.  Expected: %s Actual: %s", expected, actual);
        end

        assert(dut.get_num_strings() < 520) else $error("Too many live strings (%d) at the end of the test", dut.get_num_strings());

        $info("SUCCESS");
        $finish;
    end

    KanagawaStringTable
    dut
    (
        .clk(clk)
    );
endmodule
