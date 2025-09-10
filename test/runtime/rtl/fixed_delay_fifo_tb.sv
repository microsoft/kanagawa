// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

module KanagawaFixedDelayFifo_tb
#(
    parameter TB_WIDTH = 16,
    parameter TB_DELAY = 8,
    parameter TB_USE_DSP = 0
)
(
);
    bit clk;
    bit rst = 1'b1;

    typedef logic [TB_WIDTH-1:0] data_t;

    data_t data_in;
    data_t data_out;

    clocking cb @(posedge clk);
        output rst;
    endclocking

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        $display("Starting KanagawaFixedDelayFifo_tb width: %d delay: %d use_dsp: %d", TB_WIDTH, TB_DELAY, TB_USE_DSP);

        // wait for reset to propagate
        for(int i=0; i < 10; i=i+1) @(cb);

        // clear reset signal
        cb.rst <= 1'b0;
        @(cb);

        for (int i = 0; i < 1024; i++) begin
            data_in <= i;

            if (i > TB_DELAY) begin
                assert(data_out == (i - TB_DELAY - 1)) else $error("Incorrect output data");
            end

            @(cb);
        end

        $info("SUCCESS");
        $finish;
    end

    KanagawaFixedDelayFifo
    #(
        .WIDTH(TB_WIDTH),
        .DELAY(TB_DELAY),
        .USE_LUTRAM(1),
        .DEVICE_FAMILY("Stratix10"),
        .USE_DSP(TB_USE_DSP)
    )
    dut
    (
        .clk(clk),
        .rst(rst),

        .data_in(data_in),
        .data_out(data_out)
    );
endmodule
