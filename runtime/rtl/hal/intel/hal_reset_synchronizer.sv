// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaHALResetSynchronizer

    This module converts an asynchronous reset into a reset synchronous with the provided clock.
    This circuit has been discussed in many contexts, but the Xilinx-specific RTL was found in
    this Xilinx app note:

    https://forums.xilinx.com/t5/Adaptable-Advantage-Blog/Demystifying-Resets-Synchronous-Asynchronous-other-Design/ba-p/882252

Ports:
    clk             - Clock (input)
    arst            - Asynchronous (relative to clk) reset (input)
    rst             - Synchronous reset (output)

Authors:
    - Matt Humphrey (mhumphr@microsoft.com)
*/
module KanagawaHALResetSynchronizer
(
    input  wire     clk,
    input  wire     arst,
    output wire     rst
);

    reg [1:0] sreg = '1
    /* synthesis preserve dont_replicate */
    /* synthesis ALTERA_ATTRIBUTE = "-name SDC_STATEMENT \"set_false_path -to [get_pins -compatibility_mode {*KanagawaHALResetSynchronizer:*|sreg*|clrn}]\" "; */
    ;

    always @(posedge clk or posedge arst) begin
        if (arst) begin
            sreg[0] <= 1'b1;
            sreg[1] <= 1'b1;
        end
        else begin
            sreg[0] <= 1'b0;
            sreg[1] <= sreg[0];
        end
    end

    assign rst = sreg[1];

endmodule : KanagawaHALResetSynchronizer

