// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSkidBuffer

	This module implements a skid buffer. A skid buffer provides a way to add an
	additional  register to a data path while also adding a register to the
	feedback flow control path. It does this by registering the input ready
	signal that goes to the upstream logic while providing an overflow
	register to capture any data that may be sent by the upstream module due
	to the one cycle delay in the input ready signal.

	This module instantiates duplicate registers for two of the control signals
    that are used internally. This is done to guarantee that the module ouputs
    have no load placed on them by the logic internal to the module.

    Note that this module is a simple port of SkidBuffer from the
    CatapultFpgaCommon package. The module was ported to enable removal of all
    dependencies with this external package. To maintain compatibility with
    this module, the port names from the original module were retained.

Authors:
	- Matthew Humphrey (mhumphr@microsoft.com)
*/

module KanagawaSkidBuffer
#(
    parameter WIDTH
)
(
    input  wire                 clk,
    input  wire                 rst,

    input  wire [WIDTH-1:0]     input_data_in,
    input  wire                 input_valid_in,
    output logic                input_ready_out,

    output logic [WIDTH-1:0]    output_data_out,
    output logic                output_valid_out,
    input  wire                 output_ready_in
);

    logic               input_ready_ff;
    logic               overflow_valid_ff;
    logic               output_valid_ff;

    // These extra registers ensure that output signals have no load from internal logic
    (*preserve*)logic   internal_input_ready_ff;
    (*preserve*)logic   internal_output_valid_ff;

    logic [WIDTH-1:0]   output_data_ff;
    logic [WIDTH-1:0]   overflow_data_ff;

    always_comb begin
        input_ready_out  = input_ready_ff;

        output_data_out  = output_data_ff;
        output_valid_out = output_valid_ff;
    end

    always @(posedge clk) begin
        // Sink
        if (internal_output_valid_ff && output_ready_in) begin
            input_ready_ff <= 1'b1;
            internal_input_ready_ff <= 1'b1;

            if (overflow_valid_ff) begin
                // Overflow has data - send this to output
                overflow_valid_ff <= 1'b0;
                output_data_ff <= overflow_data_ff;
            end
            else begin
                // May get overridden below
                output_valid_ff <= 1'b0;
                internal_output_valid_ff <= 1'b0;
            end
        end

        // Source
        if (input_valid_in && internal_input_ready_ff) begin
            output_valid_ff <= 1'b1;
            internal_output_valid_ff <= 1'b1;
            overflow_data_ff <= input_data_in;

            if (output_ready_in || !internal_output_valid_ff) begin
                // Write directly to output
                output_data_ff <= input_data_in;
            end
            else begin
                // Write to overflow
                overflow_valid_ff <= 1'b1;
                input_ready_ff <= 1'b0;
                internal_input_ready_ff <= 1'b0;
            end
        end

        //synopsys sync_set_reset "rst"
        if (rst) begin
            input_ready_ff <= 1'b1;
            internal_input_ready_ff <= 1'b1;
            overflow_valid_ff <= 1'b0;
            output_valid_ff <= 1'b0;
            internal_output_valid_ff <= 1'b0;
        end
    end

    // synopsys translate_off
`ifndef NO_DYNAMIC_ASSERTS
    always @(posedge clk) begin
        assert(input_ready_ff === internal_input_ready_ff);
        assert(output_valid_ff === internal_output_valid_ff);
        assert(~(~rst & input_valid_in & input_ready_out & overflow_valid_ff & ~output_ready_in)) else begin
            $error("%m overflow");
        end
    end
`endif
    // synopsys translate_on

endmodule // KanagawaSkidBuffer
