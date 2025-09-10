// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


module KanagawaRegisterFifoSkid
#(
    parameter WIDTH
)
(
    input  wire                 clock,
    input  wire                 rst,

    input  wire                 wrreq,
    input  wire [WIDTH-1:0]     data,
    output logic                full,

    input  wire                 rdreq,
    output logic                empty,
    output logic [WIDTH-1:0]    q
);

    logic               full_ff;
    logic               overflow_ff;
    logic               empty_ff;

    // Extra registers ensure that output signals have no load from internal logic
    // I am duplicating that approach which was in the Altera Synthesis Cookbook
    (*preserve*)logic   internal_empty_ff;

    logic [WIDTH-1:0]   output_data_ff;
    logic [WIDTH-1:0]   overflow_data_ff;

    always_comb begin
        full  = full_ff;
        q  = output_data_ff;
        empty = empty_ff;
    end

    always @(posedge clock) begin
        // Sink
        if (rdreq) begin
            full_ff <= 1'b0;    // May get overridden below

            if (overflow_ff) begin
                // Overflow has data - send this to output
                overflow_ff <= 1'b0;
                output_data_ff <= overflow_data_ff;
            end
            else begin
                // May get overridden below
                empty_ff <= 1'b1;
                internal_empty_ff <= 1'b1;
            end
        end

        // Source
        if (wrreq) begin
            empty_ff <= 1'b0;
            internal_empty_ff <= 1'b0;
            overflow_data_ff <= data;

            if (rdreq || internal_empty_ff) begin
                // Write directly to output
                output_data_ff <= data;
            end
            else begin
                // Write to overflow
                overflow_ff <= 1'b1;
                full_ff <= 1'b1;
            end
        end

        //synopsys sync_set_reset "rst"
        if (rst) begin
            full_ff <= 1'b0;
            overflow_ff <= 1'b0;
            empty_ff <= 1'b1;
            internal_empty_ff <= 1'b1;
        end
    end

    // synopsys translate_off
`ifndef NO_DYNAMIC_ASSERTS
    always @(posedge clock) begin
        assert(empty_ff === internal_empty_ff);
        assert(!((rst===1'b0) && (wrreq===1'b1) && (full===1'b1))) else begin
            $error("%m overflow");
        end
        assert(!((rst===1'b0) && (rdreq===1'b1) && (empty===1'b1))) else begin
            $error("%m underflow");
        end
    end
`endif
    // synopsys translate_on

endmodule
