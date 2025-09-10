// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Variable length chain of registers where the Xilinx EDA tool can control the length based
// on placement and routing needs. 
module KanagawaHALAutoPipelineChain
#(
    // Width of the register chain
    parameter integer WIDTH,

    // Minimum depth of the generated pipeline. If this value is at least two, in this (Xilinx) 
    // version of the module, we put one register stage on input and one on output so that we
    // are guaranteed to meet the requirement of driving the AP stage from a register output
    // and driving the output with a fanout of 1. If the value of this parameter is 1, we just
    // put the fixed register stage on the output and it's up to the consumer of this module
    // to ensure that the input is driven by a register. If the value is zero, we do not put
    // any input or output stages - just the auto-pipeline stage.
    parameter integer MIN_DEPTH, 

    // Maximum depth of the generated pipeline
    parameter integer MAX_DEPTH,

    // The EDA tool will ensure all auto-pipeline registers with the same group name have the same length.
    // You MUST override this value if auto-pipelining is enabled (MAX_DEPTH != MIN_DEPTH)
    parameter AUTO_PIPELINE_GROUP = "UNUSED"
)
(
    input  wire                 clk,

    input  wire                 enable_in,
    input  wire  [WIDTH-1:0]    data_in,

    output logic                enable_out,
    output logic [WIDTH-1:0]    data_out  
);

    // Actual auto-pipeline depth
    localparam AUTO_PIPE_DEPTH = MAX_DEPTH - MIN_DEPTH;

    initial begin
        assert(MIN_DEPTH <= MAX_DEPTH)
        else $error("%m: MIN_DEPTH (%0d) must be <= MAX_DEPTH (%0d)", MIN_DEPTH, MAX_DEPTH);

        assert(AUTO_PIPE_DEPTH == 0 || MAX_DEPTH <= 26) 
        else $error("%m: On the Xilinx platform, the maximum supported value for MAX_DEPTH (%0d) is 26", MAX_DEPTH);

        assert(AUTO_PIPE_DEPTH == 0 || AUTO_PIPELINE_GROUP != "UNUSED") 
        else $error("%m: You must assign a value for AUTO_PIPELINE_GROUP if auto-pipelining is enabled (MAX_DEPTH != MIN_DEPTH). Set this to the same value for instances where you want the EDA tool to choose the same value.");
    end

    typedef struct packed 
    {
        logic enable;
        logic [WIDTH-1:0] data;
    } EnableAndData_t;

    EnableAndData_t in, out, pre_out, auto_out, post_out;
    assign in = '{enable:enable_in, data:data_in};
    assign data_out = out.data;
    assign enable_out = out.enable;

    generate        
        ////////////////////////////////////
        // Pre-pipeline stage
        
        if (MIN_DEPTH > 1) begin
            localparam PRE_DEPTH = MIN_DEPTH - 1;
            (* keep *) EnableAndData_t [PRE_DEPTH-1:0] pre_ff;

            always_ff @(posedge clk) begin
                pre_ff[0] <= in;

                for (int i = 1; i < PRE_DEPTH; ++i) begin
                    pre_ff[i] <= pre_ff[i-1];
                end
            end

            assign pre_out = pre_ff[PRE_DEPTH-1];
        end
        else begin
            assign pre_out = in;
        end

        ////////////////////////////////////
        // Auto pipeline stage

        if (AUTO_PIPE_DEPTH > 0) begin

            // Convert AUTO_PIPE_DEPTH to a string so we can use it as a parameter below
            localparam AUTO_PIPE_LIMIT_STR = {((AUTO_PIPE_DEPTH / 10) % 10) + 8'd48, (AUTO_PIPE_DEPTH % 10) + 8'd48};
            (* autopipeline_group = AUTO_PIPELINE_GROUP, autopipeline_limit = AUTO_PIPE_LIMIT_STR *) EnableAndData_t auto_pipe;

            assign auto_pipe = pre_out;
            assign auto_out = auto_pipe;
        end
        else begin
            assign auto_out = pre_out;
        end

        ////////////////////////////////////
        // Post-pipeline, single register - guarantees fan-out of 1
        if (MIN_DEPTH > 0) begin

            EnableAndData_t post_ff;

            // One extra flop guarantees we meet the requirement for a fan-out of 1 on the auto-pipeline
            always_ff @(posedge clk) begin
                post_ff <= auto_out;        
            end

            assign out = post_ff;
        end
        else begin
            assign out = auto_out;
        end
    endgenerate

endmodule

