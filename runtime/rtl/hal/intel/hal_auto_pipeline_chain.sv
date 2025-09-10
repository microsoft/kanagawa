// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Variable length chain of registers where the Intel EDA tool can control the length based
// on placement and routing needs. 
module KanagawaHALAutoPipelineChain
#(
    // Width of the register chain
    parameter integer WIDTH,

    // Minimum depth of the generated pipeline. Due to constraints of the Intel auto-pipelinin feature
    // this value must be at least 1 if auto-pipelining is enabled (MAX_DEPTH != MIN_DEPTH).
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

        assert(AUTO_PIPE_DEPTH == 0 || MAX_DEPTH <= 99) 
        else $error("%m: On the Intel platform, the maximum supported value for MAX_DEPTH (%0d) is 99", MAX_DEPTH);

        assert(AUTO_PIPE_DEPTH == 0 || MIN_DEPTH > 0) 
        else $error("%m: When auto-pipelining is enabled (MAX_DEPTH != MIN_DEPTH), MIN_DEPTH (%0d) must be >= 1", MIN_DEPTH);

        assert(AUTO_PIPE_DEPTH == 0 || MAX_DEPTH <= 99) 
        else $error("%m: On the Intel platform, the maximum supported value for MAX_DEPTH (%0d) is 99", MAX_DEPTH);

        assert(AUTO_PIPE_DEPTH == 0 || AUTO_PIPELINE_GROUP != "UNUSED") 
        else $error("%m: You must assign a value for AUTO_PIPELINE_GROUP if auto-pipelining is enabled (MAX_DEPTH != MIN_DEPTH). Set this to the same value for instances where you want the EDA tool to choose the same value.");
    end

    typedef struct packed 
    {
        logic enable;
        logic [WIDTH-1:0] data;
    } EnableAndData_t;

    EnableAndData_t in, out, pre_out;
    assign in = '{enable:enable_in, data:data_in};
    assign data_out = out.data;
    assign enable_out = out.enable;

    generate        
        ////////////////////////////////////
        // Pre-pipeline stage

        if (MIN_DEPTH > 1) begin
            localparam PRE_DEPTH = (AUTO_PIPE_DEPTH == 0) ? MIN_DEPTH : MIN_DEPTH - 1;
            EnableAndData_t [PRE_DEPTH-1:0] pre_ff;

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

            (* altera_attribute = {"-name ADV_NETLIST_OPT_ALLOWED NEVER_ALLOW; -name HYPER_RETIMER_ADD_PIPELINING ", AUTO_PIPE_LIMIT_STR, "; -name HYPER_RETIMER_ADD_PIPELINING_GROUP ", AUTO_PIPELINE_GROUP} *)
            logic [WIDTH:0] auto_pipe_ff /* synthesis preserve */;

            always_ff @(posedge clk) begin
                auto_pipe_ff <= pre_out;
            end

            assign out = auto_pipe_ff;
        end
        else begin
            assign out = pre_out;
        end
    endgenerate

endmodule
