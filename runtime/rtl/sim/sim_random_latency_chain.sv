// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This simulation-only module implements a chain of registers whose length can
// randomly vary. If enabled, the random length changes only occur when the 
// (maximum length) pipeline is empty, as determined by the enable input. 

module KanagawaSimRandomLatencyChain
#(
    // Width of the register chain
    parameter integer WIDTH,

    // Minimum depth of the generated pipeline.
    // The Xilinx version of this module has a fixed register stage on the input and the output so
    // that we are guaranteed to meet the Xilinx requirement of the AP registers being driven by
    // a register and having a fanout of one on the output.
    parameter integer MIN_DEPTH = 0, 

    // Maximum depth of the generated pipeline
    parameter integer MAX_DEPTH = 0
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
        assert(MIN_DEPTH >= 0)
        else $error("%m: MIN_DEPTH (%0d) must be >= 0", MIN_DEPTH);

        assert(MAX_DEPTH >= 0)
        else $error("%m: MAX_DEPTH (%0d) must be >= 0", MAX_DEPTH);

        assert(MIN_DEPTH <= MAX_DEPTH)
        else $error("%m: MIN_DEPTH (%0d) must be <= MAX_DEPTH (%0d)", MIN_DEPTH, MAX_DEPTH);
    end

    typedef struct packed 
    {
        logic enable;
        logic [WIDTH-1:0] data;
    } EnableAndData_t;

    EnableAndData_t in, out;
    assign in = '{enable:enable_in, data:data_in};
    assign data_out = out.data;
    assign enable_out = out.enable;

    generate
        if (MAX_DEPTH == 0) begin
            assign out = in;
        end
        else begin
            function automatic bit all_enables_clear(input EnableAndData_t [MAX_DEPTH-1:0] pipe);
                bit result = 1'b1;
                for (int i = 0; i < MAX_DEPTH; ++i) begin
                    if (pipe[i].enable !== 1'b0) begin
                        result = 1'b0;
                    end
                end
                return result;
            endfunction

            logic [$clog2(MAX_DEPTH):0] selected_ff;
            EnableAndData_t [MAX_DEPTH-1:0] pipeline_ff;
            EnableAndData_t [MAX_DEPTH:0] pipeline;

            clocking cb @(posedge clk);
                output selected_ff;
                input #0 pipeline_ff;
            endclocking

            initial begin
                automatic bit armed = 1'b0;
                cb.selected_ff <= MAX_DEPTH;

                forever begin
                    @(cb);
                    
                    if (armed && all_enables_clear(cb.pipeline_ff)) begin
                        armed = 1'b0;
                        cb.selected_ff <= $urandom_range(MIN_DEPTH, MAX_DEPTH);                
                    end
                    else if (!all_enables_clear(cb.pipeline_ff)) begin
                        armed = 1'b1;
                    end
                end
            end

            always_ff @(posedge clk) begin
                // Push incoming data into the pipeline
                pipeline_ff[0]  <= in;

                for (int i = 1; i < MAX_DEPTH; ++i) begin
                    pipeline_ff[i] <= pipeline_ff[i-1];
                end
            end

            always_comb begin
                pipeline[0] = in;
                for (int i = 1; i <= MAX_DEPTH; ++i) begin
                    pipeline[i] = pipeline_ff[i-1];
                end
            end

            assign out = pipeline[selected_ff];
        end
    endgenerate

endmodule
