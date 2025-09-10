// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Module that implements pipelined function calls
// Input comes from a FIFO output interface looks like a FIFO
// Note that this assumes that an entry with count == 0  is not possible
//

`default_nettype none

module KanagawaLoopGenerator
#(
    parameter TOTAL_WIDTH,
    parameter COUNTER_WIDTH,
    parameter HAS_LITERAL_MAX_THREAD_ID,
    parameter LITERAL_MAX_THREAD_ID, // If HAS_LITERAL_MAX_THREAD_ID is set, then the thread ID is always this constant
    parameter OFFSET,
    parameter ONLY_ONE_THREAD_OFFSET // offset to a bit that is set when thread count = 1
)
(
    input  wire clk,
    input  wire rst,

    input wire empty_in,
    input wire [TOTAL_WIDTH-1:0] data_in,
    output logic rden_out,

    output logic [TOTAL_WIDTH-1:0] data_out,
    output logic empty_out,
    input wire rden_in,

    output logic underflow_out
);
    // Track underflows
    KanagawaFifoDebug dbg
    (
        .clk(clk),
        .rst(rst),

        .state_in(empty_out),
        .req_in(rden_in),

        .error_out(underflow_out)
    );

    logic [COUNTER_WIDTH-1:0] max_thread_id;
    logic only_one_thread;

    // Generate if statement to ensure proper code coverage measurement
    generate
        if (HAS_LITERAL_MAX_THREAD_ID) begin : genLiteralThreadCount
            assign max_thread_id = LITERAL_MAX_THREAD_ID;
            assign only_one_thread = LITERAL_MAX_THREAD_ID == 0;
        end
        else begin : genDynamicThreadCount
            assign max_thread_id = data_in[COUNTER_WIDTH+OFFSET-1:OFFSET];
            assign only_one_thread = data_in[ONLY_ONE_THREAD_OFFSET];
        end
    endgenerate

    assign empty_out = empty_in;

    logic incr_thread_count;

    logic count_reached;
    logic [COUNTER_WIDTH-1:0] current_thread_id;

    KanagawaThreadCounter
    #(
        .THREAD_COUNT_WIDTH(COUNTER_WIDTH),
        .HAS_LITERAL_MAX_THREAD_ID(HAS_LITERAL_MAX_THREAD_ID),
        .LITERAL_MAX_THREAD_ID(LITERAL_MAX_THREAD_ID)
    )
    thread_counter
    (
        .clk(clk),
        .rst(rst),

        .incr_in(incr_thread_count),
        .only_one_thread_in(only_one_thread),
        .max_thread_id_in(max_thread_id),

        .count_reached_out(count_reached),
        .thread_id_out(current_thread_id)
    );

    always_comb begin
        rden_out = 1'b0;

        incr_thread_count = 1'b0;

        // Patch the current counter value into the output data
        data_out = data_in;
        data_out[COUNTER_WIDTH+OFFSET-1:OFFSET] = current_thread_id;

        if (rden_in) begin

            //  Update the counter
            incr_thread_count = 1'b1;

            if (count_reached) begin
                // The max count has been reached
                // Dequeue from the input FIFO
                rden_out = 1'b1;
            end
        end
    end
`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clk) (!rst && rden_in) |-> (current_thread_id <= max_thread_id)) else $error ("%m invalid output thread id");
    // synopsys translate_on
`endif
endmodule
