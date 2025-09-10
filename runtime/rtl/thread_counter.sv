// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Modules that counts a certain number of threads
//

`default_nettype none

// Used to implement pipelined function calls
// tracks current thread index within a given call to a pipelined function
module KanagawaThreadCounter
#(
    parameter THREAD_COUNT_WIDTH,
    parameter HAS_LITERAL_MAX_THREAD_ID,
    parameter LITERAL_MAX_THREAD_ID // If HAS_LITERAL_MAX_THREAD_ID is set, then the thread ID is always this constant
)
(
    input wire clk,
    input wire rst,

    input wire incr_in, // 1 on cycles when the count should be incremented
    input wire only_one_thread_in, // 1 when the this group of threads only has 1.  valid when incr_in is 1
    input wire [THREAD_COUNT_WIDTH-1:0] max_thread_id_in, //  (Target thread count - 1).  valid when incr_in is 1

    output logic count_reached_out, // 1 when the thread count has been reached
    output logic [THREAD_COUNT_WIDTH-1:0] thread_id_out // the index of the current thread
);
    typedef struct packed
    {
        // Counts number of threads
        logic [THREAD_COUNT_WIDTH-1:0] current_count;

        // The same as current_count, but 1 higher - to help timing
        logic [THREAD_COUNT_WIDTH-1:0] current_count_plus_one;

        // Set to 1 when the next thread will be the last one
        logic thread_id_minus_one_reached;
    } thread_count_data_t;

    thread_count_data_t thread_count_data_ff, thread_count_data_next;

    logic reg_enable;

    assign thread_id_out = thread_count_data_ff.current_count;

    logic [THREAD_COUNT_WIDTH-1:0] max_thread_id;
    logic only_one_thread;

    // Generate if statement to ensure proper code coverage measurement
    generate
        if (HAS_LITERAL_MAX_THREAD_ID) begin : genLiteralThreadCount
            assign max_thread_id = LITERAL_MAX_THREAD_ID;
            assign only_one_thread = LITERAL_MAX_THREAD_ID == 0;
        end
        else begin : genDynamicThreadCount
            assign max_thread_id = max_thread_id_in;
            assign only_one_thread = only_one_thread_in;
        end
    endgenerate

    always_comb begin
        thread_count_data_next = thread_count_data_ff;

        count_reached_out = 1'b0;

        if (incr_in) begin
            // Increment both counters
            thread_count_data_next.current_count = THREAD_COUNT_WIDTH'(thread_count_data_ff.current_count + 1);

            thread_count_data_next.current_count_plus_one = THREAD_COUNT_WIDTH'(thread_count_data_ff.current_count_plus_one + 1);

            // Set thread_id_minus_one_reached for the next iteration
            thread_count_data_next.thread_id_minus_one_reached = (thread_count_data_ff.current_count_plus_one == max_thread_id);

            // For groups of threads with thread count > 1, thread_count_data_ff.thread_id_minus_one_reached determines when the last thread is reached
            // For groups of threads with thread count == 1, thread_count_data_ff.thread_id_minus_one_reached won't work because it is computed a cycle early
            // in this case, the first time incr_in is set indicates the last (and only) thread in the group has been counted
            count_reached_out = only_one_thread | thread_count_data_ff.thread_id_minus_one_reached;
        end

        if (count_reached_out) begin
            // Reset the thread count registers for the next group of threads
            thread_count_data_next.current_count = (THREAD_COUNT_WIDTH)'('d0);
            thread_count_data_next.current_count_plus_one =(THREAD_COUNT_WIDTH)'('d1);
            thread_count_data_next.thread_id_minus_one_reached = 1'b0;
        end

        reg_enable = incr_in;
    end

    always_ff @(posedge clk) begin
        thread_count_data_ff <= reg_enable ? thread_count_data_next : thread_count_data_ff;

        //synopsys sync_set_reset "rst"
        if (rst) begin
            thread_count_data_ff.current_count <= (THREAD_COUNT_WIDTH)'('d0);
            thread_count_data_ff.current_count_plus_one <= (THREAD_COUNT_WIDTH)'('d1);
            thread_count_data_ff.thread_id_minus_one_reached <= 1'b0;
        end
    end
endmodule
