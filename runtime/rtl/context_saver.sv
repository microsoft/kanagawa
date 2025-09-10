// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Module that holds live variables while a synchronous function call is executing
//
`default_nettype none

module KanagawaContextSaver
import KanagawaTypes::*;
#(
    parameter
        DST_WIDTH = 32,
        CALLEE_WIDTH = 32,
        CALLER_WIDTH = 32,
        DEPTH = 32,
        INSTANCE_OFFSET = 0,
        INSTANCE_WIDTH = 9,
        DEVICE_FAMILY = "UNUSED"
)
(
    input wire clk,
    input wire rst,

    //
    // Caller interface
    //

    // 1 when caller data is valid
    input wire caller_wren_in,

    // The live variables associated with the caller
    input wire [CALLER_WIDTH-1:0] caller_data_in,

    // When caller_wren_in = 1, this is set to the instance index
    // which represents where the caller data was stored
    // This should be passed back to this module when the callee is done
    output logic [INSTANCE_WIDTH-1:0] caller_invocation_index_out,

    //
    // Callee FIFO interface
    //

    // 0 when callee data and instance index are valid
    input wire callee_empty_in,

    // Return value & invocation index from callee
    input wire [CALLEE_WIDTH-1:0] callee_data_in,

    // 1 when an item is dequeue from callee fifo
    output logic callee_rden_out,

    //
    // Return site interface
    //

    // 1 when output fifo is 1 entry away full
    input wire output_almost_full_in,

    // 1 when the output data should be enqueued to the output
    output logic wren_out,

    // Callee data passed to the return site
    output logic [CALLEE_WIDTH-1:0] callee_data_out,

    // Callee data passed to the return site
    output logic [CALLER_WIDTH-1:0] caller_data_out
);

   // slot_in_use[i] = 1 if slot[i] has a call outstanding
   logic [DEPTH-1:0] slot_in_use_ff;
   logic [DEPTH-1:0] slot_in_use_next;

   // no -1 in INSTANCE_WIDTH to ensure that it can count the case when the object is full
   // for example, if INSTANCE_WIDTH is 9, then num_slots_in_use_ff must be able to store the number 512
   // which requires 10 bits
   logic [INSTANCE_WIDTH:0] num_slots_in_use_ff;
   logic [INSTANCE_WIDTH:0] num_slots_in_use_next;

   logic [INSTANCE_WIDTH-1:0] callee_invocation_index;

   logic output_next_cycle;

   logic accept_callee_data;

   // Ensure the context save area is implemented as a BRAM
   // port a is for write
   // port b is for read
   KanagawaSimpleDualPortRAM
   #(
        .DATA_WIDTH(CALLER_WIDTH),
        .ADDR_WIDTH($clog2(DEPTH)),
        .USE_LUTRAM(0),
        .USE_BRAM(1),
        .USE_OUTPUT_REG(0),
        .DEVICE_FAMILY(DEVICE_FAMILY)
   )
   slots
   (
        .clk(clk),
        .rst(rst),

        .writeaddr_in(caller_invocation_index_out[$clog2(DEPTH)-1:0]),
        .data_in(caller_data_in),
        .wren_in(caller_wren_in),

        .rden_in(1'b1),
        .readaddr_in(callee_invocation_index[$clog2(DEPTH)-1:0]),
        .data_out(caller_data_out)
   );

    always_comb begin
        // Initialize outputs
        caller_invocation_index_out = 0;

        callee_rden_out = 0;

        // Initialize internal variables
        callee_invocation_index = 0;

        slot_in_use_next = slot_in_use_ff;
        num_slots_in_use_next = num_slots_in_use_ff;

        output_next_cycle = 0;

        accept_callee_data = 0;

        if (caller_wren_in) begin
            // Find a free slot
            for (int i = 0; i < DEPTH; i++) begin
                if (slot_in_use_ff[i] == 0) begin
                    caller_invocation_index_out = i;
                    slot_in_use_next[i] = 1;
                    break;
                end
            end

            num_slots_in_use_next = num_slots_in_use_next + 1;
        end

        // output almost_full value is checked because
        // the output happens on the next cycle
        if (~callee_empty_in & ~output_almost_full_in) begin
            // Decode the invocation index from the input data
            callee_invocation_index = callee_data_in[INSTANCE_OFFSET + INSTANCE_WIDTH - 1 : INSTANCE_OFFSET];

            // Write to output on the next cycle - because the read from slots will happen on the next cycle
            output_next_cycle = 1;

            // Free the associated slot
            slot_in_use_next[callee_invocation_index] = 0;
            num_slots_in_use_next = num_slots_in_use_next - 1;

            // Dequeue from callee fifo
            callee_rden_out = 1;

            // Remember that callee data was accepted
            accept_callee_data = 1;
        end
    end

    always_ff @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        slot_in_use_ff <= (rst) ? 0 : slot_in_use_next;
        num_slots_in_use_ff <= (rst) ? 0 : num_slots_in_use_next;

        // Output writes are delayed by once cycle
        // because the context save data is stored in a mram module
        // which has registered outputs
        wren_out <= 0;
        callee_data_out <= 0;

        if (output_next_cycle) begin
            wren_out <= 1;
            callee_data_out <= callee_data_in;
        end
    end
endmodule

// optimized to take advantage of the fact that threads come out of the callee in the same order they entered
module ContextSaverOrdered
import KanagawaTypes::*;
#(
    parameter int unsigned
        DST_WIDTH,
        CALLEE_IN_WIDTH,
        CALLEE_OUT_WIDTH,
        CALLER_WIDTH,
        LOG_DEPTH,
        DEPTH, // must be <= 2**LOG_DEPTH
        USE_LUTRAM,
        ALMOSTFULL_ENTRIES,
        IS_PIPELINED,
        LOOP_COUNT_WIDTH,
        RETURN_ELEMENT_WIDTH,
        RETURN_ELEMENT_COUNT,
        PENDING_COUNT_WIDTH,
        ALMOSTEMPTY_VAL,
        WRITE_DELAY,
        BUFFER_FIFO_OPTIMIZATION,
        CALLEE_VOID_RETURN_TYPE
)
(
    input wire clk,
    input wire rst,

    //
    // Caller interface
    //

    // 1 when caller data is valid
    input wire caller_wren_in,

    // The live variables associated with the caller
    input wire [CALLER_WIDTH-1:0] caller_data_in,

    // Number of expected returns for this call
    input wire [LOOP_COUNT_WIDTH-1:0] caller_loop_count_in,

    // almost full for the caller interface
    output logic caller_almost_full_out,

    //
    // Callee FIFO interface
    //

    // 0 when callee data and instance index are valid
    input wire callee_empty_in,

    // Return value & invocation index from callee
    input wire[CALLEE_IN_WIDTH - 1:0] callee_data_in,

    // 1 when an item is dequeue from callee fifo
    output logic callee_rden_out,

    //
    // Return site interface
    //

    // 1 when output fifo can accept data
    input wire output_rdy_in,

    // 1 when the output data should be sent to the output basic block
    output logic valid_out,

    // Callee data passed to the return site
    output logic[CALLEE_OUT_WIDTH - 1:0] callee_data_out,

    // Callee data passed to the return site
    output logic[CALLER_WIDTH - 1:0] caller_data_out
);
    // Enable buffering of N+1 return elements
    // to ensure no bubbles
    localparam RETURN_ARRAY_BUFFER_SIZE = RETURN_ELEMENT_COUNT + 1;

    typedef logic [$clog2(RETURN_ARRAY_BUFFER_SIZE)-1:0] array_buffer_index_t;

    // 1 when item should be dequeued from internal fifo
    logic fifo_rden;

    typedef logic[CALLEE_OUT_WIDTH-1:0] outputArrayType;

    // Data stored in internal fifo
    typedef logic[CALLER_WIDTH-1:0] live_variable_fifo_data_t;

    live_variable_fifo_data_t live_variable_fifo_data_in;
    live_variable_fifo_data_t live_variable_fifo_data_out;

    // During enqueue, save a bit to handle the case where there is only 1 thread in the pipelined function call
    assign live_variable_fifo_data_in = caller_data_in;

    assign caller_data_out = live_variable_fifo_data_out;

    // Control signals related to live variables are handled separately to improve timing
    typedef logic[LOOP_COUNT_WIDTH-1:0] live_variable_control_fifo_data_t;

    live_variable_control_fifo_data_t live_variable_control_fifo_data_in;
    live_variable_control_fifo_data_t live_variable_control_fifo_data_out;

    // Extract the max_thread_id bits from caller data
    assign live_variable_control_fifo_data_in = caller_loop_count_in;

    logic live_variable_fifos_empty;

    // FIFO that holds live_variable_fifo_data_t
    // LOG_DEPTH is large enough so that this will never overflow
    // Note that after an element is placed into this fifo, it takes 2 cycles
    // for it to be visible on the output port.
    // There is no race condition, because the call from the call site to the caller
    // also goes through a FIFO, with no better latency
    //
    // LOG_DEPTH = 0 means he call site is a single-threaded function
    // so the fifo can only have 1 element
    generate
        if (LOG_DEPTH == 0) begin: gen_log_depth_0
            // In this case fifo overflow will never occur
            // There can only be 1 entry in the fifo at a time
            assign caller_almost_full_out = 1'b0;

            logic live_variable_control_fifo_empty;

            assign live_variable_fifos_empty = live_variable_control_fifo_empty;

            KanagawaOneRegisterFifo
            #(
                .LOG_DEPTH  (LOG_DEPTH),
                .WIDTH      ($bits(live_variable_fifo_data_t) + $bits(live_variable_control_fifo_data_t)),
                .USE_LUTRAM (USE_LUTRAM)
            )
            live_variable_control_fifo
            (
                .clock        (clk),
                .rst          (rst),

                .wrreq        (caller_wren_in),
                .data         ({live_variable_fifo_data_in, live_variable_control_fifo_data_in}),
                .full         (/*unused*/),
                .almost_full  (/*unused*/),
                .usedw        (/*unused*/),
                .rdreq        (fifo_rden),
                .empty        (live_variable_control_fifo_empty),
                .almost_empty (/*unused*/),
                .q            ({live_variable_fifo_data_out, live_variable_control_fifo_data_out})
            );
        end
        else begin: gen_log_depth_gt_0
            logic live_variable_almost_full;
            logic live_variable_control_almost_full;

            if (BUFFER_FIFO_OPTIMIZATION == 0) begin: gen_fifo_opt_eq_0
                assign caller_almost_full_out = live_variable_almost_full;

                logic live_variable_control_fifo_empty;

                assign live_variable_fifos_empty = live_variable_control_fifo_empty;

                KanagawaWriteDelayFifo
                #(
                    .DEPTH  (DEPTH),
                    .WRITE_DELAY(WRITE_DELAY),
                    .WIDTH      ($bits(live_variable_fifo_data_t) + $bits(live_variable_control_fifo_data_t)),
                    .ALMOSTFULL_ENTRIES(ALMOSTFULL_ENTRIES),
                    .USE_LUTRAM (USE_LUTRAM),
                    .ALMOSTEMPTY_VAL(ALMOSTEMPTY_VAL)
                )
                live_variable_fifo
                (
                    .clock        (clk),
 					.rst          (rst),

					.wrreq        (caller_wren_in),
                    .data         ({live_variable_fifo_data_in, live_variable_control_fifo_data_in}),
                    .full         (live_variable_almost_full),
                    .overflow_out (/*unused*/),

                    .rdreq        (fifo_rden),
                    .empty        (live_variable_control_fifo_empty),
                    .q            ({live_variable_fifo_data_out, live_variable_control_fifo_data_out}),
                    .underflow_out(/*unused*/)
                );
            end
            else begin: gen_fifo_opt_ne_0
                assign caller_almost_full_out = live_variable_almost_full || live_variable_control_almost_full;

                logic live_variable_fifo_empty;
                logic live_variable_control_fifo_empty;

                assign live_variable_fifos_empty = live_variable_control_fifo_empty || live_variable_fifo_empty;

                KanagawaWriteDelayFifo
                #(
                    .DEPTH  (DEPTH),
                    .WRITE_DELAY(WRITE_DELAY),
                    .WIDTH      ($bits(live_variable_fifo_data_t)),
                    .ALMOSTFULL_ENTRIES(ALMOSTFULL_ENTRIES),
                    .USE_LUTRAM (USE_LUTRAM),
                    .ALMOSTEMPTY_VAL(ALMOSTEMPTY_VAL)
                )
                live_variable_fifo
                (
                    .clock        (clk),
                    .rst          (rst),

                    .wrreq        (caller_wren_in),
                    .data         (live_variable_fifo_data_in),
                    .full         (live_variable_almost_full),
                    .overflow_out (/*unused*/),

                    .rdreq        (fifo_rden),
                    .empty        (live_variable_fifo_empty),
                    .q            (live_variable_fifo_data_out),
                    .underflow_out(/*unused*/)
                );

                KanagawaInternalBufferFifo
                #(
                    .DEPTH   (DEPTH),
                    .WRITE_DELAY (WRITE_DELAY),
                    .WIDTH       ($bits(live_variable_control_fifo_data_t)),
                    .ALMOSTFULL_ENTRIES(ALMOSTFULL_ENTRIES),
                    .USE_LUTRAM  (USE_LUTRAM)
                )
                live_variable_control_fifo
                (
                    .clock         (clk),
                    .rst           (rst),

                    .wrreq         (caller_wren_in),
                    .data          (live_variable_control_fifo_data_in),
                    .full          (live_variable_control_almost_full),
                    .overflow_out  (/*unused*/),

                    .rdreq         (fifo_rden),
                    .empty         (live_variable_control_fifo_empty),
                    .q             (live_variable_control_fifo_data_out),
                    .underflow_out (/*unused*/)
                );
            end
        end

        if (IS_PIPELINED) begin :genPipelined
    		localparam logic [PENDING_COUNT_WIDTH-1:0] PENDING_COUNT_THRESHOLD =
        	// Only buffer returns from a single call
        	(CALLEE_VOID_RETURN_TYPE == 0) ? RETURN_ARRAY_BUFFER_SIZE[PENDING_COUNT_WIDTH-1:0] : '1; // not used in this case

            logic [PENDING_COUNT_WIDTH-1:0] pending_count_ff, pending_count_next;

			localparam int unsigned COMPARE_WIDTH = (PENDING_COUNT_WIDTH >= CALLER_WIDTH) ? PENDING_COUNT_WIDTH : CALLER_WIDTH;
			logic fifo_data_out_le_pend_count;
			assign fifo_data_out_le_pend_count = COMPARE_WIDTH'(live_variable_control_fifo_data_out) <= COMPARE_WIDTH'(pending_count_ff);

            // Send results on output interface if data from enough threads has been received
            always_comb begin
                valid_out = 1'b0;

                if (output_rdy_in && !live_variable_fifos_empty) begin
                    if (fifo_data_out_le_pend_count) begin
                        valid_out = 1'b1;
                    end
                end
            end

            // Dequeue from internal FIFOs when writing output
            assign fifo_rden = valid_out;

            always_ff @(posedge clk) begin
                //synopsys sync_set_reset "rst"
                pending_count_ff <= rst ? '0 : pending_count_next;
            end

`ifndef NO_DYNAMIC_ASSERTS
            // synopsys translate_off
            assert property (@(posedge clk) (!rst && callee_rden_out) |-> (pending_count_ff < ((2**PENDING_COUNT_WIDTH) - 1))) else $error ("%m context saver pending count overflow PENDING_COUNT_WIDTH");
            assert property (@(posedge clk) (!rst && callee_rden_out) |-> (pending_count_ff < PENDING_COUNT_THRESHOLD))        else $error ("%m context saver pending count overflow PENDING_COUNT_THRESHOLD");
            // synopsys translate_on
`endif

            if (CALLEE_VOID_RETURN_TYPE) begin :genVoidReturn
                always_comb begin
                    callee_rden_out = 1'b0;

                    pending_count_next = pending_count_ff;

                    if (!callee_empty_in) begin
                        // Consume one more output from the callee if it would not cause overflow
                        // Overflow is impossible in this case
                        // PENDING_COUNT_WIDTH is wide enough to ensure this
                        callee_rden_out = 1'b1;

                        // Increment pending_count_next if a new output was consumed
                        pending_count_next++;
                    end

                    callee_data_out = '0;

                    // Compare against pending_count_ff rather than pending_count_next for improved timing
                    // PENDING_COUNT_THRESHOLD is set to avoid bubbles in spite of this
                    if (valid_out) begin
                        pending_count_next = PENDING_COUNT_WIDTH'(pending_count_next - live_variable_control_fifo_data_out);
                    end
                end
            end
            else begin :genNonVoidReturn
                // For pipelined calls that return a value, this holds the array to be returned
                logic [RETURN_ARRAY_BUFFER_SIZE-1:0][RETURN_ELEMENT_WIDTH-1:0] returnArrayBuffer_ff;
                logic [RETURN_ARRAY_BUFFER_SIZE-1:0][RETURN_ELEMENT_WIDTH-1:0] returnArrayBuffer_next;
				logic [RETURN_ARRAY_BUFFER_SIZE-1:0][RETURN_ELEMENT_WIDTH-1:0] returnArrayBuffer_temp;
                logic [RETURN_ARRAY_BUFFER_SIZE-1:0][RETURN_ELEMENT_WIDTH-1:0] returnArrayBuffer_before_shift;

                always_comb begin
                    callee_rden_out = 1'b0;

                    returnArrayBuffer_next = returnArrayBuffer_ff;

                    pending_count_next = pending_count_ff;

                    if (!callee_empty_in) begin
                        // Consume one more output from the callee if it would not cause overflow
                        // Avoid wrapping pending_count_ff and avoid overflowing returnArrayBuffer_ff
                        if (pending_count_ff < PENDING_COUNT_THRESHOLD) begin
                            callee_rden_out = 1'b1;

                            returnArrayBuffer_next[pending_count_ff] = callee_data_in;
                        end

                        // Increment pending_count_next if a new output was consumed
                        if (callee_rden_out) begin
                            pending_count_next++;
                        end
                    end

                    callee_data_out = outputArrayType'(returnArrayBuffer_next[RETURN_ELEMENT_COUNT-1:0]);
                    // Ensure the synthesis tool doesn't infer loop-carried dependencies
                    // by explicitly making a copy of returnArrayBuffer_next
                    returnArrayBuffer_before_shift = returnArrayBuffer_next;

                    for(array_buffer_index_t ii = 0; ii < (RETURN_ARRAY_BUFFER_SIZE - 1); ii++) begin: gen_rab_temp
                        array_buffer_index_t idx;
                        idx = array_buffer_index_t'(ii + live_variable_control_fifo_data_out);
                        returnArrayBuffer_temp[ii] = returnArrayBuffer_before_shift[idx];
                    end

                    // Compare against pending_count_ff rather than pending_count_next for improved timing
                    // PENDING_COUNT_THRESHOLD is set to avoid bubbles in spite of this
                    if (valid_out) begin
                        pending_count_next -= live_variable_control_fifo_data_out;

                        // returnArrayBuffer could contain data for future threads
                        // shift results down
                        // (-1) here because there the last element either stays the same, or can have an undefined value

                        for (int unsigned i = 0; i < (RETURN_ARRAY_BUFFER_SIZE - 1); i++) begin
                            returnArrayBuffer_next[i] = returnArrayBuffer_temp[i];
                        end
                    end
                end

                always_ff @(posedge clk) begin
                    //synopsys sync_set_reset "rst"
                    returnArrayBuffer_ff <= returnArrayBuffer_next;
                end
            end
        end
        else begin :genNonPipelined
            always_comb begin
                fifo_rden = 1'b0;
                valid_out = 1'b0;
                callee_rden_out = 1'b0;

                callee_data_out = callee_data_in;

                if (output_rdy_in && !live_variable_fifos_empty && (!live_variable_control_fifo_data_out || !callee_empty_in)) begin
                    // Send results on output interface if data from enough threads has been received
                    valid_out = 1'b1;

                    // Dequeue from the internal FIFOs
                    fifo_rden = 1;

                    // Dequeue from callee if the call was not predicated (thread count = 1)
                    callee_rden_out = live_variable_control_fifo_data_out;
                end
            end
        end
    endgenerate

    // If IS_PIPELINED && !CALLEE_VOID_RETURN_TYPE, and the thread count exceeds RETURN_ELEMENT_COUNT
    // then the context saver will hang (because returnArrayBuffer will fill up, so pending_count_ff will be stack at PENDING_COUNT_THRESHOLD)
`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clk) (!rst && IS_PIPELINED && !CALLEE_VOID_RETURN_TYPE && caller_wren_in) |-> (live_variable_control_fifo_data_in <= RETURN_ELEMENT_COUNT)) else $error ("%m context saver hang due to high thread count");
    // synopsys translate_on
`endif
endmodule
