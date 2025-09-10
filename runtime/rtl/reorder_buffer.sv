// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Module which exposes a fifo interface, but reorders threads
//

`default_nettype none

// Memory module used by KanagawReorderBuffer
// Dual port memory.  Writes are random access, reads are sequential.
// This module has 1 cycle read latency, but enable the memory output register (for timing closure)
// This is acomplished by an additional register that can hold 1 data element, and with bypass support
//
// This module is implemented as a state machine:
// State[0] is 1 when the data in the register is valid
// State[1] is 1 when the data in memory output register is valid
// State[2] is 1 when the read address was incremented last cycle, which will cause the memory output register to change next cycle
//
// Load Pending | Memory Output Reg Valid   | Data Reg valid
// State[2]     | State[1]                  | State[0]      | State index
//       0              0                           0             0 (STATE_IDLE)
//       0              0                           1             1 (invalid)
//       0              1                           0             2 (invalid)
//       0              1                           1             3 (STATE_FULL)
//       1              0                           0             4 (STATE_INITIAL_LOAD)
//       1              0                           1             5 (STATE_DATA_REG_VALID)
//       1              1                           0             6 (STATE_MEMORY_REG_VALID)
//       1              1                           1             7 (invalid)
module KanagawaReorderMemory
import KanagawaTypes::*;
#(
     parameter DATA_WIDTH = 16,
     parameter ADDR_WIDTH = 16,
     parameter DEVICE_FAMILY = "UNUSED"
)
(
     input wire clk,
     input wire rst,

     input wire rden_in,
     output logic [DATA_WIDTH-1:0] data_out,
     output logic valid_out,

     input wire wren_in,
     input wire [ADDR_WIDTH-1:0] writeaddr_in,
     input wire [DATA_WIDTH-1:0] data_in
);
    typedef enum logic [2:0]
    {
        STATE_IDLE = 3'd0,
        STATE_FULL = 3'd3,
        STATE_INITIAL_LOAD = 3'd4,
        STATE_DATA_REG_VALID = 3'd5,
        STATE_MEMORY_REG_VALID = 3'd6
    } state_t;

    typedef struct packed
    {
        state_t state;
        logic [DATA_WIDTH-1:0] data_reg;
        logic [ADDR_WIDTH-1:0] read_addr;
        logic valid;
    } regs_t;

    regs_t regs_ff, regs_next;

    // The address coressponding to the data that will be in data_reg on the next cycle
    // This is re-computed on each cycle
    logic [ADDR_WIDTH-1:0] data_reg_addr_next;

    logic [DATA_WIDTH-1:0] memory_output;

    // the underlying memory object
    // It is important that READ_WRITE_MODE is specified in this memory
    // because a reorder buffer can read and write
    // the same slot on the same cycle
    // BRAM is used because OLD_DATA mode only works with unregistered outputs if the type is BRAM
    // Bypass is used to handle cases where a write occurs on the same cycle as a read
    KanagawaSimpleDualPortRAM
    #(
        .DATA_WIDTH(DATA_WIDTH),
        .ADDR_WIDTH(ADDR_WIDTH),
        .USE_LUTRAM(0),
        .USE_BRAM(1),
        .USE_OUTPUT_REG(1),
        .NUM_BYPASS_SLOTS(2),
        .WRITE_DELAY(0),
        .DEVICE_FAMILY(DEVICE_FAMILY)
    )
    memory
    (
        .clk(clk),
        .rst(rst),

        .rden_in(1'b1),
        .readaddr_in(regs_next.read_addr),
        .data_out(memory_output),

        .wren_in(wren_in),
        .writeaddr_in(writeaddr_in),
        .data_in(data_in)
    );

    // valid_out is registered
    assign valid_out = regs_ff.valid;

    always_comb begin
        regs_next = regs_ff;

        data_out = 'x;

        data_reg_addr_next = 'x;

        case (regs_ff.state)
            // This state only occurs on reset
            // Trigger a read from the memory
            STATE_IDLE: begin
                regs_next.state = STATE_INITIAL_LOAD;
            end

            // This state only occurs the cycle after a reset
            // Wait for the 1st read to finish, and kick off the next read
            STATE_INITIAL_LOAD: begin
                regs_next.state = STATE_MEMORY_REG_VALID;
            end

            // The memory output register data is valid
            // The data output register is valid also
            // There is no read in flight
            // Display data output register on output
            // If rden goes high, then the FSM transitions to STATE_DATA_REG_VALID (moving the current memory output register contents into the data register)
            // If rden goes low, then the FSM stays in STATE_FULL
            STATE_FULL: begin
                data_out = regs_ff.data_reg;
                regs_next.state = rden_in ? STATE_DATA_REG_VALID : STATE_FULL;
                regs_next.data_reg = rden_in ? memory_output : regs_ff.data_reg;
                data_reg_addr_next = rden_in ? regs_ff.read_addr : ADDR_WIDTH'(regs_ff.read_addr - 1);
            end

            // The memory output register data is valid
            // The data output register is not
            // There is a read in flight
            // Display memory output register on output
            // If rden goes high, the FSM stays in this state (with the next memory element available next cycle)
            // If rden goes low, then transition to STATE_FULL (moving the current memory output register contents into the data register)
            STATE_MEMORY_REG_VALID: begin
                data_out = memory_output;
                regs_next.state = rden_in ? STATE_MEMORY_REG_VALID : STATE_FULL;
                regs_next.data_reg = rden_in ? regs_ff.data_reg : memory_output;
                data_reg_addr_next = ADDR_WIDTH'(regs_ff.read_addr - 1); // doesn't matter if rden = 1 (data register is invalid in STATE_MEMORY_REG_VALID)
            end

            // The data output register data is valid
            // The memory output register is not
            // There is a read in flight
            // Display data output register on output
            // If rden goes high, then the FSM goes to STATE_MEMORY_REG_VALID (with data from the in-flight load)
            // If rden goes low, then transition to STATE_FULL (keeping the data register contstant)
            STATE_DATA_REG_VALID: begin
                data_out = regs_ff.data_reg;
                regs_next.state = rden_in ? STATE_MEMORY_REG_VALID : STATE_FULL;
                data_reg_addr_next = ADDR_WIDTH'(regs_ff.read_addr - 1); // doesn't matter if rden = 1 (data register is invalid in STATE_MEMORY_REG_VALID)
            end
        endcase

        // Look at next state to determine if read pointer should be advanced
        if (regs_next.state[2]) begin
            regs_next.read_addr = regs_ff.read_addr + 1'b1;
        end

        // Update data register contents if a write occurs with a matching address
        if (wren_in & (writeaddr_in == data_reg_addr_next)) begin
            regs_next.data_reg = data_in;
        end

        // Determine valid_out for the next cycle
        regs_next.valid = 1'b0;

        case (regs_next.state)
            STATE_IDLE: begin
                regs_next.valid = 1'b0;
            end

            STATE_INITIAL_LOAD: begin
                regs_next.valid = 1'b0;
            end

            STATE_FULL: begin
                regs_next.valid = 1'b1;
            end

            STATE_MEMORY_REG_VALID: begin
                regs_next.valid = 1'b1;
            end

            STATE_DATA_REG_VALID: begin
                regs_next.valid = 1'b1;
            end
        endcase
    end

    always_ff @(posedge clk) begin
        regs_ff <= regs_next;

        //synopsys sync_set_reset "rst"
        if (rst) begin
            regs_ff.state <= STATE_IDLE;
            regs_ff.valid <= 1'b0;
            regs_ff.read_addr <= '1; // The startup sequence requires the read address to be -1 when in STATE_IDLE
        end
    end

`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clk) (!rst && !valid_out)  |-> !rden_in) else $error ("%m underflow");
    // synopsys translate_on
`endif
endmodule


module KanagawaReorderBuffer
import KanagawaTypes::*;
#(
    parameter WIDTH = 16,
    parameter DEPTH = 16,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter SLOT_ID_OFFSET = 0,
    parameter SLOT_ID_WIDTH = 4,
    parameter USEDW_WIDTH = LOG_DEPTH + 1,
    parameter DEVICE_FAMILY = "UNUSED"
)
(
    // Parameters match FPGACommon FIFO
    input wire                       clock,
    input wire                       rst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     full,
    output logic                     almost_full,
    output logic [USEDW_WIDTH-1:0]   usedw,
    output logic                     overflow_out,

    input wire                       rdreq,
    output logic                     empty,
    output logic                     almost_empty,
    output logic [WIDTH-1:0]         q,
    output logic                     underflow_out
);

    // synopsys translate_off
    initial begin
        // This module requires a power of 2 depth
        if (DEPTH != 2**LOG_DEPTH) begin
            $error("%m: DEPTH/LOG_DEPTH not set correctly");
        end

        if (USEDW_WIDTH != (LOG_DEPTH + 1)) begin
            $error("%m: USEDW_WIDTH not set correctly");
        end
    end
    // synopsys translate_on

    // Sticky bits that track overflow/underflow
    KanagawaSingleClockFifoDebug debug
    (
        .clk(clock),
        .rst(rst),

        .full_in(full),
        .wren_in(wrreq),
        .overflow_out(overflow_out),

        .empty_in(empty),
        .rden_in(rdreq),
        .underflow_out(underflow_out)
    );

    // not used
    assign almost_empty = 1'b0;

    // Outputs that are not used by callers
    assign usedw = 0;

    // 1-bit per slot memory which tracks if the thread is ready to go
    // Initialized to 1
    // The first time through, a value of 0 means that the thread is ready
    // The second time through, a value of 1 indicates the thread is ready
    // The meaing of the value flips on each time around the buffer
    logic slot_flag_memory_data_write_value;
    logic [LOG_DEPTH-1:0] slot_flag_memory_data_write_addr;
    logic slot_flag_memory_data_wren;

    logic slot_flag_memory_data_rden;
    logic slot_flag_memory_data_valid;
    logic slot_flag_memory_data_read_data;

    KanagawaReorderMemory
    #(
        .DATA_WIDTH(1),
        .ADDR_WIDTH(LOG_DEPTH),
        .DEVICE_FAMILY(DEVICE_FAMILY)
    )
    slot_flag_memory
    (
        .clk(clock),
        .rst(rst),

        .rden_in(slot_flag_memory_data_rden),
        .data_out(slot_flag_memory_data_read_data),
        .valid_out(slot_flag_memory_data_valid),

        .wren_in(slot_flag_memory_data_wren),
        .writeaddr_in(slot_flag_memory_data_write_addr),
        .data_in(slot_flag_memory_data_write_value)
    );

    // DATA_WIDTH per slot memory which holds per-thread live variables
    // Note that this is a KanagawaHALSimpleDualPortRAM rather than a KanagawaReorderMemory
    // KanagawaReorderMemory would work, but consumes more area
    // Output register is disabled, which allows immediate response to a read request
    // Bypass support is unnecessary, because data is only read out of this memory
    // after the slot_flag_memory indicates that it is safe
    logic [WIDTH-1:0] slot_data_memory_data_write_value;
    logic [LOG_DEPTH-1:0] slot_data_memory_data_write_addr;
    logic slot_data_memory_data_wren;

    logic [LOG_DEPTH-1:0] slot_data_memory_data_read_addr;
    logic [WIDTH-1:0] slot_data_memory_data_read_data;

    KanagawaSimpleDualPortRAM
    #(
        .DATA_WIDTH(WIDTH),
        .ADDR_WIDTH(LOG_DEPTH),
        .USE_LUTRAM(0),
        .USE_BRAM(1),
        .USE_OUTPUT_REG(0),
        .DEVICE_FAMILY(DEVICE_FAMILY)
    )
    slot_data_memory
    (
        .clk(clock),
        .rst(rst),

        .rden_in(1'b1),
        .readaddr_in(slot_data_memory_data_read_addr),
        .data_out(slot_data_memory_data_read_data),

        .wren_in(slot_data_memory_data_wren),
        .writeaddr_in(slot_data_memory_data_write_addr),
        .data_in(slot_data_memory_data_write_value)
    );

    logic slot_data_fifo_ptrs_wren;
    logic slot_data_fifo_ptrs_wren_delayed;

    logic [LOG_DEPTH-1:0] slot_data_fifo_ptrs_read_addr;

    // To improve timing, delay writes into slot_data_memory_data_fifo_ptrs
    // No need to track full/almost full here
    // The reorder buffer is sized to handle the maximum number of requests that could possibly be in flight
    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(1),
        .DEPTH(KANAGAWA_LARGE_AND_FAST_WRITE_DELAY)
    )
    fifo_ptrs_delay_write
    (
        .clk(clock),

        .data_in(slot_data_fifo_ptrs_wren),
        .data_out(slot_data_fifo_ptrs_wren_delayed)
    );

    KanagawaFifoPtrsEx
    #(
        .DEPTH(DEPTH),
        .LOG_DEPTH(LOG_DEPTH)
    )
    slot_data_memory_data_fifo_ptrs
    (
        .clk(clock),
        .rst(rst),

        .wrreq_in(slot_data_fifo_ptrs_wren_delayed),
        .full_out(),
        .almost_full_out(),
        .wrptr_out(),

        .usedw_out(),

        .rdreq_in(rdreq),
        .empty_out(empty),
        .almost_empty_out(),
        .rdptr_out(slot_data_fifo_ptrs_read_addr)
    );

    logic [LOG_DEPTH:0] num_slots_initialized;
    logic initialized;
    assign initialized = (num_slots_initialized == DEPTH);

    // The input data is 2 fields
    // The most significant bit is the value to write into the slot memory
    // It is 0 the first time through the buffer, 1 the next time through, and alternates
    // The reset of the bits the index into the slot memory
    logic [SLOT_ID_WIDTH-1:0] input_slot_id;
    assign input_slot_id = data[SLOT_ID_WIDTH+SLOT_ID_OFFSET-2:SLOT_ID_OFFSET]; // -2 to not get the most significant bit

    logic input_slot_flag_value;
    assign input_slot_flag_value = data[SLOT_ID_WIDTH+SLOT_ID_OFFSET-1:SLOT_ID_WIDTH+SLOT_ID_OFFSET-1];

    always_comb begin
        if (initialized) begin
            // When new data comes in, write the data to slot_data_memory, and set the bit in slot_flag_memory indicating that the
            // data for this thread is ready
            slot_flag_memory_data_write_value = input_slot_flag_value;
            slot_flag_memory_data_write_addr = LOG_DEPTH'(input_slot_id);
            slot_flag_memory_data_wren = wrreq;
        end
        else begin
            // After a reset - initialize all slots in slot_flag_memory 1
            // During this process, mark the FIFO as full so that threads do not try to write
            // into the FIFO before it is initialized
            slot_flag_memory_data_write_value = 1'b1;
            slot_flag_memory_data_write_addr = LOG_DEPTH'(num_slots_initialized);
            slot_flag_memory_data_wren = 1'b1;
        end

        // Also write to slot_data
        slot_data_memory_data_write_value = data;
        slot_data_memory_data_write_addr = LOG_DEPTH'(input_slot_id);
        slot_data_memory_data_wren = wrreq;
    end

    // The compiler ensures that the buffer is large enough to hold the function maximum thread count
    // So the only reason to set the *full signals is when the initialization process has not finished
    assign almost_full = ~initialized;
    assign full = ~initialized;

    always_ff@(posedge clock) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            num_slots_initialized <= 0;
        end
        else begin
            if (~initialized) begin
                num_slots_initialized <= (num_slots_initialized + 1'b1);
            end
        end
    end

    // Monitor slot_flag_memory.  If the oldest thread is ready to go, then release it into the internal FIFO
    // read_pointer has 1 "extra" bit.  This is used to determine which value (1 or 0) indicates that a thread is ready to go
    // Note that this is complicated by the fact that the reads from the slot memory are registered
    logic [SLOT_ID_WIDTH-1:0] read_pointer_ff, read_pointer_next;

    // Extract the most significant bit
    logic ready_value;
    assign ready_value = read_pointer_ff[SLOT_ID_WIDTH-1:SLOT_ID_WIDTH-1];

    assign q = slot_data_memory_data_read_data;

    always_comb begin
        read_pointer_next = read_pointer_ff;

        slot_flag_memory_data_rden = 1'b0;

        slot_data_fifo_ptrs_wren = 1'b0;

        slot_data_memory_data_read_addr = slot_data_fifo_ptrs_read_addr;

        // Only do this processing if the slot memory has been initialized
        if (initialized) begin

            // Check the output of the slot_flag memory against the ready value
            if (slot_flag_memory_data_valid & (slot_flag_memory_data_read_data == ready_value)) begin
                // Record that another thread is ready to be dispatched
                slot_data_fifo_ptrs_wren = 1'b1;

                // Notify the slot_flag_memory_data that a read is occuring
                slot_flag_memory_data_rden = 1'b1;

                // Increment the read pointer
                read_pointer_next = read_pointer_ff + 1'b1;
            end

            // If a read is requested, then increment the read address for slot_data_memory_data
            // so that the proper data will be shown on the next cycle
            if (rdreq) begin
                slot_data_memory_data_read_addr = slot_data_fifo_ptrs_read_addr + 1'b1;
            end
        end
    end

    always_ff@(posedge clock) begin
        //synopsys sync_set_reset "rst"
        read_pointer_ff <= rst ? 0 : read_pointer_next;
    end

endmodule

