// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// When input signal goes high, the output signal goes high and stays high for DELAY_CYCLES cycles
module KanagawaHoldReset
#(
    parameter int unsigned DELAY_CYCLES = 1
)
(
    input wire clk,
    input wire rst_in,

    output logic rst_out
);
    // Initial values are used to ensure determinisic behavior on power up
    logic [$clog2(DELAY_CYCLES)-1:0] counter;
    logic counter_not_zero;

    assign rst_out = rst_in | counter_not_zero;

    always @(posedge clk) begin
        //synopsys sync_set_reset "rst_in"
        if (rst_in) begin
            counter <= $clog2(DELAY_CYCLES)'(DELAY_CYCLES - 1);
            counter_not_zero <= 1'b1;
        end
        else if (counter_not_zero) begin
            counter <= counter - 1'b1;
            counter_not_zero <= (counter != 1);
        end
        else begin
            counter <= $clog2(DELAY_CYCLES)'(0);
            counter_not_zero <= 1'b0;
        end
    end
endmodule

module KanagawaResetFlipFlopChain
#(
    parameter WIDTH,
    parameter int unsigned DEPTH,
    parameter logic INIT_VAL
)
(
    input wire clk,

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

    generate
        if(DEPTH > 0) begin: gen_registers
            logic [DEPTH-1:0][WIDTH-1:0] data_nxt, data;

            // KanagawaHALNonMergeableRegisters is used to ensure that the duplicate reset signals are not all collapsed into 1
            KanagawaHALNonMergeableRegisters
            #(
                .WIDTH(WIDTH),
                .DEPTH(DEPTH),
                .INIT_VAL({{DEPTH * WIDTH}{INIT_VAL}})
            )
            non_mergeable_reg
            (
                .clk        (clk),
                .data_in    (data_nxt),
                .data_out   (data)
            );

            // Slot[0] is the output slot
            assign data_out = data[0];

            always_comb begin
                // Initialize the input slots
                data_nxt[DEPTH-1] = data_in;

                // Pass values down the chain
                if (DEPTH > 1)
                begin
                    for (int unsigned i=0; i < (DEPTH - 1); i++) begin
                        data_nxt[i] = data[i+1];
                    end
                end
            end
        end
        else begin: gen_passthrough
            assign data_out = data_in;
        end
    endgenerate

endmodule

module KanagawaResetFlipFlopChainWithClear
#(
    parameter int unsigned WIDTH,
    parameter int unsigned DEPTH,
    parameter logic INIT_VAL = 1'b0
)
(
    input wire clk,
    input wire clr,

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

    generate
        if(DEPTH > 0) begin: gen_registers
            logic [DEPTH-1:0][WIDTH-1:0] data_nxt, data;

            // KanagawaHALNonMergeableRegistersWithClear is used to ensure that the duplicate reset signals are not all collapsed into 1
            KanagawaHALNonMergeableRegistersWithClear
            #(
                .WIDTH(WIDTH),
                .DEPTH(DEPTH),
                .INIT_VAL({{DEPTH * WIDTH}{INIT_VAL}})
            )
            non_mergeable_reg
            (
                .clk        (clk),
                .clr        (clr),
                .data_in    (data_nxt),
                .data_out   (data)
            );

            // Slot[0] is the output slot
            assign data_out = data[0];

            always_comb begin
                // Initialize the input slots
                data_nxt[DEPTH-1] = data_in;

                // Pass values down the chain
                if (DEPTH > 1)
                begin
                    for (int unsigned i=0; i < (DEPTH - 1); i++) begin
                        data_nxt[i] = data[i+1];
                    end
                end
            end
        end
        else begin: gen_passthrough
            assign data_out = data_in;
        end
    endgenerate

endmodule

module ResetFanOutTree
#(
    parameter WIDTH,
    parameter int unsigned DEPTH,
    parameter logic INIT_VAL
)
(
    input wire clk,

    input wire  data_in,
    output logic [WIDTH-1:0] data_out
);
    localparam LEFT_WIDTH = WIDTH / 2;
    localparam RIGHT_WIDTH = WIDTH - LEFT_WIDTH;

    generate
        if (DEPTH == 0) begin: gen_depth_eq_zero
            always_comb begin
                for (int unsigned i = 0; i < WIDTH; i++) begin
                    data_out[i] = data_in;
                end
            end
        end
        else begin: gen_depth_gt_zero
            // Fan out the input signal to 2 intermediate signals in 1 cycle
            logic [1:0] reset_delayed;

            KanagawaResetFlipFlopChain
            #(
                .WIDTH(2),
                .DEPTH(1),
                .INIT_VAL(INIT_VAL)
            ) fan_out
            (
                .clk(clk),

                .data_in({2{data_in}}),
                .data_out(reset_delayed)
            );

            // Recurse
            if (LEFT_WIDTH > 0) begin: gen_left
                ResetFanOutTree
                #(
                    .WIDTH(LEFT_WIDTH),
                    .DEPTH(DEPTH - 1),
                    .INIT_VAL(INIT_VAL)
                )
                left
                (
                    .clk(clk),
                    .data_in(reset_delayed[0]),
                    .data_out(data_out[LEFT_WIDTH-1:0])
                );
            end

            if (RIGHT_WIDTH > 0) begin: gen_right
                ResetFanOutTree
                #(
                    .WIDTH(RIGHT_WIDTH),
                    .DEPTH(DEPTH - 1),
                    .INIT_VAL(INIT_VAL)
                )
                right
                (
                    .clk(clk),
                    .data_in(reset_delayed[1]),
                    .data_out(data_out[WIDTH-1:LEFT_WIDTH])
                );
            end
        end
    endgenerate
endmodule

// When rst_in goes high,
// after DELAY_CYCLES, rst_delayed_out goes high and stays that way for HOLD_CYCLES
// It is assumed that rst_in is will be 1 on power-up
module KanagawaResetControl
#(
    parameter int unsigned WIDTH = 1,
    parameter int unsigned DELAY_CYCLES = 1,
    parameter int unsigned HOLD_CYCLES = 1,
    parameter int unsigned ADDTIONAL_LATENCY = 1,
    parameter int unsigned FAN_OUT_LEVELS = 0,
    parameter logic INIT_VAL = 1'b0
)
(
    input wire clk,
    input wire rst_in,

    // Consolidate required input signals to generate rst_and_startup_done_out
    // in the single ResetControl module
    input wire has_others_completed_in,
    output logic rst_and_startup_done_out,
    output logic [WIDTH-1:0] rst_delayed_out,
    output logic reset_sequence_finished_this_cycle_out
);
    logic reset_sequence_finished_this_cycle;
    logic reset_sequence_finished_ff;
    logic rst_and_startup_done_raw;

    // synopsys translate_off
    initial begin
        assert(FAN_OUT_LEVELS <= DELAY_CYCLES) else $error("%m: DELAY_CYCLES must not be less than FAN_OUT_LEVELS");
    end
    // synopsys translate_on

    // Generate a signal that goes high for HOLD_CYCLES cycles
    logic held_reset;
    KanagawaHoldReset
    #(
        .DELAY_CYCLES(HOLD_CYCLES)
    )  hold_reset_internal
    (
        .clk(clk),
        .rst_in(rst_in),

        .rst_out(held_reset)
    );

    // Use FAN_OUT_LEVELS clock cycles to fan out the reset signal
    // via a reset tree
    logic [WIDTH-1:0] rst_after_initial_fan_out;

    ResetFanOutTree
    #(
        .WIDTH(WIDTH),
        .DEPTH(FAN_OUT_LEVELS),
        .INIT_VAL(INIT_VAL)
    )
    fan_out
    (
        .clk(clk),

        .data_in(held_reset),
        .data_out(rst_after_initial_fan_out)
    );

    // Pipeline each reset signal for (DELAY_CYCLES - FAN_OUT_LEVELS) cycles
    // KanagawaResetFlipFlopChain is used to ensure that rst_delayed_out powers on to 1
    KanagawaResetFlipFlopChain
    #(
        .WIDTH(WIDTH),
        .DEPTH(DELAY_CYCLES - FAN_OUT_LEVELS),
        .INIT_VAL(INIT_VAL)
    ) flip_flop_chain
    (
        .clk(clk),

        .data_in(rst_after_initial_fan_out),
        .data_out(rst_delayed_out)
    );

    // Count cycles after a reset to know when to signal reset_sequence_finished_this_cycle_out
    localparam int unsigned TOTAL_CYCLES = HOLD_CYCLES + DELAY_CYCLES;

    logic [$clog2(TOTAL_CYCLES)-1:0] sequence_counter;

    // Initialize reset_sequence_finished_this_cycle to 1'b0 at power-on since
    // the parent module's rst_and_startup_done depends on this value
    always @(posedge clk) begin
        //synopsys sync_set_reset "rst_in"
        if (rst_in) begin
            sequence_counter <= $clog2(TOTAL_CYCLES)'(TOTAL_CYCLES - 1);
            reset_sequence_finished_this_cycle <= 1'b0;
            reset_sequence_finished_ff <= 1'b0;
        end
        else begin
            if (sequence_counter != 0)
                sequence_counter <= sequence_counter - 1'b1;
            reset_sequence_finished_this_cycle <= (sequence_counter == 1);
            reset_sequence_finished_ff <= (reset_sequence_finished_this_cycle ? 1'b1 : reset_sequence_finished_ff);
        end
    end

    // generate rst_and_startup_done_out
    assign rst_and_startup_done_raw = reset_sequence_finished_ff && has_others_completed_in;

    KanagawaResetFlipFlopChainWithClear
    #(
        .WIDTH      (1),
        .DEPTH      (ADDTIONAL_LATENCY),
        .INIT_VAL   (1'b0)
    )
    rst_and_startup_done_delay_chain
    (
        .clk        (clk),
        .clr        (rst_in),
        .data_in    (rst_and_startup_done_raw),
        .data_out   (rst_and_startup_done_out)
    );

    // reset_sequence_finished_this_cycle_out is required to trigger memory initialization
    assign reset_sequence_finished_this_cycle_out = reset_sequence_finished_this_cycle;

endmodule
