// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


// Module that counts the number of cycles where a data race occurs

`default_nettype none

module KanagawaRaceCounterStage
import KanagawaTypes::*;
#(
    parameter INPUT_WIDTH,
    parameter OUTPUT_WIDTH,
    parameter REDUCTION_SIZE
)
(
    input wire clk,

    // 1 bit for each race condition that could occur
    // each bit is set to 1 on a cycle where the race occurs
    input wire [INPUT_WIDTH-1:0] data_in,

    // Counter that increments on each cycle where a race condition occcurs
    output logic [OUTPUT_WIDTH-1:0] data_out
);
    // Note that there is no reset here - this assumes that while reset is held high
    // data_in will settle down to '0
    always_ff @(posedge clk) begin
        for (int i = 0; i < OUTPUT_WIDTH; i++) begin
            data_out[i] <= |(data_in[i * REDUCTION_SIZE +: REDUCTION_SIZE]);
        end
    end

endmodule

module KanagawaRaceCounter
import KanagawaTypes::*;
#(
    parameter COUNTER_WIDTH = 8,
    parameter INPUT_WIDTH = 8,
    parameter REDUCTION_SIZE = 2,
    parameter DEPTH = 3
)
(
    input wire clk,
    input wire rst,

    // 1 bit for each race condition that could occur
    // each bit is set to 1 on a cycle where the race occurs
    input wire [INPUT_WIDTH-1:0] race_occured_in,

    // Counter that increments on each cycle where a race condition occcurs
    output logic [COUNTER_WIDTH-1:0] race_count_out
);

    // Pipelined reduction tree of the input
    logic race_reduction;

    genvar depth;

    generate
        for (depth = 0; depth < DEPTH; depth++) begin : genDepth

            localparam STAGE_INPUT_WIDTH = INPUT_WIDTH / (REDUCTION_SIZE**depth);
            localparam STAGE_OUTPUT_WIDTH = STAGE_INPUT_WIDTH / REDUCTION_SIZE;

            logic [STAGE_INPUT_WIDTH-1:0] stage_input;
            logic [STAGE_OUTPUT_WIDTH-1:0] stage_output;

            if (depth == 0) begin
                assign stage_input = race_occured_in;
            end else begin
                assign stage_input = genDepth[depth-1].stage_output;
            end

            if (depth == (DEPTH-1)) begin
                assign race_reduction = stage_output;
            end

            KanagawaRaceCounterStage
            #(
                .INPUT_WIDTH(STAGE_INPUT_WIDTH),
                .OUTPUT_WIDTH(STAGE_OUTPUT_WIDTH),
                .REDUCTION_SIZE(REDUCTION_SIZE)
            ) stage
            (
                .clk(clk),
                .data_in(stage_input),
                .data_out(stage_output)
            );

        end
    endgenerate

    // counter
    logic [COUNTER_WIDTH-1:0] race_count_ff;

    always_ff @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            race_count_ff <= '0;
        end
        else begin
            race_count_ff <= (race_count_ff + race_reduction);
        end
    end

    assign race_count_out = race_count_ff;

endmodule
