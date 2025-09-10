// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Reconfigurable arbiter with the arbitration probability on port #0 of 1/RATIO, where RATIO > 1
// Does not starve either input
// Supports transactions (sticking with 1 input until the end of a transaction
// is signaled)
`default_nettype none
module KanagawaArbitrationChainNode
#(
    parameter WIDTH = 8,

    // If true, then once an input port has been selected
    // it must remain selected until the end of a transaction
    parameter IS_TRANSACTIONAL = 1'b0,

    // Offset of 1 bit within the data that indicates
    // the end of a transaction
    parameter END_TRANSACTION_OFFSET = 1'b0,

    // The probability of request grant on port #0 is 1/RATIO
    parameter int unsigned RATIO = 2
)
(
    input wire clk,
    input wire rst,

    input wire[1:0][WIDTH-1:0] data_in,
    input wire [1:0] empty_in,
    output logic [1:0] rden_out,

    output logic [WIDTH-1:0] data_out,
    output logic wren_out,
    input wire full_in
);

    // Counter threshold is (RATIO - 1)
    localparam int unsigned THRESH = RATIO - 1;
    localparam int unsigned DW_THRESH = $clog2(RATIO);

    // DW_RATIO-bit value for priority counter for port #0
    logic [DW_THRESH-1:0] priority_ff;
    logic [DW_THRESH-1:0] priority_next;
    logic priority_ff_eq_thresh;
    assign priority_ff_eq_thresh = (priority_ff == DW_THRESH'(THRESH));

    typedef enum logic [1:0]
    {
        TS_NO_TRANSACTION = 2'b00,
        TS_TRANSACTION_PORT_0 = 2'b01,
        TS_TRANSACTION_PORT_1 = 2'b10
    } transaction_state_t;

    transaction_state_t transaction_state_ff, transaction_state_next;

    always_ff @(posedge clk) begin
        priority_ff <= rst ? DW_THRESH'('d0) : priority_next;
        transaction_state_ff <= (rst || !IS_TRANSACTIONAL) ? TS_NO_TRANSACTION : transaction_state_next;
    end

    // Input selection
    logic selected_input;

    logic end_transaction;
    assign end_transaction = IS_TRANSACTIONAL ? data_in[selected_input][END_TRANSACTION_OFFSET] : 1'b1;

    logic [5:0] combined_state;
    assign combined_state = {full_in, empty_in[1], empty_in[0], transaction_state_ff, priority_ff_eq_thresh};

    always_comb begin
        casez (combined_state)

            // data on input 0 only, not in a transaction
            6'b01000?: begin selected_input = 1'b0; wren_out = 1'b1; end

            // data on input 0 only, TS_TRANSACTION_PORT_0
            6'b01001?: begin selected_input = 1'b0; wren_out = 1'b1; end

            // data on input 1 only, not in a transaction
            6'b00100?: begin selected_input = 1'b1; wren_out = 1'b1; end

            // data on input 1 only, TS_TRANSACTION_PORT_1
            6'b00110?: begin selected_input = 1'b1; wren_out = 1'b1; end

            // data on both inputs, TS_NO_TRANSACTION - break ties with priority_ff
            6'b000000: begin selected_input = 1'b1; wren_out = 1'b1; end
            6'b000001: begin selected_input = 1'b0; wren_out = 1'b1; end

            // data on both inputs, TS_TRANSACTION_PORT_0
            6'b00001?: begin selected_input = 1'b0; wren_out = 1'b1; end

            // data on both inputs, TS_TRANSACTION_PORT_1
            6'b00010?: begin selected_input = 1'b1; wren_out = 1'b1; end

            default: begin selected_input = 1'b0; wren_out = 1'b0; end
        endcase

        // Assign read-enable signals
        rden_out[0] = 1'b0;
        rden_out[1] = 1'b0;

        rden_out[selected_input] = wren_out;

        // Mux connects input and output data
        data_out = data_in[selected_input];

        // Update transaction state
        transaction_state_next = transaction_state_ff;

        // Update port priority
        priority_next = priority_ff;

        if (wren_out) begin
            transaction_state_next = end_transaction ? TS_NO_TRANSACTION : transaction_state_t'(TS_TRANSACTION_PORT_0 + selected_input);
            priority_next = end_transaction ? (priority_ff_eq_thresh ? DW_THRESH'('d0) : (priority_ff + 1'b1)) : priority_ff;
        end
    end
endmodule
