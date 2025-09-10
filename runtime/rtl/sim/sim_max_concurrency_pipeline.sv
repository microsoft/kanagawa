// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Simple module that is useful when doing things like throughput simulations
// when there is some downstream module that has a cap on the maximum number
// of requests it can handle concurrently. Acknowledge completion of an item
// via the ack_in port.

module KanagawaSimMaxConcurrencyPipelineWithAck
#(
    parameter type T,
    parameter integer MAX_CONCURRENCY = 0,
    parameter integer LATENCY = 0
)
(
    input  wire         clk,
    input  wire         rst,

    output logic        ready_out,
    input  wire         valid_in,
    input  T            data_in,

    output logic        valid_out,
    output T            data_out,
    input  wire         ready_in,

    input  wire         ack_in
);

    localparam integer ACTUAL_LATENCY = LATENCY == 0 ? MAX_CONCURRENCY : LATENCY;

    initial begin
        assert(LATENCY >= MAX_CONCURRENCY || 0 == LATENCY)
        else $error("LATENCY (%0d) must be >= MAX_CONCURRENCY (%0d)", LATENCY, MAX_CONCURRENCY);
    end

    localparam integer COUNT_WIDTH = MAX_CONCURRENCY == 0 ? 1 : $clog2(MAX_CONCURRENCY);

    typedef struct packed {
        T data;
        bit valid;
    } T_with_valid_t;

    T_with_valid_t data_with_valid, data_with_valid_delayed;

    logic [COUNT_WIDTH:0] in_flight_count;

    assert property (@(posedge clk) (!rst && ack_in)  |-> (in_flight_count != 0 || MAX_CONCURRENCY == 0)) else $error ("%m underflow");

    always_comb begin
        data_out = data_with_valid_delayed.data;
        valid_out = data_with_valid_delayed.valid;
        ready_out = ready_in && (MAX_CONCURRENCY == 0 || (in_flight_count < MAX_CONCURRENCY));
        data_with_valid = '{data: data_in, valid: (valid_in & ready_out)};
    end

    always_ff @(posedge clk) begin
        if (valid_in && ready_out && !ack_in) begin
            in_flight_count <= in_flight_count + 1'b1;
        end
        else if (ack_in && !(valid_in && ready_out)) begin
            in_flight_count <= in_flight_count - 1'b1;
        end
        // else in_flight_count unchanged

        if (rst) begin
            in_flight_count <= '0;
        end
    end

    KanagawaFlipFlopChainWithEnableAndReset
    #(
        .WIDTH  ($bits(T_with_valid_t)),
        .DEPTH  (ACTUAL_LATENCY)
    ) sr_delay
    (
        .clk        (clk),
        .rst        (rst),
        .enable     (ready_in),
        .data_in    (data_with_valid),
        .data_out   (data_with_valid_delayed)
    );

endmodule


// Simple module that is useful when doing things like throughput simulations
// when there is some downstream module that has a cap on the maximum number
// of requests it can handle concurrently. This module is identical to
// KanagawaSimMaxConcurrencyPipelineWithAck except that the acknowledgement
// is implied when the downstream module consumes a data item (ready_in
// and valid_out are both asserted).

module KanagawaSimMaxConcurrencyPipeline
#(
    parameter type T,
    parameter integer MAX_CONCURRENCY = 0,
    parameter integer LATENCY = 0
)
(
    input  wire         clk,
    input  wire         rst,

    output logic        ready_out,
    input  wire         valid_in,
    input  T            data_in,

    output logic        valid_out,
    output T            data_out,
    input  wire         ready_in
);


    KanagawaSimMaxConcurrencyPipelineWithAck
    #(
        .T                  (T),
        .MAX_CONCURRENCY    (MAX_CONCURRENCY),
        .LATENCY            (LATENCY)
    ) impl
    (
        .clk                (clk),
        .rst                (rst),

        .ready_out          (ready_out),
        .valid_in           (valid_in),
        .data_in            (data_in),

        .valid_out          (valid_out),
        .data_out           (data_out),
        .ready_in           (ready_in),

        .ack_in             (ready_in & valid_out)
    );
endmodule

