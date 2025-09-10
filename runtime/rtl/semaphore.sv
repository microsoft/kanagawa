// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Used to limit the number of threads within a region of code
module KanagawaSemaphore
#(
    parameter MAX_VALUE,
    parameter SEM_DELAY
)
(
    input wire clk,
    input wire rst,

    input wire incr_count_in,
    input wire decr_count_in,

    output logic full_out
);
    localparam LOG_DEPTH = $clog2(MAX_VALUE);

    // KanagawaFifoPtrs has requirements on WRITE_DELAY not being too large
    // Account for that here
    localparam ADJUSTED_DELAY = ((MAX_VALUE - (SEM_DELAY * 2)) >= 1) ? SEM_DELAY : 0;

    // Delay decrement operations by SEM_DELAY + 1
    // This ensures that underflow of fifo_ptrs won't occur
    // if incr_count_in and decr_count_in go high on the same cycle
    logic decr_count_delayed;

    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(1),
        .DEPTH(ADJUSTED_DELAY + 1)
    )
    delay_decr
    (
        .clk(clk),
        .data_in(decr_count_in),
        .data_out(decr_count_delayed)
    );

    KanagawaFifoPtrs
    #(
        .LOG_DEPTH(LOG_DEPTH),
        .WRITE_DELAY(ADJUSTED_DELAY),
        .MAX_SIZE(MAX_VALUE)
    )
    fifo_ptrs
    (
        .clk(clk),
        .rst(rst),

        .wrreq(incr_count_in),
        .almost_full(full_out),

        .rdreq(decr_count_delayed),
        .empty()
    );

`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clk) (!rst && full_out)  |-> !incr_count_in) else $error ("%m overflow at time %0t", $time);
    // synopsys translate_on
`endif
endmodule
