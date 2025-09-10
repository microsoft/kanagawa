// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSimFifoReadToMailbox

    Interfaces with the read side interface of a FIFO to read values out of
    the FIFO and redirect them to a mailbox. Uses strong data typing based on
    the parameter, T, that you supply.

    The module also has an additional parameter, STALL_POLICY, that can be used to
    inject artificial stalls in ready_out, and which is useful for constrained
    random testing.

    For convenience, the module provides several tasks that can be used to
    retrieve items from the mailbox, check the mailbox state, etc.
*/

module KanagawaSimFifoReadToMailbox
import KanagawaSimStallerPolicies::*;
#(
    parameter type T = logic,
    parameter DEPTH = 0,
    parameter CLEAR_ON_RESET = 1,
    parameter type STALL_POLICY = NullStallPolicy,
    parameter STALLER_SEED = 0
)
(
    input  wire     clk,
    input  wire     rst,

    output wire     rdreq_out,
    input  wire     empty_in,
    input  T        data_in
);


    KanagawaSimMailboxReader #(T, DEPTH) mb_if(clk);

    logic mb_full_ff;
    logic stall;

    KanagawaSimStaller
    #(
        .STALL_POLICY   (STALL_POLICY),
        .SEED           (STALLER_SEED)
    ) staller
    (
        .clk                (clk),
        .rst                (rst),
        .stalled_out        (stall),
        .not_stalled_out    ()
    );

    assign rdreq_out = ~rst & ~mb_full_ff & ~stall & ~empty_in;

    always_ff @(posedge clk) begin
        mb_full_ff <= (DEPTH != 0) & (mb_if.num() >= DEPTH);

        if (rst) begin
            if (CLEAR_ON_RESET) begin
                mb_if.clear();
            end
        end
        else begin
            if (rdreq_out) begin
                bit result;

                result = mb_if.internal_try_put(data_in);
                assert(result) else $error("%m Overflow");
            end
        end
    end


   // User Visible API
   // Mirrors Interface API

    task automatic get(ref T item);
       mb_if.get(item);
    endtask

    function automatic bit try_get(ref T item);
        return mb_if.try_get(item);
    endfunction

    task automatic get_with_timeout(input int timeout_cycles, ref T item, output bit timed_out);
       mb_if.get_with_timeout(timeout_cycles, item, timed_out);
    endtask

    function automatic int num();
        return mb_if.num();
    endfunction

    function automatic bit is_empty();
       return mb_if.is_empty();
    endfunction

    function automatic void clear();
       mb_if.clear();
    endfunction

endmodule // KanagawaSimFifoReadToMailbox
