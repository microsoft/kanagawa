// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSimFifoWriteToMailbox

    Implements the write side interface of a mock FIFO which uses a mailbox as
    the backing store. The FIFO uses strong data typing based on the
    parameter, T, that you supply.

    The module has an additional parameter, STALL_POLICY, that can be used to
    inject random stalls via the full/almost_full output. Note that the actual
    output is stalled the cycle after the internal stall signal is asserted
    and wrreq_in is also asserted (to simulate realistic behavior of a FIFO).

    For convenience, the module provides several tasks that can be used to
    retrieve items from the mailbox, check the mailbox state, etc.
*/

module KanagawaSimFifoWriteToMailbox
import KanagawaSimStallerPolicies::*;
#(
    parameter DEPTH,
    parameter type T,
    parameter ALMOSTFULL_ENTRIES = 0,
    parameter CLEAR_ON_RESET = 1,
    parameter type STALL_POLICY = NullStallPolicy,
    parameter STALLER_SEED = 0
)
(
    input  wire         clk,
    input  wire         rst,

    input  wire         wrreq_in,
    input  T            data_in,
    output logic        full_out,
    output logic        almost_full_out
);

    localparam ALMOSTFULL_THRESHOLD = DEPTH - ALMOSTFULL_ENTRIES;

    logic   stall;
    KanagawaSimMailboxReader #(T, DEPTH) mb_if(clk);

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

    always_ff @(posedge clk) begin
        if (rst) begin
            if (CLEAR_ON_RESET) begin
                mb_if.clear();
                full_out <= 1'b0;
                almost_full_out <= 1'b0;
            end
            else begin
                full_out <= mb_if.num() >= DEPTH;
                almost_full_out <= mb_if.num() >= ALMOSTFULL_THRESHOLD;
            end
        end
        else begin
            if (wrreq_in) begin
                bit result;

                result = mb_if.internal_try_put(data_in);
                assert(result) else $error("Overflow");

                if (stall) begin
                    full_out <= 1'b1;
                    almost_full_out <= 1'b1;
                end
                else begin
                    full_out <= mb_if.num() >= DEPTH;
                    almost_full_out <= mb_if.num() >= ALMOSTFULL_THRESHOLD;
                end
            end
            else if (!(full_out && stall)) begin
                full_out <= mb_if.num() >= DEPTH;
                almost_full_out <= mb_if.num() >= ALMOSTFULL_THRESHOLD;
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

    function automatic int is_empty();
        return mb_if.num() == 0;
    endfunction

    function automatic void clear();
       mb_if.clear();
    endfunction

endmodule // KanagawaSimFifoWriteToMailbox
