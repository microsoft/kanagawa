// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


// Implements a module that exposes an interface with ready/valid flow
// control and which writes the input data to a mailbox. The module uses
// strong data typing based on the parameter, T, that you supply.

// An optional parameter, DEPTH, puts an upper bound on the size of the
// mailbox and will deassert the ready_out signal when the mailbox is full.

// The module also has an additional parameter, STALL_POLICY, that can be used to
// inject artificial stalls in ready_out, and which is useful for constrained
// random testing. Note that because of the way the module is implemented,
// assertion of the internally generated stalls will not impact ready_out
// until the subsequent clock cycle.

// For convenience, the module provides several tasks that can be used to
// retrieve items from the mailbox, check the mailbox state, etc.

module KanagawaSimReadyValidToMailbox
import KanagawaSimStallerPolicies::*;
#(
    parameter type T,
    parameter DEPTH = 0,
    parameter CLEAR_ON_RESET = 1,
    parameter type STALL_POLICY = NullStallPolicy,
    parameter STALLER_SEED = 0
)
(
    input  wire         clk,
    input  wire         rst,

    output logic        ready_out,
    input  wire         valid_in,
    input  T            data_in
);

    logic stall;
    logic [$clog2(DEPTH):0] last_depth_ff; // For use by testbench to prevent a race condition

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
            ready_out <= 1'b0;
            if (CLEAR_ON_RESET) begin
                mb_if.clear();
            end
        end
        else begin
            last_depth_ff <= mb_if.num();
            ready_out <= ~stall && ~is_full_next_cycle();

            if (valid_in && ready_out) begin
                bit result;
                result = mb_if.internal_try_put(data_in);
                assert(result) else $error("Overflow");
            end
        end
    end


   // User Visible API
   // Mirrors Interface API

    function automatic bit is_full_next_cycle();
        if (DEPTH == 0) begin
            return 1'b0;
        end

        return ((mb_if.num() + (valid_in & ready_out)) >= DEPTH);
    endfunction

    task automatic get(ref T item);
        mb_if.get(item);
    endtask

    function automatic bit try_get(ref T item);
        return mb_if.try_get(item);
    endfunction

    function automatic int num();
        return mb_if.num();
    endfunction

    task automatic get_with_timeout(input int timeout_cycles, ref T item, output bit timed_out);
       mb_if.get_with_timeout(timeout_cycles, item, timed_out);
    endtask

    function automatic void clear();
       mb_if.clear();
    endfunction

endmodule // KanagawaSimReadyValidToMailbox
