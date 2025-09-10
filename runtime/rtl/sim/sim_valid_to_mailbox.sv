// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


// Implements a module that exposes an interface with simple valid flow
// control and which writes the input data to a mailbox. The module uses
// strong data typing based on the parameter, T, that you supply.

// For convenience, the module provides several tasks that can be used to
// retrieve items from the mailbox, check the mailbox state, etc.

module KanagawaSimValidToMailbox
#(
    parameter type T,
    parameter CLEAR_ON_RESET = 1
)
(
    input  wire         clk,
    input  wire         rst,

    input  wire         valid_in,
    input  T            data_in
);

    KanagawaSimMailboxReader #(T) mb_if(clk);

    always_ff @(posedge clk) begin
        if (rst) begin
            if (CLEAR_ON_RESET) begin
                mb_if.clear();
            end
        end
        else begin
            if (valid_in) begin
                bit result;
                result = mb_if.internal_try_put(data_in);
                assert(result);
            end
        end
    end // always_ff @ (posedge clk)


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

    function automatic void clear();
       mb_if.clear();
    endfunction

endmodule // KanagawaSimValidToMailbox
