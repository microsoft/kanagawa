// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSimMailboxToFifoWrite

    Takes data items from a mailbox and drives an interface that looks like
    the write side of a FIFO.

    The module has an additional parameter, STALL_POLICY, that can be used to
    inject random stalls in the output interface.

    For convenience, the module provides several tasks that can be used to
    place items in the mailbox, check the mailbox state, etc.
*/

module KanagawaSimMailboxToFifoWrite
import KanagawaSimStallerPolicies::*;
#(
    parameter type T,
    parameter DEPTH = 0, // Limits size of mailbox - add_item will block if exceeded! Set to zero for no limit
    parameter CLEAR_ON_RESET = 1,
    parameter type STALL_POLICY = NullStallPolicy,
    parameter STALLER_SEED = 0
)
(
    input  wire         clk,
    input  wire         rst,

    output logic        wrreq_out,
    output T            wrdata_out,
    input  wire         wrfull_in
);

    logic   wrdata_valid_ff = 1'b0;
    logic   stall;
    KanagawaSimMailboxWriter #(T, DEPTH) mb_if(clk);

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

    // Feed output interface from mailbox
    always @(posedge clk) begin
        T item;

        if (rst) begin
            if (CLEAR_ON_RESET) begin
                mb_if.clear();
                wrdata_valid_ff <= 1'b0;
                wrdata_out <= 'x;
            end
        end
        else begin
            if (!stall && (mb_if.num() > 0) && (!wrfull_in || !wrdata_valid_ff)) begin
                T new_data;
                bit result;

                result = mb_if.internal_try_get(new_data);
                assert(result);
                wrdata_out <= new_data;
                wrdata_valid_ff <= 1'b1;
            end
            else if (!wrfull_in) begin
                wrdata_valid_ff <= 1'b0;
            end
        end
    end

    assign wrreq_out = wrdata_valid_ff & ~wrfull_in;


   //User Visible API

    function automatic int num();
        return mb_if.num();
    endfunction

    function automatic bit try_put(input T item);
        return  mb_if.try_put(item);
    endfunction

    task automatic put(input T item);
        mb_if.put(item);
    endtask

    function automatic void clear();
        mb_if.clear();
    endfunction

    function automatic bit is_empty();
        return mb_if.empty();
    endfunction

endmodule // KanagawaSimMailboxToFifoWrite
