// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSimMailboxToValid

    Takes data items from a mailbox and drives an interface that outputs data
    and a valid strobe.

    The module has an additional parameter, STALL_POLICY, that can be used to
    inject random stalls in the output interface.

    For convenience, the module provides several tasks that can be used to
    place items in the mailbox, check the mailbox state, etc.
*/

module KanagawaSimMailboxToValid
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

    output wire         valid_out,
    output T            data_out
);

    KanagawaSimMailboxWriter #(T, DEPTH) mb_if(clk);
    logic stall;
    logic valid_ff = 1'b0;


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
                valid_ff <= 1'b0;
                data_out <= 'x;
            end
        end
        else begin
            if (stall || mb_if.num() == 0) begin
                valid_ff <= 1'b0;
                data_out <= 'x;
            end
            else begin
                T new_data;
                bit result;

                result = mb_if.internal_try_get(new_data);
                assert(result);
                valid_ff <= 1'b1;
                data_out <= new_data;
            end
        end
    end

    assign valid_out = ~rst & valid_ff;


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

endmodule // KanagawaSimMailboxToValid
