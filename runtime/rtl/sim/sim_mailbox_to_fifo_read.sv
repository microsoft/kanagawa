// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSimMailboxToFifoRead

    Implements the read side interface of a mock FIFO which uses a mailbox as
    the backing store. The FIFO uses strong data typing based on the
    parameter, T, that you supply.

    The module has an additional parameter, STALL_POLICY, that can be used to
    inject random stalls in the input. Note that the actual output is stalled
    the cycle after the internal stall signal is asserted and rdreq_in is also
    asserted (to simulate realistic behavior of a FIFO).

    For convenience, the module provides several tasks that can be used to
    place items in the mailbox, check the mailbox state, etc.
*/

module KanagawaSimMailboxToFifoRead
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

    input  wire         rdreq_in,
    output T            rddata_out,
    output wire         rdempty_out
);

    KanagawaSimMailboxWriter #(T, DEPTH) mb_if(clk);
    logic rdempty_ff = 1'b1;
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

    // Feed IMC interface from mailbox
    always @(posedge clk) begin
        T item;

        if (rst) begin
            if (CLEAR_ON_RESET) begin
                mb_if.clear();
                rdempty_ff <= 1'b1;
                rddata_out <= 'x;
            end
        end
        else begin
            if (rdreq_in) begin
                rdempty_ff <= 1'b1;
                rddata_out <= 'x;
            end
            if (!stall && (rdempty_ff || rdreq_in)) begin
                if (mb_if.internal_try_get(item)) begin
                    rdempty_ff <= 1'b0;
                    rddata_out <= item;
                end
            end
        end
    end

    assign rdempty_out = rdempty_ff;


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

endmodule // KanagawaSimMailboxToFifoRead
