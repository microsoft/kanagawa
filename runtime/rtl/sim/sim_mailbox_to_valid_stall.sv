// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaSimMailboxToValidStall

    Takes data items from a mailbox and drives an interface in which data
    is consumed by the downstream module whenever valid_out is
    asserted, and the downstream module may stop the flow of data by asserting
    internal_stall. There may be STALL_VALID_LATENCY cycles of delay before changes to
    internal_stall have effect.

    The module has an additional parameter, STALL_POLICY, that can be used to
    inject random stalls in the output interface.
*/

module KanagawaSimMailboxToValidStall
import KanagawaSimStallerPolicies::*;
#(
    parameter type T,
    parameter DEPTH = 0, // Limits size of mailbox - add_item will block if exceeded! Set to zero for no limit
    parameter STALL_VALID_LATENCY = 0,
    parameter CLEAR_ON_RESET = 1,
    parameter type STALL_POLICY = NullStallPolicy,
    parameter STALLER_SEED = 0
)
(
    input  wire         clk,
    input  wire         rst,

    output wire         valid_out,
    output T            data_out,
    input  wire         stall_in
);

    logic internal_stall;
    logic ready;
    KanagawaSimMailboxWriter #(T, DEPTH) mb_if(clk);
    logic valid_ff = 1'b0;

    KanagawaSimStaller
    #(
        .STALL_POLICY   (STALL_POLICY),
        .SEED           (STALLER_SEED)
    ) staller
    (
        .clk                (clk),
        .rst                (rst),
        .stalled_out        (internal_stall),
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
            if (mb_if.num() > 0 && ready && !internal_stall) begin
                T new_data;
                bit result;

                result = mb_if.internal_try_get(new_data);
                assert(result);
                valid_ff <= 1'b1;
                data_out <= new_data;
            end
            else begin
                valid_ff <= 1'b0;
                data_out <= 'x;
            end
        end
    end

    assign valid_out = ~rst & valid_ff;

    KanagawaShiftRegWithClear
    #(
        .WIDTH      (1),
        .DELAY      (STALL_VALID_LATENCY)
    ) ready_sr
    (
        .clk        (clk),
        .clr        (rst),
        .in         (~stall_in),
        .out        (ready)
    );


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


endmodule // KanagawaSimMailboxToValidStall
