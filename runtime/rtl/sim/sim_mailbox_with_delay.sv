// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

// Implements a mailbox that adds a configurable delay after put/try_put before the item
// shows up as available for get/try_get.

module KanagawaSimMailboxWithDelay
#(
    parameter type T,
    parameter DEPTH = 0,
    parameter VERBOSE = 0
);

    mailbox #(T) mb_items;
    mailbox #(time) mb_times;

    initial begin
        if (DEPTH != 0) begin
            mb_items = new(DEPTH);
        end
        else begin
            mb_items = new;
        end
        mb_times = new;
    end

    function automatic bit try_put(input T item, input time tdelay);
        if (mb_items.try_put(item)) begin
            time tvalid, now;

            now = $time;
            tvalid = now + tdelay;
            if (VERBOSE) begin
                $display("[%0t] > try_put %p: item valid @ %0t", now, item, tvalid);
            end
            assert(mb_times.try_put(tvalid));

            return 1'b1;
        end

        return 1'b0;
    endfunction

    task automatic put(input T item, input time tdelay);
        time tdelta, tvalid, now;

        mb_items.put(item);

        now = $time;
        tvalid = now + tdelay;
        if (VERBOSE) begin
            $display("[%0t] > put %p: item valid @ %0t", now, item, tvalid);
        end
        mb_times.put(tvalid);
    endtask

    task automatic get(ref T item);
        time tvalid, now, tdelta;

        mb_items.get(item);
        mb_times.get(tvalid);
        now = $time;

        if (VERBOSE) begin
            $display("[%0t]  | start get %p: item valid at %0t", now, item, tvalid);
        end
        if (tvalid > now) begin
            tdelta = tvalid - now;
            if (VERBOSE) begin
                $display("[%0t]  | %p waiting for %0t", $time, item, tdelta);
            end
            #(tdelta);
        end
        if (VERBOSE) begin
            $display("[%0t]  < get %p", $time, item);
        end
    endtask

    function automatic bit try_get(ref T item);
        time tvalid, now;
        bit success;

        if (!mb_times.try_peek(tvalid)) begin
            return 1'b0;
        end

        now = $time;
        if (tvalid > now) begin
            return 1'b0;
        end

        success = mb_items.try_get(item);
        if (success) begin
            assert(mb_times.try_get(tvalid));
            assert(tvalid <= now);

            if (VERBOSE) begin
                $display("[%0t]  | try_get %p", now, item);
            end
        end

        return success;
    endfunction

endmodule // KanagawaSimMailboxWithDelay
