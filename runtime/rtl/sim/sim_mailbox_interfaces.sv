// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Interface definitions for mailbox adapters.  These let you pass
// around a virtual interface in SV class code to accesses a mailbox
// adapter module.

interface KanagawaSimMailboxReader
   #(
     parameter type T,
     parameter DEPTH = 0
     )
   (
    input logic clk
    );

    mailbox #(T) mb_items = new(DEPTH);

   //Try to put item in mailbox.  returns success status. For use by KanagawaSim* modules
   function automatic bit internal_try_put(input T data);
      int result;
      result = mb_items.try_put(data);
      return result != 0;
   endfunction // internal_try_put

   //Put item in mailbox or fail.  For use by KanagawaSim* modules
   function automatic void internal_must_put(ref T data);
      int result;
      result = mb_items.try_put(data);
      assert(result != 0) else $error("[%0t] Overflow", $time);
   endfunction // internal_must_put

   // User Visible API

   //blocking pop
   task automatic get(ref T item);
      mb_items.get(item);
   endtask

   //Nonblocking pop.  returns whether item is popped.
   function automatic bit try_get(ref T item);
      return 0 != mb_items.try_get(item);
   endfunction

   //Try to get an item for timeout_cycles cycles.  Sets timed_out to 1 if no item popped.
   task automatic get_with_timeout(input int timeout_cycles, ref T item, output bit timed_out);
      do begin
         if (0 != mb_items.try_get(item)) begin
            timed_out = 1'b0;
            return;
         end
         wait_cycle();
      end while (timeout_cycles-- != 0);
      timed_out = 1'b1;
   endtask

   //returns whether the mailbox is empty
   function automatic bit is_empty();
      return mb_items.num() == 0;
   endfunction // empty

   //Returns number of items in the mailbox
   function automatic int num();
      return mb_items.num();
   endfunction

   //Empty the mailbox
   function automatic void clear();
      T dummy;
      while (mb_items.try_get(dummy) != 0);
   endfunction // clear

   //Wait for one cycle on the clock associated with this adapter
   task automatic wait_cycle();
      @(posedge clk);
   endtask // wait_cycle

endinterface


interface KanagawaSimMailboxWriter
   #(
     parameter type T,
     parameter DEPTH = 0
     )
   (
    input logic clk
    );

   mailbox #(T) mb_items = new(DEPTH);

   //Try to get item in mailbox, non-blocking.  returns success status. For use by KanagawaSim* modules
   function automatic bit internal_try_get(ref T item);
      if(mb_items.try_get(item) != 0) begin //a little verbose, but hold out for the competion version
         //internal_complete();
         return '1;
      end
      return '0;
   endfunction


   //User Visible API

   function automatic bit try_put(T item);
      return mb_items.try_put(item) != 0;
   endfunction

   task automatic put(input T item);
      mb_items.put(item);
   endtask

   function automatic int num();
      return mb_items.num();
   endfunction

   function automatic bit empty();
      return mb_items.num() == 0;
   endfunction

   function automatic void clear();
      T dummy;
      //FIXME: should clear trigger completions?
      while (mb_items.try_get(dummy) != 0);
   endfunction

   task automatic wait_cycle();
      @(posedge clk);
   endtask // wait_cycle

endinterface
