// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
	Package: KanagawaSimTaskHelpers

	Contains miscellaneous classes, functions, or tasks useful in the writing
	of SystemVerilog testbenches.
*/
`timescale 1ps/1ps

package KanagawaSimTaskHelpers;


	// Helper class for testbenches that have one or more tasks that compare the
	// output of an interface with expected values retrieved from a mailbox. This
	// class wraps the mailbox items and exposes a simple shutdown mechanism that
	// can be used to shut down the task once all expected values have been
	// checked.

	class KanagawaSimMailboxWithShutdown #(type T) ;
	    typedef struct
	    {
	        bit shutdown;
	        T   item;
	    } mb_type_t;

	    local bit shutdown_posted = 1'b0; // This is solely for use in catching errors where items are posted after shutdown

	    mailbox #(mb_type_t) mb_items = new;

	    task automatic get(output T item, output bit shutdown);
	        mb_type_t x;
	        mb_items.get(x);
	        item = x.item;
	        shutdown = x.shutdown;
	    endtask

	    function automatic bit try_get(output T item, output bit shutdown);
	        mb_type_t x;

	        if (!mb_items.try_get(x)) begin
	            shutdown = 1'b0;
	            return 1'b0;
	        end

	        item = x.item;
	        shutdown = x.shutdown;
	        return 1'b1;
	    endfunction

	    task automatic put(input T item);
	        mb_type_t x;
	        assert(!shutdown_posted);
	        x.item = item;
	        x.shutdown = 1'b0;
	        mb_items.put(x);
	    endtask

	    function automatic bit try_put(input T item);
	        mb_type_t x;
	        assert(!shutdown_posted);
	        x.item = item;
	        x.shutdown = 1'b0;
	        return mb_items.try_put(x);
	    endfunction

	    task automatic peek_shutdown(output bit shutdown);
	    	mb_type_t x;

	    	mb_items.peek(x);
	    	shutdown = x.shutdown;
	    endtask

	    task automatic shutdown();
	        mb_type_t x;
	        x.shutdown = 1'b1;
	        shutdown_posted = 1'b1;
	        mb_items.put(x);
	    endtask

	    function automatic bit is_empty();
	    	return mb_items.num() == 0;
	    endfunction

	    task automatic wait_empty();
	    	wait(mb_items.num() == 0);
	    endtask

	    task automatic reset();
	    	mb_type_t x;

	    	while (mb_items.try_get(x));

	    	shutdown_posted = 1'b0;
	    endtask
	endclass

endpackage
