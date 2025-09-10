// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

// Single clock version
module KanagawaSingleClockFifoDebug_tb
(
);
    KanagawaSingleClockFifoDebug_tb_inst inst
    ();
endmodule

module KanagawaSingleClockFifoDebug_tb_inst
();
	logic clk = 1'b0;
	logic rst = 1'b1;

    // Interface with dut
    logic wrreq = 1'b0;
    logic full = 1'b0;

    logic rdreq = 1'b0;
    logic empty = 1'b0;

    logic overflow;
    logic underflow;

    // Generate clock
    always #5 clk = ~clk;

    initial begin
        $info("Starting single clock fifo debug test");

        // wait for reset to propagate
        for(int i=0; i < 50; i=i+1) @(posedge clk);

        // clear reset signal
        rst <= 1'b0;
        @(posedge clk);

        // Bits should initially be 0
        assert(overflow == 1'b0) else $error("overflow not initially 0");
        assert(underflow == 1'b0) else $error("underflow not initially 0");

        // wren should not cause overflow
        wrreq <= 1'b1;
        @(posedge clk);
        wrreq <= 1'b0;
        @(posedge clk);

        assert(overflow == 1'b0) else $error("overflow not 0 after write");
        assert(underflow == 1'b0) else $error("underflow not 0 after write");

        // wren & full should cause overflow
        wrreq <= 1'b1;
        full <= 1'b1;
        @(posedge clk);
        wrreq <= 1'b0;
        full <= 1'b0;
        @(posedge clk);

        assert(overflow == 1'b1) else $error("overflow not 1 after write during full");
        assert(underflow == 1'b0) else $error("underflow not 0 after write during full");

        // rdreq should not cause underflow
        rdreq <= 1'b1;
        @(posedge clk);
        rdreq <= 1'b0;
        @(posedge clk);

        assert(overflow == 1'b1) else $error("overflow not 1 after read");
        assert(underflow == 1'b0) else $error("underflow not 0 after read");

        // rdreq & empty should cause underflow
        rdreq <= 1'b1;
        empty <= 1'b1;
        @(posedge clk);
        rdreq <= 1'b0;
        empty <= 1'b0;
        @(posedge clk);

        assert(overflow == 1'b1) else $error("overflow not 1 after read during empty");
        assert(underflow == 1'b1) else $error("underflow not 1 after read during empty");

        // Reset should clear
        rst <= 1'b1;
        @(posedge clk);
        rst <= 1'b0;
        @(posedge clk);

        assert(overflow == 1'b0) else $error("overflow not 0 after reset");
        assert(underflow == 1'b0) else $error("underflow not 0 after reset");

        $info("SUCCESS");
        $finish;
    end

	KanagawaSingleClockFifoDebug
    dut
    (
        .clk(clk),
        .rst(rst),

        .full_in(full),
        .wren_in(wrreq),

        .empty_in(empty),
        .rden_in(rdreq),

        .overflow_out(overflow),
        .underflow_out(underflow)
    );
endmodule


// Dual clock version
module KanagawaDualClockFifoDebug_tb
(
);
    KanagawaDualClockFifoDebug_tb_inst inst
    ();
endmodule

module KanagawaDualClockFifoDebug_tb_inst
();
	logic wr_clk = 1'b0;
    logic wr_rst = 1'b1;

	logic rd_clk = 1'b0;
    logic rd_rst = 1'b1;

    // Interface with dut
    logic wrreq = 1'b0;
    logic full = 1'b0;

    logic rdreq = 1'b0;
    logic empty = 1'b0;

    logic overflow;
    logic underflow;

    // Generate clocks
    always #5 wr_clk = ~wr_clk;
    always #4 rd_clk = ~rd_clk;

    initial begin
        $info("Starting dual clock fifo debug test");

        //////////////////////////////////////////////////////////////////////
        // Test write side

        // clear reset signal
        @(posedge wr_clk);
        wr_rst <= 1'b0;
        @(posedge wr_clk);

        // Bits should initially be 0
        assert(overflow == 1'b0) else $error("overflow not initially 0");

        // wren should not cause overflow
        wrreq <= 1'b1;
        @(posedge wr_clk);
        wrreq <= 1'b0;
        @(posedge wr_clk);

        assert(overflow == 1'b0) else $error("overflow not 0 after write");

        // wren & full should cause overflow
        wrreq <= 1'b1;
        full <= 1'b1;
        @(posedge wr_clk);
        wrreq <= 1'b0;
        full <= 1'b0;
        @(posedge wr_clk);

        assert(overflow == 1'b1) else $error("overflow not 1 after write during full");

        // Reset should clear
        wr_rst <= 1'b1;
        @(posedge wr_clk);
        wr_rst <= 1'b0;
        @(posedge wr_clk);

        assert(overflow == 1'b0) else $error("overflow not 0 after reset");

        //////////////////////////////////////////////////////////////////////
        // Test read side
        //////////////////////////////////////////////////////////////////////

        // clear reset signal
        rd_rst <= 1'b0;
        @(posedge rd_clk);

        // Bits should initially be 0
        assert(underflow == 1'b0) else $error("underflow not initially 0");

        // rdreq should not cause underflow
        rdreq <= 1'b1;
        @(posedge rd_clk);
        rdreq <= 1'b0;
        @(posedge rd_clk);

        assert(underflow == 1'b0) else $error("underflow not 0 after read");

        // rdreq & empty should cause underflow
        rdreq <= 1'b1;
        empty <= 1'b1;
        @(posedge rd_clk);
        rdreq <= 1'b0;
        empty <= 1'b0;
        @(posedge rd_clk);
        assert(underflow == 1'b1) else $error("underflow not 1 after read during empty");

        // Reset should clear
        rd_rst <= 1'b1;
        @(posedge rd_clk);
        rd_rst <= 1'b0;
        @(posedge rd_clk);

        assert(underflow == 1'b0) else $error("underflow not 0 after reset");

        $info("SUCCESS");
        $finish;
    end

	KanagawaDualClockFifoDebug
    dut
    (
        .wr_clk(wr_clk),
        .wr_rst(wr_rst),
        .wr_full_in(full),
        .wr_wren_in(wrreq),
        .wr_overflow_out(overflow),

        .rd_clk(rd_clk),
        .rd_rst(rd_rst),
        .rd_empty_in(empty),
        .rd_rden_in(rdreq),
        .rd_underflow_out(underflow)
    );
endmodule
