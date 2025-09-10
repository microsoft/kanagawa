// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*
Module: KanagawaHALReadyValidFifo

    This module implements both single and dual clock FIFOs for Xilinx devices.  It is
    distinct from the other HAL FIFO primitives mainly in that it implements the ready/valid
    flow control protocol without introducing any additional combinational logic on the
    inputs or outputs.  In some cases, such an implementation results in better timing
    characteristics.  It does this by making use of the over/underflow protection that
    is built into the Xilinx FIFOs.

    -------------------------------------
    Parameters:

    LOG_DEPTH
        The power of 2 depth of the desired FIFO.  Non-powers of 2 are not supported
        by this component, though are possible using different implementation techniques.

    USE_LUTRAM
        0 if the user would like the FIFO to be inmplemented in block RAM
        1 if the user would ike the FIFO to be implemented in distributed (CLB) memory

    ALMOST_FULL_THRESHOLD
        The input_almost_full signal will be asserted when the number of entries in
        the FIFO is >= this parameter.  The input_almost_full signal is derived from
        the prog_full signal coming out of the Xilinx primitives, but this signal
        has bounds on allowed values that are not present in the Intel implementation.
        These bounds are calculated and rolled up into the ADJUSTED_ALMOST_FULL
        parameter.  The result is that the input_almost_full signal may be asserted
        a few entries earlier than the ALMOST_FULL_THRESHOLD parameter specified by
        the user.

    DUAL_CLOCK
        0 if the same clock is used for input and output interfaces (uses input_clk)
        1 if the input and output interfaces use different clocks
    -------------------------------------
    Interface signals:

    input_clk
        Input interface clock for dual clock operation, and functions as both the
        input and output clock for single clock operation

    input_rst
        Input interface reset, and in fact the only reset for both sides

    input_intf
        Encapsulates the data transfer interface and consists of the valid, ready and
        data signals for the input.  The width of the data is made explicit in the
        declaration of the pd_fifo_intf declaration instance  that is instantiated
        for KanagawaHALReadyValidFifo instance.  Data transfer occurs on every cycle
        in which both ready and valid are asserted.

    input_usedw
        Returns the number of FIFO entries that are currently in use.  For single clock
        operation, this value should be up to date as of the previous clock cycle.  For
        dual clock operation, this value may lead or lag the true value by a bit as
        values are added or removed and cross clock domains.

    input_almost_full
        Asserted high whenever the number of entries in the FIFO is >= the
        ALMOST_FULL_THRESHOLD paramter specified by the user.  Ideally, this is valid for
        single clock operation on the cycle AFTER the ALMOST_FULL_THRESHOLD entry is clocked
        into the FIFO, and is deasserted the cycle AFTER the ALMOST_FULL_THRESHOLD entry
        is remove.  However, Xilinx takes an extra cycle to update the value, so the
        module subtracts 1 from ALMOST_FULL_THRESHOLD and uses ALMOST_FULL_THRESHOLD_MINUS_ONE
        as the programmable value.  This avoids the situation where input_almost_full is updated
        in the second cycle after the ALMOST_FULL_THRESHOLD is crossed.  Further,
        input_almost_full is not valid for the dual clock case.  It could be made to work,
        but isn't needed for current use cases.

    output_clk
        The clock used for the output interface in the dual clock case.  Ignored otherwise.

    output_intf
        Just like input_intf, but for the output side.  In theory, the READ_DATA_WIDTH
        could allow the output interface width to be different from the input interface
        width, but this is not supported now due to compiler issues.  See the comment
        at the beginning of the module.

    The implementation parameterizes more functionality than is exposed in the interface
    in order to match the interfaces exposed by the other architectures, while perhaps
    future proofing against further iterations of the standard interface.  For example,
    the Xilinx FIFOs support the concept of different write and read widths.

Authors:
    - Ray Bittner (raybit@microsoft.com)
*/
`default_nettype none

module KanagawaHALReadyValidFifo
#(
    parameter DEPTH,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH,
    parameter USE_LUTRAM,
    parameter ALMOSTFULL_ENTRIES = 0, // Must be 0 for dual-clock
    parameter DUAL_CLOCK
)
(
    input wire                  input_clk,
    input wire                  input_rst,
    pd_fifo_intf                input_intf,
    output wire [LOG_DEPTH:0]   input_usedw,
    output wire                 input_almost_full,   // Does not apply for dual clock

    input wire                  output_clk,
    pd_fifo_intf                output_intf
);
    localparam ALMOST_FULL_THRESHOLD = DEPTH - ALMOSTFULL_ENTRIES;

    initial begin
        if (LOG_DEPTH != $clog2(DEPTH)) begin
            $error("%m: LOG_DEPTH not set correctly");
        end

        if (DEPTH != (2**LOG_DEPTH)) begin
            $error("%m: DEPTH must be a power of 2");
        end

        if ((DUAL_CLOCK != 0) && (ALMOSTFULL_ENTRIES != 0)) begin
            $error("%m: ALMOSTFULL_ENTRIES must be zero when DUAL_CLOCK=1");
        end
    end

    // It should be possible to have different write and read widths by specifying the data width for the instance declarations of
    //  the input and output interfaces.  However, both Modelsim 2019.1 and Vivado 2019.1 have compiler problems with the correct
    //  syntax if that is done.  And they are in different parts of the code.  So for now, the input and output widths must be the
    //  same, and the code below assumes that.  In the future, it is expected that these problems will be fixed in the respective
    //  compilers, and the code below can be changed to enable different I/O widths.  The places that have been changed are marked
    //  with the "Change for different I/O widths" along with the compiler that throws the error.

    // Xilinx requires that PROG_FULL_THRESH be within MIN and MAX.
    // From Xilinx Ultrascale Architecture Libraries Guide
    localparam FIFO_WRITE_DEPTH = DEPTH;
    // It should be possible to do this:
// Change for different I/O widths (Vivado 2019.1):
    //      localparam WRITE_DATA_WIDTH = WIDTH;
    //      localparam READ_DATA_WIDTH = WIDTH;
    localparam WRITE_DATA_WIDTH = 1;
    localparam READ_DATA_WIDTH = 1;

    localparam FIFO_READ_DEPTH = FIFO_WRITE_DEPTH * WRITE_DATA_WIDTH / READ_DATA_WIDTH;
    localparam READ_MODE = "fwft";
    localparam READ_MODE_VAL = (READ_MODE == "std") ? 0 : 1;
    localparam CDC_SYNC_STAGES = 2;
    localparam XILINX_ALMOST_FULL_MIN = 3 + (READ_MODE_VAL*2*(FIFO_WRITE_DEPTH/FIFO_READ_DEPTH)) + (DUAL_CLOCK ? CDC_SYNC_STAGES : 0);
    localparam XILINX_ALMOST_FULL_MAX = (FIFO_WRITE_DEPTH-3) - (READ_MODE_VAL*2*(FIFO_WRITE_DEPTH/FIFO_READ_DEPTH));

    // It takes one extra clock cycle after the final write for the Xilinx prog_full flag to update.  So lower the almost full threshold by
    //  one value to account for that.  It means that one entry in the FIFO will be wasted, but it's better than losing a value due to an overflow.
    localparam ALMOST_FULL_THRESHOLD_MINUS_ONE = ALMOST_FULL_THRESHOLD - 1;

    localparam ADJUSTED_ALMOST_FULL_LOWER_CLAMP = (XILINX_ALMOST_FULL_MIN > ALMOST_FULL_THRESHOLD_MINUS_ONE) ? XILINX_ALMOST_FULL_MIN : ALMOST_FULL_THRESHOLD_MINUS_ONE;
    localparam ADJUSTED_ALMOST_FULL = (ADJUSTED_ALMOST_FULL_LOWER_CLAMP > XILINX_ALMOST_FULL_MAX) ? XILINX_ALMOST_FULL_MAX : ADJUSTED_ALMOST_FULL_LOWER_CLAMP;

    wire input_full;
    assign input_intf.ready = ~input_full;

    // Check parameters
    initial begin
        assert(CDC_SYNC_STAGES >= 2 && CDC_SYNC_STAGES <= 8) else begin
            $error( "%m: CDC_SYNC_STAGES = %u, must be >= 2 && <= 8", CDC_SYNC_STAGES );
        end
        assert(FIFO_WRITE_DEPTH >= 16 && FIFO_WRITE_DEPTH <= 4194304) else begin
            $error( "%m: FIFO_WRITE_DEPTH = %u, must be >= 16 && <= 4194304", FIFO_WRITE_DEPTH );
        end
        assert(FIFO_READ_DEPTH >= 16 && FIFO_READ_DEPTH <= 4194304) else begin
            $error( "%m: FIFO_READ_DEPTH = %u, must be >= 16 && <= 4194304", FIFO_READ_DEPTH );
        end
    end

    generate
        if (DUAL_CLOCK) begin
            xpm_fifo_async
            #(
                .FIFO_WRITE_DEPTH(FIFO_WRITE_DEPTH),
                .PROG_FULL_THRESH(ADJUSTED_ALMOST_FULL),
// Change for different I/O widths (Modelsim 2019.1):
//                .READ_DATA_WIDTH(WIDTH),
                .READ_DATA_WIDTH(WIDTH),
                .READ_MODE(READ_MODE),
                .CDC_SYNC_STAGES(CDC_SYNC_STAGES),
                .FULL_RESET_VALUE(1),   // Ensures that full, prog_full and almost_full are asserted during the extended reset (wr_rst_busy and rd_rst_busy)
                .FIFO_READ_LATENCY(0),
                .WRITE_DATA_WIDTH(WIDTH),
                .WR_DATA_COUNT_WIDTH(LOG_DEPTH+1),
                .FIFO_MEMORY_TYPE(USE_LUTRAM ? "distributed" : "block"),
                .USE_ADV_FEATURES( "1006" )
            )
            fifo
            (
                .wr_clk(input_clk),
                .rst(input_rst),
                .wr_en(input_intf.valid),
                .full(input_full),
                .prog_full(input_almost_full),
                .din(input_intf.data),
                .wr_data_count(input_usedw),

                .rd_clk(output_clk),
                .rd_en(output_intf.ready),
                .data_valid(output_intf.valid),
                .dout(output_intf.data),

                .dbiterr(),
                .sbiterr(),
                .injectdbiterr(),
                .injectsbiterr(),
                .almost_empty(),
                .empty(),
                .prog_empty(),
                .rd_data_count(),
                .almost_full(),
                .wr_ack(),
                .rd_rst_busy(),
                .wr_rst_busy(),
                .overflow(),
                .underflow(),
                .sleep()
            );
        end
        else begin
            xpm_fifo_sync
            #(
                .FIFO_WRITE_DEPTH(FIFO_WRITE_DEPTH),
                .PROG_FULL_THRESH(ADJUSTED_ALMOST_FULL),
// Change for different I/O widths (Modelsim 2019.1):
//                .READ_DATA_WIDTH(WIDTH),
                .READ_DATA_WIDTH(WIDTH),
                .READ_MODE(READ_MODE),
                .FULL_RESET_VALUE(1),   // Ensures that full, prog_full and almost_full are asserted during the extended reset (wr_rst_busy and rd_rst_busy)
                .FIFO_READ_LATENCY(0),
                .WRITE_DATA_WIDTH(WIDTH),
                .WR_DATA_COUNT_WIDTH(LOG_DEPTH+1),
                .FIFO_MEMORY_TYPE(USE_LUTRAM ? "distributed" : "block"),
                .USE_ADV_FEATURES( "1006" )
            )
            fifo
            (
                .wr_clk(input_clk),
                .rst(input_rst),
                .wr_en(input_intf.valid),
                .full(input_full),
                .prog_full(input_almost_full),
                .din(input_intf.data),
                .wr_data_count(input_usedw),

                .rd_en(output_intf.ready),
                .data_valid(output_intf.valid),
                .dout(output_intf.data),

                .dbiterr(),
                .sbiterr(),
                .injectdbiterr(),
                .injectsbiterr(),
                .almost_empty(),
                .empty(),
                .prog_empty(),
                .rd_data_count(),
                .almost_full(),
                .wr_ack(),
                .rd_rst_busy(),
                .wr_rst_busy(),
                .overflow(),
                .underflow(),
                .sleep()
            );
        end
    endgenerate

endmodule
