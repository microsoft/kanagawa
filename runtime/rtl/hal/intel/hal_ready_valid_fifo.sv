// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

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
    pd_fifo_intf.IN             input_intf,
    output wire [LOG_DEPTH:0]   input_usedw,
    output wire                 input_almost_full,   // Does not apply for dual clock

    input wire                  output_clk,
    pd_fifo_intf.OUT            output_intf
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
    
    wire input_full;
    assign input_intf.ready = ~input_full;

    wire output_empty;
    assign output_intf.valid = ~output_empty;

    generate
        if (DUAL_CLOCK) begin
            wire output_rst;
            KanagawaHALResetSynchronizer rst_sync
            (
                .clk(output_clk),
                .arst(input_rst),
                .rst(output_rst)
            );

            // Instance name MUST be "pd_dcfifo_component" so that constraints in KanagawaHALDcFifoTiming.sdc will be applied
            dcfifo 
            #(
                .add_usedw_msb_bit("ON"),
                .intended_device_family("Stratix 10"),
                .lpm_numwords(DEPTH),
                .lpm_showahead("ON"),
                .lpm_type("dcfifo"),
                .lpm_width(WIDTH),
                .lpm_widthu(LOG_DEPTH+1),
                .overflow_checking("ON"),
                .rdsync_delaypipe(5),
                .read_aclr_synch("ON"),
                .underflow_checking("ON"),
                .use_eab(USE_LUTRAM ? "OFF" : "ON"),
                .ram_block_type(USE_LUTRAM ? "MLAB" : "M20K"),
                .lpm_hint("DISABLE_DCFIFO_EMBEDDED_TIMING_CONSTRAINT=TRUE"),
                .write_aclr_synch("ON"),
                .wrsync_delaypipe(5)
            )
            pd_dcfifo_component
            (
                .rdclk      (output_clk),
                .wrreq      (input_intf.valid),
                .aclr       (input_rst),
                .data       (input_intf.data),
                .rdreq      (output_intf.ready),
                .wrclk      (input_clk),
                .wrempty    (),
                .wrfull     (input_full),
                .q          (output_intf.data),
                .rdempty    (output_empty),
                .rdfull     (),
                .wrusedw    (input_usedw),
                .rdusedw    (),
                .eccstatus  ()
            );
        end
        else begin
            scfifo
            #(
                .lpm_numwords(DEPTH),
                .lpm_width(WIDTH),
                .lpm_widthu(LOG_DEPTH),
                .lpm_showahead("ON"),
                .overflow_checking("ON"),
                .underflow_checking("ON"),
                .use_eab(USE_LUTRAM ? "OFF" : "ON"),
                .almost_full_value(ALMOST_FULL_THRESHOLD)
            )
            fifo
            (
                .clock(input_clk),
                .sclr(input_rst),
                .aclr(input_rst),

                .wrreq(input_intf.valid),
                .full(input_full),
                .almost_full(input_almost_full),
                .almost_empty(),
                .data(input_intf.data),
                .usedw(input_usedw[LOG_DEPTH-1:0]),

                .rdreq(output_intf.ready),
                .empty(output_empty),
                .q(output_intf.data),

                .eccstatus()
            );
            assign input_usedw[LOG_DEPTH] = input_full;
        end
    endgenerate

endmodule
