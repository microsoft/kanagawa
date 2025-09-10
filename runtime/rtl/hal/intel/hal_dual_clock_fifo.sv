// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Platform-specific dual-clock FIFO module
// Output has show-ahead behavior
module KanagawaHALDualClockFifo
#(
    parameter DEPTH,
    parameter LOG_DEPTH = $clog2(DEPTH),
    parameter WIDTH,
    parameter USEDW_WIDTH = LOG_DEPTH + 1,
    parameter ALMOSTFULL_ENTRIES,
    parameter USE_LUTRAM,
    parameter OVER_UNDER_FLOW_PROTECTION = 0
)
(
    input wire                       wrclk,
    input wire                       wrrst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     full,
    output logic                     almost_full,
    output logic [USEDW_WIDTH-1:0]   usedw,

    input wire                       rdclk,
    input wire                       rdrst,

    input wire                       rdreq,
    output logic                     empty,
    output logic [WIDTH-1:0]         q
);
    localparam ALMOSTFULL_VAL = DEPTH - ALMOSTFULL_ENTRIES;

    // The Intel dcfifo requires this one-cycle adjustment to avoid overflow
    localparam INTERNAL_ALMOSTFULL_VAL = ALMOSTFULL_VAL - 1;

    initial begin
        if (LOG_DEPTH != $clog2(DEPTH)) begin
            $error("%m: LOG_DEPTH not set correctly");
        end

        if (INTERNAL_ALMOSTFULL_VAL < 1) begin
            $error("%m: ALMOSTFULL_ENTRIES/DEPTH not set correctly (note: this module internally increases ALMOSTFULL_ENTRIES by one to accomadate the Intel dcfifo behavior)");
        end
    end

    assign usedw = '0;

    logic internal_wrreq;

    always_comb begin
        internal_wrreq = wrreq;

        // Before reset, wrreq from basic blocks can be 'x
        // In this case, KanagawaDualClockFifoWithWriteDelay will error out
        // Convert 'x to 1'b0 during simulation

        // synopsys translate_off
        if (internal_wrreq === 'x) begin
            internal_wrreq = 1'b0;
        end
        //synopsys translate_on
    end

    // pipeline write reset signal by 1 cycle
    // this is safe because of the reset control strategy
    // that holds reset high for enough cycles to clear all pipelines
    // This helps timing on Stratix 10 by creating a dedicated register that drives
    // and asynchronous clear signal (register that drive async clear cannot be retimed)
    logic wrrst_delayed;

    KanagawaFlipFlopChainNoEnable
    #(
        .WIDTH(1),
        .DEPTH(1)
    )
    delay_wr_rst
    (
        .clk(wrclk),
        .data_in(wrrst),
        .data_out(wrrst_delayed)
    );

    logic [LOG_DEPTH:0] internal_wrusedw;

    assign almost_full = wrrst_delayed ? 1'b1 : ((internal_wrusedw >= INTERNAL_ALMOSTFULL_VAL) | full);

    logic               internal_rdreq;
    logic               internal_rdempty;

    logic               rddata_valid_ff = 1'b0;
    logic               rddata_valid_nxt;

    always @(posedge rdclk) begin
        rddata_valid_ff <= rdrst ? 1'b0 : rddata_valid_nxt;
    end

    always_comb begin   // Questa/ModelSim generates warnings on always_comb in generate block
        // Defaults
        rddata_valid_nxt = rddata_valid_ff;

        internal_rdreq = ~rdrst & ~internal_rdempty & (rdreq | ~rddata_valid_ff);
        if (internal_rdreq) begin
            rddata_valid_nxt = 1'b1;
        end
        else if (rdreq) begin
            rddata_valid_nxt = 1'b0;
        end

        empty = !rddata_valid_ff;
    end

`ifndef NO_DYNAMIC_ASSERTS
// synopsys translate_off
    // Overflow / underflow checks

    // Delay overflow/underfflow checks until we see a valid reset assertion. This works around issue with clients that route reset through a flip-flop chain.
    logic overflow_assertion_enabled = 1'b0;
    always @(posedge wrclk) begin
        if (wrrst_delayed === 1'b1 && OVER_UNDER_FLOW_PROTECTION == 0) begin
            overflow_assertion_enabled <= 1'b1;
        end
    end
    logic underflow_assertion_enabled = 1'b0;
    always @(posedge rdclk) begin
        if (rdrst === 1'b1 && OVER_UNDER_FLOW_PROTECTION == 0) begin
            underflow_assertion_enabled <= 1'b1;
        end
    end

    assert property (@(posedge wrclk)  internal_wrreq |-> (!overflow_assertion_enabled || !full))
        else $error ("%m overflow");
    assert property (@(posedge rdclk)  rdreq |-> (!underflow_assertion_enabled || !empty))
        else $error ("%m underflow");
    assert property (@(posedge rdclk)  internal_rdreq |-> (!underflow_assertion_enabled || !internal_rdempty))
        else $error ("%m underflow");
// synopsys translate_on
`endif

    // Instance name MUST be "pd_dcfifo_component" so that constraints in KanagawaHALDcFifoTiming.sdc will be applied
    dcfifo #(
        .add_usedw_msb_bit("ON"),
        .intended_device_family("Stratix 10"),
        .lpm_numwords(DEPTH),
        .lpm_showahead("OFF"),
        .lpm_type("dcfifo"),
        .lpm_width(WIDTH),
        .lpm_widthu(LOG_DEPTH+1),
        .overflow_checking(OVER_UNDER_FLOW_PROTECTION ? "ON" : "OFF"),
        .rdsync_delaypipe(5),
        .read_aclr_synch("ON"),
        .underflow_checking(OVER_UNDER_FLOW_PROTECTION ? "ON" : "OFF"),
        .use_eab("ON"),
        .ram_block_type(USE_LUTRAM ? "MLAB" : "M20K"),
        .lpm_hint("DISABLE_DCFIFO_EMBEDDED_TIMING_CONSTRAINT=TRUE"),
        .write_aclr_synch("ON"),
        .wrsync_delaypipe(5)
    )
    pd_dcfifo_component
    (
        .rdclk      (rdclk),
        .wrreq      (internal_wrreq),
        .aclr       (wrrst_delayed),
        .data       (data),
        .rdreq      (internal_rdreq),
        .wrclk      (wrclk),
        .wrempty    (),
        .wrfull     (full),
        .q          (q),
        .rdempty    (internal_rdempty),
        .rdfull     (),
        .wrusedw    (internal_wrusedw),
        .rdusedw    (),
        .eccstatus  ()
    );

endmodule

