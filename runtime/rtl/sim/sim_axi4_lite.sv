// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

`ifdef QUESTA
`default_nettype none
`define WT wire
`else
`define WT
`endif

module KanagawaSimAxi4Lite
#(
    parameter integer ADDR_WIDTH,
    parameter integer DATA_WIDTH
)
(
    input wire                      clk,
    input wire                      rst,
    
    input  wire                     awready,
    output wire                     awvalid,
    output wire [ADDR_WIDTH-1:0]    awaddr,

    input  wire                     wready,
    output wire                     wvalid,
    output wire [DATA_WIDTH-1:0]    wdata,
    output wire [DATA_WIDTH/8-1:0]  wstrb,

    output wire                     bready,
    input  wire                     bvalid,
    input  wire [1:0]               bresp,

    input  wire                     arready,
    output wire                     arvalid,
    output wire [ADDR_WIDTH-1:0]    araddr,

    output wire                     rready,
    input  wire                     rvalid,
    input  wire [DATA_WIDTH-1:0]    rdata,
    input  wire [1:0]               rresp
);

    typedef logic [ADDR_WIDTH-1:0] Address_t;
    typedef logic [DATA_WIDTH-1:0] Data_t;
    typedef logic [1:0] Resp_t;

    typedef struct packed
    {
        logic [DATA_WIDTH/8-1:0] strb;
        Data_t data;
    } WFlit_t;

    typedef struct packed
    {
        Resp_t resp;
        Data_t data;
    } RFlit_t;

    WFlit_t wflit;
    RFlit_t rflit;

    assign wdata = wflit.data;
    assign wstrb = wflit.strb;

    assign rflit.data = rdata;
    assign rflit.resp = rresp;

    KanagawaSimMailboxToReadyValid
    #(
        .T                  (Address_t)
    ) mb_aw
    (
        .clk                (clk),
        .rst                (rst),

        .ready_in           (awready),
        .valid_out          (awvalid),
        .data_out           (awaddr)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T                  (WFlit_t)
    ) mb_w
    (
        .clk                (clk),
        .rst                (rst),

        .ready_in           (wready),
        .valid_out          (wvalid),
        .data_out           (wflit)
    );

    KanagawaSimReadyValidToMailbox
    #(
        .T                  (Resp_t)
    ) mb_b
    (
        .clk                (clk),
        .rst                (rst),

        .ready_out          (bready),
        .valid_in           (bvalid),
        .data_in            (bresp)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T                  (Address_t)
    ) mb_ar
    (
        .clk                (clk),
        .rst                (rst),

        .ready_in           (arready),
        .valid_out          (arvalid),
        .data_out           (araddr)
    );

    KanagawaSimReadyValidToMailbox
    #(
        .T                  (RFlit_t)
    ) mb_r
    (
        .clk                (clk),
        .rst                (rst),

        .ready_out          (rready),
        .valid_in           (rvalid),
        .data_in            (rflit)
    );

    task write(input logic [ADDR_WIDTH-1:0] addr, input logic [DATA_WIDTH-1:0] data, input logic [DATA_WIDTH/8-1:0] strb, output logic [1:0] resp, input int timeout_cycles, output bit timed_out);
        WFlit_t wflit;

        wflit.strb = strb;
        wflit.data = data;

        mb_aw.put(addr);
        mb_w.put(wflit);

        mb_b.get_with_timeout(timeout_cycles, resp, timed_out);
    endtask

    task read(input logic [ADDR_WIDTH-1:0] addr, output logic [DATA_WIDTH-1:0] data, output logic [1:0] resp, input int timeout_cycles, output bit timed_out);
        RFlit_t rflit;

        mb_ar.put(addr);
        mb_r.get_with_timeout(timeout_cycles, rflit, timed_out);

        if (!timed_out) begin
            resp = rflit.resp;
            data = rflit.data;
        end
    endtask

endmodule : KanagawaSimAxi4Lite
