// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

`ifdef QUESTA
`default_nettype none
`define WT wire
`else
`define WT
`endif

// Provides the backing store for KanagawaSimAxiRam. By splitting the
// functionality this way, we allow external test code to read or write the
// RAM data independent of the module that implements the AXI bus protocol.

interface KanagawaSimRamIfc
#(
    parameter integer DATA_WIDTH = 512,
    parameter integer ADDRESS_WIDTH = 64
);
    localparam DATA_BYTES = DATA_WIDTH/8;
    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDRESS_WIDTH-1:0] address_t;
    typedef logic [DATA_WIDTH/8-1:0] wrstrb_t;
    typedef logic [ADDRESS_WIDTH-$clog2(DATA_BYTES)-1:0] word_address_t;

    data_t mock_mem[word_address_t];
    semaphore sem_mem = new(1);
    bit _fail_unitialized_reads = 1'b1;

    function automatic word_address_t word_address_from_byte_adddress(input address_t addr);
        return addr[$bits(address_t)-1 -: $bits(word_address_t)];
    endfunction

    function automatic bit get_fail_unitialized_reads();
        return _fail_unitialized_reads;
    endfunction

    function automatic void set_fail_unitialized_reads(bit fail_unitialized_reads);
        _fail_unitialized_reads = fail_unitialized_reads;
    endfunction

    task automatic reset();
        sem_mem.get(1);
            mock_mem.delete();
        sem_mem.put(1);
    endtask

    task automatic write(input address_t addr, input data_t data, input wrstrb_t strb = '1);
        data_t new_data;
        word_address_t waddr;

        assert(!$isunknown(addr));
        assert(!$isunknown(strb));
        assert(~|addr[$clog2(DATA_BYTES)-1:0]);

        waddr = word_address_from_byte_adddress(addr);

        sem_mem.get(1);
            if (!mock_mem.exists(waddr)) begin
                new_data = 'x;
            end
            else begin
                new_data = mock_mem[waddr];
            end

            for (int i = 0; i < DATA_BYTES; ++i) begin
                if (strb[i]) begin
                    new_data[i*8 +: 8] = data[i*8 +: 8];
                end
            end
            mock_mem[waddr] = new_data;
        sem_mem.put(1);
    endtask

    task automatic read(input address_t addr, output data_t data);
        word_address_t rdaddr;
        data_t rddata;

        assert(!$isunknown(addr));
        assert(~|addr[$clog2(DATA_BYTES)-1:0]);

        rdaddr = word_address_from_byte_adddress(addr);

        rddata = 'x;
        sem_mem.get(1);
            if (mock_mem.exists(rdaddr)) begin
                rddata = mock_mem[rdaddr];
            end
            else if (_fail_unitialized_reads != 0) begin
                $error("Unitialized read from address %0d'h%x", $bits(addr), addr);
            end
        sem_mem.put(1);
        data = rddata;
    endtask

endinterface

// Provides a mock RAM with an AXI interface for use in testbenches.  The
// backing store is implemented with an associative array. The module has the
// following limitations:

// 1) Read and write burst type (ARBURST, AWBURST) must be type INCR (2'b01)
// 2) Ordering with respect to read and write address IDs is ignored; transactions
//     are processed as they are received, with no guarantee of ordering between
//     read and write channels, as per the AXI specification. RID value returned
//     is always 0.
// 3) Write lock types (AWLOCK) are not supported.
// 4) Cache type (ARCACHE, AWCACHE) is ignored.
// 5) Access / protection (AWPROT, ARPROT) is ignored.
// 6) QOS (ARQOS, AWQOS) is ignored.
// 7) Region (ARREGION, AWREGION) is ignored.
// 8) Burst size (ARSIZE, AWSIZE) is ignored and assumed to match DATAWIDTH.

module KanagawaSimAxiRam
#(
    parameter integer DATA_WIDTH = 512,
    parameter integer ADDRESS_WIDTH = 64,
    parameter integer MIN_WR_DELAY_NS = 50,
    parameter integer MAX_WR_DELAY_NS = 150,
    parameter integer MIN_RD_DELAY_NS = 100,
    parameter integer MAX_RD_DELAY_NS = 150,
    parameter real    RAM_CLOCK_NS = 3.75,
    parameter integer VERBOSE = 0
)
(
    input  wire                                 clk,
    input  wire                                 rst,

    input  wire                                 aw_valid_in,
    output logic                                aw_ready_out,
    input  wire [ADDRESS_WIDTH-1:0]             aw_addr_in,
    input  wire [7:0]                           aw_len_in,

    input  wire                                 w_valid_in,
    output logic                                w_ready_out,
    input  wire [DATA_WIDTH-1:0]                w_data_in,
    input  wire [DATA_WIDTH/8-1:0]              w_strb_in,
    input  wire                                 w_last_in,

    output logic                                b_valid_out,
    input  wire                                 b_ready_in,

    input  wire                                 ar_valid_in,
    output logic                                ar_ready_out,
    input  wire [ADDRESS_WIDTH-1:0]             ar_addr_in,
    input  wire [7:0]                           ar_len_in,

    output logic                                r_valid_out,
    input  wire                                 r_ready_in,
    output logic[DATA_WIDTH - 1:0]              r_data_out,
    output logic                                r_last_out,
    output logic[1:0]                           r_resp_out
);

    KanagawaSimRamIfc #(DATA_WIDTH, ADDRESS_WIDTH) ram_if();

    localparam DATA_BYTES = DATA_WIDTH/8;
    localparam QUEUE_DEPTH = 4;
    localparam FIFO_DEPTH = 4;

    typedef logic [DATA_WIDTH-1:0] data_t;
    typedef logic [ADDRESS_WIDTH-1:0] address_t;
    typedef logic [DATA_WIDTH/8-1:0] wrstrb_t;
    typedef logic [ADDRESS_WIDTH-$clog2(DATA_BYTES)-1:0] word_address_t;
    typedef logic [7:0] len_t;

    typedef logic [1:0] bresp_t;
    typedef logic [1:0] rresp_t;

    typedef struct packed {
        address_t address;
        len_t len;
    } awflit_t;

    typedef struct packed {
        address_t address;
        len_t len;
    } artrans_t;

    typedef struct packed {
        wrstrb_t strb;
        data_t data;
    } wflit_t;

    typedef struct packed {
        logic   last;
        rresp_t resp;
        data_t  data;
    } rflit_t;

    logic       ram_clk = 1'b0;

    awflit_t    awtrans;
    artrans_t   artrans;
    wflit_t     wflit;
    bresp_t     bresp;
    rflit_t     rflit;

    KanagawaSimMailboxWithDelay #(.T(awflit_t), .DEPTH(QUEUE_DEPTH)) mb_delay_aw();
    KanagawaSimMailboxWithDelay #(.T(artrans_t), .DEPTH(QUEUE_DEPTH)) mb_delay_ar();

    always #(RAM_CLOCK_NS*1ns) ram_clk = ~ram_clk;

    always_comb begin
        awtrans.address = aw_addr_in;
        awtrans.len = aw_len_in;

        artrans.address = ar_addr_in;
        artrans.len = ar_len_in;

        wflit.strb = w_strb_in;
        wflit.data = w_data_in;

        r_last_out = rflit.last;
        r_resp_out = rflit.resp;
        r_data_out = rflit.data;
    end

    KanagawaSimReadyValidToMailbox
    #(
        .T      (awflit_t)
    ) mb_aw
    (
        .clk                (clk),
        .rst                (rst),

        .ready_out          (aw_ready_out),
        .valid_in           (aw_valid_in),
        .data_in            (awtrans)
    );

    KanagawaSimReadyValidToMailbox
    #(
        .T      (wflit_t)
    ) mb_w
    (
        .clk                (clk),
        .rst                (rst),

        .ready_out          (w_ready_out),
        .valid_in           (w_valid_in),
        .data_in            (wflit)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T      (bresp_t)
    ) mb_b
    (
        .clk                (clk),
        .rst                (rst),

        .ready_in           (b_ready_in),
        .valid_out          (b_valid_out),
        .data_out           (bresp)
    );

    KanagawaSimReadyValidToMailbox
    #(
        .T      (artrans_t)
    ) mb_ar
    (
        .clk                (clk),
        .rst                (rst),

        .ready_out          (ar_ready_out),
        .valid_in           (ar_valid_in),
        .data_in            (artrans)
    );

    KanagawaSimMailboxToReadyValid
    #(
        .T      (rflit_t)
    ) mb_r
    (
        .clk                (clk),
        .rst                (rst),

        .ready_in           (r_ready_in),
        .valid_out          (r_valid_out),
        .data_out           (rflit)
    );

    clocking cb_ram @(posedge ram_clk);
    endclocking

    function automatic bit get_fail_unitialized_reads();
        return ram_if.get_fail_unitialized_reads();
    endfunction

    function automatic void set_fail_unitialized_reads(bit fail_unitialized_reads);
        ram_if.set_fail_unitialized_reads(fail_unitialized_reads);
    endfunction

    task automatic ram_reset();
        ram_if.reset();
    endtask

    task automatic ram_write(input address_t addr, input data_t data, input wrstrb_t strb = '1);
        if (VERBOSE > 0) begin
            $display("%t: %m: ram_write [%0d'h%x] <= %0d'h%x | %0d'h%x",
                $time, $bits(addr), addr, $bits(data), data, $bits(strb), strb);
        end
        ram_if.write(addr, data, strb);
    endtask

    task automatic ram_read(input address_t addr, output data_t data);
        ram_if.read(addr, data);
        if (VERBOSE > 0) begin
            $display("%t: %m: ram_read [%0d'h%x] => %0d'h%x", $time, $bits(addr), addr, $bits(data), data);
        end
    endtask

    task automatic aw_fifo_to_mb_delay();
        awflit_t aw;
        forever begin
            mb_aw.get(aw);
            mb_delay_aw.put(aw, $urandom_range(MIN_WR_DELAY_NS,MAX_WR_DELAY_NS)*1ns);
        end
    endtask

    task automatic ar_fifo_to_mb_delay();
        artrans_t ar;
        forever begin
            mb_ar.get(ar);
            mb_delay_ar.put(ar, $urandom_range(MIN_RD_DELAY_NS,MAX_RD_DELAY_NS)*1ns);
        end
    endtask

    task automatic process_reads();
        artrans_t ar;
        rflit_t flit;
        address_t address;

        flit.resp = '0; // OK
        forever begin
            mb_delay_ar.get(ar);

            address = ar.address;
            for (int i = 0; i < integer'(ar.len)+1; ++i) begin
                @(cb_ram);
                ram_if.read(address, flit.data);
                if (VERBOSE > 0) begin
                    $display("%t: %m: READ [%0d'h%x] => %0d'h%x", $time, $bits(address), address, $bits(flit.data), flit.data);
                end
                flit.last = (i == (ar.len));
                mb_r.put(flit);
                address += DATA_BYTES;
            end
        end
    endtask

    task automatic process_writes();
        awflit_t aw;
        wflit_t w;
        bresp_t bresp;
        address_t address;

        bresp = '0; // OK
        forever begin
            mb_delay_aw.get(aw);

            address = aw.address;
            repeat (integer'(aw.len) + 1) begin
                @(cb_ram);
                mb_w.get(w);

                if (VERBOSE > 0) begin
                    $display("%t: %m: WRITE [%0d'h%x] <= %0d'h%x | %0d'h%x",
                        $time, $bits(address), address, $bits(w.data), w.data, $bits(w.strb), w.strb);
                end
                ram_if.write(address, w.data, w.strb);

                address += DATA_BYTES;
            end
            mb_b.put(bresp);
        end
    endtask

    initial begin
        fork
            aw_fifo_to_mb_delay();
            ar_fifo_to_mb_delay();
            process_writes();
            process_reads();
        join
    end
endmodule
