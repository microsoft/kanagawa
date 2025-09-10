// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Register based show-ahead FIFO.

module KanagawaShowAheadRegisterFifo
#(
    parameter integer DEPTH,
    parameter integer LOG_DEPTH = $clog2(DEPTH),
    parameter integer WIDTH,
    parameter integer ALMOSTFULL_ENTRIES,
    parameter integer ALMOSTEMPTY_VAL = 0,
    // On underflow (read when empty) or overflow (write when full)
    //  0 - Hardware: FIFO behavior undefined
    //  1 - Hardware: FIFO will ignore read (underflow) or write (overflow)
    parameter integer OVER_UNDER_FLOW_PROTECTION = 0,
    parameter integer ASSERT_ON_OVER_UNDER_FLOW = 1,

    // Selects between 2 implementations
    // MUX_ON_READ = 1 has WIDTH number of DEPTH:1 muxes used to implement reads, but internal registers are clocked gated more often
    // MUX_ON_READ = 0 does not have these muxes and internal registers are clocked gated less often
    parameter integer MUX_ON_READ = 1
)
(
    input wire                       clock,
    input wire                       rst,

    input wire                       wrreq,
    input wire [WIDTH-1:0]           data,
    output logic                     full,
    output logic                     almost_full,
    output logic [LOG_DEPTH:0]       usedw,

    input wire                       rdreq,
    output logic                     empty,
    output logic                     almost_empty,
    output logic [WIDTH-1:0]         q
);
    localparam integer ALMOSTFULL_VAL = DEPTH - ALMOSTFULL_ENTRIES;
    localparam integer ALMOSTEMPTY_VAL_PLUS_ONE = ALMOSTEMPTY_VAL + 1;
    localparam integer ALMOSTFULL_VAL_MINUS_ONE = ALMOSTFULL_VAL - 1;

    logic pop, push;

    assign pop  = rdreq & ((OVER_UNDER_FLOW_PROTECTION == 0) | ~empty);
    assign push = wrreq & ((OVER_UNDER_FLOW_PROTECTION == 0) | ~full);

`ifndef NO_DYNAMIC_ASSERTS
    // synopsys translate_off
    assert property (@(posedge clock) (!rst && full  && (ASSERT_ON_OVER_UNDER_FLOW != 0))  |-> !wrreq) else $error ("%m overflow");
    assert property (@(posedge clock) (!rst && empty && (ASSERT_ON_OVER_UNDER_FLOW != 0))  |-> !rdreq) else $error ("%m underflow");
    // synopsys translate_on
`endif

    generate
        if (MUX_ON_READ == 1) begin: gen_with_mux
            logic [LOG_DEPTH-1:0] wrptr;
            logic [LOG_DEPTH-1:0] rdptr;

            KanagawaFifoPtrsEx
            #(
                .DEPTH(DEPTH),
                .LOG_DEPTH(LOG_DEPTH),
                .ALMOST_FULL_MARGIN(ALMOSTFULL_ENTRIES),
                .ALMOST_EMPTY_MARGIN(ALMOSTEMPTY_VAL)
            )
            ptrs
            (
                .clk(clock),
                .rst(rst),

                .wrreq_in(push),
                .full_out(full),
                .almost_full_out(almost_full),
                .wrptr_out(wrptr),

                .usedw_out(usedw),

                .rdreq_in(pop),
                .empty_out(empty),
                .almost_empty_out(almost_empty),
                .rdptr_out(rdptr)
            );

            logic [WIDTH-1:0] values[DEPTH-1:0];
            assign q = values[rdptr];

            always_ff @(posedge clock) begin
                if (push) begin
                    values[wrptr] <= data;
                end
            end
        end
        else begin: gen_no_mux
            logic [WIDTH-1:0] values[DEPTH-1:0];
            logic [LOG_DEPTH:0] wptr;
            logic rst_d1;

            assign q = values[0];
            assign usedw = wptr;

            always_ff @(posedge clock) begin
                rst_d1 <= rst;

                case ({push, pop})
                    2'b01: begin //Read Only
                        empty <= (wptr == 1);
                        almost_empty <= (wptr <= ALMOSTEMPTY_VAL_PLUS_ONE);
                        full <= 1'b0;
                        almost_full <= (wptr > ALMOSTFULL_VAL);

                        for (integer i = 1; i < DEPTH; ++i) begin
                            values[i-1] <= values[i];
                        end

                        wptr <= wptr-1;
                    end

                    2'b10: begin //Write Only
                        empty <= 1'b0;
                        almost_empty <= (wptr < ALMOSTEMPTY_VAL);
                        full <= (wptr == (DEPTH-1));
                        almost_full <= (wptr >= ALMOSTFULL_VAL_MINUS_ONE);

                        values[wptr] <= data;
                        wptr <= wptr + 1'b1;
                    end

                    2'b11: begin //Read & Write
                        for (integer i = 1; i < DEPTH; ++i) begin
                            if (wptr == i) begin
                                values[i-1] <= data;
                            end
                            else begin
                                values[i-1] <= values[i];
                            end
                        end
                        values[DEPTH-1]  <= data;
                    end

                    default: begin
                        // De-assert full signals when coming out of reset
                        if (rst_d1) begin
                            full <= 1'b0;
                            almost_full <= 1'b0;
                        end
                    end
                endcase

                //synopsys sync_set_reset "rst"
                if (rst) begin
                    almost_empty <= 1'b1;
                    empty <= 1'b1;
                    full <= 1'b1;
                    almost_full <= 1'b1;
                    wptr <= '0;
                end
            end
        end
    endgenerate
endmodule
