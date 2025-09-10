// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1 ns / 1 ps

import KanagawaSimStallerPolicies::*;
import KanagawaSimTaskHelpers::*;

module KanagawaExternReturnRouter_tb
#(
    parameter DUAL_CLOCK = 0
)
();

	bit input_clk, output_clk;
	bit rst = 1'b1;

    // Generate clocks
    always #5 output_clk = ~output_clk;
    generate
        if (DUAL_CLOCK)
            always #7 input_clk = ~input_clk;
        else
            assign input_clk = output_clk;
    endgenerate

    initial begin
        // Assert reset signal
        rst <= 1'b1;
        // Clear reset signal
        rst <= #(5*50 + 2.5) 1'b0;
    end

    initial $timeformat(-9, 2, " ns", 15);

    localparam NUM_SIMULATED_TRANSACTIONS = 10000;
    localparam NUM_OUTPUT_PORTS = 3;
    localparam DATA_WIDTH = 16;

    localparam INDEX_WIDTH = $clog2(NUM_OUTPUT_PORTS);

    pd_fifo_intf #( .DATA_WIDTH(INDEX_WIDTH) ) input_index_intf();
    pd_fifo_intf #( .DATA_WIDTH(DATA_WIDTH) ) input_data_intf();
    pd_fifo_intf #( .DATA_WIDTH(DATA_WIDTH) ) output_switch_node_intfs[NUM_OUTPUT_PORTS]();

    // Sends index input
    typedef struct packed {
        logic [$clog2(NUM_OUTPUT_PORTS)-1:0] data;
    } index_payload_t;
    index_payload_t index_input_payload_translator;
    assign input_index_intf.data = index_input_payload_translator.data;
    KanagawaSimMailboxToReadyValid
    #(
        .T( index_payload_t ),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,2,1,2)),
        .STALLER_SEED(5)
    )
    index_input_FIFO
    (
        .clk(input_clk),
        .rst(rst),

        .valid_out( input_index_intf.valid ),
        .ready_in( input_index_intf.ready ),
        .data_out( index_input_payload_translator )
    );

    // Sends data input
    typedef struct packed {
        logic [DATA_WIDTH-1:0] data;
    } data_payload_t;
    data_payload_t data_input_payload_translator;
    assign input_data_intf.data = data_input_payload_translator.data;
    KanagawaSimMailboxToReadyValid
    #(
        .T( data_payload_t ),
        .STALL_POLICY(InclusiveRangeStallPolicy#(1,3,1,3)),
        .STALLER_SEED(5)
    )
    data_input_FIFO
    (
        .clk(output_clk),
        .rst(rst),

        .valid_out( input_data_intf.valid ),
        .ready_in( input_data_intf.ready ),
        .data_out( data_input_payload_translator )
    );

    // Receives data outputs
    data_payload_t data_output_translators[NUM_OUTPUT_PORTS];
    KanagawaSimMailboxWithShutdown #( .T( data_payload_t ) ) data_output_expected_FIFOs[NUM_OUTPUT_PORTS];
    generate
        for( genvar output_port = 0; output_port < NUM_OUTPUT_PORTS; output_port++ ) begin: output_ports
            assign data_output_translators[output_port].data = output_switch_node_intfs[output_port].data;
            KanagawaSimReadyValidToMailbox
            #(
                .T( data_payload_t ),
                .STALL_POLICY(InclusiveRangeStallPolicy#(3,3,3,3)),
                .STALLER_SEED(5)
            )
            data_output_actual_FIFO
            (
                .clk(output_clk),
                .rst(rst),

                .valid_in( output_switch_node_intfs[output_port].valid ),
                .ready_out( output_switch_node_intfs[output_port].ready ),
                .data_in( data_output_translators[output_port] )
            );
        end
    endgenerate

//    typedef logic [2:0] PORT_INDEX_MAP_TYPE[0:3];
    localparam PORT_INDEX_MAP_VALUE_WIDTH = 3;
    localparam PORT_INDEX_MAP_DEPTH = 4;
    localparam [0:PORT_INDEX_MAP_DEPTH-1][PORT_INDEX_MAP_VALUE_WIDTH-1:0] PORT_INDEX_MAP = { 3'h7, 3'h0, 3'h1, 3'h3 };
    KanagawaExternReturnRouter
    #(
        .DUAL_CLOCK(DUAL_CLOCK),
        .LOG_DEPTH($clog2(NUM_SIMULATED_TRANSACTIONS)),   // Must be deep enough to ensure that it never overflows
        .INDEX_WIDTH(INDEX_WIDTH),
        .DATA_WIDTH(DATA_WIDTH),
        .USE_LUTRAM(0),
        .NUM_OUTPUT_PORTS(NUM_OUTPUT_PORTS),
//        .PORT_INDEX_MAP_TYPE(PORT_INDEX_MAP_TYPE),
//        .PORT_INDEX_MAP('{ 3'h7, 3'h0, 3'h1, 3'h3 })
        .PORT_INDEX_MAP_VALUE_WIDTH(PORT_INDEX_MAP_VALUE_WIDTH),
        .PORT_INDEX_MAP_DEPTH(PORT_INDEX_MAP_DEPTH),
        .PORT_INDEX_MAP(PORT_INDEX_MAP)
    )
    dut
    (
        .input_clk(input_clk),
        .input_rst(rst),
        .input_index_intf(input_index_intf),
        .input_data_intf(input_data_intf),

        .output_clk(output_clk),
        .output_rst(rst),

        .output_switch_node_intfs(output_switch_node_intfs)
    );

    // Generate test input vectors and expected output vectors
    index_payload_t index_input_payload;
    data_payload_t data_input_payload, data_output_payload;
    logic [$clog2(NUM_OUTPUT_PORTS):0] target_output;
    initial begin

        // Wait for reset to occur
        wait (rst == 1'b1);
        wait (rst == 1'b0);

        for( int iOutputPort = 0; iOutputPort < NUM_OUTPUT_PORTS; iOutputPort++ )
            data_output_expected_FIFOs[iOutputPort] = new();

        $display( "KanagawaExternReturnRouter_tb Test Starting" );

        for( int iTransaction = 0; iTransaction < NUM_SIMULATED_TRANSACTIONS; iTransaction++ ) begin

            target_output = $urandom_range(NUM_OUTPUT_PORTS-1);

            // $display( "Time %t: Queuing Port %u, Data %u", $time, target_output, iTransaction );

            index_input_payload.data = target_output;
            index_input_FIFO.put(index_input_payload);

            data_input_payload.data = iTransaction;
            data_input_FIFO.put(data_input_payload);

            data_output_payload = data_input_payload;
            data_output_expected_FIFOs[target_output].put(data_output_payload);

        end

        for( int iOutputPort = 0; iOutputPort < NUM_OUTPUT_PORTS; iOutputPort++ )
            data_output_expected_FIFOs[iOutputPort].shutdown();

    end

    // Check actual outputs against expected outputs on all interfaces
    bit [NUM_OUTPUT_PORTS-1:0] shutdown_bits;
    generate
        for( genvar output_port = 0; output_port < NUM_OUTPUT_PORTS; output_port++ ) begin: check_output_ports
            data_payload_t data_expected_payload, data_actual_payload;
            always @(posedge output_clk) begin
                if (rst) begin
                end
                else begin

                    // Check every output port
                    if (!data_output_expected_FIFOs[output_port].is_empty() && KanagawaExternReturnRouter_tb.output_ports[output_port].data_output_actual_FIFO.num() > 0) begin
                        data_output_expected_FIFOs[output_port].get(data_expected_payload, shutdown_bits[output_port]);
                        assert(!shutdown_bits[output_port]) else begin
                            $error( "Output Port %d Extra Data", output_port );
                            $stop;
                        end
                        KanagawaExternReturnRouter_tb.output_ports[output_port].data_output_actual_FIFO.get(data_actual_payload);

                        assert(data_expected_payload === data_actual_payload) else begin
                            $error( "Port %d Data Expected %X, Actual %X", output_port, data_expected_payload.data, data_actual_payload.data );
                            $stop;
                        end
                    end

                    // Peek for termination condition
                    data_output_expected_FIFOs[output_port].peek_shutdown( shutdown_bits[output_port] );
                end

            end
        end

    endgenerate

    always @(posedge output_clk) begin
        if (rst) begin
        end
        else begin
            // Check for termination condition
            if (&shutdown_bits == 1'b1) begin
                $display( "KanagawaExternReturnRouter_tb Test Successful" );
                $finish;
            end
        end
    end

endmodule
