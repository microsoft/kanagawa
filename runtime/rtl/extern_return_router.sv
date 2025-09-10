// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Handles connecting to extern synchronous functions
//
`default_nettype none

interface pd_extern_return_router_switch_intf #( parameter NUM_OUTPUT_PORTS, parameter DATA_WIDTH );
    logic                               valid;
    logic                               ready;
    // Must be number of required bits for representation + 1, to allow underflow bit to work
    logic [$clog2(NUM_OUTPUT_PORTS):0]  match_counter;
    logic [DATA_WIDTH-1:0]              data;

    modport IN  (input valid, input match_counter, input data, output ready);
    modport OUT (output valid, output match_counter, output data, input ready);
endinterface

// Saves call index and uses it to route return values
// Must be instantiated large enough so as to never overflow
// This can be a dual-clock module.  Call index is written on the primary role clock.
// Call index is read out on the clock that the external module runs on
module KanagawaExternReturnRouter
import KanagawaTypes::*;
#(
    parameter DUAL_CLOCK,
    // Must be deep enough to ensure that it never overflows
    parameter LOG_DEPTH,
    parameter INDEX_WIDTH,
    parameter DATA_WIDTH,
    parameter USE_LUTRAM,
    parameter NUM_OUTPUT_PORTS,
    // This parameter array maps the switch node chain order to the incoming switch port index
    // Because the PORT_INDEX_MAP is used to initialize a table lookup ROM, the PORT_INDEX_MAP must be passed
    //  in as an initialization array.  In Quartus Pro 19, the type of the array is declared outside of the module and
    //  passed in as parameter PORT_INDEX_MAP_TYPE, and then the PORT_INDEX_MAP itself is declared to be a parameter
    //  of that type.
    //    parameter type PORT_INDEX_MAP_TYPE,
    //    parameter PORT_INDEX_MAP_TYPE PORT_INDEX_MAP
    // For Quartus Standard 17, this works:
    parameter PORT_INDEX_MAP_VALUE_WIDTH,
    parameter PORT_INDEX_MAP_DEPTH,
    parameter logic [0:PORT_INDEX_MAP_DEPTH-1][PORT_INDEX_MAP_VALUE_WIDTH-1:0] PORT_INDEX_MAP
)
(
    input wire          input_clk,
    input wire          input_rst,
    pd_fifo_intf.IN     input_index_intf,

    input wire          output_clk,
    input wire          output_rst,
    pd_fifo_intf.IN     input_data_intf,    // The data is assumed to already be in the output clock domain
    pd_fifo_intf.OUT    output_switch_node_intfs[NUM_OUTPUT_PORTS]
);
    localparam DEPTH = 2**LOG_DEPTH;

    // FIFO to hold the read destination indices while the memory access is occuring
    // The depth of the index FIFO is set to be at least as deep as the read request FIFO
    //   going to the memory, so the flow control on the read request FIFO will act as
    //   the flow control for this FIFO.
    // Ready/Valid protocol is still used here for future changes.
    pd_fifo_intf #(.DATA_WIDTH(INDEX_WIDTH)) index_fifo_output_intf();

    // The source of the indices may be in a different clock domain
    generate
        if (DUAL_CLOCK) begin :gen_dc_fifo
            KanagawaHALReadyValidFifo
            #(
                .DEPTH(DEPTH),
                .WIDTH(INDEX_WIDTH),
                .USE_LUTRAM(USE_LUTRAM),
                .DUAL_CLOCK(1)
            )
            call_index_fifo
            (
                .input_clk(input_clk),
                .input_rst(input_rst),
                .input_intf(input_index_intf),
                .input_usedw(),
                .input_almost_full(),

                .output_clk(output_clk),
                .output_intf(index_fifo_output_intf)
            );
        end
        else begin :gen_sc_fifo
            KanagawaHALReadyValidFifo
            #(
                .DEPTH(DEPTH),
                .WIDTH(INDEX_WIDTH),
                .USE_LUTRAM(USE_LUTRAM),
                .DUAL_CLOCK(0)
            )
            call_index_fifo
            (
                .input_clk(output_clk),
                .input_rst(output_rst),
                .input_intf(input_index_intf),
                .input_usedw(),
                .input_almost_full(),

                .output_clk(output_clk),
                .output_intf(index_fifo_output_intf)
            );
        end
    endgenerate

    // Create a switch interface for every switch node, plus an extra one to connect to the end of the chain
    pd_extern_return_router_switch_intf #(.NUM_OUTPUT_PORTS(NUM_OUTPUT_PORTS), .DATA_WIDTH(DATA_WIDTH)) switch_intfs[NUM_OUTPUT_PORTS+1]();

    // Create a ROM to translate call site indices to switch decode down counters in the chain
    //  Note that the output here is an extra bit wide because the relative locations are pre-decremented
    //  so that the value coming out can be immediately fed into the first call site switch un-altered, and so
    //  the first switch can just look at the upper bit to see if it should accept the value.
    // Thereafter, the index value will be decremented by each switch as it is passed down so that the underflow
    //  will eventually cause the upper bit to turn to 1 and match.
    // This is implemented as a ROM rather than logic because it is performing three different functions and this
    //  seemed like the most efficient implementation, though perhaps a bit slower.  The functions are:
    //  1. Translate the non-contiguous call site indices into the linearly contiguous switch addresses.  Some of
    //      the site indices that are passed down are non-contiguous because the compiler may optimize them out.
    //  2. Reorder the call site indices to match the same order that is used by the command processors so that
    //      place and route will be able to follow the same general path for this chain.
    //  3. Pre-decrement the translated switch addresses so that I don't have to do that here and add another layer
    //      of logic between the ROM and the first switch.
    // There was a lot of trouble in figuring out how to pass down a parameter that could be used to initialize the ROM.
    //  The original intent was something like this:
    //      const logic [$bits(switch_intfs.match_counter)-1:0] call_site_translation_rom[0:(2**INDEX_WIDTH)-1] = PORT_INDEX_MAP;
    //  For Quartus Pro 19, a type was defined in the module that instantiated KanagawaExternReturnRouter:
    //      typedef logic [2:0] PORT_INDEX_MAP_TYPE[0:3];
    //  and passed in as a type parameter "parameter type PORT_INDEX_MAP_TYPE", then here the ROM was instantiated by:
    //      const PORT_INDEX_MAP_TYPE call_site_translation_rom = PORT_INDEX_MAP;
    //  That didn't work for Quartus Standard 17 (supporting Stratix 5), but this does, and it works for Quartus Pro 19 as well:
    const logic [0:PORT_INDEX_MAP_DEPTH-1][PORT_INDEX_MAP_VALUE_WIDTH-1:0] call_site_translation_rom = PORT_INDEX_MAP;

    // Join the output of the index FIFO with the incoming data FIFO
    logic index_sidetrack_valid;
    logic [INDEX_WIDTH-1:0] index_sidetrack_data;
    wire [INDEX_WIDTH-1:0] muxed_index;
    assign muxed_index = index_sidetrack_valid ? index_sidetrack_data : index_fifo_output_intf.data;
    logic data_sidetrack_valid;
    logic [DATA_WIDTH-1:0] data_sidetrack_data;
    wire [DATA_WIDTH-1:0] muxed_data;
    assign muxed_data = data_sidetrack_valid ? data_sidetrack_data : input_data_intf.data;
    always @(posedge output_clk) begin
        //synopsys sync_set_reset "output_rst"
        if (output_rst == 1'b1) begin
            index_fifo_output_intf.ready <= 1'b0;
            input_data_intf.ready <= 1'b0;
            switch_intfs[0].valid <= 1'b0;
            index_sidetrack_valid <= 1'b0;
            data_sidetrack_valid <= 1'b0;
        end
        else begin
            // If the output is ready and both inputs are ready
            if (
                ((switch_intfs[0].valid == 1'b1 && switch_intfs[0].ready == 1'b1) || switch_intfs[0].valid == 1'b0) &&
                ((index_fifo_output_intf.valid == 1'b1 && index_fifo_output_intf.ready == 1'b1) || index_sidetrack_valid == 1'b1) &&
                ((input_data_intf.valid == 1'b1 && input_data_intf.ready == 1'b1) || data_sidetrack_valid == 1'b1)
            ) begin
                index_sidetrack_valid <= 1'b0;
                data_sidetrack_valid <= 1'b0;
                switch_intfs[0].valid <= 1'b1;
                // Convert the index to one-hot for switch node decoding
                switch_intfs[0].match_counter <= call_site_translation_rom[muxed_index];
                switch_intfs[0].data <= muxed_data;
                index_fifo_output_intf.ready <= 1'b1;
                input_data_intf.ready <= 1'b1;
            end
            // If the output is not ready or either input is not ready
            else begin
                // Clear the output if it was accepted
                if (switch_intfs[0].valid == 1'b1 && switch_intfs[0].ready == 1'b1)
                    switch_intfs[0].valid <= 1'b0;

                // Take care of flow control for each input separately
                if (index_fifo_output_intf.valid == 1'b1 && index_fifo_output_intf.ready == 1'b1) begin
                    index_fifo_output_intf.ready <= 1'b0;
                    index_sidetrack_data <= index_fifo_output_intf.data;
                    index_sidetrack_valid <= 1'b1;
                end
                else if (index_sidetrack_valid == 1'b0)
                    index_fifo_output_intf.ready <= 1'b1;

                if (input_data_intf.valid == 1'b1 && input_data_intf.ready == 1'b1) begin
                    input_data_intf.ready <= 1'b0;
                    data_sidetrack_data <= input_data_intf.data;
                    data_sidetrack_valid <= 1'b1;
                end
                else if (data_sidetrack_valid == 1'b0)
                    input_data_intf.ready <= 1'b1;
            end
        end
    end

    // Generate the chain of switch nodes
    generate
        genvar chain_index;
        for(chain_index = 0; chain_index < NUM_OUTPUT_PORTS; chain_index = chain_index + 1) begin : genSwitchNode
            KanagawaExternReturnRouter_switch_node #(.DATA_WIDTH(DATA_WIDTH)) switch_chain_node
            (
                .clk(output_clk),
                .rst(output_rst),

                .switch_input_intf(switch_intfs[chain_index]),
                .switch_output_intf(switch_intfs[chain_index+1]),

                .fifo_output_intf(output_switch_node_intfs[chain_index])
            );
        end
    endgenerate

    // Tie off the output interface from the final switch node
    assign switch_intfs[NUM_OUTPUT_PORTS].ready = 1'b1;

endmodule



module KanagawaExternReturnRouter_switch_node
import KanagawaTypes::*;
#(
    // DATA_WIDTH parameter should not be needed, since it is $bits(switch_input_intf.data), but modelsim
    //  threw a compilation error that way.
    parameter DATA_WIDTH = 16
)
(
    input wire clk,
    input wire rst,

    pd_extern_return_router_switch_intf.IN  switch_input_intf,
    pd_extern_return_router_switch_intf.OUT switch_output_intf,

    pd_fifo_intf.OUT                        fifo_output_intf
);

    logic sidetrack_valid;
    logic [$bits(switch_input_intf.match_counter)-1:0] sidetrack_match_counter;
    logic [DATA_WIDTH-1:0] sidetrack_data;
    wire [$bits(switch_input_intf.match_counter)-1:0] muxed_match_counter;
    wire [DATA_WIDTH-1:0] muxed_data;
    assign muxed_match_counter = (sidetrack_valid) ? sidetrack_match_counter : switch_input_intf.match_counter;
    assign muxed_data = (sidetrack_valid) ? sidetrack_data : switch_input_intf.data;

    always @(posedge clk) begin
        //synopsys sync_set_reset "rst"
        if (rst) begin
            switch_input_intf.ready <= 1'b0;
            switch_output_intf.valid <= 1'b0;
            fifo_output_intf.valid <= 1'b0;
            sidetrack_valid <= 1'b0;
        end
        else begin
            if (switch_output_intf.valid == 1'b1 && switch_output_intf.ready == 1'b1)
                switch_output_intf.valid <= 1'b0;
            if (fifo_output_intf.valid == 1'b1 && fifo_output_intf.ready == 1'b1)
                fifo_output_intf.valid <= 1'b0;

            // The tag uses a down counter with underflow in order to determine the match.  At each stage,
            //  the tag is decremented as it is passed to the next stage, so that when it reaches the
            //  correct stage in the switch pipeline, the tag will underflow and the upper bit of the tag
            //  down counter will get set.  This avoids having to do a comparison at each stage, which would
            //  slow it down.
            // So, if the upper bit of the tag down counter is a 1, this is the target stage for the data.
            if (muxed_match_counter[$bits(switch_input_intf.match_counter)-1] == 1'b1) begin

                if ((fifo_output_intf.valid == 1'b1 && fifo_output_intf.ready == 1'b1) || fifo_output_intf.valid == 1'b0) begin
                    if ((switch_input_intf.valid == 1'b1 && switch_input_intf.ready == 1'b1) || sidetrack_valid == 1'b1) begin
                        sidetrack_valid <= 1'b0;
                        fifo_output_intf.valid <= 1'b1;
                        fifo_output_intf.data <= muxed_data;
                    end
                    else
                        fifo_output_intf.valid <= 1'b0;
                    switch_input_intf.ready <= 1'b1;
                end
                else if (switch_input_intf.valid == 1'b1 && switch_input_intf.ready == 1'b1) begin
                    switch_input_intf.ready <= 1'b0;
                    sidetrack_match_counter <= switch_input_intf.match_counter;
                    sidetrack_data <= switch_input_intf.data;
                    sidetrack_valid <= 1'b1;
                end
                else if (sidetrack_valid == 1'b0)
                    switch_input_intf.ready <= 1'b1;

            end
            else begin

                if ((switch_output_intf.valid == 1'b1 && switch_output_intf.ready == 1'b1) || switch_output_intf.valid == 1'b0) begin
                    if ((switch_input_intf.valid == 1'b1 && switch_input_intf.ready == 1'b1) || sidetrack_valid == 1'b1) begin
                        sidetrack_valid <= 1'b0;
                        switch_output_intf.valid <= 1'b1;
                        switch_output_intf.match_counter <= $bits(switch_output_intf.match_counter)'(muxed_match_counter - 1);
                        switch_output_intf.data <= muxed_data;
                    end
                    else
                        switch_output_intf.valid <= 1'b0;
                    switch_input_intf.ready <= 1'b1;
                end
                else if (switch_input_intf.valid == 1'b1 && switch_input_intf.ready == 1'b1) begin
                    switch_input_intf.ready <= 1'b0;
                    sidetrack_match_counter <= switch_input_intf.match_counter;
                    sidetrack_data <= switch_input_intf.data;
                    sidetrack_valid <= 1'b1;
                end
                else if (sidetrack_valid == 1'b0)
                    switch_input_intf.ready <= 1'b1;

            end

        end
    end

endmodule
