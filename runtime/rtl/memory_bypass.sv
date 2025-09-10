// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


// Bypass logic for RAMs
// Tracks recent data written to RAM
// and returns that data rather than
// data read from the RAM
module KanagawaMemoryBypass
#(
    parameter DATA_WIDTH,
    parameter ADDR_WIDTH,
    parameter NUM_BYPASS_SLOTS,
    parameter READ_ADDRESS_EARLY,
    parameter READ_DATA_EARLY
)
(
    input wire clk,

    // mirror of RAM read port

    // If ADDRESS_EARLY == 0, then read_addr_in is provided on the cycle when read_data_out is produced
    // If ADDRESS_EARLY == 1, then read_addr_in is provided one cycle earlier
    input wire [ADDR_WIDTH-1:0] read_addr_in,

    // If DATA_EARLY == 0, then read_data_in is provided on the cycle when read_data_out is produced
    // If DATA_EARLY == 1, then read_data_in is provided one cycle earlier
    input wire [DATA_WIDTH-1:0] read_data_in,

    // processed read data (either read_data_in or a recently written value)
    output logic [DATA_WIDTH-1:0] read_data_out,

    // mirror of RAM write port
    input wire wren_in,
    input wire [ADDR_WIDTH-1:0] write_addr_in,
    input wire [DATA_WIDTH-1:0] data_in
);
generate
    if (NUM_BYPASS_SLOTS > 0)
    begin: gen_bypass_slots
        typedef struct packed
        {
            logic valid;
            logic [ADDR_WIDTH-1:0] addr;
            logic [DATA_WIDTH-1:0] data;
        } BypassRecord;

        // Design Compiler doesn't process the conditional generate statement correctly, so when
        // NUM_BYPASS_SLOTS == 0, it still tries to create this variable and errors out.
        // This creates an extra BypassRecord, but this should get optimized away.
        BypassRecord bypass_records[NUM_BYPASS_SLOTS + 1];

        // Address comparison
        logic match_found;
        logic [DATA_WIDTH-1:0] matching_data;

        if (READ_ADDRESS_EARLY == 0) begin: gen_address_normal
            always_comb begin
                match_found = 1'b0;
                matching_data = '0;

                for (int bypass_slot_index = 0; bypass_slot_index < NUM_BYPASS_SLOTS; bypass_slot_index++) begin
                    if (bypass_records[bypass_slot_index].valid && (bypass_records[bypass_slot_index].addr == read_addr_in)) begin
                        match_found = 1'b1;
                        matching_data = bypass_records[bypass_slot_index].data;
                    end
                end
            end

            if (READ_DATA_EARLY == 0) begin: gen_address_normal_data_normal
                assign read_data_out = match_found ? matching_data : read_data_in;
            end
            else begin: gen_address_normal_data_early
                // Delay data by 1 cycle
                logic [DATA_WIDTH-1:0] read_data_ff;

                always_ff @(posedge clk) begin
                    read_data_ff <= read_data_in;
                end

                assign read_data_out = match_found ? matching_data : read_data_ff;
            end
        end
        else begin: gen_address_early
            always_comb begin
                match_found = 1'b0;
                matching_data = '0;

                for (int bypass_slot_index = 1; bypass_slot_index < NUM_BYPASS_SLOTS; bypass_slot_index++) begin
                    if (bypass_records[bypass_slot_index].valid && (bypass_records[bypass_slot_index].addr == read_addr_in)) begin
                        match_found = 1'b1;
                        matching_data = bypass_records[bypass_slot_index].data;
                    end
                end

                if (wren_in && (write_addr_in == read_addr_in)) begin
                    match_found = 1'b1;
                    matching_data = data_in;
                end
            end

            if (READ_DATA_EARLY == 0) begin: gen_address_early_data_normal
                // Delay match information by 1 cycle
                logic use_matching_data_ff;
                logic [DATA_WIDTH-1:0] matching_data_ff;

                always_ff @(posedge clk) begin
                    use_matching_data_ff <= match_found;
                    matching_data_ff <= matching_data;
                end

                assign read_data_out = use_matching_data_ff ? matching_data_ff : read_data_in;
            end
            else begin: gen_address_early_data_early
                // All bypass logic occurs the cycle before read_data_out is used
                always_ff @(posedge clk) begin
                    read_data_out <= match_found ? matching_data : read_data_in;
                end
            end
        end

        // when NUM_BYPASS_SLOTS == 1 && READ_ADDRESS_EARLY == 1
        // then bypass_records is not used.
        // Avoid DC errors about this always_ff block not mapping to any flip flops
        if ((NUM_BYPASS_SLOTS > 1) || (READ_ADDRESS_EARLY == 0)) begin: gen_save_bypass_records
            always_ff @(posedge clk) begin
                for (int bypass_slot_index = 0; bypass_slot_index < NUM_BYPASS_SLOTS; bypass_slot_index++) begin
                    if(bypass_slot_index != (NUM_BYPASS_SLOTS - 1)) begin
                        // Shift bypass elements down
                        bypass_records[bypass_slot_index] <= bypass_records[bypass_slot_index + 1];
                    end
                    else begin
                        // Save the information associated with the current write
                        // Note that there is no need to reset these registers
                        // The reset protocol ensures that wren_in will be 0 for at least NUM_BYPASS_SLOTS
                        bypass_records[bypass_slot_index].valid <= wren_in;
                        bypass_records[bypass_slot_index].addr <= write_addr_in;
                        bypass_records[bypass_slot_index].data <= data_in;
                    end
                end
            end
        end
    end else
    begin: gen_passthrough
        assign read_data_out = read_data_in;
    end
endgenerate
endmodule
