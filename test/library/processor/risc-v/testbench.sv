//  Copyright (c), Microsoft Corporation.
//  Licensed under the MIT License.

`timescale 1ns/1ps
`default_nettype none

// This is a generic testbench to be used with RISC-V unit tests.
// The testbench does the following:
// - Populates instruction and data memory
// - Resets the DUT
// - Sends a start-up command to the device. This command includes starting program counter
//   values for each HART.
// - Monitors the DUT for completion of the test.
// - Reads the results from DMEM and compares those with expected values.
// Note that most of the test parameters are passed in via the command-line.
// This allows re-use of a single Verilator simulation build across multiple
// test runs.
module risc_v_testbench
#(
    // Set to 1'b1 to dump actual and expected DMEM values after test run
    parameter bit DUMP_RESULTS = 1'b0
);

    localparam int unsigned IMEM_LENGTH = 524288; // 2MB
    localparam int unsigned DMEM_LENGTH = 32'h20000; // 128KB

    logic clk = 1'b0;
    logic rst = 1'b1;
    logic rst_and_startup_done;

    logic start_pc_offset_valid;
    logic [31:0] start_pc_offset_value;
    logic start_enable_trace;
    logic start_pc_offset_rdy;

    logic start_result_rden;
    logic start_result_empty;

    logic write_imem_valid;
    logic [31:0] write_imem_addr;
    logic [31:0] write_imem_value;
    logic write_imem_response_valid;

    logic read_dmem_valid;
    logic [31:0] read_dmem_addr;
    logic read_dmem_response_valid;
    logic [31:0] read_dmem_response_value;

    logic write_dmem_valid;
    logic [31:0] write_dmem_addr;
    logic [31:0] write_dmem_value;
    logic write_dmem_response_valid;

    longint unsigned TIMEOUT_CYCLES = 100000;

    typedef logic [7:0] uint8_t;

    logic stdout_char_valid;
    uint8_t stdout_char_ch;

    always #5 clk = ~clk; // 100 MHz clock

    risc_v_wrapper processor
    (
        .clk                                (clk),
        .rst                                (rst),
        .rst_and_startup_done_out           (rst_and_startup_done),

        .start_valid_in                     (start_pc_offset_valid),
        .start_rdy_out                      (start_pc_offset_rdy),
        .start_pc_offset_in                 (start_pc_offset_value),
        .start_enable_trace_in              (start_enable_trace),

        .start_rden_in                      (start_result_rden),
        .start_empty_out                    (start_result_empty),

        .write_imem_valid_in                (write_imem_valid),
        .write_imem_addr_in                 (write_imem_addr),
        .write_imem_value_in                (write_imem_value),

        .write_imem_valid_out               (write_imem_response_valid),

        .read_dmem_valid_in                 (read_dmem_valid),
        .read_dmem_addr_in                  (read_dmem_addr),

        .read_dmem_valid_out                (read_dmem_response_valid),
        .read_dmem_result_out               (read_dmem_response_value),

        .write_dmem_valid_in                (write_dmem_valid),
        .write_dmem_addr_in                 (write_dmem_addr),
        .write_dmem_value_in                (write_dmem_value),

        .write_dmem_valid_out               (write_dmem_response_valid),

        .send_stdout_char_valid_out         (stdout_char_valid),
        .send_stdout_char_ch_out            (stdout_char_ch),

        .stall_rate_in                      ('0),
        .stall_rate_valid_in                (1'b0),
        .stall_rate_supported_out           ()
    );

    KanagawaSimValidToMailbox
    #(
        .T          (uint8_t)
    ) mb_stdout_ch
    (
        .clk        (clk),
        .rst        (rst),

        .valid_in   (stdout_char_valid),
        .data_in    (stdout_char_ch)
    );

    clocking cb @(posedge clk);
        input rst_in = rst, rst_and_startup_done;
        output rst_out = rst;

        input start_pc_offset_rdy;
        input #0 start_result_empty;

        input #0 write_imem_response_valid;

        input read_dmem_response_valid;
        input read_dmem_response_value;

        input write_dmem_response_valid;

        output start_pc_offset_valid, start_result_rden;
        output start_pc_offset_value, start_enable_trace;
        output write_imem_valid, write_imem_addr, write_imem_value;
        output read_dmem_valid, read_dmem_addr;
        output write_dmem_valid, write_dmem_addr, write_dmem_value;
    endclocking

    class TimeoutHelper;
        // This class is used to help with timeouts in the testbench.
        // It provides a simple interface to wait for a condition to be met or timeout.
        // The user can specify a timeout in cycles, and the class will wait for the condition
        // to be met or timeout after the specified number of cycles.
        local longint unsigned timeout_cycles;
        local longint unsigned cycles_remaining;
        function new(longint unsigned timeout_cycles = TIMEOUT_CYCLES);
            this.timeout_cycles = timeout_cycles;
            this.cycles_remaining = timeout_cycles;
        endfunction
        function automatic bit has_timed_out();
            if (this.cycles_remaining == 0) begin
                return 1'b1;
            end
            this.cycles_remaining--;
            return 1'b0;
        endfunction
        function automatic void reset();
            this.cycles_remaining = this.timeout_cycles;
        endfunction
    endclass

`ifdef DISPLAY_CYCLE_COUNTER
    longint unsigned cycles;
    always_ff @(posedge clk) begin
        if (rst) begin
            cycles <= 0;
        end
        else begin
            if (0 == (cycles % 10000)) begin
                $display("Cycles: %d", cycles);
            end
            cycles <= cycles + 1'b1;
        end
    end
`endif

    task automatic reset_and_wait_for_startup_done;
        longint unsigned timeout_cycles_remaining;

        cb.start_pc_offset_valid <= 1'b0;
        cb.start_result_rden <= 1'b0;
        cb.write_imem_valid <= 1'b0;
        cb.write_imem_addr <= 'x;
        cb.write_imem_value <= 'x;
        cb.read_dmem_valid <= 1'b0;
        cb.read_dmem_addr <= 'x;
        cb.write_dmem_valid <= 1'b0;
        cb.write_dmem_addr <= 'x;
        cb.write_dmem_value <= 'x;
        cb.rst_out <= 1'b1;
        repeat (2) @(cb);
        cb.rst_out <= 1'b0;
        @(cb);

        timeout_cycles_remaining = TIMEOUT_CYCLES;
        while (!cb.rst_and_startup_done) begin
            @(cb);
            timeout_cycles_remaining--;
            if (timeout_cycles_remaining == 0) begin
                $error("Timeout waiting for startup done signal.");
                timeout_cycles_remaining = TIMEOUT_CYCLES; // Reset timeout in case user wants to continue in GUI
            end
        end
    endtask

    task automatic load_imem(input string file_name);
        int unsigned i;
        logic [31:0] instr;
        int unsigned imem[IMEM_LENGTH];

        for (int unsigned i = 0; i < IMEM_LENGTH; ++i) begin
            imem[i] = '0;
        end

        $display("Loading up to %0d words into IMEM from %s", IMEM_LENGTH, file_name);
        $readmemh(file_name, imem, 0);

        @(cb); // Align with clock edge
        for (int unsigned i = 0; i < $size(imem); ++i) begin
            instr = imem[i];
            cb.write_imem_addr <= i;
            cb.write_imem_value <= instr;
            cb.write_imem_valid <= 1'b1;
            @(cb);
            while (!cb.write_imem_response_valid) begin
                @(cb);
            end
        end
        cb.write_imem_valid <= 1'b0;
        cb.write_imem_addr <= 'x;
        cb.write_imem_value <= 'x;
        @(cb);
    endtask

    task automatic load_dmem(input string file_name);
        int unsigned i;
        logic [31:0] data;
        int unsigned dmem[DMEM_LENGTH / 4];
        for (int unsigned i = 0; i < DMEM_LENGTH / 4; ++i) begin
            dmem[i] = '0;
        end

        $display("Loading up to %0d words from %s", DMEM_LENGTH / 4, file_name);
        $readmemh(file_name, dmem);

        @(cb); // Align with clock edge
        for (int unsigned i = 0; i < $size(dmem); ++i) begin
            data = dmem[i];
            cb.write_dmem_addr <= i * 4;
            cb.write_dmem_value <= data;
            cb.write_dmem_valid <= 1'b1;
            @(cb);
            while (!cb.write_dmem_response_valid) begin
                @(cb);
            end
        end
        cb.write_dmem_valid <= 1'b0;
        cb.write_dmem_addr <= 'x;
        cb.write_dmem_value <= 'x;
        @(cb);
    endtask

    task automatic run_test(input int unsigned start_pc, bit enable_instruction_trace);
        TimeoutHelper timeout_helper = new(TIMEOUT_CYCLES);

        @(cb); // Align with clock edge

        $display("Starting RISC-V test at PC offset: 0x%0h", start_pc);

        // Assert valid and set input values
        cb.start_pc_offset_valid <= 1'b1;
        cb.start_pc_offset_value <= start_pc;
        cb.start_enable_trace <= enable_instruction_trace;
        @(cb);
        // If ready was asserted last cycle, we're done, otherwise wait until it is.
        while (!cb.start_pc_offset_rdy) begin
            assert(!timeout_helper.has_timed_out()) else
                $error("Timeout waiting for start_pc_offset_rdy signal to be asserted.");
            @(cb);
        end
        cb.start_pc_offset_valid <= 1'b0;
        cb.start_pc_offset_value <= 'x;
        cb.start_enable_trace <= 1'bx;

        // Wait for the empty signal to be de-asserted, indicating that the processor has finished running.
        timeout_helper.reset();
        while (cb.start_result_empty) begin
            if (timeout_helper.has_timed_out()) begin
                 $error("Timeout waiting for start_result_empty signal to be deasserted.");
            end
            @(cb);
        end
        cb.start_result_rden <= 1'b1;
        @(cb);
        cb.start_result_rden <= 1'b0;
        @(cb);
    endtask

    task automatic readmemh_array
    (
        input  string filename,
        input  int unsigned max_size,   // maximum allowed words
        output int unsigned arr[]       // dynamic array returned
    );
        int fd;
        int unsigned value;
        int num_read;
        string line;
        int unsigned temp[]; // temporary fixed-size storage
        int count;

        begin
            count = 0;
            temp = new[max_size]; // allocate max storage

            fd = $fopen(filename, "r");
            if (fd == 0) begin
                $fatal(1, "ERROR: Could not open file '%s'", filename);
            end

            while (!$feof(fd)) begin
                line = "";
                void'($fgets(line, fd));

                num_read = $sscanf(line, "%h", value);
                if (num_read == 1) begin
                    if (count >= max_size) begin
                        $fatal(1, "ERROR: File '%s' has more than %0d words", filename, max_size);
                    end
                    temp[count] = value;
                    count++;
                end
            end

            $fclose(fd);

            // Allocate output array to exact size and copy
            arr = new[count];
            for (int i = 0; i < count; i++)
                arr[i] = temp[i];
        end
    endtask

    task automatic dmem_results(input string results_file, input int unsigned results_offset);
        int unsigned i;
        bit mismatch;
        bit matched;
        logic [31:0] expected_value;
        int unsigned expected[];
        TimeoutHelper timeout_helper = new(TIMEOUT_CYCLES);

        assert(results_offset % 4 == 0) else
            $error("DMEM results offset %0d is not 32-bit word aligned.", results_offset);

        assert(results_offset < DMEM_LENGTH) else
            $error("Results offset %0d exceeds maximum DMEM length %0d.", results_offset, DMEM_LENGTH);

        readmemh_array(results_file, DMEM_LENGTH/4, expected);

        $display("Checking DMEM at offset %0d for %0d values from %s", results_offset, expected.size(), results_file);

        matched = 1'b1;

        @(cb); // Align with clock edge
        for (int unsigned i = 0; i < expected.size(); ++i) begin
            logic [31:0] addr;

            addr = i * 4 + results_offset;
            cb.read_dmem_addr <= addr;
            cb.read_dmem_valid <= 1'b1;
            @(cb);
            cb.read_dmem_valid <= 1'b0;
            cb.read_dmem_addr <= 'x;

            timeout_helper.reset();
            while (!cb.read_dmem_response_valid) begin
                if (timeout_helper.has_timed_out()) begin
                    $error("Timeout waiting for read_dmem_response_valid signal to be asserted for address %0h.", addr);
                    timeout_helper.reset();
                end
                @(cb);
            end

            expected_value = expected[i];

            mismatch = (cb.read_dmem_response_value !== expected_value);
            if (mismatch) begin
                matched = 1'b0;
            end

            if (DUMP_RESULTS) begin
                $display("[0x%08h]: A = 0x%08h, E = 0x%08h, %s",
                    addr, cb.read_dmem_response_value, expected_value, (mismatch ? "*" : ""));
            end
            else if (mismatch) begin
                $error("Mismatch at address %0h: expected 0x%0h, got 0x%0h",
                    addr, expected_value, cb.read_dmem_response_value);
            end
        end

        if (matched) begin
            $display("SUCCESS: All DMEM values matched expected values");
        end
        else begin
            $error("One or more DMEM values did not match the expected value");
        end
    endtask

    task automatic check_results(input string check_results_file);
        int fd;
        string line;
        string code_str;
        string label_line;
        string addr_line;
        string expected_line;
        int unsigned addr;
        int unsigned expected;
        logic [31:0] actual;
        bit at_least_one_check = 1'b0;
        TimeoutHelper timeout_helper = new(TIMEOUT_CYCLES);
        int group_idx = 0;

        fd = $fopen(check_results_file, "r");
        if (fd == 0) begin
            $error("Could not open check results file '%s'", check_results_file);
            return;
        end

        while (1) begin
            // Read code line (skip blank / whitespace-only lines)
            code_str = "";
            while (code_str == "" && !$feof(fd)) begin
                line = "";
                void'($fgets(line, fd));
                if ($sscanf(line, "%s", code_str) != 1)
                    code_str = "";
            end
            if (code_str == "") begin
                break; // EOF or only blanks left
            end
            if (!(code_str == "p" || code_str == "c")) begin
                $error("Group %0d: Invalid code '%s' (expected 'p' or 'c')", group_idx, code_str);
                break;
            end

            // Label line
            if ($feof(fd)) begin
                $error("Group %0d: Unexpected EOF reading label line", group_idx);
                break;
            end
            label_line = "";
            void'($fgets(label_line, fd));
            // Retain label as-is (may be empty)

            // Address line
            if ($feof(fd)) begin
                $error("Group %0d: Unexpected EOF reading address line", group_idx);
                break;
            end
            addr_line = "";
            void'($fgets(addr_line, fd));
            if ($sscanf(addr_line, "%h", addr) != 1) begin
                $error("Group %0d: Could not parse address from '%s'", group_idx, addr_line);
                break;
            end

            // Expected value if compare
            expected = '0;
            if (code_str == "c") begin
                if ($feof(fd)) begin
                    $error("Group %0d: Unexpected EOF reading expected value line", group_idx);
                    break;
                end
                expected_line = "";
                void'($fgets(expected_line, fd));
                if ($sscanf(expected_line, "%h", expected) != 1) begin
                    $error("Group %0d: Could not parse expected value from '%s'", group_idx, expected_line);
                end
            end

            // Issue DMEM read
            @(cb);
            cb.read_dmem_addr  <= addr;
            cb.read_dmem_valid <= 1'b1;
            @(cb);
            cb.read_dmem_valid <= 1'b0;
            cb.read_dmem_addr  <= 'x;

            timeout_helper.reset();
            while (!cb.read_dmem_response_valid) begin
                if (timeout_helper.has_timed_out()) begin
                    $error("Group %0d: Timeout waiting for DMEM read response at address 0x%08h", group_idx, addr);
                    timeout_helper.reset();
                end
                @(cb);
            end
            actual = cb.read_dmem_response_value;

            // Output / compare
            if (code_str == "p") begin
                if (label_line.len() > 0) begin
                    $display("%s [0x%08h]: 0x%08h", label_line, addr, actual);
                end
                else begin
                    $display("[0x%08h]: 0x%08h", addr, actual);
                end
            end
            else begin
                bit mismatch = (actual !== expected);
                at_least_one_check = 1'b1;
                if (label_line.len() > 0) begin
                    $display("%s [0x%08h]: 0x%08h", label_line, addr, actual);
                end
                else begin
                    $display("[0x%08h]: 0x%08h", addr, actual);
                end

                if (mismatch) begin
                    $error("Mismatch at address 0x%08h: expected 0x%08h, got 0x%08h",
                        addr, expected, actual);
                end
            end

            group_idx++;
        end

        if (at_least_one_check) begin
            $display("SUCCESS: Matched all expected values");
        end

        $fclose(fd);
    endtask

    mailbox #(string) mb_stdout = new;

    task automatic stdout_handler;
        uint8_t ch;
        string line = "";

        forever begin
            mb_stdout_ch.get(ch);

            if (ch == 8'h0A || ch == 8'h0D || ch == 8'h00) begin
                if (line.len() > 0) begin
                    $display("    > %s", line);
                    mb_stdout.put(line);
                    line = ""; // reset
                end
            end
            else begin
                line = {line, ch};
            end
        end
    endtask

    function automatic bit string_contains(string line_to_search, string match_expr);
        int line_len = line_to_search.len();
        int match_len = match_expr.len();

        if (match_len == 0) return 1'b1;
        if (match_len > line_len) return 1'b0;

        for (int i = 0; i <= line_len - match_len; i++) begin
            if (line_to_search.substr(i, i + match_len - 1) == match_expr) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction

    task automatic check_stdout(input string expected);
        string line;
        bit found = 1'b0;

        while (mb_stdout.try_get(line)) begin
            if (string_contains(line, expected)) begin
                found = 1'b1;
                break;
            end
        end

        if (found) begin
            $display("SUCCESS: found expected value, '%s', in stdout", expected);
        end
        else begin
            $error("Expected value, '%s' not found in stdout", expected);
        end
    endtask

    task automatic run();
        string imem_file, dmem_file, dmem_results_file, check_results_file, check_results_stdout, timeout_cycles_str;
        int enable_instruction_trace;
        int unsigned dmem_results_offset, start_pc_offset, timeout_cycles;
        // Timing variables for run_test
        time run_start_time, run_end_time, run_elapsed_time;
        longint unsigned run_cycle_count;
        real run_elapsed_ms;

        if ($value$plusargs("TIMEOUT_CYCLES=%d", timeout_cycles_str)) begin
            assert($sscanf(timeout_cycles_str, "%d", TIMEOUT_CYCLES) == 1)
            else $error("Invalid value for TIMEOUT_CYCLES: %s", timeout_cycles_str);
            $display("Using TIMEOUT_CYCLES: %0d", TIMEOUT_CYCLES);
        end
        else begin
            TIMEOUT_CYCLES = 100000;
            $display("Using default TIMEOUT_CYCLES value: %0d", TIMEOUT_CYCLES);
        end

        reset_and_wait_for_startup_done();

        if ($value$plusargs("IMEM_FILE=%s", imem_file)) begin
            load_imem(imem_file);
        end else begin
            $error("No instruction memory file specified.");
        end

        if ($value$plusargs("DMEM_FILE=%s", dmem_file)) begin
            load_dmem(dmem_file);
        end else begin
            $display("No data memory file specified, skipping DMEM load.");
        end

        if ($value$plusargs("START_PC_OFFSET=%d", start_pc_offset)) begin
            $display("Using START_PC_OFFSET: 0x%08h", start_pc_offset);
        end
        else begin
            start_pc_offset = 0;
            $display("Using default START_PC_OFFSET: 0x%08h", start_pc_offset);
        end

        if (!$value$plusargs("ENABLE_INSTRUCTION_TRACE=%d", enable_instruction_trace)) begin
            enable_instruction_trace = 0;
        end
        if (enable_instruction_trace != 0) begin
            $display("Enabling instruction trace");
        end

        // Start timing and run test
        run_start_time = $time;
        run_test(start_pc_offset, (enable_instruction_trace != 0));
        run_end_time = $time;
        run_elapsed_time = run_end_time - run_start_time;
        run_cycle_count = run_elapsed_time / 10; // 10ns per cycle at 100 MHz
        run_elapsed_ms = run_elapsed_time / 1e6; // ns -> ms
        $display("Test execution completed in %0t ns (%0d cycles @100MHz, %0.3f ms)",
                 run_elapsed_time, run_cycle_count, run_elapsed_ms);

        if ($value$plusargs("DMEM_RESULTS_FILE=%s", dmem_results_file)) begin
            if (!$value$plusargs("DMEM_RESULTS_OFFSET=%d", dmem_results_offset)) begin
                dmem_results_offset = 0;
            end
            dmem_results(dmem_results_file, dmem_results_offset);
        end
        else if ($value$plusargs("CHECK_RESULTS_FILE=%s", check_results_file)) begin
            check_results(check_results_file);
        end
        else if ($value$plusargs("CHECK_RESULTS_STDOUT=%s", check_results_stdout)) begin
            check_stdout(check_results_stdout);
        end
        else begin
            $display("No results check specified, skipping.");
        end

        $finish;
    endtask

    initial begin
        fork
            run();
            stdout_handler();
        join
    end

endmodule
