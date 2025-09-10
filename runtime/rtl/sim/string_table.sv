// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


// A mapping of integers to strings, with CRUD operations based on integer string handles.

module KanagawaStringTable
(
    input logic clk
);
    typedef logic [63:0] string_handle_t;
    typedef longint signed int64_t;

    // Maps handle to string
    string string_table[string_handle_t];

    // Maps handle to reference count
    int64_t ref_count[string_handle_t];

    // Assign an unused handle to a string value
    function automatic void allocate(input string value, input string_handle_t handle);
        // The null handle should never be used
        assert(handle != 0) else $error("NULL handle allocated.  Value: %s", value);

        assert(!string_table.exists(handle)) else $error ("%m string handle reused.  Handle: %u, new value: %s, old_value: %s", handle, value, string_table[handle]);
        assert(!ref_count.exists(handle)) else $error ("%m string handle (refcount) reused.  Handle: %u, new value: %s, old_value: %s", handle, value, string_table[handle]);

        string_table[handle] = value;

        ref_count[handle] = 1;
    endfunction

    // Adjust the reference count of a string (up or down)
    function automatic void reference_string(input string_handle_t handle, input int64_t amount, input bit enable);
        if (!enable || (handle == 0)) begin
            // Adjusting the reference count on the null handle is a NOP
        end
        else begin
            assert(string_table.exists(handle)) else $error("%m handle: %u does not exist for add_reference", handle);
            assert(ref_count.exists(handle)) else $error("%m reference count does not exist for specified handle (add_reference): %u", handle);
            ref_count[handle] = ref_count[handle] + amount;
        end
    endfunction

    // Get the value of a string from a handle
    function automatic string get(input string_handle_t handle, input bit enable);
        if ((handle == 0) || !enable) begin
            // result can be used for string member variables that are initialized to an empty string
            return "";
        end

        assert(string_table.exists(handle)) else $error("%m handle: %u does not exist for get", handle);

        return string_table[handle];
    endfunction

    function automatic int get_num_strings();
        assert(ref_count.size() == string_table.size()) else $error("Ref count and string table size mismatch");

        return string_table.size();
    endfunction

    function automatic void assert_strings_equal(input string_handle_t handle1, input string_handle_t handle2, input bit enable, input string error_message);
        if (enable) begin
            string str1;
            string str2;

            if (handle1 != 0) begin
                assert(string_table.exists(handle1)) else $error("%m invalid handle1 passed to assert_strings_equal.  Handle: %u", handle1);
                str1 = string_table[handle1];
            end
            else begin
                str1 = "";
            end

             if (handle2 != 0) begin
                assert(string_table.exists(handle2)) else $error("%m invalid handle2 passed to assert_strings_equal.  Handle: %u", handle2);
                str2 = string_table[handle2];
            end
            else begin
                str2 = "";
            end

            assert(str1 == str2) else $error("Strings do not match: string1: %s string2: %s.  Stack: %s", str1, str2, error_message);
        end
    endfunction

    function automatic string align(string str, int alignment);
        string result = str;

        // Pad with space on the left
        if (alignment > 0) begin
            while (result.len() < alignment) begin
                result = {" ", result};
            end
        end

        // Pad with space on the right
        if (alignment < 0) begin
            while (result.len() < -alignment) begin
                result = {result, " "};
            end
        end

        return result;
    endfunction

    function automatic string itoa(string str, int precision, int alignment, bit to_upper, bit negative_sign);
        string result = str;

        // Remove leading spaces (inserted by $sformatf)
        while (result[0] == " ") begin
            result = result.substr(1, result.len() - 1);
        end

        if (negative_sign) begin
            result = {"-", result};
        end

        if (precision != 0) begin
            // Pad with leading zeros to ensure the result is not narrower than precision
            while (result.len() < precision) begin
                result = {"0", result};
            end

            while ((result.len() > precision) && (result[0] == "0")) begin
                // Remove leading spaces (inserted by $sformatf on some simulators)
                result = result.substr(1, result.len() - 1);
            end

            // Convert characters to upper case if requested
            if (to_upper) begin
                result = result.toupper();
            end
        end

        result = align(result, alignment);

        return result;
    endfunction

    always_ff @(negedge clk) begin
        string_handle_t zero_ref_count_list[$];

        // Find strings with reference count = 0
        // This is not done in reference_string so that
        // reference_string calls from different always_ff blocks
        // can occur in any order, and the only value that matters
        // is the reference count at the very end
        foreach (ref_count[h]) begin
            automatic string_handle_t handle = h;
            automatic int64_t rc = ref_count[h];

            assert(rc >= 0) else $error("%m negative reference count for handle: %u", handle);

            if (rc == 0) begin
                zero_ref_count_list.push_back(handle);
            end
        end

        foreach (zero_ref_count_list[h]) begin
            automatic string_handle_t handle = zero_ref_count_list[h];

            assert(string_table.exists(handle)) else $error("%m handle: %u does not exist for delete (delayed)", handle);
            string_table.delete(handle);
            ref_count.delete(handle);
        end

        zero_ref_count_list.delete();
    end

endmodule // KanagawaStringTable
