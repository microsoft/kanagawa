// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`ifndef KANAGAWA_SV_INCLUDED
`define KANAGAWA_SV_INCLUDED

package KanagawaTypes;

/* verilator lint_off UNUSEDPARAM */

// 64-bits that represents the value of an inspectable variable
localparam INSPECTABLE_VALUE_WIDTH = 16;
localparam INSPECTABLE_INDEX_WIDTH = 16; // if this changes, the compiler validation of max number of inspectables must change too
localparam INSPECTABLE_FLIT_INDEX_WIDTH = 12;
localparam INSPECTABLE_ELEMENT_INDEX_WIDTH = 16;

localparam NUM_INSPECTABLE_WIDTH = INSPECTABLE_INDEX_WIDTH;
typedef logic [NUM_INSPECTABLE_WIDTH-1:0] NumInspectables_t;
localparam NumInspectables_t NUM_INSPECTABLES_MAX = (1 << NUM_INSPECTABLE_WIDTH) - 2;

// This is a data structure used for a feature that is currently disabled. The compiler has code to generate
// a chain of "inspectables", which would allow for introspection of internal state of variables. The data
// structure and the core compiler code has been retained, although disabled, with the intent that
// in the future an improved version of this feature could be implemented.
typedef struct packed
{
    // Set to 1 if the value was not available because the variable is a memory and the memory value
    // for the specified element (address) was not ready to go.
    logic retry;

    // Set to 1 on output when this represents the last element in the memory
    // output only
    logic last_element;

    // Set to 1 on output when this represents the last flit in the variable
    // output only
    logic last_flit;

    // 1 is this structure contains valid output data
    // 0 when this structure contains only input data
    // output only
    logic valid;

    // For large variables, identifies which subset of the content value refers to - input and output
    logic [INSPECTABLE_FLIT_INDEX_WIDTH-1:0] flit_index;

    // For memories, identifies which memory element the content value refers to - input and output
    logic [INSPECTABLE_ELEMENT_INDEX_WIDTH-1:0] element_index;

        // Identifies the variable - input and output
    logic [INSPECTABLE_INDEX_WIDTH-1:0] variable_index;

    // 16 bits of the content of the variable - output only
    logic [INSPECTABLE_VALUE_WIDTH-1:0] value;
} InspectableValue;

// Number of clock cycles for write delays in large-and-fast configuration
localparam KANAGAWA_LARGE_AND_FAST_WRITE_DELAY = 3;

/* verilator lint_off UNUSEDPARAM */

endpackage

// Interface used for FIFO-likes using ready/valid protocol
/* verilator lint_off DECLFILENAME */
interface pd_fifo_intf #( parameter DATA_WIDTH = 8);
    logic                   valid;
    logic                   ready;
    logic [DATA_WIDTH-1:0]  data;

    modport IN  (input valid, input data, output ready);
    modport OUT (output valid, output data, input ready);
endinterface
/* verilator lint_on DECLFILENAME */

`endif
