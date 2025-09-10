// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
The module implements a RISC-V processor core using RV32I base ISA with optional
support for MUL* instructions from the "M" standard extension. Its features
include support for multiple hardware threads (harts), Memory Mapped IO,
instruction fetching from external memory (with an option to implement
instruction cache), and support for custom instructions. The microarchitecture
deploys a five-stage pipeline and dynamic branch prediction.

Processor pipeline stalls occur only when a handler provided by the Execution
Environment doesn't return results immediately (e.g., when implementing
asynchronous Memory Mapped IO load/store or instruction fetching from external
memory with multi-cycle read), or if a trap/exception handler stalls. There are
no stalls due to late results or data hazards.

The RISC-V core can be configured into various variants that make different
area/fmax/performance trade-offs.

### Hardware Threads

The core can be configured with 1, 2, 4, or 8 hardware threads (harts). Harts
have independent architectural states but are time-multiplexed onto underlying
core hardware and may share micro-architecture state (e.g., branch target
buffer). All harts share tightly coupled instruction memory, and depending on
configuration, may or may not share data memory or external instruction memory.

#### One Hardware Thread

The 1-hart configuration is designed to balance single-threaded execution
performance and small core size.

Instruction                                         Cycles
------------------                                  --------
Branch predicted (taken or not taken)               1
Branch mispredicted                                 4
Memory load                                         2
ECALL, EBREAK                                       1+
MUL, MULH, MULHSU, MULHU                            5
All other instructions                              1

#### Two Hardware Threads

The 2-hart configuration provides 2 hardware execution threads and is designed
to balance high IPC (instructions per cycle) and single-thread execution
performance.

Instruction                                         Cycles
------------------                                  --------
Branch predicted (taken or not taken)               1
Branch mispredicted                                 3 (or 4 in Fmax optimized configuration)
Memory load                                         1
ECALL, EBREAK                                       1+
MUL, MULH, MULHSU, MULHU                            4
All other instructions                              1

#### Four Hardware Threads

The 4-hart configuration provides 4 hardware execution threads and supports
1-cycle execution of all instructions, including multiplication instructions
from the "M" extension.

Instruction                                         Cycles
------------------                                  --------
Branch predicted (taken or not taken)               1
Branch mispredicted                                 2 (or 3 in Fmax optimized configuration)
Memory load                                         1
ECALL, EBREAK                                       1+
MUL, MULH, MULHSU, MULHU                            1
All other instructions                              1

#### Eight Hardware Threads

The 8-hart configuration provides 8 hardware execution threads with an average
performance of 1 instruction per cycle. This configuration is designed for
highly parallel workloads that need maximum aggregate performance but can
tolerate lower per-thread throughput.

Instruction                                         Cycles
------------------                                  --------
Branch predicted (taken or not taken)               1
Branch mispredicted                                 1
Memory load                                         1
ECALL, EBREAK                                       1+
MUL, MULH, MULHSU, MULHU                            1
All other instructions                              1

### Memory Map

Memory map can be controlled using `RISC_V` template parameters. In the default
configuration, instruction memory starts at address 0, and separate data memory
for each hardware thread follows immediately after instruction memory, e.g.:

`RISC_V<2, 0x1000, 0x2000> core;`

@@
    Address
    -------

      24576  +----------+----------+
             |          |          |
             |          |          |
             |          |          |
             |          |          |
             |  Hart 0  |  Hart 1  |
             |          |          |
             | DMEM TCM | DMEM TCM |
             |          |          |
             |          |          |
             |          |          |
             |          |          |
             |          |          |
      16384  +----------+----------+
             |                     |
             |                     |
             |                     |
             |      IMEM TCM       |
             |                     |
             |                     |
             |                     |
             |                     |
          0  +---------------------+
@@

The core implements fast unaligned memory access in hardware, and there is no
benefit to compile programs to force only aligned access (e.g., using gcc
strict-align flag).

All memory and MMIO addresses in the interfaces between the core and the
execution environment are byte-aligned and all sizes are in bytes, unless
explicitly specified otherwise.

### Extensibility

The core can be customized via callbacks provided by the Execution Environment
to handle Memory Mapped IO, instruction fetching from external memory, system
traps and exceptions, decoding and execution of custom instructions, and dynamic
instruction tracing. All callbacks have default values and need to be overridden
only when a particular functionality is enabled.

Callbacks can be implemented as regular inline functions or fixed latency extern
functions implemented in Verilog.

### Branch Prediction

The RISC-V core implements dynamic branch prediction using a 2-bit Branch Target
Buffer.

@@
          +------------+
          |            |
    taken |            v
          |     +-------------+     not taken     +-------------+
          |     | take:  true | +---------------> | take: true  |
          +-----+-------------|                   |-------------|
                | hist:  true | <---------------+ | hist: false |
                +-------------+     taken         +-------------+
                       ^                                 +
                       |                                 |
                       | taken                           | not taken
                       |                                 |
                       +                                 v
                +-------------+     taken         +-------------+
                | take: false | <---------------+ | take: false |
                |-------------|                   |-------------+----+
                | hist: true  | +---------------> | hist: false |    |
                +-------------+     not taken     +-------------+    |
                                                         ^           | not taken
                                                         |           |
                                                         +-----------+
@@

Size of BTB can be configured via `BTB_SIZE` parameter.
*/
module processor.risc_v
    { RISC_V
    , Base
    , Extension
    , Trap
    , Optimize
    , Option
    , MemorySize
    , RVG
    , Funct3
    , Funct7
    , Format
    }

import processor.risc_v.internal.core
import processor.risc_v.isa
import processor.risc_v.trace as T

template <auto N>
inline void async_pipelined_do(() -> bool body, () -> void completion)
{
    using body_t = decltype(body);
    using completion_t = decltype(completion);

    template <typename Body, typename Completion>
    class async_pipelined_helper
    {
    public:
        [[pipelined, async]] void loop(auto tid, Body body, Completion completion)
        {
            [[unordered]]
            do ; while (body());

            bool last;

            atomic
            {
                static index_t<N> exited = 0;

                last = exited == N - 1;
                exited++;
            }

            if (last)
            {
                completion();
            }
        }
    }

    static async_pipelined_helper<body_t, completion_t> helper;
    helper.loop(N, body, completion);
}

/*|
Class template implementing RISC-V processor core.

#### __Examples__

A simple 1-hart core with 8KB of data and 64KB of instruction tightly coupled
memory (TCM).

    import processor.risc_v

    const auto HARTS = 1;
    const auto IMEM_LENGTH = 0x4000;
    const auto DMEM_LENGTH = 0x2000;

    RISC_V<1, IMEM_LENGTH DMEM_LENGTH> core;

By default the data TCM is implemented using simple dual-port memory, and the
methods that expose DMEM access to the Execution Environment use the same
read/write ports as the core. In this configuration, access to data TCM is safe
only when the core is not running. Data TCM can be configured to use quad-port
memory (on devices that support it) using the `Memory` template argument. In
this configuration, the Execution Environment can read and write data memory
concurrently with the core. Note that concurrent writes to the same address are
still undefined. The Execution Environment may have at most one call-site for
a DMEM read method and one for a DMEM write method.

    import processor.risc_v

    const auto HARTS = 1;
    const auto IMEM_LENGTH = 0x4000;
    const auto DMEM_LENGTH = 0x2000;
    const auto MMIO_LENGTH = 0x0;

    template <typename T, auto N>
    using memory_quad_port = [[memory, quad_port]] T[N];

    RISC_V<
        HARTS,
        IMEM_LENGTH,
        DMEM_LENGTH,
        MMIO_LENGTH,
        0,
        (IMEM_LENGTH << 2),
        0,
        IMEM_LENGTH,
        memory_quad_port> core;

*/
template <
    //| Number of hardware threads (harts), must be a power of 2.
    auto HARTS,
    //| Length of instruction memory address space in 32-bit words.
    auto IMEM_LENGTH,
    //| Length of data memory in bytes. By default the value specifies length
    // of per-hart data memory. If `Option::HartsShareDMEM` bit of `CONFIG`
    // parameter is set, the value specifies total length of data memory shared
    // by all harts. In multi-hart configuration `DMEM_LENGTH` must be a power
    // of 2 unless shared DMEM is used.
    auto DMEM_LENGTH,
    //| Length of Memory Mapped IO address space in bytes. The default value is
    // `0` which disables MMIO support.
    auto MMIO_LENGTH = 0,
    //| Start address of address space mapped to instruction memory in 32-bit
    // words.
    auto IMEM_ORIGIN = 0,
    //| Start address of address space mapped to data memory. By default data
    // memory starts right after instruction memory.
    auto DMEM_ORIGIN = ((IMEM_ORIGIN + IMEM_LENGTH) << 2),
    //| Beginning address of Memory Mapped IO address space. By default memory
    // mapped IO starts right after data memory.
    auto MMIO_ORIGIN = DMEM_ORIGIN + DMEM_LENGTH,
    //| Size in 32-bit words of tightly coupled instruction memory (TCM)
    // instantiated by the core. The default value is equal to `IMEM_LENGTH`,
    // which means that all of the instruction address space is mapped to the
    // internal TCM. If the specified value is less than the `IMEM_LENGTH`, the
    // reminder of instruction memory address space is handled by `external_fetch`
    // callback.
    auto IMEM_TCM_SIZE = IMEM_LENGTH,
    //| Type alias template providing underlying implementation of data memory.
    // The default is `mmemory_norep`.
    template <typename, auto> typename DataMemory = memory_norep,
    //|  ISA extensions implemented by the core. The default is
    // `Extension::None`. The only other valid value is `Extension::M`
    auto EXTENSIONS = Extension::None,
    //| Configuration flags. The default is `Optimize::Area`. Possible
    // optimization flags are `Optimize::Area`, `Optimize::Fmax` (mutually
    // exclusive). Other supported flags are defined by the `Option` enum.
    auto CONFIG = Optimize::Area,
    //| Specifies the base ISA. The default and the only supported value at
    // this time is `Base::RV32I`
    Base ISA = Base::RV32I,
    //| Number of entries in Branch Target Buffer. The default of 1024 is
    // usually a good value for BTB realized as block RAM. If using block RAM
    // is undesirable, a small BTB, for example with 8 entries, can give good
    // results. When BTB_SIZE is less than 1024, the BTB is implemented as an
    // array. The value must be a power of 2.
    auto BTB_SIZE = 1024,
    //| Type alias template providing underlying implementation of instruction memory.
    // The default is `mmemory_init` which instantiates memory with initialization.
    // Specifying `memory` instead may be useful when initialization is not desirable,
    // for example to allow using memories such as Xilinx UltraRAM which don't support
    // initialization. Note that when using memory without initialization, the execution 
    // environment must make sure that that any area of IMEM the core might read from
    // is initialized before starting the core. Failure to do so will likely result
    // in asserts during simulation due to undefined data being read from IMEM.
    template <typename, auto> typename InstrMemory = memory_init>
class RISC_V
{
    using core_t = Core<
        HARTS,
        IMEM_LENGTH,
        DMEM_LENGTH,
        MMIO_LENGTH,
        IMEM_ORIGIN,
        DMEM_ORIGIN,
        MMIO_ORIGIN,
        IMEM_TCM_SIZE,
        DataMemory,
        EXTENSIONS,
        CONFIG,
        ISA,
        BTB_SIZE,
        InstrMemory>;

public:
    using system_trap_t    = core_t::system_trap_t;
    using trace_t          = core_t::trace_t;
    using mmio_access_t    = core_t::mmio_access_t;
    using mmio_load_t      = core_t::mmio_load_t;
    using mmio_store_t     = core_t::mmio_store_t;
    using external_fetch_t = core_t::external_fetch_t;
    using custom_decode_t  = core_t::custom_decode_t;
    using custom_execute_t = core_t::custom_execute_t;

    using imem_addr_t      = core_t::imem_addr_t;
    using dmem_addr_t      = core_t::dmem_addr_t;
    using register_index_t = core_t::register_index_t;
    using int_t            = core_t::int_t;
    using uint_t           = core_t::uint_t;
    using hart_index_t     = core_t::hart_index_t;

    //| Start the core.
    inline void start
        ( uint_t[HARTS] pc //< Array of initial program counters for each hart.
        )
    {
        bool start = false;

        atomic
        {
            if (!running)
            {
                start = true;
                running = true;
            }
        }

        if (start)
        {
            core.initialize(pc);

            async_pipelined_do<threads>(
                []
                {
                    core.pipeline();
                    return !exit;
                },
                []
                {
                    atomic
                    {
                        assert(exit);
                        assert(running);

                        exit = false;
                        running = false;
                    }
                }
            );
        }
    }

    //| Request the core to stop. Note that the method doesn't block until the
    // core has finished running. Use `is_running` method to wait for the core to stop.
    inline void stop()
    {
        atomic
        {
            if (running)
            {
                assert(!exit);
                exit = true;
            }
        }
    }

    //| Returns `true` if the core is running. Can be used to wait for the core
    // to finish running after stop request.
    inline bool is_running()
    {
        return running;
    }

    //| Initialize stack pointers. Not necessary if the firmware startup code
    // performs the initialization. When the option `Option::HartsShareDMEM` is set
    // then each hart is allocated its own stack of the specified size, with the stack
    // for hart 0 starting at the specified address, and stacks for remaining harts
    // following below it in DMEM address space.
    inline void init_stack_pointers
        ( uint_t stack_start //< Start address of the stack. Must be 16 byte aligned.
                             // Since the stack grows down, the starting address usually
                             // is set to `DMEM_ORIGIN + DMEM_LENGTH - 0x10`
        , uint_t stack_size  //< Size of the stack for each hart.
        )
    {
        assert(running == false);

        static for(const auto hid : HARTS)
        {
            // By ABI calling convention stack pointer should be 16 bytes aligned.
            assert((stack_start & 0xf) == 0);

            // Don't generate invalid stack pointers
            assert(stack_start - stack_size >= DMEM_ORIGIN && stack_start < DMEM_ORIGIN + DMEM_LENGTH ||
                       stack_start - stack_size >= MMIO_ORIGIN && stack_start < MMIO_ORIGIN + MMIO_LENGTH);

            core.register_set(hid, ABI::sp, cast<int_t>(stack_start));

            static if ((CONFIG & Option::HartsShareDMEM) == Option::HartsShareDMEM)
            {
                // Set a different stack pointer for each hart.
                stack_start -= stack_size;
            }
        }
    }

    //| Print a diagram of memory address space configuration.
    inline void print_memory_map()
    {
        T::print_memory_map<
            IMEM_LENGTH,
            DMEM_LENGTH,
            MMIO_LENGTH,
            IMEM_ORIGIN,
            DMEM_ORIGIN,
            MMIO_ORIGIN,
            IMEM_TCM_SIZE>();
    }

    //| Print dynamic instruction trace. The method can be used to initialize
    // the `trace` callback.
    inline void print_trace
        ( hart_index_t hid      //< Hart index.
        , imem_addr_t addr      //< Address of 32-bit instruction word.
        , uint_t inst           //< Instruction word.
        , Decoded decoded       //< Decoded instruction. Note that `Decoded` is an internal
                                // data type and may change w/o notice.
        , optional<int_t> value //< An optional 32-bit value written by the instruction to the
                                // target register.
        )
    {
        T::print_trace(hid, addr, inst, decoded, value);
    }

    //| Complete asynchronous external instruction fetch.
    inline void external_fetch_result
        ( hart_index_t hid  //< Index of the hart that performed the fetch.
        , uint_t addr       //< Address of 32-bit instruction word that was fetched.
        , uint_t value      //< 32-bit instruction word.
        )
    {
        core.external_fetch_result(hid, checked_cast<imem_addr_t>(addr), value);
    }

    //| Complete asynchronous load from memory mapped IO.
    inline void mmio_load_result
        ( hart_index_t hid  //< Index of the hart that performed the asynchronous load
                            // from memory mapped IO.
        , int_t value       //< Result of memory mapped IO load.
        )
    {
        core.mmio_load_result(hid, value);
    }

    //| Complete asynchronous store to memory mapped IO.
    inline void mmio_store_completed
        ( hart_index_t hid  //< Index of the hart that performed the asynchronous store
                            // to memory mapped IO.
        )
    {
        core.mmio_store_completed(hid);
    }

    //| Write 32-bit word to tightly coupled instruction memory.
    // The method is safe to call only when the core is not running.
    inline void imem_write
        ( uint_t addr  //< Address of 32-bit word in instruction TCM to write.
        , uint_t value //< 32-bit instruction word.
        )
    {
        core.imem_write(checked_cast<imem_addr_t>(addr), value);
    }

    //| Read a 32-bit word from hart's data memory.
    inline int_t dmem_read
        ( hart_index_t hid  //< Hart index.
        , uint_t addr       //< DMEM byte address to read from.
        )
    {
        return core.dmem_read(hid, checked_cast<dmem_addr_t>(addr));
    }

    //| Write specified number of bytes to hart's data memory.
    inline void dmem_write
        ( hart_index_t hid  //< Hart index.
        , uint_t addr       //< DMEM byte address to write to.
        , int_t value       //< Value to write.
        , count_t<4> size   //< Number of bytes of `value` to write.
        )
    {
        core.dmem_write(hid, checked_cast<dmem_addr_t>(addr), value, size);
    }

    //| Read a 32-bit word from hart's data memory at a 32-bit aligned address.
    // More resource efficient than `dmem_read` when unaligned address support
    // is not required.
    inline int_t dmem_read_aligned
        ( hart_index_t hid  //< Hart index.
        , uint_t addr       //< DMEM byte address to read from.
                            // Must be 32-bit aligned.
        )
    {
        return core.dmem_read_aligned(hid, checked_cast<dmem_addr_t>(addr));
    }

    //| Write a 32-bit word to hart's data memory at a 32-bit aligned address.
    // More resource efficient than `dmem_write` when unaligned address support
    // is not required.
    inline void dmem_write_aligned
        ( hart_index_t hid  //< Hart index.
        , uint_t addr       //< DMEM byte address to write to.
                            // Must be 32-bit aligned.
        , int_t value       //< 32-bit value to write.
        )
    {
        core.dmem_write_aligned(hid, checked_cast<dmem_addr_t>(addr), value);
    }

private:
    //| Callback to handle exceptions and traps. The default callback asserts
    // in simulation on any system exception.
    system_trap_t system_trap = core.default_system_trap;

    //| Dynamic instruction trace callback. The default callback is no-op.
    // The `print_trace` method can be used to print disassembled instructions
    // to the log stream.
    trace_t trace = core.default_trace;

    //| The callback handles memory load/store within MMIO address space.
    // If the access operation completes immediately, the `is_valid` of the
    // returned result should be set to true. For load access the `value` of
    // the returned result is the result of the load. For store access the
    // `value` of the returned result is ignored.
    // If the operation will complete asynchronously the `is_valid` of the
    // returned result should be set to false, and execution environment should
    // indicate completion of load/store by calling the `mmio_load_result` or
    // `mmio_store_completed` method respectively.
    mmio_access_t mmio_access = core.default_mmio_access;

    //| The callback handles memory loads within MMIO address space.
    // The callback result is `optional<int_t>`. The callback may return result
    // immediately, setting `is_valid` to true, or asynchronously by calling
    // the `mmio_load_result` method later. By default the callback asserts in
    // simulation. The callbacks is used only when `mmio_access` callback uses
    // the default implementation.
    mmio_load_t mmio_load = core.default_mmio_load;

    //| The callback handles memory stores within MMIO address space.
    // The callback result is `bool`. The callback should return `true` if the
    // program running on the core can consider the store as committed, or
    // `false` if the store should be considered as pending. In the latter
    // case, the hart that made the request stalls until the core's method
    // `mmio_store_completed` is called. By default the callback asserts in
    // simulation. The callbacks is used only when `mmio_access` callback
    // uses the default implementation.
    mmio_store_t mmio_store = core.default_mmio_store;

    //| Optional external instruction memory support is enabled by specifying
    // `IMEM_TCM_SIZE` that is less than `IMEM_LENGTH`. In this case instruction
    // fetch for addresses above `IMEM_TCM_SIZE` are directed to the Execution
    // Environment via this callback. The default callback asserts in simulation.
    external_fetch_t external_fetch = core.default_external_fetch;

    //| The `custom_decode` callback is called for instructions using one of the
    // four major opcodes reserved for custom extensions:
    //
    //     RVG::custom_0
    //     RVG::custom_1
    //     RVG::custom_2
    //     RVG::custom_3
    //
    // The callback should return one of the standard encoding formats used the
    // custom instruction, or `Format::Invalid` if the major opcode is not
    // implemented by the custom extension. The default callback decodes all
    // custom instructions as invalid.
    custom_decode_t custom_decode = core.default_custom_decode;

    //| Callback is called to execute a custom instruction.
    // The result returned by the callback is written to the destination
    // register if the instruction encoding format specifies one. The result is
    // ignored for formats `B` and `S`, or when destination register is `zero`.
    // The default callback is no-op.
    custom_execute_t custom_execute = core.default_custom_execute;

    core_t core = {
        .system_trap    = system_trap,
        .trace          = trace,
        .mmio_access    = mmio_access,
        .mmio_load      = mmio_load,
        .mmio_store     = mmio_store,
        .external_fetch = external_fetch,
        .custom_decode  = custom_decode,
        .custom_execute = custom_execute
    };

    bool exit = false;
    bool running = false;
    const auto threads = 64;
}

