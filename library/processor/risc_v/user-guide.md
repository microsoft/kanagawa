# RISC-V processor soft core user guide

## Overview

The processor soft core implements RISC-V RV32I base ISA with optional support
for MUL* instructions from the "M" standard extension. Features include
multi-hart support, Memory Mapped IO, instruction fetch from external, slow
memory (with an option to implement instruction cache) and support for custom
instructions. The microarchitecture deploys a 5 stage pipeline and dynamic
branch prediction. For more details on the implementation please refer to
microarchitecture.md.

## Getting started

The core is implemented by `RISC_V` class template defined in `processor.risc_v`
module. The code snippet below illustrates the basic usage:

```
    import processor.risc_v

    const auto HARTS = 1;
    const auto IMEM_LENGTH = 0x4000;
    const auto DMEM_LENGTH = 0x2000;
    const auto STACK_START = 0x5ff0;
    const auto STACK_SIZE = 0x300;

    using RV = RISC_V<HARTS, IMEM_LENGTH, DMEM_LENGTH>;
    using int_t = RV::int_t;
    using imem_addr_t = RV::imem_addr_t;

    class MyCore
    {
        RV core;

    public:
        void start()
        {
            core.init_stack_pointers(STACK_START, STACK_SIZE);
            core.start({0});
        }

        [[no_backpressure]]
        void write_imem(RV::imem_addr_t addr, int_t value)
        {
            core.imem_write(addr, value);
        }

        [[no_backpressure]]
        int_t read_dmem(RV::dmem_addr_t addr)
        {
            return core.dmem_read_aligned(0, addr);
        }
    }

    export MyCore;
```

## RISC_V class methods

### `start`

    inline void start(uint_t[HARTS] pc, uint_t[HARTS] sp)

Start the core.

The method takes an array of initial program counters for each hart. Note that
the method doesn't set initial value of stack pointers. Stack pointer should be
initialized by firmware startup code or by calling `init_stack_pointers` method
before calling `start`.

Note that ABI calling convention assumes that the stack is 16 bytes aligned.
A helper function `stack_pointers` can be used to generate stack pointer
initialization vector for given core configuration. Its arguments are the stack
starting address and size. Since the stack grows down, the starting address
usually is set to `DMEM_ORIGIN + DMEM_LENGTH - 0x10`.

### `init_stack_pointers`

    inline void init_stack_pointers(uint_t stack_start, uint_t stack_size)

Initialize stack pointers. Not necessary if the firmware startup code performs
the initialization.

### `stop`

    inline void stop()

Request the core to stop. Note that the method doesn't block until the core has
finished running.

### `is_running`

    inline bool is_running()

Returns `true` if the core is running. Can be used to wait for the core to
finish running after stop request:

    core.stop();
    atomic do; while(core.is_running());

### `imem_write`

    inline void imem_write(imem_addr_t addr, int_t value)

Write a word to instruction TCM at specify word index. Safe to call only when
the core is not running.

### Data TCM access

The following methods give the Execution Environment access to the core's data
TCM. The rules for concurrent access between the core and the Execution
Environment vary depending on [data memory the configuration](#data-memory).

#### `dmem_read`

    inline int_t dmem_read(hart_index_t hid, dmem_addr_t addr)

Read a word from the hart's data TCM at a byte address.

#### `dmem_write`

    inline void dmem_write(hart_index_t hid, dmem_addr_t addr, int_t value, count_t<4> size)

Write `size` bytes to the hart's data TCM at a byte address.

#### `dmem_read_aligned`

    inline int_t dmem_read_aligned(hart_index_t hid, dmem_addr_t addr)

Read a word from the hart's data TCM at a 32-bit aligned address.

#### `dmem_write_aligned`

    inline void dmem_write_aligned(hart_index_t hid, dmem_addr_t addr, int_t value)

Write a word to the harts data TCM at a 32-bit aligned address.

## Configuration options

The core is configured via two means: `RISC_V` class template arguments and
callbacks.

### Template parameters

The `RISC_V` class template has 3 required and 6 optional parameters:

    template <
        auto HARTS,
        auto IMEM_LENGTH,
        auto DMEM_LENGTH,
        auto MMIO_LENGTH = 0,
        auto IMEM_ORIGIN = 0,
        auto DMEM_ORIGIN = ((IMEM_ORIGIN + IMEM_LENGTH) << 2),
        auto MMIO_ORIGIN = DMEM_ORIGIN + DMEM_LENGTH,
        auto IMEM_TCM_SIZE = IMEM_LENGTH,
        template <typename, auto> typename Memory = memory_norep,
        auto EXTENSIONS = Extension::None,
        auto CONFIG = Optimize::Area,
        Base ISA = Base::RV32I,
        BTB_SIZE = 1024>
    class RISC_V

- `HARTS` specifies number of harts. Valid values are 1, 2 and 4.

- `IMEM_LENGTH` is the length of instruction memory address space in 32-bit words.

- `IMEM_ORIGIN` is the start address of address space mapped to instruction
  memory in 32-bit words.

- `DMEM_LENGTH` is length of data memory in bytes. By default the value
  specifies per-hart data memory length. If `Option::HartsShareDMEM` bit of
  `CONFIG` parameter is set, the value specifies total length of data memory
  shared by all harts. In multi-hart configuration `DMEM_LENGTH` must be a 
  power of 2 unless shared DMEM is used.

- `DMEM_ORIGIN` is the start address of address space mapped to data memory. By
  default data memory starts right after instruction memory.

- `MMIO_LENGTH` is length of Memory Mapped IO address space in bytes. The
  default value is `0` which disables MMIO support.

- `MMIO_ORIGIN` is beginning address of Memory Mapped IO address space. By
  default memory mapped IO starts right after data memory.

- `IMEM_TCM_SIZE` specifies size in 32-bit words of tightly coupled instruction
  memory instantiated by the core.
  The default value is equal to `IMEM_LENGTH`, which means that all of the
  instruction address space is mapped to the internal TCM. If the specified
  value is less than the `IMEM_LENGTH`, the reminder of instruction memory
  address space is handled by [external fetch](#external-fetch).

- `Memory` is a template providing underlying implementation of data memory.
  The default is `mmemory_norep`. See [data memory](#data-memory) for more
  details.

- `EXTENSIONS` specifies standard ISA extensions implemented by the core.
  The default is `Extension::None`. The only other valid value is
  `Extension::M`.

- `CONFIG` specifies configuration flags.
  The default is `Optimize::Area`. Possible flags are `Optimize::Area`,
  `Optimize::Fmax` (mutually exclusive). The `Optimize::Fmax` configuration is
  supported only by the 4 hart variant.
  
  Additionally the following `Option` flags can be specified:

  - `Option::HartsShareDMEM` (see [data memory](#data-memory) for details).
  - `Option::NoExceptionForCompressedEncoding` disables checking of lower two
  bits of instruction word which are used for compressed instruction encoding.
  This allows saving some block RAM by synthesizing out these bits.

- `ISA` specifies the base ISA. The default and the only supported value at this
  time is `Base::RV32I`.

- `BTB_SIZE` specifies number of entries in Branch Target Buffer. The default
  of 1024 is usually a good value for BTB realized as block RAM. If using block
  RAM is undesirable, a small BTB, for example with 8 entries, can give good
  results. When BTB_SIZE is less than 1024 the BTB is implemented as array.
  The value must be a power of 2.

The following diagrams shows memory/address space layout for some illustrative
example configurations.

Simple 2-hart:
    
```
    import processor.risc_v
    
    using core_t = RISC_V<2, 0x1000, 0x2000>;
```    

<!-- invalid -->
```
    Address
    -------

      24576  +---------------------+
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
      16384  +---------------------|
             |                     |
             |                     |
             |                     |
             |      IMEM TCM       |
             |                     |
             |                     |
             |                     |
             |                     |
          0  +---------------------+
```

External IMEM, shared DMEM and MMIO:

```
    import processor.risc_v

    using core_t = RISC_V<
        2,
        0x1000,            // IMEM_LENGTH
        0x2000,            // DMEM_LENGTH
        0x100,             // MMIO_LENGTH
        (0x80000000 >> 2), // IMEM_ORIGIN
        0x80004000,        // DMEM_ORIGIN
        0xA0000000,        // MMIO_ORIGIN
        0x0,               // IMEM_TCM_SIZE
        memory_norep,
        Extension::None,
        Optimize::Area | Option::HartsShareDMEM
    >;
```    

<!-- invalid -->
```
       Address
    ----------

    0xA0000100   +---------------------+
                 |                     |
                 |         MMIO        |
                 |                     |
    0xA0000000   +---------------------+




    0x80006000   +---------------------+
                 |                     |
                 |                     |
                 |                     |
                 |                     |
                 |                     |
                 |      DMEM TCM       |
                 | shared by all harts |
                 |                     |
                 |                     |
                 |                     |
                 |                     |
    0x80004000   +---------------------|
                 |                     |
                 |                     |
                 |                     |
                 |                     |
                 |    External IMEM    |
                 |                     |
                 |                     |
                 |                     |
                 |                     |
    0x80000000   +---------------------+

```

### Multi-hart configuration

The core can be configures with 1, 2 or 4 harts (or hardware threads). Harts
have independent architecture state but are time-multiplexed onto underlying
core hardware and may share micro-architecture state (e.g. branch target
buffer). All harts share instruction memory TCM, and may or may not share
[data memory](#data-memory) or [external instruction memory](#external-fetch).

See microarchitecture.md for more details on performance characteristics of
different hart configurations.

### Instruction Memory

Instruction memory is implemented as internally instantiated TCM, with size in
words specified by `IMEM_TCM_SIZE` template argument, and optional external
memory such as DRAM.

The Execution Environment can write to the instruction TCM using core's
`imem_write` method.

#### External fetch

Optional external instruction memory support is enabled by specifying
`IMEM_TCM_SIZE` that is less than `IMEM_LENGTH`. In this case fetch for
addresses above `IMEM_TCM_SIZE` are directed to the Execution Environment
via `external_fetch` callback:

    (hart_index_t, imem_addr_t) -> optional<uint_t> external_fetch

The callback takes 2 arguments, hart index and instruction memory address, and
should return an `optional<uint_t>` with either fetched instruction word or an
invalid flag if the fetch can't be satisfied immediately (e.g. if the
instruction must be fetched from slow DRAM). In the latter case the fetch result
is provided later via a call to the core `external_fetch_result` method.

### Data Memory

Data memory is implemented as internally instantiated TCM, with size in bytes
specified by `DMEM_LENGTH` template argument. By default each hart has its
separate block of data memory with address space spanning `[DMEM_ORIGIN, DMEM_LENGTH)`.
In multi-hart configuration when harts have their own DMEM block the `DMEM_LENGTH` 
must be a power of 2. The `Option::HartsShareDMEM` flags can be specified in `COFIG`
template argument to enable shared data memory, e.g. to allow programs running on
different harts to communicate via memory. With shared DMEM the `DMEM_LENGTH` does
not need to be a power of 2.

```
    import processor.risc_v

    const auto HARTS = 1;
    const auto IMEM_LENGTH = 0x4000;
    const auto DMEM_LENGTH = 0x1500;
    const auto MMIO_LENGTH = 0x0;

    using core_t = RISC_V<
        HARTS,
        IMEM_LENGTH,
        DMEM_LENGTH,
        MMIO_LENGTH,
        0,
        (IMEM_LENGTH << 2),
        0,
        IMEM_LENGTH,
        memory_norep,
        Extension::None,
        Optimize::Area | Option::HartsShareDMEM>;
```

The Execution Environment can access data TCM using core's `dmem_read` and
`dmem_write` methods. By default the TCM is implemented using simple dual port
memory, and these methods use the same read/write ports as the core. In this
configuration access to data TCM is safe only when the core is not running. Data
TCM can be configured to use quad port memory (on devices that support it) using
the `Memory` template argument:

```
    import processor.risc_v

    const auto HARTS = 1;
    const auto IMEM_LENGTH = 0x4000;
    const auto DMEM_LENGTH = 0x2000;
    const auto MMIO_LENGTH = 0x0;

    template <typename T, auto N>
    using memory_quad_port = [[memory, quad_port]] T[N];

    using core_t = RISC_V<
        HARTS,
        IMEM_LENGTH,
        DMEM_LENGTH,
        MMIO_LENGTH,
        0,
        (IMEM_LENGTH << 2),
        0,
        IMEM_LENGTH,
        memory_quad_port>;
```

In this configuration the Execution Environment can read and write data memory
concurrently with the core (writing to the same address is still undefined).
Note that there must be only one call-site for each of `dmem_read` and
`dmem_write` methods.

The core implements fast unaligned memory access in hardware and there is no
need to compile programs to force only aligned access (e.g. gcc strict-align flag).

### MMIO

Memory Mapped IO support is enabled using `MMIO_ORIGIN` template argument. The
value specifies the beginning of the address spaces mapped to IO. Memory loads
and stores within this address space are routed to the `mmio_load` and `mmio_store`
callbacks provided by the execution environment:

    (hart_index_t, uint_t, MemorySize, bool) -> optional<int_t> mmio_load,
    
The `mmio_load` callback takes 4 arguments:

- hart index
- address
- size of load: `MemorySize::B`, `MemorySize::H` or `MemorySize::W`
  corresponding to byte, half word and word respectively.
- boolean flag specifying if sub-word load should be sign-extended.

The callback result is `optional<int_t>`. The callback can return result
immediately, setting `is_valid` to true, or asynchronously by calling
`mmio_load_result` method exposed by the core later.

The callback can delegate to a fixed latency callback implemented in
Verilog, e.g.:

    [[latency(N)]]
    (uint_t addr) -> int_t Load;

where `N < HARTS`. The callback can also call a regular extern function using
`async_exec` to get result of the load asynchronously, e.g.:

```    
    import processor.risc_v

    using RV = RISC_V<1, 0x1000, 0x1000>;

    using int_t = RV::int_t;
    using uint_t = RV::uint_t;
    using hart_index_t = RV::hart_index_t;

    class MyCore
    {
        RV core = {
            .mmio_load = mmio_load
        };

        (uint_t addr) -> int_t LoadBlocking;

        inline optional<int_t> mmio_load(hart_index_t hid, uint_t addr, MemorySize size, bool sign)
        {
            async_exec([hid, addr]()
            {
                // potentially blocking, asynchronous load implemented in Verilog
                core.mmio_load_result(hid, LoadBlocking(addr));
            });

            return {};
        }
    }

    export MyCore;
```    

When the `mmio_load` callback blocks, it stalls all the harts and thus this should
be avoided. A pending MMIO load (i.e. waiting for `mmio_load_result` callback)
stalls the hart that made the request, but doesn't affect other harts.

The MMIO store callback has the following signature:

    (hart_index_t, uint_t, MemorySize, int_t) -> bool mmio_store,

Its arguments are:

- hart index
- address
- size of store: `MemorySize::B`, `MemorySize::H` or `MemorySize::W`
  corresponding to byte, half word and word respectively.
- value to be stored

The callback result is `bool`. The callback should return `true` if the program
running on the core can consider the store as committed, or `false` if the
store should be considered as pending. In the latter case, the hart that made
the request stalls until the method `mmio_store_completed` exposed by the core
is called.

The store callback can delegate to an fixed latency extern function, e.g.:

    [[latency(N)]]
    extern bool StoreLatency1(uint_t addr, int_t value);

where `N < HARTS`. If the external store implementation can block, the
callback can also call a variable latency extern function. If the program
running on the core doesn't need to wait for store completion then the extern
function can be declared as `[[async]]`, and the callback can always return
`true`, e.g.:

```    
    import processor.risc_v

    using RV = RISC_V<1, 0x1000, 0x1000>;

    using int_t = RV::int_t;
    using uint_t = RV::uint_t;
    using hart_index_t = RV::hart_index_t;

    class MyCore
    {
        RV core = {
            .mmio_store = mmio_store
        };

        [[async]] (uint_t addr, int_t value) ->void StoreBlocking;

        inline bool mmio_store(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
        {
            // potentially blocking [[async]] extern
            StoreBlocking(addr, value);
            return true;
        }
    }

    export MyCore;
```

If the program must stall until the store is committed, and the Verilog handler
can't guarantee that it can always queue up the call, then a regular extern
function can be used, e.g.:

```    
    import processor.risc_v

    using RV = RISC_V<1, 0x1000, 0x1000>;

    using int_t = RV::int_t;
    using uint_t = RV::uint_t;
    using hart_index_t = RV::hart_index_t;

    class MyCore
    {
        RV core = {
            .mmio_store = mmio_store
        };

        (uint_t addr, int_t value) -> bool StoreBlocking;

        inline bool mmio_store(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
        {
            // potentially blocking [[async]] extern
            return StoreBlocking(addr, value);
        }
    }

    export MyCore;
```

When the `mmio_store` callback blocks, it stalls all the harts and thus this should
be avoided. A pending MMIO store (i.e. waiting for `mmio_store_completed`
callback) stalls the hart that made the request, but doesn't affect other
harts.

Note that the program order of load/store operations delegated to an external Verilog
implementation is maintained only for `[[latency(N)]]` extern functions. If
a store and/or load is asynchronous, the external Verilog implementation can't assume
the order.

### System traps

System exceptions and the `ECALL` and `EBREAK` instructions trigger system
traps that can be handled by the Execution Environment provided `system_trap`
callback:

    (hart_index_t, Trap, imem_addr_t) -> optional<imem_addr_t> system_trap

The callback takes 3 arguments:

- index of the hart that triggered the trap
- trap kind:
    `Trap::ECALL`
    `Trap::EBREAK`
    `Trap::InvalidInstructionAddress`
    `Trap::AccessFault`
    `Trap::IllegalInstruction`
- address of the instruction word that triggered the trap

The hart that triggered a trap immediately pauses execution until the callback
returns control to the core. If the trap callback blocks, all harts eventually
pause execution.

The callback can return optional address of instruction word that should be next
executed by the hart (i.e. jump to a software callback). If the result's
`is_valid` is false the hart continues execution normally.

The `InvalidInstructionAddress` exception is reported on a jump or taken branch
to an address that is not 32-bit aligned or is outside of IMEM range.

The `AccessFault` exception is reported on memory access outside of mapped data
memory/mmio address space.

The `IllegalInstruction` exception is reported when the word at current PC
doesn't decode to a valid instruction in the base ISA or one of the enabled
extensions (if any).

### Dynamic instruction trace

The `trace` callback provided by the Execution Environment receives traces of
retired instructions:

    (hart_index_t, imem_addr_t, uint_t, Decoded, optional<int_t>) -> void trace

The `trace` callback takes 5 arguments:

- hart index
- program counter (address of instruction word)
- instruction word
- decoded instruction
- optional value written to destination register

The struct `Decode` is defined in `processor.risc_v.internal.decoder` module and
is considered an internal implementation detail that can change without notice,
thus taking dependency on definition of `Decode` should be avoided. The `RISC_V`
class defines `print_trace` method takes the same arguments as `trace` callback
and pretty-prints dynamic instruction trace.

```
    import processor.risc_v

    using RV = RISC_V<2, 0x4000, 0x2000>;

    class MyCore
    {
        RV core = {
            .trace = core.print_trace
        };
    }
    
    export MyCore;
```

### ISA Extensions

The core supports both standard RISC-V ISA extensions as well as custom
instructions.

#### "M" standard extension

Support for multiplication instructions `MUL`, `MULH`, `MULHSU` and `MULHU`
defined as part of "M" standard extension can be enabled by specifying
`Extension::M` as argument for the `EXTENSIONS` template parameter. The division
instructions are not supported and programs using "M" extension must be compiled
with gcc `-mno-div` flag.

#### Custom extensions

Custom ISA extension are implemented via two handlers provided by the Execution
Environment:

    (RVG) -> Format custom_decode

    (hart_index_t, RVG, Funct3, int_t, int_t, int_t, Funct7) -> int_t custom_execute

The `custom_decode` callback is called for instructions using one of the 4 major
opcodes reserved in the general-purpose ISA:

    RVG::custom_0 = 0b00010
    RVG::custom_1 = 0b01010
    RVG::custom_2 = 0b10110
    RVG::custom_3 = 0b11110

The callback should return one of the standard encoding formats used the custom
instruction, or `Format::Invalid` if the major opcode is not implemented by the
custom extension.

The `custom_execute` callback is called to execute a custom instruction. It takes
5 arguments:

- hart index
- major opcode
- minor opcode (undefined for instructions using format `U` or `J`)
- operand 1, contains value of source register 1 (undefined for instructions
  using format `U` or `J`)
- operand 2, contains value of source register 2 for formats that specify it,
  or the immediate otherwise
- immediate (undefined for instructions using `R` format)
- funct7 (defined only for instructions using `R` format)

The result returned by the `custom_execute` callback is written to the
destination register if the instruction encoding format specifies one. The
result is ignored for formats `B` and `S`, or when destination register is `x0`.

The `custom_execute` callback can be provided as a fixed latency extern function
implemented in Verilog. The latency of the function must be `<= HARTS - 1`, e.g.
for 2-hart configuration:

```
    import processor.risc_v

    using RV = RISC_V<2, 0x4000, 0x2000>;
    using int_t = RV::int_t;
    using hart_index_t = RV::hart_index_t;

    class MyCore
    {
        RV core = {
            .custom_decode = CustomDecode,
            .custom_execute = CustomExecute
        };

        [[latency(1)]] 
        (hart_index_t hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7) -> int_t CustomInstr;

        inline int_t CustomExecute(hart_index_t hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7)
        {
            return CustomInstr(hid, major_opcode, minor_opcode, op1, op2, imm, funct7);
        }

        inline Format CustomDecode(RVG major_opcode)
        {
            return major_opcode == RVG::custom_0 ? Format::R : Format::Invalid;
        }

    public:
        void StartRISCV()
        {
            // ...

            core.init_stack_pointers(0x5FF0, 0x100);
            core.start({0, 0});
        }
    }

    export MyCore;
```

The `latency` attribute specifies how many registers are in the logic
implementing the extern function. 

    MyCore dut
    (
        .clk(clk),
        .rst_in(rst),
        .rst_and_startup_done_out(rst_and_startup_done_out),

        .StartRISCV_valid_in(StartRISCV_valid),
        .StartRISCV_rdy_out(StartRISCV_rdy),
        .StartRISCV_rden_in(StartRISCV_rden),
        .StartRISCV_empty_out(StartRISCV_empty),

        .CustomInstr_valid_out(CustomInstr_valid),
        .CustomInstr_hid_out(CustomInstr_hid),
        .CustomInstr_major_opcode_out(CustomInstr_major_opcode),
        .CustomInstr_minor_opcode_out(CustomInstr_minor_opcode),
        .CustomInstr_op1_out(CustomInstr_op1),
        .CustomInstr_op2_out(CustomInstr_op2),
        .CustomInstr_imm_out(CustomInstr_imm),
        .CustomInstr_funct7_out(CustomInstr_funct7),
        .CustomInstr_result_in(CustomInstr_result)
    );

    logic [31:0] custom_instr_state[1:0];

    always @(posedge clk) begin
        CustomInstr_result <= 32'hdeadbeef;

        if (CustomInstr_valid) begin
            if (CustomInstr_major_opcode == _processor_risc_v_isa__RVG_custom_0) begin
                if (CustomInstr_minor_opcode == 0) begin
                    custom_instr_state[CustomInstr_hid] <= CustomInstr_op1;
                end
                if (CustomInstr_minor_opcode == 1) begin
                    custom_instr_state[CustomInstr_hid] <= custom_instr_state[CustomInstr_hid] + CustomInstr_op2;
                end
                if (CustomInstr_minor_opcode == 2) begin
                    CustomInstr_result <= custom_instr_state[CustomInstr_hid];
                end
            end
        end
    end

