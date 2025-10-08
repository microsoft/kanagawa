// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
*/

module risc_v_wrapper
{ risc_v_wrapper
}

import processor.risc_v
import processor.risc_v.trace
import processor.risc_v.internal.decoder

/*|
This class is a simplistic wrapper over processor.risc_v::RISC_V for
the purpose of allowing an exported class with the methods needed
for testing.
*/
template <
    //| Number of hardware threads (harts), must be a power of 2.
    auto HARTS = 1,
    auto EXTENSIONS = Extension::None
>
class risc_v_wrapper
{
    const auto IMEM_ORIGIN = 0x80000000 >> 2;
    const auto DMEM_ORIGIN = 0x80200000;
    const auto IMEM_LENGTH = 524288; // 2MB
    const auto DMEM_LENGTH = 0x20000; // 128KB

    const auto MMIO_ORIGIN = 0x80220000; // DMEM_ORIGIN + DMEM_LENGTH;
    const auto MMIO_LENGTH = 16;

    const auto MMIO_STDOUT_OFFSET = 0;
    const auto MMIO_STDOUT_ADDR = MMIO_ORIGIN + MMIO_STDOUT_OFFSET;
    const auto MMIO_HID_OFFSET = 4;
    const auto MMIO_HID_ADDR = MMIO_ORIGIN + MMIO_HID_OFFSET;
    const auto MMIO_TRAP_MASK_OFFSET = 8; // 0x80220008
    const auto MMIO_TRAP_MASK_ADDR = MMIO_ORIGIN + MMIO_TRAP_MASK_OFFSET;
    const auto MMIO_TRAP_CAPTURE_OFFSET = 12; // // 0x8022000C
    const auto MMIO_TRAP_CAPTURE_ADDR = MMIO_ORIGIN + MMIO_TRAP_CAPTURE_OFFSET;

    const auto STACK_START = DMEM_ORIGIN + DMEM_LENGTH - 0x10;
    const auto STACK_SIZE = 0x500;

    // MMIO must not overlap DMEM range
    static assert(DMEM_ORIGIN + DMEM_LENGTH <= MMIO_ORIGIN);

    using processor_t = RISC_V<
        HARTS,
        IMEM_LENGTH,
        DMEM_LENGTH,
        MMIO_LENGTH,
        IMEM_ORIGIN,
        DMEM_ORIGIN,
        MMIO_ORIGIN,
        IMEM_LENGTH, // IMEM_TCM_SIZE
        memory_norep,
        EXTENSIONS,
        Option::HartsShareDMEM>;

public:
    using uint_t        = processor_t::uint_t;
    using int_t         = processor_t::int_t;
    using imem_addr_t   = processor_t::imem_addr_t;
    using dmem_addr_t   = processor_t::dmem_addr_t;
    using hart_index_t  = processor_t::hart_index_t;

    //| Start the core.
    void start(
        uint_t pc_offset, //| Initial program counters for all harts.
                          // This is a byte address relative to the start of imem.
        bool enable_trace
    )
    {
        _enable_trace = enable_trace;

        _processor.init_stack_pointers(STACK_START, STACK_SIZE);

        uint_t[HARTS] pc;

        static for( const auto i : HARTS)
        {
            pc[i]  = pc_offset + (IMEM_ORIGIN << 2);
        }

        _processor.start(pc);

        atomic do; while(!_done);
    }

    //| Write 32-bit word to tightly coupled instruction memory.
    // The method is safe to call only when the core is not running.
    [[no_backpressure]]
    void write_imem(imem_addr_t addr, uint_t value)
    {
        _processor.imem_write(addr, value);
    }

    //| Read a 32-bit word from hart's data memory.
    [[no_backpressure]]
    uint_t read_dmem(dmem_addr_t addr)
    {
        return cast<uint_t>(_processor.dmem_read_aligned(0, addr));
    }

    //| Write a 32-bit word to hart's data memory.
    [[no_backpressure]]
    void write_dmem(dmem_addr_t addr, uint_t value)
    {
        _processor.dmem_write_aligned(0, addr, cast<int_t>(value));
    }

private:
    //| Output interface that sends a single character of standard output.
    [[async, no_backpressure]] (uint8 ch) -> void send_stdout_char;

    inline optional<int_t> mmio_load(hart_index_t hid, uint_t addr, MemorySize size, bool sign)
    {
        optional<int_t> result = {};

        assert(addr >= MMIO_ORIGIN && addr < MMIO_ORIGIN + MMIO_LENGTH);

        if (addr == MMIO_HID_ADDR)
        {
            result.value = hid;
            result.is_valid = true;
        }
        else if (addr == MMIO_TRAP_CAPTURE_ADDR)
        {
            result.value = _trapCapture;
            result.is_valid = true;
            _trapCapture = 0; // Clear on read
        }
        else if (addr == MMIO_TRAP_MASK_ADDR)
        {
            result.value = _trapMask;
            result.is_valid = true;
        }
        else
        {
            assert(false);
        }

        return result;
    }

    //| MMIO write handler.
    inline bool mmio_store(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
    {
        if (addr == MMIO_STDOUT_ADDR)
        {
            assert(size == MemorySize::B);

            if (hid == 0)
            {
                send_stdout_char(static_cast(value));
            }
        }
        else if (addr == MMIO_TRAP_MASK_ADDR)
        {
            _trapMask = static_cast(value);
        }
        else
        {
            assert(false);
        }

        return true;
    }

    inline optional<imem_addr_t> system_trap(hart_index_t hid, Trap trap, imem_addr_t pc)
    {
        optional<imem_addr_t> result = make_optional(true, pc);

        uint8 trap_one_hot = (1 << trap);
        bool is_trap_masked = (0 != (_trapMask & trap_one_hot));

        if (trap == Trap::ECALL || (is_trap_masked && (trap == Trap::InvalidInstructionAddress)))
        {
            // ECALL is used to terminate the simulation
            _done = true;
        }
        else if (is_trap_masked)
        {
            // The program is expecting this trap
            atomic
            {
                _trapCapture |= trap_one_hot; // Capture the trap
                result.is_valid = false; // Continue execution
            }
        }
        else
        {
            // Unexpected trap. Error out simulation.
            print("UNEXPECTED TRAP:  {trap}\n");
            assert(false);
        }

        return result;
    }

    inline void trace(hart_index_t hid, imem_addr_t pc, uint_t instr, Decoded decoded, optional<int_t> value)
    {
        if (_enable_trace)
        {
            print_trace(hid, IMEM_ORIGIN | pc, instr, decoded, value);
        }
    }

    processor_t _processor = {
        .system_trap = system_trap,
        .mmio_store = mmio_store,
        .mmio_load = mmio_load,
        .trace = trace
    };

    bool _done = false;
    bool _enable_trace = false;
    uint8 _trapMask = 0;
    uint8 _trapCapture = 0;
}
