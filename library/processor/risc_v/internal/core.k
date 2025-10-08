// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
This is internal implementation of RISC-V processor core.
Use definitions from `processor.risc_v` module instead.
*/
module processor.risc_v.internal.core
    { Core
    , Trap
    , Optimize
    , Option
    , MemorySize
    , Decoded
    }

import data.memory.byte_addressable
import numeric.int.operator.modular as modular
import processor.risc_v.internal.decoder

//| System trap/exception identifier
enum Trap : uint3
{
    //| Program counter moved to address outside specified instruction memory
    // address space.
    InvalidInstructionAddress = 0,

    //| Memory access to address outside of data memory and memory mapped IO
    //address spaces.
    AccessFault = 1,

    //| Invalid or illegal instruction was decoded.
    IllegalInstruction = 2,

    //| ECALL instruction was executed.
    ECALL = 4,

    //| EBREAK instruction was executed.
    EBREAK = 5
}

static assert(SystemOp::ECALL == Trap::ECALL);
static assert(SystemOp::EBREAK == Trap::EBREAK);

//| Optimization configuration bits
enum Optimize : uint1
{
    //| Optimize for minimal area.
    Area = 0b0,

    //| Optimize for maximum frequency.
    Fmax = 0b1
}

template<auto ORIGIN, auto LENGTH>
inline bool addr_in_range(auto addr)
{
    static if (LENGTH == 0)
    {
        return false;
    }
    else
    {
        return addr >= ORIGIN && addr < ORIGIN + LENGTH;
    }
}

inline auto high_order_bit(auto x)
{
    return cast<uint1>(x >> (bitsizeof x - 1));
}

template <
    auto HARTS,
    auto IMEM_LENGTH,
    auto DMEM_LENGTH,
    auto MMIO_LENGTH = 0,
    auto IMEM_ORIGIN = 0, // specified in words
    auto DMEM_ORIGIN = ((IMEM_ORIGIN + IMEM_LENGTH) << 2),
    auto MMIO_ORIGIN = DMEM_ORIGIN + DMEM_LENGTH,
    auto IMEM_TCM_SIZE = IMEM_LENGTH,
    template <typename, auto> typename DataMemory = memory_norep,
    auto EXTENSIONS = Extension::None,
    auto CONFIG = Optimize::Area,
    Base ISA = Base::RV32I,
    auto BTB_SIZE = 1024,
    template <typename, auto> typename InstrMemory = memory_init>
class Core
{
    const bool EXTERNAL_FETCH = IMEM_TCM_SIZE < IMEM_LENGTH;
    const bool ENABLE_MUL     = (EXTENSIONS & Extension::M) == Extension::M;
    const bool ENABLE_MMIO    = MMIO_LENGTH != 0;
    const bool OPTIMIZE_FMAX  = (CONFIG & Optimize::Fmax) == Optimize::Fmax && !USE_MICRO_OPS;
    const bool SHARED_DMEM    = (HARTS == 1) || ((CONFIG & Option::HartsShareDMEM) == Option::HartsShareDMEM);

    static assert(HARTS != 0);
    static assert((BTB_SIZE & (BTB_SIZE - 1)) == 0);
    static assert(OPTIMIZE_FMAX == false || EXTERNAL_FETCH == false);
    static assert(IMEM_TCM_SIZE <= IMEM_LENGTH);
    static assert(DMEM_ORIGIN == ((DMEM_ORIGIN >> clog2(DMEM_LENGTH == 0 ? 1 : DMEM_LENGTH))
                                               << clog2(DMEM_LENGTH == 0 ? 1 : DMEM_LENGTH)));
    static assert(IMEM_ORIGIN == ((IMEM_ORIGIN >> clog2(IMEM_LENGTH)) << clog2(IMEM_LENGTH)));
    static assert(MMIO_ORIGIN == ((MMIO_ORIGIN >> clog2(MMIO_LENGTH == 0 ? 1 : MMIO_LENGTH))
                                               << clog2(MMIO_LENGTH == 0 ? 1 : MMIO_LENGTH)));

    static assert(!EXTERNAL_FETCH || (((IMEM_TCM_SIZE - 1) & IMEM_TCM_SIZE) == 0));

    const auto MUL_CYCLES     = 3;
    const auto LOAD_CYCLES    = 2;
    const auto LOAD_MICRO_OPS = ((LOAD_CYCLES + HARTS - 1) / HARTS);
    const auto MUL_MICRO_OPS  = ((MUL_CYCLES + HARTS - 1) / HARTS) + /* ALU */ 1 + /* feedback */ 1;

    const bool MICRO_OP_MUL   = ENABLE_MUL && MUL_CYCLES > HARTS;
    const bool MICRO_OP_LOAD  = LOAD_CYCLES > HARTS;
    const bool USE_MICRO_OPS  = MICRO_OP_MUL || MICRO_OP_LOAD;

    static assert(SHARED_DMEM || ((DMEM_LENGTH & (DMEM_LENGTH - 1)) == 0));

    const auto DMEM_TCM_SIZE  = SHARED_DMEM ? DMEM_LENGTH : (DMEM_LENGTH << clog2(HARTS));

    using register_index_t    = Types<ISA>::register_index_t;
    using register_file_t     = memory<int_t, (1 << (bitsizeof register_index_t + clog2(HARTS)))>;
    using dmem_t              = byte_addressable_memory<int_t, (DMEM_TCM_SIZE > bytesizeof int_t) ? DMEM_TCM_SIZE : bytesizeof int_t, DataMemory>;

public:
    //| Address of 32-bit word in instruction memory.
    using imem_addr_t         = index_t<IMEM_LENGTH>;

    //| Address of byte in data memory.
    using dmem_addr_t         = index_t<DMEM_LENGTH>;

    //| Signed, XLEN bits wide integer.
    using int_t               = Types<ISA>::int_t;

    //| Unsigned, XLEN bits wide integer.
    using uint_t              = Types<ISA>::uint_t;

    //| Index of hardware thread (hart).
    using hart_index_t        = index_t<HARTS>;

    //| Type of callback to handle system exceptions and traps.
    using system_trap_t =
        ( hart_index_t hid      //< Identifier of the hart that triggered the exception or trap.
        , Trap trap             //< Trap kind.
        , imem_addr_t addr      //< 32-bit word based instruction address.
        ) -> optional<imem_addr_t>;

    //| Type of dynamic instruction trace callback.
    using trace_t =
        ( hart_index_t hid      //< Identifier of the hart executing the instruction.
        , imem_addr_t addr      //< Address of 32-bit instruction word.
        , uint_t inst           //< Instruction word.
        , Decoded decoded       //< Decoded instruction.
        , optional<int_t> value //< If valid specifies value written by instruction
                                // to destination register specified by `decoded.format.rd`.
        ) -> void;

    //| Type of callback to handle memory mapped IO access.
    // If the access operation completes immediately, the `is_valid` of the
    // returned result should be set to true. For load access the `value` of
    // the returned result is the result of the load. For store access the
    // `value` of the returned result is ignored.
    // If the operation will complete asynchronously the `is_valid` of the
    // returned result should be set to false, and execution environment should
    // indicate completion of load/store by calling the `mmio_load_result` or
    // `mmio_store_completed` method respectively.
    using mmio_access_t =
        ( hart_index_t hid      //< Identifier of the hart executing the load.
        , uint_t load_addr      //< Memory mapped IO address used for load MMIO access.
        , uint_t store_addr     //< Memory mapped IO address used for store MMIO access.
        , MemorySize size       //< Size of MMIO access.
        , bool sign_extend      //< Boolean flag specifying if sub-word load should be sign-extended.
        , optional<int_t> value //< Value to be stored, valid for store MMIO access
        ) -> optional<int_t>;

    //| Type of callback to handle load from memory mapped IO.
    // The load result may be returned immediately, by setting `is_valid` of the callback
    // return value to true, or asynchronously, by calling `mmio_load_result` method
    // after the callback returns.
    using mmio_load_t =
        ( hart_index_t hid      //< Identifier of the hart executing the load.
        , uint_t addr           //< Memory mapped IO address to load from.
        , MemorySize size       //< Size of load.
        , bool sign_extend      //< Boolean flag specifying if sub-word load should be sign-extended.
        ) -> optional<int_t>;

    //| Type of callback to handle store to memory mapped IO.
    // The callback should return `true` if the program running on the core can
    // consider the store as committed, or `false` if the store should be
    // considered as pending. In the latter case, the hart that made the
    // request is stalled until the execution environment indicates the store
    // as completed by calling the `mmio_store_completed` method.
    using mmio_store_t =
        ( hart_index_t hid      //< Identifier of the hart executing the store.
        , uint_t addr           //< Memory mapped IO address to store to.
        , MemorySize size       //< Size of data to store.
        , int_t value           //< Value to be stored.
        ) -> bool;

    //| Type of external instruction fetch handler.
    using external_fetch_t =
        ( hart_index_t hid      //< Identifier of the hart performing the instruction fetch.
        , imem_addr_t addr      //< 32-bit word address of instruction to fetch.
        ) -> optional<uint_t>;

    //| Type of callback of custom instruction decode handler.
    using custom_decode_t =
        ( RVG major_opcode      //< Major opcode of the instruction.
        ) -> Format;

    //| Type of callback to execute custom instruction
    using custom_execute_t =
        ( hart_index_t hid      //< Identifier of the hart executing the instruction.
        , RVG major_opcode      //< Major opcode of the instruction.
        , Funct3 minor_opcode   //< Minor opcode of the instruction
                                // (undefined for instructions using format `U` or `J`).
        , int_t op1             //< First operand. Contains value of source register 1 (undefined for instructions
                                // using format `U` or `J`).
        , int_t op2             //< Second operand. Contains value of source register 2 for formats that specify it,
                                // or an immediate otherwise.
        , int_t imm             //< Immediate (undefined for instructions using `R` format)
        , Funct7 funct7         //< funct7 field of decoded instructions using `R` format, undefined otherwise.
        ) -> int_t;

private:
    using extended_int_t      = int<1 + bitsizeof int_t>;
    using btb_index_t         = index_t<BTB_SIZE>;
    using btb_tag_t           = uint<(IMEM_LENGTH > BTB_SIZE) ? bitsizeof imem_addr_t - bitsizeof btb_index_t : 1>;
    using tid_t               = index_t<HARTS * (ENABLE_MUL ? MUL_MICRO_OPS : LOAD_MICRO_OPS)>;

    struct MicroOp
    {
        imem_addr_t             pc;
        optional<imem_addr_t>   next_pc;
        int_t                   mul;
        bool                    pending;
        Instr                   instr;
        tid_t                   end_tid;
    }

    struct Current
    {
        hart_index_t            hid;
        imem_addr_t             pc;
        imem_addr_t             next_pc;
        imem_addr_t             predicted_pc;
        optional<Instr>         instr;
        bool                    last_micro_op;
        bool                    recovered_from_pipeline_flush;
        bool                    received_pending_fetch;
        bool                    committed;
    }

    struct Operands
    {
        int_t                   op1;
        int_t                   op2;
        int_t                   rs2;
        int_t                   add1;
        extended_int_t          sub1;
        extended_int_t          sub2;
    }

    struct Results
    {
        uint_t                  data_addr;
        uint_t                  instr_addr;
        int_t                   compute;
        int_t                   mul;
        int_t                   csr;
        int_t                   written;
        imem_addr_t             next_pc;
        bool                    branch_taken;
        bool                    mmio;
        bool                    trap;
        bool                    invalid_next_pc;
    }

    struct Prediction
    {
        bool                    take;
        bool                    history;
        btb_tag_t               tag;
        imem_addr_t             target_pc;
    }

    struct Hart
    {
        imem_addr_t             pc;
        optional<imem_addr_t>   pipeline_flush_pc;
        bool                    commit;
        uint_t                  instret;
        bool                    mmio_pending;
        bool                    mmio_completed;
        int_t                   mmio_load_result;
        bool                    external_fetch_pending;
        optional<imem_addr_t>   external_fetch_pc;
        optional<Instr>         external_fetch_result;
        bool                    fetch_enable;
        bool                    trap;
        MicroOp                 micro_op;
    }

    Hart[HARTS]                 hart;

    static if (BTB_SIZE < 1024)
    {
        using btb_t           = array<Prediction, BTB_SIZE>;
    }
    else
    {
        using btb_t           = memory<Prediction, BTB_SIZE>;
    }

    btb_index_t                 btb_index;
    hart_index_t                hid;

    btb_t                       btb = {};
    register_file_t             register_file = {};

    dmem_t                      dmem;

    static if (IMEM_TCM_SIZE > 0)
    {
        using imem_t          = InstrMemory<Instr, IMEM_TCM_SIZE>;

        imem_t                  imem;
    }

    system_trap_t               system_trap = default_system_trap;
    trace_t                     trace = default_trace;
    mmio_access_t               mmio_access = default_mmio_access;
    mmio_load_t                 mmio_load = default_mmio_load;
    mmio_store_t                mmio_store = default_mmio_store;
    external_fetch_t            external_fetch = default_external_fetch;
    custom_decode_t             custom_decode = default_custom_decode;
    custom_execute_t            custom_execute = default_custom_execute;

public:
    inline void pipeline()
    {
        Current current;
        Decoded decoded;
        Results results;
        Operands operands;

        const auto mispredicted_branch
            = HARTS == 1 ? 4
            : HARTS == 2 ? OPTIMIZE_FMAX ? 4
                                         : 3
            : HARTS == 4 ? OPTIMIZE_FMAX ? 3
                                         : 2
            : 1;

        const auto execute_cycles
            = HARTS < 4 ? HARTS
            : OPTIMIZE_FMAX ? 4
                            : 3;

        const auto multiply_cycles
            = MICRO_OP_MUL ? MUL_CYCLES
                           : 0;

        const auto pipeline_cycles = multiply_cycles + mispredicted_branch * HARTS + 1;

        [[schedule(pipeline_cycles)]]
        {
            current = fetch();
            decoded = decode(current);

            [[schedule(execute_cycles)]]
            {
                if (current.recovered_from_pipeline_flush)
                {
                    assert(!hart[current.hid].commit);
                    hart[current.hid].commit = true;
                }

                operands = read(current, decoded);

                const bool committable = hart[current.hid].commit
                    && (static(!USE_MICRO_OPS) || current.last_micro_op)
                    && (static(!EXTERNAL_FETCH) || current.instr.is_valid);

                int_t custom;

                if (committable)
                {
                    custom = custom_execute(
                        current.hid,
                        decoded.major_opcode,
                        decoded.minor_opcode,
                        operands.op1,
                        operands.op2,
                        decoded.format.imm,
                        decoded.format.funct7);
                }

                results = execute(current, decoded, operands, custom);

                static if (MICRO_OP_LOAD)
                {
                    static uint_t addr;

                    const auto load_addr = addr;

                    addr = results.data_addr;
                }
                else
                {
                    const auto load_addr = results.data_addr;
                }

                int_t loaded;

                if (((decoded.kind == InstrKind::Memory) && !decoded.store))
                {
                    loaded = load(current, decoded, load_addr);
                }

                if (committable)
                {
                    assert(load_addr == results.data_addr || decoded.kind != InstrKind::Memory || decoded.store);
                    const bool is_dmem_addr = addr_in_range<DMEM_ORIGIN, DMEM_LENGTH>(results.data_addr);

                    static if (ENABLE_MMIO)
                    {
                        bool is_mmio_addr;

                        if (hart[current.hid].mmio_completed)
                        {
                            assert(decoded.kind == InstrKind::Memory);
                            is_mmio_addr = true;
                            results.mmio = true;
                            loaded = hart[current.hid].mmio_load_result;
                            hart[current.hid].mmio_completed = false;
                        }
                        else
                        {
                            is_mmio_addr = addr_in_range<MMIO_ORIGIN, MMIO_LENGTH>(results.data_addr);
                            results.mmio = (decoded.kind == InstrKind::Memory) && is_mmio_addr;

                            if (results.mmio)
                            {
                                assert(!decoded.illegal);
                                assert(!hart[current.hid].mmio_pending);

                                const auto x = mmio_access(current.hid, load_addr, results.data_addr, decoded.instr.mem.size,
                                    decoded.instr.mem.sign_extend, make_optional(decoded.store, operands.rs2));

                                loaded = x.value;

                                if (!x.is_valid)
                                {
                                    hart[current.hid].mmio_pending = true;
                                    results.next_pc = current.pc;
                                }
                            }
                        }
                    }
                    else
                    {
                        const auto is_mmio_addr = false;
                    }

                    results.trap = decoded.trap || (results.invalid_next_pc && !hart[current.hid].mmio_pending);

                    const bool invalid_addr = !is_mmio_addr && !is_dmem_addr;

                    if (decoded.illegal || (decoded.kind == InstrKind::Memory) && invalid_addr)
                    {
                        results.trap = true;
                    }
                    else
                    {
                        current.committed = !hart[current.hid].mmio_pending;

                        results.written = write(current, decoded, loaded, results);
                        store(current, decoded, operands, results);
                    }

                    assert(hart[current.hid].trap == false);
                    hart[current.hid].trap = results.trap;
                    hart[current.hid].commit = results.next_pc == current.predicted_pc && !results.trap;

                    assert(!hart[current.hid].pipeline_flush_pc.is_valid);
                    hart[current.hid].pipeline_flush_pc = make_optional(!hart[current.hid].commit, results.next_pc);
                    hart[current.hid].fetch_enable = hart[current.hid].commit;
                    hart[current.hid].instret = modular::increment_if(hart[current.hid].instret, current.committed || results.trap);
                }
            }

            static if (MICRO_OP_MUL)
            {
                [[schedule(MUL_CYCLES)]]
                {
                    hart[current.hid].micro_op.mul = multiply(decoded, operands);
                }
            }
        }

        update_branch_prediction(current, decoded, results);

        if (current.committed || results.trap)
        {
            assert(current.instr.is_valid);

            trace(current.hid, current.pc, static_cast(current.instr.value), decoded, make_optional(current.committed, decoded.store ? operands.rs2 : results.written));
        }

        if (results.trap)
        {
            assert(hart[current.hid].trap == true);
            assert(hart[current.hid].commit == false);
            assert(hart[current.hid].pipeline_flush_pc.is_valid == true);
            assert(hart[current.hid].mmio_pending == false);

            Trap trap = Trap::AccessFault;

            if (results.invalid_next_pc)
            {
                trap = Trap::InvalidInstructionAddress;
            }

            if (decoded.trap)
            {
                trap = reinterpret_cast<Trap>(decoded.instr.system.op);
            }

            if (decoded.illegal)
            {
                trap = Trap::IllegalInstruction;
            }

            const auto next_pc = system_trap(current.hid, trap, current.pc);

            if (next_pc.is_valid)
            {
                hart[current.hid].pipeline_flush_pc.value = next_pc.value;
            }

            hart[current.hid].trap = false;
        }
    }

    // Initialize micro-architecture state
    inline void initialize(uint_t[HARTS] pc)
    {
        static for(const auto i : HARTS)
        {
            hart[i].pc = static_cast(pc[i] >> 2);
            hart[i].commit = true;
            hart[i].instret = 0;
            hart[i].pipeline_flush_pc.is_valid = false;
            hart[i].mmio_pending = false;
            hart[i].mmio_completed = false;
            hart[i].external_fetch_pending = false;
            hart[i].external_fetch_pc.is_valid = false;
            hart[i].fetch_enable = true;
            hart[i].trap = false;
            hart[i].micro_op.next_pc.is_valid = false;
            hart[i].micro_op.pending = false;
        }
        hid = 0;
        btb_index = static_cast(hart[hid].pc);
    }

    inline void mmio_store_completed(hart_index_t hid)
    {
        assert(hart[hid].mmio_pending == true);
        assert(hart[hid].mmio_completed == false);

        atomic
        {
            hart[hid].mmio_completed = true;
        }
    }

    inline void mmio_load_result(hart_index_t hid, int_t value)
    {
        assert(hart[hid].mmio_pending == true);
        assert(hart[hid].mmio_completed == false);

        atomic
        {
            hart[hid].mmio_completed = true;
            hart[hid].mmio_load_result = value;
        }
    }

    inline void external_fetch_result(hart_index_t hid, imem_addr_t addr, uint_t instr)
    {
        atomic
        {
            if (hart[hid].external_fetch_pc.is_valid && hart[hid].external_fetch_pc.value == addr)
            {
                hart[hid].external_fetch_result = make_optional(true, cast<Instr>(instr));
            }
        }
    }

    // Fetch next instruction
    inline Current fetch()
    {
        static imem_addr_t fetch_pc;

        Current current = {};
        tid_t current_tid;

        atomic
        {
            static tid_t tid = 0;
            bool try_predict = true;
            btb_index_t btb_index_nxt;

            current_tid = tid;
            tid = static_cast(tid + 1);

            static if (HARTS != 1)
            {
                current.hid = hid;
                static assert((HARTS & (HARTS - 1)) == 0);
                hid = static_cast(hid + 1);
                btb_index_nxt = static_cast(hart[hid].pc);
            }
            else
            {
                current.hid = 0;
            }

            fetch_pc = hart[current.hid].pc;

            if (static(USE_MICRO_OPS) && hart[current.hid].micro_op.next_pc.is_valid)
            {
                fetch_pc = hart[current.hid].micro_op.next_pc.value;
                hart[current.hid].micro_op.next_pc.is_valid = current_tid != hart[current.hid].micro_op.end_tid;

                try_predict = false;
            }

            if (static(EXTERNAL_FETCH) && hart[current.hid].external_fetch_pc.is_valid)
            {
                fetch_pc = hart[current.hid].external_fetch_pc.value;
                hart[current.hid].external_fetch_pc.is_valid = !hart[current.hid].external_fetch_result.is_valid;

                try_predict = false;
                current.received_pending_fetch = hart[current.hid].external_fetch_result.is_valid;
            }

            assert(!hart[current.hid].mmio_pending || hart[current.hid].pipeline_flush_pc.is_valid);
            assert(!hart[current.hid].trap || hart[current.hid].pipeline_flush_pc.is_valid);

            if (hart[current.hid].pipeline_flush_pc.is_valid)
            {
                // Recover after pipeline flush (e.g. after branch misprediction, mmio or trap)
                if (!(hart[current.hid].mmio_pending && !hart[current.hid].mmio_completed) &&
                    !hart[current.hid].trap)
                {
                    current.recovered_from_pipeline_flush = true;
                }

                fetch_pc = hart[current.hid].pipeline_flush_pc.value;
                hart[current.hid].pipeline_flush_pc.is_valid = !current.recovered_from_pipeline_flush;

                if (static(USE_MICRO_OPS) && hart[current.hid].micro_op.next_pc.is_valid)
                {
                    hart[current.hid].micro_op.next_pc.is_valid = false;
                }

                if (static(EXTERNAL_FETCH) && hart[current.hid].external_fetch_pc.is_valid)
                {
                    hart[current.hid].external_fetch_pc.is_valid = false;
                }

                try_predict = false;

                if (hart[current.hid].mmio_pending)
                {
                    hart[current.hid].mmio_pending = !current.recovered_from_pipeline_flush;
                }
            }

            static if (OPTIMIZE_FMAX)
            {
                current.pc = fetch_pc;
            }

            current.next_pc = static_cast(fetch_pc + 1);

            current.predicted_pc = try_predict ? predict(current) : current.next_pc;
            hart[current.hid].pc = current.predicted_pc;

            static if (HARTS == 1)
            {
                btb_index_nxt = static_cast(current.predicted_pc);
            }

            btb_index = btb_index_nxt;
        }

        atomic
        {
            if (current.recovered_from_pipeline_flush)
            {
                assert(!hart[current.hid].commit);
                assert(!hart[current.hid].external_fetch_pc.is_valid);
                assert(!hart[current.hid].micro_op.next_pc.is_valid);

                hart[current.hid].fetch_enable = true;
                hart[current.hid].external_fetch_pending = false;
                hart[current.hid].micro_op.pending = false;
            }

            current.instr.is_valid = true;

            static if (OPTIMIZE_FMAX)
            {
                if (hart[current.hid].fetch_enable)
                {
                    current.instr.value = imem[current.pc];
                }
            }
            else if (static(USE_MICRO_OPS) && hart[current.hid].micro_op.pending)
            {
                assert(!hart[current.hid].external_fetch_pending);

                current.instr.value = hart[current.hid].micro_op.instr;
                current.pc = hart[current.hid].micro_op.pc;
                current.next_pc = hart[current.hid].micro_op.next_pc.value;
                current.predicted_pc = current.next_pc;
            }
            else if (static(EXTERNAL_FETCH) && hart[current.hid].external_fetch_pending)
            {
                assert(hart[current.hid].external_fetch_result.is_valid || !current.received_pending_fetch);

                hart[current.hid].external_fetch_pending = !current.received_pending_fetch;

                current.instr = make_optional(
                        current.received_pending_fetch && hart[current.hid].fetch_enable,
                        hart[current.hid].external_fetch_result.value);
                current.pc = fetch_pc;
            }
            else if (static(EXTERNAL_FETCH) && (static(IMEM_TCM_SIZE == 0) || (fetch_pc & (~(IMEM_TCM_SIZE - 1))) != 0))
            {
                bool external_fetch_pending = false;

                if (hart[current.hid].fetch_enable)
                {
                    current.instr = reinterpret_cast<optional<Instr>>(external_fetch(current.hid, fetch_pc));

                    external_fetch_pending = !current.instr.is_valid;
                }

                assert(!hart[current.hid].external_fetch_pc.is_valid);

                hart[current.hid].external_fetch_pc = make_optional(external_fetch_pending, fetch_pc);
                hart[current.hid].external_fetch_pending = external_fetch_pending;
                hart[current.hid].external_fetch_result.is_valid = false;

                current.pc = fetch_pc;
            }
            else static if (IMEM_TCM_SIZE > 0)
            {
                if (hart[current.hid].fetch_enable)
                {
                    current.instr.value = imem[fetch_pc];
                }
                current.pc = fetch_pc;
            }

            static if (USE_MICRO_OPS)
            {
                if (!hart[current.hid].micro_op.pending)
                {
                    const bool mul = static(MICRO_OP_MUL) &&
                        current.instr.value.r.opcode.opcode == RVG::OP && current.instr.value.r.funct7 == 0b_0000001;

                    const bool load = static(MICRO_OP_LOAD) &&
                        current.instr.value.r.opcode.opcode == RVG::LOAD;

                    if ((load || mul) && hart[current.hid].fetch_enable)
                    {
                        hart[current.hid].micro_op.pending = current.instr.is_valid;
                    }

                    assert(!hart[current.hid].micro_op.next_pc.is_valid);
                    hart[current.hid].micro_op.next_pc = make_optional(hart[current.hid].micro_op.pending, current.next_pc);
                    hart[current.hid].micro_op.pc = current.pc;
                    hart[current.hid].micro_op.instr = current.instr.value;

                    hart[current.hid].micro_op.end_tid = cast<tid_t>(mul
                        ? current_tid + MUL_MICRO_OPS * HARTS
                        : current_tid + LOAD_MICRO_OPS * HARTS);
                }
                else
                {
                    const auto next_tid = cast<tid_t>(current_tid + HARTS);
                    hart[current.hid].micro_op.pending = next_tid != hart[current.hid].micro_op.end_tid;
                }
            }

            current.last_micro_op = !hart[current.hid].micro_op.pending;
        }

        return current;
    }

    inline Decoded decode(Current current)
    {
        return decode_instr<EXTENSIONS, CONFIG>(current.instr.value, custom_decode);
    }

    inline Operands read(Current current, Decoded decoded)
    {
        Operands operands;

        if (decoded.format.rs1.is_valid)
        {
            operands.op1 = register_get(current.hid, decoded.format.rs1.value);
        }

        if (decoded.format.rs2.is_valid)
        {
            operands.rs2 = register_get(current.hid, decoded.format.rs2.value);
        }

        operands.op2 = decoded.format.rs2.is_valid
            ? operands.rs2
            : decoded.format.imm;

        operands.sub1 = decoded.unsigned_comparison
            ? cast<uint_t>(operands.op1)
            : operands.op1;

        operands.sub2 = decoded.unsigned_comparison
            ? cast<uint_t>(operands.op2)
            : operands.op2;

        operands.add1 = decoded.pc_plus
            ? cast<int_t>((current.pc | IMEM_ORIGIN) << 2)
            : operands.op1;

        return operands;
    }

    inline int_t load(Current current, Decoded decoded, uint_t addr)
    {
        const auto value = dmem_read(current.hid, static_cast(addr));

        int_t b;
        int_t hw;
        int_t w = value;

        if (decoded.instr.mem.sign_extend)
        {
            b = sign_extend(cast<uint8>(value));
            hw = sign_extend(cast<uint16>(value));
        }
        else
        {
            b = cast<uint8>(value);
            hw = cast<uint16>(value);
        }

        const auto dont_care = w;

        return mux(decoded.instr.mem.size, b, hw, w, dont_care);
    }

    inline int_t multiply(Decoded decoded, Operands in)
    {
        static assert (MulIn::Unsigned == 1);
        const auto op1 = mux(decoded.instr.compute.mul.op1, in.op1, cast<uint_t>(in.op1));
        const auto op2 = mux(decoded.instr.compute.mul.op2, in.rs2, cast<uint_t>(in.rs2));

        const auto mul = op1 * op2;

        static assert (MulOut::Upper == 1);

        return cast<int_t>(mux(decoded.instr.compute.mul.result, mul, mul >> bitsizeof int_t));
    }

    inline Results execute(Current current, Decoded decoded, Operands in, int_t custom_result)
    {
        Results results;

        const auto sub = checked_cast<extended_int_t>(in.sub1 - in.sub2);

        const int_t or     = in.op1 | in.op2;
        const int_t and    = in.op1 & in.op2;
        const int_t xor    = in.op1 ^ in.op2;

        assert(!(decoded.kind == InstrKind::Compute &&
                     decoded.instr.compute.base.op == ComputeOp::Logical &&
                     decoded.instr.compute.base.logical == LogicalOp::LUI)
                  || in.op2 == decoded.format.imm);

        const int_t lui    = in.op2;

        const auto logical = mux(decoded.instr.compute.base.logical, xor, lui, or, and);
        const auto add     = cast<int_t>(in.add1 + in.op2);
        const auto sll     = cast<int_t>(in.op1 << cast<uint5>(in.op2));
        const auto sr      = cast<int_t>((decoded.instr.compute.base.sra ? in.op1 : cast<uint_t>(in.op1)) >> cast<uint5>(in.op2));

        const bool eq       = in.op1 == in.op2;
        const bool lt       = sub < 0;

        const bool condition =
            mux(decoded.instr.control.condition, eq, !eq, lt, !lt);

        static if (ENABLE_MUL && !MICRO_OP_MUL)
        {
            results.mul = multiply(decoded, in);
        }

        results.compute =
            mux(decoded.instr.compute.base.op, add, sll, cast<uint1>(lt), cast<uint1>(lt), cast<int_t>(sub), sr, custom_result, logical);

        results.branch_taken =
            decoded.control && (decoded.instr.control.op == ControlOp::JUMP || condition == true);

        results.data_addr = static_cast(in.op1 + decoded.format.imm);
        results.instr_addr = static_cast(in.add1 + decoded.format.imm);

        static assert(cast<uint2>(SystemOp::CSRRS_CYCLE) == 0);
        static assert(cast<uint2>(SystemOp::CSRRS_INSTRET) == 1);
        static assert(cast<uint2>(SystemOp::CSRRS_CYCLEH) == 2);
        static assert(cast<uint2>(SystemOp::CSRRS_MHARTID) == 3);

        results.csr = static_cast(mux(reinterpret_cast<uint2>(decoded.instr.system.op), cycles(), hart[current.hid].instret, cycles() >> 32, current.hid));

        if (results.branch_taken)
        {
            // Branch target address is mis-aligned if bit 1 is set.
            // For JAL, and B* instructions, bit 0 cannot be set because
            // the instruction encoding does not allow this.
            // For JALR instructions, the specification requires that
            // the least-significant bit be ignored.
            results.invalid_next_pc = (cast<uint1>(results.instr_addr >> 1) != 0) || !addr_in_range<IMEM_ORIGIN, IMEM_LENGTH>(results.instr_addr >> 2);
            results.next_pc = static_cast(results.instr_addr >> 2);
        }
        else
        {
            assert(current.next_pc == cast<imem_addr_t>(current.pc + 1));
            results.invalid_next_pc = high_order_bit(current.pc) == 1 && high_order_bit(current.next_pc) == 0;
            results.next_pc = current.next_pc;
        }

        return results;
    }

    inline void store(Current current, Decoded decoded, Operands in, Results results)
    {
        if (decoded.store && !results.mmio)
        {
            assert(!decoded.illegal);
            assert(addr_in_range<DMEM_ORIGIN, DMEM_LENGTH>(results.data_addr));
            const auto bytes = checked_cast<dmem_t::byte_count_t>(1 << decoded.instr.mem.size);
            dmem_write(current.hid, static_cast(results.data_addr), in.rs2, bytes);
        }
    }

    inline int_t write(Current current, Decoded decoded, int_t loaded, Results results)
    {
        const int_t value = decoded.mul
            ? MICRO_OP_MUL
                ? hart[current.hid].micro_op.mul
                : results.mul
            : mux(decoded.kind,
                  results.compute,                                   // Compute
                  cast<int_t>((current.next_pc | IMEM_ORIGIN) << 2), // Control
                  loaded,                                            // Memory
                  results.csr);                                      // System

        if (decoded.format.rd.is_valid)
        {
            assert(!decoded.illegal);
            register_set(current.hid, decoded.format.rd.value, value);
        }

        return value;
    }

private:
    inline imem_addr_t predict(Current current)
    {
        static if (HARTS <= 4)
        {
            assert(btb_index == cast<btb_index_t>(hart[current.hid].pc));

            const auto prediction = btb[btb_index];

            return prediction.take && (hart[current.hid].pc >> bitsizeof btb_index_t) == prediction.tag
                ? prediction.target_pc
                : current.next_pc;
        }
        else
        {
            return current.next_pc;
        }
    }

    inline void update_branch_prediction(Current current, Decoded decoded, Results results)
    {
        static if (HARTS <= 4)
        {
            if (current.committed)
            {
                Prediction prediction;

                prediction.take = results.branch_taken;
                prediction.history = results.branch_taken;
                prediction.tag = current.pc >> bitsizeof btb_index_t;
                prediction.target_pc = static_cast(results.instr_addr >> 2);

                atomic
                {
                    auto oldPrediction = btb[current.pc];
                                                                                             /*
                          +------------+
                          |            |
                    taken |            v
                          |     +-------------+     not taken     +-------------+
                          |     | take:  true | +---------------> | take: true  |
                          +----+|-------------|                   |-------------|
                                | hist:  true | <---------------+ | hist: false |
                                +-------------+     taken         +-------------+
                                       ^                                 +
                                       |                                 |
                                       |taken                            | not taken
                                       |                                 |
                                       +                                 v
                                +-------------+     taken         +-------------+
                                | take: false | <---------------+ | take: false |
                                |-------------|                   |-------------|+---+
                                | hist: true  | +---------------> | hist: false |    |
                                +-------------+     not taken     +-------------+    |
                                                                         ^           | not taken
                                                                         |           |
                                                                         +-----------+
                                                                                             */

                    // Control instructions always cause update of BTB, even when
                    // existing value applies to a different address.
                    //
                    // For non-control instructions there are two cases to consider:
                    // - The tag doesn't match
                    //   The buffer applies to a different address and lower bits
                    //   of the address collided. We leave the buffer unchanged.
                    // - The tag matches
                    //   The buffer is "poisoned", possibly by another program that
                    //   ran on the core and had a control instruction at this
                    //   address. We update the buffer to cure the poisoning.

                    bool update = decoded.control;

                    if (oldPrediction.tag == prediction.tag)
                    {
                        update = true;

                        prediction.take = results.branch_taken
                            ? oldPrediction.take || oldPrediction.history
                            : oldPrediction.take && oldPrediction.history;
                    }

                    if (update)
                    {
                        btb[current.pc] = prediction;
                    }
                }
            }
        }
    }

    inline auto hart_address(hart_index_t hid, auto addr)
    {
        static if (SHARED_DMEM)
        {
            return addr;
        }
        else
        {
            return concat(hid, addr);
        }
    }

public:
    inline auto dmem_read(hart_index_t hid, dmem_addr_t addr)
    {
        return dmem.read(hart_address(hid, addr));
    }

    inline void dmem_write(hart_index_t hid, dmem_addr_t addr, int_t value, dmem_t::byte_count_t size)
    {
        dmem.write(hart_address(hid, addr), value, size);
    }

    inline int_t dmem_read_aligned(hart_index_t hid, dmem_addr_t addr)
    {
        return dmem.read_aligned(hart_address(hid, addr));
    }

    inline void dmem_write_aligned(hart_index_t hid, dmem_addr_t addr, int_t value)
    {
        dmem.write_aligned(hart_address(hid, addr), value);
    }

    inline void imem_write(imem_addr_t addr, uint_t value)
    {
        assert(IMEM_TCM_SIZE > 0);

        static if (IMEM_TCM_SIZE > 0) imem[addr] = static_cast(value);
    }

    inline auto register_get(hart_index_t hid, register_index_t reg)
    {
        const auto value = register_file[concat(hid, reg)];

        // Validate that register_file memory has been initialized
        if (reg == ABI::zero) assert(value == 0);

        return value;
    }

    inline void register_set(hart_index_t hid, register_index_t reg, int_t value)
    {
        register_file[concat(hid, reg)] = value;
    }

    inline auto default_system_trap(hart_index_t hid, Trap trap, imem_addr_t pc)
    {
        assert(trap != Trap::InvalidInstructionAddress);
        assert(trap != Trap::IllegalInstruction);
        assert(trap != Trap::AccessFault);
        return make_optional(false, pc);
    }

    inline void default_trace(hart_index_t hid, imem_addr_t addr, uint_t inst, Decoded decoded, optional<int_t> value)
    {
    }

    inline optional<int_t> default_mmio_access(hart_index_t hid, uint_t load_addr, uint_t store_addr, MemorySize size, bool sign_extend, optional<int_t> value)
    {
        optional<int_t> result;

        if (!value.is_valid)
        {
            result = mmio_load(hid, load_addr, size, sign_extend);
        }
        else
        {
            result.is_valid = mmio_store(hid, store_addr, size, value.value);
        }

        return result;
    }

    inline optional<int_t> default_mmio_load(hart_index_t hid, uint_t addr, MemorySize size, bool sign_extend)
    {
        assert(false);
        return {};
    }

    inline bool default_mmio_store(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
    {
        assert(false);
        return {};
    }

    inline optional<uint_t> default_external_fetch(hart_index_t hid, imem_addr_t addr)
    {
        assert(false);
        return {};
    }

    inline Format default_custom_decode(RVG major_opcode)
    {
        return Format::Invalid;
    }

    inline int_t default_custom_execute(hart_index_t hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7)
    {
        return {};
    }
}
