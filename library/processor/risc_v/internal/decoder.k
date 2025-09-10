// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Internal implementation of RISC-V decoder.
*/
module processor.risc_v.internal.decoder
    { // * RV32I Base Instruction Set
      ComputeOp
    , LogicalOp
    , MulIn
    , MulOut
    , MulInstr
    , BaseInstr
    , ComputeInstr
    , Condition
    , ControlOp
    , ControlInstr
    , MemoryOp
    , MemorySize
    , MemoryInstr
    , SystemOp
    , SystemInstr
    , InstrKind
    , Decoded
    , Option
    , decode_instr
    , module processor.risc_v.isa
    }

import processor.risc_v.isa

enum Option : uint3
{
    //| Data memory is implemented as internally instantiated tightly coupled
    // memory, with size in bytes specified by `DMEM_LENGTH` template argument.
    // By default each hart has its separate block of data memory with address
    // space spanning `[DMEM_ORIGIN, DMEM_LENGTH)`. In multi-hart configuration
    // when harts have their own DMEM block the `DMEM_LENGTH` must be a power of
    // 2. The `Option::HartsShareDMEM` flags can be specified in `CONFIG` template
    // argument to enable shared data memory, e.g. to allow programs running on
    // different harts to communicate via memory. With shared DMEM the
    // `DMEM_LENGTH` does not need to be a power of 2.
    HartsShareDMEM = 0b010,

    //| Disables checking of lower two bits of instruction word which are used
    // for compressed instruction encoding. This allows saving FPGA block RAM by
    // synthesizing out these bits.
    NoExceptionForCompressedEncoding = 0b100
}

enum ControlOp : uint1
{
    BRANCH,
    JUMP
}

enum Condition : uint2
{
    EQ,
    NE,
    LT,
    GE
}

static assert(Condition::EQ == Branch::BEQ);
static assert(Condition::NE == Branch::BNE);

struct ControlInstr
{
    ControlOp op;
    Condition condition; // when op == ControlFlow::BRANCH
}

enum ComputeOp : uint3
{
    ADD,
    SLL,
    SLT,
    SLTU,
    SUB,
    SR,
    Custom,
    Logical
}

static assert(ComputeOp::ADD == Op::ADD);
static assert(ComputeOp::SLL == Op::SLL);
static assert(ComputeOp::SLT == Op::SLT);
static assert(ComputeOp::SLTU == Op::SLTU);
static assert(ComputeOp::SR == Op::SR);

enum LogicalOp : uint2
{
    XOR,
    LUI,
    OR,
    AND
}

static assert(LogicalOp::XOR == (Op::XOR & 0b11));
static assert(LogicalOp::OR == (Op::OR & 0b11));
static assert(LogicalOp::AND == (Op::AND & 0b11));

struct BaseInstr
{
    ComputeOp op;
    LogicalOp logical;
    bool      sra;
}

enum MulIn : uint1
{
    Signed,
    Unsigned
}

enum MulOut : uint1
{
    Lower,
    Upper
}

struct MulInstr
{
    MulIn  op1;
    MulIn  op2;
    MulOut result;
}

union ComputeInstr
{
    BaseInstr base;
    MulInstr  mul;
}

enum MemoryOp : uint1
{
    LOAD,
    STORE
}

//| Size of memory load/store
enum MemorySize : uint2
{
    //| Byte
    B,
    //| Half-word
    H,
    //| Word
    W
}

static assert(MemorySize::B == LoadStore::B);
static assert(MemorySize::H == LoadStore::H);
static assert(MemorySize::W == LoadStore::W);

struct MemoryInstr
{
    MemoryOp    op;
    MemorySize  size;
    bool        sign_extend;
}

enum SystemOp : uint3
{
    CSRRS_CYCLE,
    CSRRS_INSTRET,
    CSRRS_CYCLEH,
    CSRRS_MHARTID,
    ECALL,
    EBREAK
}

struct SystemInstr
{
    SystemOp op;
}

union DecodedInstr
{
    ComputeInstr compute;
    ControlInstr control;
    MemoryInstr  mem;
    SystemInstr  system;
}

enum InstrKind : uint2
{
    Compute,
    Control,
    Memory,
    System
}

struct Decoded
{
    DecodedInstr                instr;
    InstrKind                   kind;
    Types<Base::RV32I>::Decoded format;
    RVG                         major_opcode;
    Funct3                      minor_opcode;
    bool                        store;
    bool                        control;
    bool                        trap;
    bool                        unsigned_comparison;
    bool                        pc_plus;
    bool                        mul;
    bool                        illegal;
}

template <auto EXTENSIONS, auto CONFIG>
inline Decoded decode_instr(Instr instr, (RVG) -> Format custom_decode)
{
    const bool ENABLE_MUL = (EXTENSIONS & Extension::M) == Extension::M;

    Format format = Format::Invalid;
    Decoded decoded = {};

    const auto opcode = instr.r.opcode.opcode;
    const auto funct3 = instr.r.funct3;
    const auto funct7_0000000 = instr.r.funct7 == 0b_0000000;
    const auto funct7_0100000 = instr.r.funct7 == 0b_0100000;
    const auto funct7_0000001 = instr.r.funct7 == 0b_0000001;

    decoded.major_opcode = opcode;
    decoded.minor_opcode = funct3;

    decoded.instr.compute.base.op = static_cast(funct3);
    decoded.instr.compute.base.logical = static_cast(funct3);
    decoded.instr.compute.base.sra = funct7_0100000;
    decoded.kind = InstrKind::Compute;

    if (opcode == RVG::LUI)
    {
        format = Format::U;
        assert(decoded.kind == InstrKind::Compute);
        decoded.instr.compute.base.op = ComputeOp::Logical;
        decoded.instr.compute.base.logical = LogicalOp::LUI;
    }
    else if (opcode == RVG::AUIPC)
    {
        format = Format::U;
        assert(decoded.kind == InstrKind::Compute);
        decoded.instr.compute.base.op = ComputeOp::ADD;
        decoded.pc_plus = true;
    }
    else if (opcode == RVG::JAL)
    {
        format = Format::J;
        decoded.kind = InstrKind::Control;
        decoded.instr.control.op = ControlOp::JUMP;
        decoded.control = true;
        decoded.pc_plus = true;
    }
    else if (opcode == RVG::JALR)
    {
        format = Format::I;
        decoded.kind = InstrKind::Control;
        decoded.instr.control.op = ControlOp::JUMP;
        decoded.control = true;
    }
    else if (opcode == RVG::BRANCH)
    {
        format = Format::B;
        decoded.kind = InstrKind::Control;
        decoded.instr.control.op = ControlOp::BRANCH;
        switch (funct3)
        {
            case Branch::BLT:
                decoded.instr.control.condition = Condition::LT;
                break;
            case Branch::BGE:
                decoded.instr.control.condition = Condition::GE;
                break;
            default:
                decoded.instr.control.condition = static_cast(funct3);
                break;
        }
        decoded.unsigned_comparison = (funct3 & 0b_010) != 0;
        decoded.control = true;
        decoded.pc_plus = true;
    }
    else if (opcode == RVG::LOAD)
    {
        format = Format::I;
        decoded.kind = InstrKind::Memory;
        decoded.instr.mem.op = MemoryOp::LOAD;
        decoded.instr.mem.size = static_cast(funct3);
        decoded.instr.mem.sign_extend = !reinterpret_cast<bool>(funct3 >> 2);
    }
    else if (opcode == RVG::STORE)
    {
        format = Format::S;
        decoded.kind = InstrKind::Memory;
        decoded.instr.mem.op = MemoryOp::STORE;
        decoded.instr.mem.size = static_cast(funct3);
        decoded.store = true;
    }
    else if (opcode == RVG::OP_IMM)
    {
        format = Format::I;
        assert(decoded.kind == InstrKind::Compute);
        assert(decoded.instr.compute.base.op == cast<ComputeOp>(funct3));

        if (funct3 == Op::XOR || funct3 == Op::OR)
        {
            static assert(ComputeOp::Logical == Op::AND);
            decoded.instr.compute.base.op = ComputeOp::Logical;
        }
        else if (funct3 == Op::SLL && !funct7_0000000)
        {
            format = Format::Invalid;
        }
        else if (funct3 == Op::SR && !(funct7_0000000 || funct7_0100000))
        {
            format = Format::Invalid;
        }

        decoded.unsigned_comparison = reinterpret_cast<bool>(funct3);
    }
    else if (opcode == RVG::OP)
    {
        assert(decoded.kind == InstrKind::Compute);
        assert(decoded.instr.compute.base.op == cast<ComputeOp>(funct3));

        decoded.unsigned_comparison = reinterpret_cast<bool>(funct3);

        if (static(ENABLE_MUL) && funct7_0000001)
        {
            format = Format::R;
            decoded.mul = true;

            static assert(MulDiv::MUL    == 0);
            static assert(MulDiv::MULH   == 1);
            static assert(MulDiv::MULHSU == 2);
            static assert(MulDiv::MULHU  == 3);

            decoded.instr.compute.mul = mux(cast<uint2>(funct3),
                    {MulIn::Signed,   MulIn::Signed,   MulOut::Lower},
                    {MulIn::Signed,   MulIn::Signed,   MulOut::Upper},
                    {MulIn::Signed,   MulIn::Unsigned, MulOut::Upper},
                    {MulIn::Unsigned, MulIn::Unsigned, MulOut::Upper});
        }
        else if (funct7_0000000)
        {
            format = Format::R;

            if (funct3 == Op::XOR || funct3 == Op::OR)
            {
                static assert(ComputeOp::Logical == Op::AND);
                decoded.instr.compute.base.op = ComputeOp::Logical;
            }
        }
        else if (funct7_0100000)
        {
            if (funct3 == Op::ADD)
            {
                format = Format::R;
                decoded.instr.compute.base.op = ComputeOp::SUB;
            }
            else if (funct3 == Op::SR)
            {
                static assert(ComputeOp::SR == Op::SR);
                assert(decoded.instr.compute.base.sra);
                format = Format::R;
            }
        }
    }
    else if (opcode == RVG::SYSTEM)
    {
        decoded.kind = InstrKind::System;

        if (funct3 == System::PRIV && instr.i.rd == ABI::zero && instr.i.rs1 == ABI::zero && (instr.i.imm_11_0 >> 1) == 0)
        {
            format = Format::I;
            decoded.trap = true;
            decoded.instr.system.op = cast<uint1>(instr.i.imm_11_0) == 0
                ? SystemOp::ECALL
                : SystemOp::EBREAK;
        }
        else if (funct3 == System::CSRRS && instr.i.rs1 == ABI::zero)
        {
            decoded.instr.system.op = SystemOp::CSRRS_MHARTID;

            if (instr.i.imm_11_0 == 0xF14)
            {
                format = Format::I;
            }
            // CSR registers 0xC00-0xC1F and 0xC80-0xC9F are reserved for peformance counters
            // and timers. The first three of these (CYCLE, TIME, and INSTRET) have dedicated
            // functions (cycle count, real time clock, and instructions retired respectively),
            // while the remaining counters, if implemented, provide platfom defined programmable
            // event counting.
            else if ((instr.i.imm_11_0 >> 5) == (0xC00 >> 5))
            {
                format = Format::I;

                switch (cast<uint2>(instr.i.imm_11_0))
                {
                    case 0:  decoded.instr.system.op = SystemOp::CSRRS_CYCLE; break;
                    case 1:  decoded.instr.system.op = SystemOp::CSRRS_CYCLE; break;
                    case 2:  decoded.instr.system.op = SystemOp::CSRRS_INSTRET; break;
                }
            }
            else if ((instr.i.imm_11_0 >> 5) == (0xC80 >> 5))
            {
                format = Format::I;

                switch (cast<uint2>(instr.i.imm_11_0))
                {
                    case 0:  decoded.instr.system.op = SystemOp::CSRRS_CYCLEH; break;
                    case 1:  decoded.instr.system.op = SystemOp::CSRRS_CYCLEH; break;
                    // high 32b of instret not implemented
                }
            }
        }
    }
    else if (opcode == RVG::MISC_MEM)
    {
        if (funct3 == MiscMem::FENCE && instr.i.rd == ABI::zero && instr.i.rs1 == ABI::zero)
        {
            format = Format::I;
            assert(decoded.kind == InstrKind::Compute);
        }
    }
    else if (opcode == RVG::custom_0
     || opcode == RVG::custom_1
     || opcode == RVG::custom_2
     || opcode == RVG::custom_3)
    {
        format = custom_decode(opcode);
        assert(decoded.kind == InstrKind::Compute);
        decoded.instr.compute.base.op = ComputeOp::Custom;
    }

    decoded.format = decode_format<Base::RV32I>(instr, format);
    decoded.illegal = format == Format::Invalid;

    static if ((CONFIG & Option::NoExceptionForCompressedEncoding) != Option::NoExceptionForCompressedEncoding)
    {
        decoded.illegal ||= instr.r.opcode.inst_1_0 != 0b_11;
    }

    return decoded;
}
