// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Pretty printer for RISC-V disassembly.
*/
module processor.risc_v.trace
    { Instruction
    , disasm
    , disasm_instr
    , print_trace
    , print_memory_map
    }

import data.string
import processor.risc_v.internal.decoder

//| RV32i assembly instruction mnemonics (including pseudo instructions)
enum Instruction : uint6
{
    unknown,
    lui,
    auipc,
    jal,
    jalr,
    beq,
    bne,
    blt,
    bge,
    bltu,
    bgeu,
    lb,
    lh,
    lw,
    lbu,
    lhu,
    sb,
    sh,
    sw,
    addi,
    slti,
    sltiu,
    xori,
    ori,
    andi,
    slli,
    srli,
    srai,
    add,
    sub,
    sll,
    slt,
    sltu,
    xor,
    srl,
    sra,
    or,
    and,
    fence,
    ecall,
    ebreak,
    mul,
    mulh,
    mulhsu,
    mulhu,
    csrr,
    rdcycle,
    rdcycleh,
    rdtime,
    rdtimeh,
    rdinstret,
    ret,
    j,
    jr,
    li,
    nop,
    mv
}

enum DisasmFormat : uint4
{
    empty,
    rd,
    rd_imm,
    rd_csr,
    rd_upper_imm,
    rd_imm_pc,
    imm_pc,
    rd_rs1,
    rd_rs1_rs2,
    rd_rs1_imm,
    rd_rs1_imm5,
    rd_imm_rs1,
    rs1,
    rs1_rs2_imm,
    rs2_imm_rs1
}

inline DisasmFormat disasm_format(Instruction instr)
{
    DisasmFormat format;

    switch (instr)
    {
        case Instruction::lui:       format = DisasmFormat::rd_upper_imm; break;
        case Instruction::auipc:     format = DisasmFormat::rd_upper_imm; break;
        case Instruction::jal:       format = DisasmFormat::rd_imm_pc; break;
        case Instruction::jalr:      format = DisasmFormat::rd_imm_rs1; break;
        case Instruction::beq:       format = DisasmFormat::rs1_rs2_imm; break;
        case Instruction::bne:       format = DisasmFormat::rs1_rs2_imm; break;
        case Instruction::blt:       format = DisasmFormat::rs1_rs2_imm; break;
        case Instruction::bge:       format = DisasmFormat::rs1_rs2_imm; break;
        case Instruction::bltu:      format = DisasmFormat::rs1_rs2_imm; break;
        case Instruction::bgeu:      format = DisasmFormat::rs1_rs2_imm; break;
        case Instruction::lb:        format = DisasmFormat::rd_imm_rs1; break;
        case Instruction::lh:        format = DisasmFormat::rd_imm_rs1; break;
        case Instruction::lw:        format = DisasmFormat::rd_imm_rs1; break;
        case Instruction::lbu:       format = DisasmFormat::rd_imm_rs1; break;
        case Instruction::lhu:       format = DisasmFormat::rd_imm_rs1; break;
        case Instruction::sb:        format = DisasmFormat::rs2_imm_rs1; break;
        case Instruction::sh:        format = DisasmFormat::rs2_imm_rs1; break;
        case Instruction::sw:        format = DisasmFormat::rs2_imm_rs1; break;
        case Instruction::addi:      format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::slti:      format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::sltiu:     format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::xori:      format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::ori:       format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::andi:      format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::slli:      format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::srli:      format = DisasmFormat::rd_rs1_imm; break;
        case Instruction::srai:      format = DisasmFormat::rd_rs1_imm5; break;
        case Instruction::add:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::sub:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::sll:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::slt:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::sltu:      format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::xor:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::srl:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::sra:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::or:        format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::and:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::fence:     format = DisasmFormat::empty; break;
        case Instruction::ecall:     format = DisasmFormat::empty; break;
        case Instruction::ebreak:    format = DisasmFormat::empty; break;
        case Instruction::mul:       format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::mulh:      format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::mulhsu:    format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::mulhu:     format = DisasmFormat::rd_rs1_rs2; break;
        case Instruction::csrr:      format = DisasmFormat::rd_csr; break;
        case Instruction::rdcycle:   format = DisasmFormat::rd; break;
        case Instruction::rdcycleh:  format = DisasmFormat::rd; break;
        case Instruction::rdtime:    format = DisasmFormat::rd; break;
        case Instruction::rdtimeh:   format = DisasmFormat::rd; break;
        case Instruction::rdinstret: format = DisasmFormat::rd; break;
        case Instruction::ret:       format = DisasmFormat::empty; break;
        case Instruction::j:         format = DisasmFormat::imm_pc; break;
        case Instruction::jr:        format = DisasmFormat::rs1; break;
        case Instruction::li:        format = DisasmFormat::rd_imm; break;
        case Instruction::nop:       format = DisasmFormat::empty; break;
        case Instruction::mv:        format = DisasmFormat::rd_rs1; break;
        case Instruction::unknown:   format = DisasmFormat::empty; break;
    }

    return format;
}

//| Return string representation of disassembled instruction
inline string disasm(uint32 binary, Decoded decoded)
{
    const auto instr = disasm_instr(decoded);
    const auto format = disasm_format(instr);

    const auto rd = cast<ABI>(decoded.format.rd.value);
    const auto rs1 = cast<ABI>(decoded.format.rs1.value);
    const auto rs2 = cast<ABI>(decoded.format.rs2.value);
    const auto imm = decoded.format.imm;

    string offset = imm < 0 ? "- {-imm}" : "+ {imm}";
    string asm;

    switch (format)
    {
        case DisasmFormat::rd:
            asm = "{instr}\t{rd}";
            break;
        case DisasmFormat::rd_imm:
            asm = "{instr}\t{rd}, {imm}";
            break;
        case DisasmFormat::rd_csr:
            switch (cast<uint12>(imm))
            {
                case 0xF14:
                    asm = "{instr}\t{rd}, mhartid";
                    break;
                default:
                    asm = "{instr}\t{rd}, {cast<uint12>(imm):x}";
                    break;
            }
            break;
        case DisasmFormat::rd_upper_imm:
            asm = "{instr}\t{rd}, 0x{imm >> 12:x0}";
            break;
        case DisasmFormat::rd_imm_pc:
            asm = "{instr}\t{rd}, pc {offset}";
            break;
        case DisasmFormat::imm_pc:
             asm = "{instr}\tpc {offset}";
             break;
        case DisasmFormat::rd_rs1_rs2:
            asm = "{instr}\t{rd}, {rs1}, {rs2}";
            break;
        case DisasmFormat::rd_rs1:
             asm = "{instr}\t{rd}, {rs1}";
             break;
        case DisasmFormat::rd_rs1_imm:
            asm = "{instr}\t{rd}, {rs1}, {imm}";
            break;
        case DisasmFormat::rd_rs1_imm5:
            asm = "{instr}\t{rd}, {rs1}, {cast<uint5>(imm)}";
            break;
        case DisasmFormat::rd_imm_rs1:
            asm = "{instr}\t{rd}, {imm}({rs1})";
            break;
        case DisasmFormat::rs1:
            asm = "{instr}\t{rs1}";
            break;
        case DisasmFormat::rs1_rs2_imm:
            asm = "{instr}\t{rs1}, {rs2}, pc {offset}";
            break;
        case DisasmFormat::rs2_imm_rs1:
             asm = "{instr}\t{rs2}, {imm}({rs1})";
             break;
        default:
            if (instr == Instruction::unknown)
                asm = "0x{binary:x}";
            else
                asm = "{instr}";
            break;
    }

    return asm;
}

//| Disassemble a decoded instruction
inline Instruction disasm_instr
    ( Decoded decoded //< Decoded instruction from `trace` callback
    )
{
    Instruction instr = Instruction::unknown;

    if (decoded.major_opcode == RVG::MISC_MEM)
    {
        if (decoded.minor_opcode == MiscMem::FENCE)
        {
            instr = Instruction::fence;
        }
    }
    else switch (decoded.kind)
    {
        case InstrKind::Compute:
            if (decoded.mul)
            {
                if (decoded.instr.compute.mul.result == MulOut::Lower)
                {
                    instr = Instruction::mul;
                }
                else
                {
                    if (decoded.instr.compute.mul.op2 == MulIn::Signed)
                    {
                        instr = Instruction::mulh;
                    }
                    else
                    {
                        if (decoded.instr.compute.mul.op1 == MulIn::Signed)
                        {
                            instr = Instruction::mulhsu;
                        }
                        else
                        {
                            instr = Instruction::mulhu;
                        }
                    }
                }
            }
            else
            {
                if (decoded.pc_plus)
                {
                    assert(decoded.instr.compute.base.op == ComputeOp::ADD);
                    instr = Instruction::auipc;
                }
                else
                {
                    const bool imm = !decoded.format.rs2.is_valid;

                    switch (decoded.instr.compute.base.op)
                    {
                        case ComputeOp::ADD:
                            if (imm)
                            {
                                if (decoded.format.rd.value == ABI::zero &&
                                    decoded.format.rs1.value == ABI::zero &&
                                    decoded.format.imm == 0)
                                {
                                    instr = Instruction::nop;
                                }
                                else if (decoded.format.rs1.value == ABI::zero)
                                {
                                    instr = Instruction::li;
                                }
                                else if (decoded.format.imm == 0)
                                {
                                    instr = Instruction::mv;
                                }
                                else
                                {
                                    instr = Instruction::addi;
                                }
                            }
                            else
                            {
                                instr = Instruction::add;
                            }
                            break;

                        case ComputeOp::SLL:
                            if (imm)
                            {
                                instr = Instruction::slli;
                            }
                            else
                            {
                                instr = Instruction::sll;
                            }
                            break;

                        case ComputeOp::SLT:
                            if (imm)
                            {
                                instr = Instruction::slti;
                            }
                            else
                            {
                                instr = Instruction::slt;
                            }
                            break;

                        case ComputeOp::SLTU:
                            if (imm)
                            {
                                instr = Instruction::sltiu;
                            }
                            else
                            {
                                instr = Instruction::sltu;
                            }
                            break;

                        case ComputeOp::SUB:
                            instr = Instruction::sub;
                            break;

                        case ComputeOp::SR:
                            if (decoded.instr.compute.base.sra)
                            {
                                if (imm)
                                {
                                    instr = Instruction::srai;
                                }
                                else
                                {
                                    instr = Instruction::sra;
                                }
                            }
                            else
                            {
                                if (imm)
                                {
                                    instr = Instruction::srli;
                                }
                                else
                                {
                                    instr = Instruction::srl;
                                }
                            }
                            break;

                        case ComputeOp::Custom:
                            instr = Instruction::unknown;
                            break;

                        case ComputeOp::Logical:
                            switch (decoded.instr.compute.base.logical)
                            {
                                case LogicalOp::XOR:
                                    if (imm)
                                    {
                                        instr = Instruction::xori;
                                    }
                                    else
                                    {
                                        instr = Instruction::xor;
                                    }
                                    break;

                                case LogicalOp::LUI:
                                    instr = Instruction::lui;
                                    break;

                                case LogicalOp::OR:
                                    if (imm)
                                    {
                                        instr = Instruction::ori;
                                    }
                                    else
                                    {
                                        instr = Instruction::or;
                                    }
                                    break;

                                case LogicalOp::AND:
                                    if (imm)
                                    {
                                        instr = Instruction::andi;
                                    }
                                    else
                                    {
                                        instr = Instruction::and;
                                    }
                                    break;
                            }
                            break;
                    }
                }
            }
            break;

        case InstrKind::Control:
            switch (decoded.instr.control.op)
            {
                case ControlOp::BRANCH:
                    switch (decoded.instr.control.condition)
                    {
                        case Condition::EQ:
                            instr = Instruction::beq;
                            break;

                        case Condition::NE:
                            instr = Instruction::bne;
                            break;

                        case Condition::LT:
                            if (decoded.unsigned_comparison)
                            {
                                instr = Instruction::bltu;
                            }
                            else
                            {
                                instr = Instruction::blt;
                            }
                            break;

                        case Condition::GE:
                            if (decoded.unsigned_comparison)
                            {
                                instr = Instruction::bgeu;
                            }
                            else
                            {
                                instr = Instruction::bge;
                            }
                            break;
                    }
                    break;

                case ControlOp::JUMP:
                    if (decoded.pc_plus)
                    {
                        if (decoded.format.rd.value == ABI::zero)
                        {
                            instr = Instruction::j;
                        }
                        else
                        {
                            instr = Instruction::jal;
                        }
                    }
                    else
                    {
                        if (decoded.format.rd.value == ABI::zero &&
                            decoded.format.imm == 0)
                        {
                            if (decoded.format.rs1.value == ABI::ra)
                            {
                                instr = Instruction::ret;
                            }
                            else
                            {
                                instr = Instruction::jr;
                            }
                        }
                        else
                        {
                            instr = Instruction::jalr;
                        }
                    }
                    break;
            }
            break;

        case InstrKind::Memory:
            switch (decoded.instr.mem.op)
            {
                case MemoryOp::LOAD:
                    switch (decoded.instr.mem.size)
                    {
                        case MemorySize::B:
                            if (decoded.instr.mem.sign_extend)
                            {
                                instr = Instruction::lb;
                            }
                            else
                            {
                                instr = Instruction::lbu;
                            }
                            break;

                        case MemorySize::H:
                            if (decoded.instr.mem.sign_extend)
                            {
                                instr = Instruction::lh;
                            }
                            else
                            {
                                instr = Instruction::lhu;
                            }
                            break;

                        case MemorySize::W:
                            instr = Instruction::lw;
                            break;
                    }
                    break;

                case MemoryOp::STORE:
                    switch (decoded.instr.mem.size)
                    {
                        case MemorySize::B:
                            instr = Instruction::sb;
                            break;

                        case MemorySize::H:
                            instr = Instruction::sh;
                            break;

                        case MemorySize::W:
                            instr = Instruction::sw;
                            break;
                    }
                    break;
            }
            break;

        case InstrKind::System:
            switch (decoded.instr.system.op)
            {
                case SystemOp::ECALL:
                    instr = Instruction::ecall;
                    break;

                case SystemOp::EBREAK:
                    instr = Instruction::ebreak;
                    break;

                case SystemOp::CSRRS_CYCLE:
                    if (cast<uint12>(decoded.format.imm) == 0xC00)
                    {
                        instr = Instruction::rdcycle;
                    }
                    else if (cast<uint12>(decoded.format.imm) == 0xC01)
                    {
                        instr = Instruction::rdtime;
                    }
                    else
                    {
                        instr = Instruction::csrr;
                    }
                    break;

                case SystemOp::CSRRS_CYCLEH:
                    if (cast<uint12>(decoded.format.imm) == 0xC80)
                    {
                        instr = Instruction::rdcycleh;
                    }
                    else if (cast<uint12>(decoded.format.imm) == 0xC81)
                    {
                        instr = Instruction::rdtimeh;
                    }
                    else
                    {
                        instr = Instruction::csrr;
                    }
                    break;

                case SystemOp::CSRRS_INSTRET:
                    instr = Instruction::rdinstret;
                    break;

                case SystemOp::CSRRS_MHARTID:
                    instr = Instruction::csrr;
                    break;
            }
            break;
    }

    return decoded.illegal ? Instruction::unknown : instr;
}

//| Print dynamic instruction trace. Can be used as `trace` handler.
inline void print_trace(auto hid, auto pc, auto instr, Decoded decoded, optional<int32> value)
{
    atomic
    {
        string update = "";

        if (value.is_valid)
        {
            if (decoded.format.rd.is_valid)
            {
                const auto rd = cast<ABI>(decoded.format.rd.value);
                update = "\t{rd} <- 0x{value.value:x0}";
            }
            else if (decoded.store)
            {
                update = "\tmem <- 0x{value.value:x0}";
            }
        }

        const auto asm = disasm(instr, decoded);

        print("0x{hid:x0} 0x{pc << 2:x8}:\t{instr:x8}\t{asm}{update}\n");
    }
}

const uint16 indentation = 18;

template <auto N>
inline void horizontal(uint32 addr)
{
    print("{"0x{addr:x8}",indentation} +{replicate<N-2>("-")}+\n");
}

inline void left()
{
    print("{"|",indentation+2}");
}

template <auto N>
inline void right()
{
    print("{"|",N}\n");
}

inline string size_unit(uint32 size)
{
    string result;

    if ((size / 1024) * 1024 == size)
    {
        result = "{"0x{size/1024:x0}",8} KB";
    }
    else
    {
        result = "{"0x{size:x0}",8} B";
    }

    return result;
}

inline void box(uint32 addr, uint32 size, optional<uint32> next_addr, const string label)
{
    const auto width = 24;
    const auto margin = (width - 8) / 2;
    const auto align = width - margin - 1;

    horizontal<width>(addr);

    left();                                    right<width-1>();
    left();  print("{label,align}");           right<margin>();
    left();  print("{size_unit(size),align}"); right<margin>();
    left();                                    right<width-1>();

    if (!next_addr.is_valid || next_addr.value != addr + size)
    {
        horizontal<width>(static_cast<uint32>(addr + size));
        print("\n");
    }
}

template <auto X1, auto X2, auto X3>
inline void sort
    ( (optional<uint32> next) -> void f1
    , (optional<uint32> next) -> void f2
    , (optional<uint32> next) -> void f3)
{
    static if (X2 < X1)
    {
        sort<X2, X1, X3>(f2, f1, f3);
    }
    else static if (X3 < X2)
    {
        sort<X1, X3, X2>(f1, f3, f2);
    }
    else
    {
        static assert(X1 <= X2 && X2 <= X3);

        f1({true, X2});
        f2({true, X3});
        f3({});
    }
}


//| Print a diagram of memory map. Usually this function is called indirectly
// via `print_memory_map` method of `RISC_V` class which passes appropriate
// template arguments.
template
    < auto IMEM_LENGTH_WORDS
    , auto DMEM_LENGTH
    , auto MMIO_LENGTH
    , auto IMEM_ORIGIN_WORDS
    , auto DMEM_ORIGIN
    , auto MMIO_ORIGIN
    , auto IMEM_TCM_SIZE_WORDS>
inline void print_memory_map()
{
    const auto IMEM_ORIGIN = IMEM_ORIGIN_WORDS << 2;
    const auto IMEM_LENGTH = IMEM_LENGTH_WORDS << 2;
    const auto IMEM_TCM_SIZE = IMEM_TCM_SIZE_WORDS << 2;

    sort<IMEM_ORIGIN, DMEM_ORIGIN, MMIO_ORIGIN>(
        [](optional<uint32> next)
        {
            static if (IMEM_TCM_SIZE != IMEM_LENGTH)
            {
                box(IMEM_ORIGIN, IMEM_TCM_SIZE, {true, IMEM_ORIGIN + IMEM_TCM_SIZE}, "IMEM TCM");
                box(IMEM_ORIGIN + IMEM_TCM_SIZE, IMEM_LENGTH - IMEM_TCM_SIZE, next, "IMEM EXT");
            }
            else
            {
                box(IMEM_ORIGIN, IMEM_LENGTH, next, "  IMEM  ");
            }
        },
        [](optional<uint32> next)
        {
            box(DMEM_ORIGIN, DMEM_LENGTH, next, "  DMEM  ");
        },
        [](optional<uint32> next)
        {
            box(MMIO_ORIGIN, MMIO_LENGTH, next, "  MMIO  ");
        });
}

