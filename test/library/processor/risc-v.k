// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import processor.risc_v.internal.decoder
import processor.risc_v.internal.core
import processor.risc_v.trace
import data.random.lfsr
import .options as opt
import test.unit as unit
import test.runner

template <auto N, auto M>
inline auto slice(auto x)
{
    uint1[N + 1] input = reinterpret_cast<uint1[N + 1]>(x);
    uint1[N - M + 1] result;

    static for(const auto i : N - M + 1)
    {
        result[i] = input[i + M];
    }

    return cast<uint<N - M + 1>>(result);
}

template <auto N, typename T>
inline auto at(T x)
{
    uint1[bitsizeof T] input = cast<uint1[bitsizeof T]>(x);

    return input[N];
}

inline void sleep(auto delay)
{
    const auto start = cycles();
    wait([delay, start]()
    {
        return cycles() - start >= delay;
    });
}

class Tests
{
    const auto SHARED_DMEM_LENGTH = 0x3000;
    const auto DMEM_LENGTH = 0x2000;
    const auto DMEM_ORIGIN = 0x10000;
    const auto STACK_SIZE = 0x300;
    const auto IMEM_LENGTH = 0x4000;
    const auto IMEM_ORIGIN = 0;
    const auto IMEM_TCM_SIZE = 0x2000;
    const auto EXTERNAL_IMEM_ADDR = IMEM_TCM_SIZE << 2;
    const auto MMIO_LENGTH = 0x110;
    const auto MMIO_ORIGIN = 0x10000000;

    using core_t           = Core<4, IMEM_LENGTH, DMEM_LENGTH, MMIO_LENGTH, IMEM_ORIGIN, DMEM_ORIGIN, MMIO_ORIGIN, IMEM_TCM_SIZE, memory_norep, Extension::M>;
    using register_index_t = core_t::register_index_t;
    using uint_t           = core_t::uint_t;
    using int_t            = core_t::int_t;
    using imem_addr_t      = core_t::imem_addr_t;
    using dmem_addr_t      = core_t::dmem_addr_t;
    using hart_index_t     = core_t::hart_index_t;

    const int_t uninit_int = cast<int_t>(0xdeadbeef);
    const uint_t uninit_uint = 0xdeadbeef;

    template <auto N, auto M, typename T>
    inline auto imm_slice(T x)
    {
        return sign_extend(cast<uint<N+1>>(slice<N, M>(x) << M));
    }

    core_t core;

    inline Format CustomDecode(RVG major_opcode)
    {
        assert(major_opcode == RVG::custom_0
                || major_opcode == RVG::custom_1
                || major_opcode == RVG::custom_2
                || major_opcode == RVG::custom_3);

        return mux(cast<uint2>(major_opcode >> 3), Format::R, Format::I, Format::U, Format::S);
    }

    inline int_t CustomExecute(hart_index_t hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7)
    {
        int_t result = uninit_int;

        if (major_opcode == RVG::custom_0 && minor_opcode == 0b011)
        {
            result = (op1 * 2) | (op2 / 2);
        }

        return result;
    }

public:
    inline void DisasmTest(unit::tag_t tag)
    {
        const int32 imm = -10;
        const auto rd = ABI::a4;
        const auto rs1 = ABI::t4;
        const auto rs2 = ABI::a7;
        const auto zero = ABI::zero;
        const auto ra = ABI::ra;

        const uint5 sha = 8;

        auto LUI    = cast<Instr>(concat(slice<31,12>(imm), rd, cast<uint7>(0b_0110111)));
        auto AUIPC  = cast<Instr>(concat(slice<31,12>(imm), rd, cast<uint7>(0b_0010111)));
        auto JAL    = cast<Instr>(concat(at<20>(imm), slice<10,1>(imm), at<11>(imm), slice<19,12>(imm), rd, cast<uint7>(0b_1101111)));
        auto J      = cast<Instr>(concat(at<20>(imm), slice<10,1>(imm), at<11>(imm), slice<19,12>(imm), zero, cast<uint7>(0b_1101111)));
        auto JALR   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_1100111)));
        auto JR     = cast<Instr>(concat(slice<11,0>(0), rs1, cast<Funct3>(0b_000), zero, cast<uint7>(0b_1100111)));
        auto RET    = cast<Instr>(concat(slice<11,0>(0), ra, cast<Funct3>(0b_000), zero, cast<uint7>(0b_1100111)));
        auto BEQ    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_000), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BNE    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_001), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BLT    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_100), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BGE    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_101), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BLTU   = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_110), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BGEU   = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_111), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto LB     = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0000011)));
        auto LH     = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0000011)));
        auto LW     = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0000011)));
        auto LBU    = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0000011)));
        auto LHU    = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0000011)));
        auto SB     = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_000), slice<4,0>(imm), cast<uint7>(0b_0100011)));
        auto SH     = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_001), slice<4,0>(imm), cast<uint7>(0b_0100011)));
        auto SW     = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_010), slice<4,0>(imm), cast<uint7>(0b_0100011)));
        auto ADDI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0010011)));
        auto MV     = cast<Instr>(concat(slice<11,0>(0), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0010011)));
        auto LI     = cast<Instr>(concat(slice<11,0>(imm), zero, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0010011)));
        auto NOP    = cast<Instr>(concat(slice<11,0>(0), zero, cast<Funct3>(0b_000), zero, cast<uint7>(0b_0010011)));
        auto SLTI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0010011)));
        auto SLTIU  = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0010011)));
        auto XORI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0010011)));
        auto ORI    = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_110), rd, cast<uint7>(0b_0010011)));
        auto ANDI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_111), rd, cast<uint7>(0b_0010011)));
        auto SLLI   = cast<Instr>(concat(cast<Funct7>(0b_0000000), sha, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0010011)));
        auto SRLI   = cast<Instr>(concat(cast<Funct7>(0b_0000000), sha, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0010011)));
        auto SRAI   = cast<Instr>(concat(cast<Funct7>(0b_0100000), sha, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0010011)));
        auto ADD    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011)));
        auto SUB    = cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011)));
        auto SLL    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011)));
        auto SLT    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011)));
        auto SLTU   = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011)));
        auto XOR    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0110011)));
        auto SRL    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0110011)));
        auto SRA    = cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0110011)));
        auto OR     = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_110), rd, cast<uint7>(0b_0110011)));
        auto AND    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_111), rd, cast<uint7>(0b_0110011)));
        auto RDCYCLE = cast<Instr>(concat(cast<uint12>(0b_110000000000), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDCYCLEH = cast<Instr>(concat(cast<uint12>(0b_110010000000), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDTIME = cast<Instr>(concat(cast<uint12>(0b_110000000001), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDTIMEH = cast<Instr>(concat(cast<uint12>(0b_110010000001), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDINSTRET = cast<Instr>(concat(cast<uint12>(0b_110000000010), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto MHARTID = cast<Instr>(concat(cast<uint12>(0b_111100010100), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto CSRR   = cast<Instr>(concat(cast<uint12>(0b_110000000011), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto FENCE  = cast<Instr>(concat(slice<11,0>(imm), cast<uint5>(0b_00000), cast<Funct3>(0b_000), cast<uint5>(0b_00000), cast<uint7>(0b_0001111)));
        auto ECALL  = cast<Instr>(concat(cast<uint12>(0b_000000000000), cast<uint5>(0b_00000), cast<Funct3>(0b_000), cast<uint5>(0b_00000), cast<uint7>(0b_1110011)));
        auto EBREAK = cast<Instr>(concat(cast<uint12>(0b_000000000001), cast<uint5>(0b_00000), cast<Funct3>(0b_000), cast<uint5>(0b_00000), cast<uint7>(0b_1110011)));
        auto MUL    = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011)));
        auto MULH   = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011)));
        auto MULHSU = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011)));
        auto MULHU  = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011)));

        template <Extension E>
        inline string disassemble(Instr instr)
        {
            return disasm(reinterpret_cast(instr), decode_instr<E, Optimize::Area>(instr, CustomDecode));
        }

        unit::assert_equal(tag, "lui	a4, 0xfffff",     disassemble<Extension::None>(LUI));
        unit::assert_equal(tag, "auipc	a4, 0xfffff",     disassemble<Extension::None>(AUIPC));
        unit::assert_equal(tag, "jal	a4, pc - 10",     disassemble<Extension::None>(JAL));
        unit::assert_equal(tag, "j	pc - 10",             disassemble<Extension::None>(J));
        unit::assert_equal(tag, "jalr	a4, -10(t4)",     disassemble<Extension::None>(JALR));
        unit::assert_equal(tag, "jr	t4",                  disassemble<Extension::None>(JR));
        unit::assert_equal(tag, "ret",                    disassemble<Extension::None>(RET));
        unit::assert_equal(tag, "beq	t4, a7, pc - 10", disassemble<Extension::None>(BEQ));
        unit::assert_equal(tag, "bne	t4, a7, pc - 10", disassemble<Extension::None>(BNE));
        unit::assert_equal(tag, "blt	t4, a7, pc - 10", disassemble<Extension::None>(BLT));
        unit::assert_equal(tag, "bge	t4, a7, pc - 10", disassemble<Extension::None>(BGE));
        unit::assert_equal(tag, "bltu	t4, a7, pc - 10", disassemble<Extension::None>(BLTU));
        unit::assert_equal(tag, "bgeu	t4, a7, pc - 10", disassemble<Extension::None>(BGEU));
        unit::assert_equal(tag, "lb	a4, -10(t4)",         disassemble<Extension::None>(LB));
        unit::assert_equal(tag, "lh	a4, -10(t4)",         disassemble<Extension::None>(LH));
        unit::assert_equal(tag, "lw	a4, -10(t4)",         disassemble<Extension::None>(LW));
        unit::assert_equal(tag, "lbu	a4, -10(t4)",     disassemble<Extension::None>(LBU));
        unit::assert_equal(tag, "lhu	a4, -10(t4)",     disassemble<Extension::None>(LHU));
        unit::assert_equal(tag, "sb	a7, -10(t4)",         disassemble<Extension::None>(SB));
        unit::assert_equal(tag, "sh	a7, -10(t4)",         disassemble<Extension::None>(SH));
        unit::assert_equal(tag, "sw	a7, -10(t4)",         disassemble<Extension::None>(SW));
        unit::assert_equal(tag, "addi	a4, t4, -10",     disassemble<Extension::None>(ADDI));
        unit::assert_equal(tag, "mv	a4, t4",              disassemble<Extension::None>(MV));
        unit::assert_equal(tag, "li	a4, -10",             disassemble<Extension::None>(LI));
        unit::assert_equal(tag, "nop",                    disassemble<Extension::None>(NOP));
        unit::assert_equal(tag, "slti	a4, t4, -10",     disassemble<Extension::None>(SLTI));
        unit::assert_equal(tag, "sltiu	a4, t4, -10",     disassemble<Extension::None>(SLTIU));
        unit::assert_equal(tag, "xori	a4, t4, -10",     disassemble<Extension::None>(XORI));
        unit::assert_equal(tag, "ori	a4, t4, -10",     disassemble<Extension::None>(ORI));
        unit::assert_equal(tag, "andi	a4, t4, -10",     disassemble<Extension::None>(ANDI));
        unit::assert_equal(tag, "slli	a4, t4, 8",       disassemble<Extension::None>(SLLI));
        unit::assert_equal(tag, "srli	a4, t4, 8",       disassemble<Extension::None>(SRLI));
        unit::assert_equal(tag, "srai	a4, t4, 8",       disassemble<Extension::None>(SRAI));
        unit::assert_equal(tag, "add	a4, t4, a7",      disassemble<Extension::None>(ADD));
        unit::assert_equal(tag, "sub	a4, t4, a7",      disassemble<Extension::None>(SUB));
        unit::assert_equal(tag, "slt	a4, t4, a7",      disassemble<Extension::None>(SLT));
        unit::assert_equal(tag, "sltu	a4, t4, a7",      disassemble<Extension::None>(SLTU));
        unit::assert_equal(tag, "xor	a4, t4, a7",      disassemble<Extension::None>(XOR));
        unit::assert_equal(tag, "or	a4, t4, a7",          disassemble<Extension::None>(OR));
        unit::assert_equal(tag, "and	a4, t4, a7",      disassemble<Extension::None>(AND));
        unit::assert_equal(tag, "sll	a4, t4, a7",      disassemble<Extension::None>(SLL));
        unit::assert_equal(tag, "srl	a4, t4, a7",      disassemble<Extension::None>(SRL));
        unit::assert_equal(tag, "sra	a4, t4, a7",      disassemble<Extension::None>(SRA));
        unit::assert_equal(tag, "rdcycle	a4",          disassemble<Extension::None>(RDCYCLE));
        unit::assert_equal(tag, "rdtime	a4",              disassemble<Extension::None>(RDTIME));
        unit::assert_equal(tag, "rdcycleh	a4",          disassemble<Extension::None>(RDCYCLEH));
        unit::assert_equal(tag, "rdtimeh	a4",          disassemble<Extension::None>(RDTIMEH));
        unit::assert_equal(tag, "rdinstret	a4",          disassemble<Extension::None>(RDINSTRET));
        unit::assert_equal(tag, "csrr	a4, c03",         disassemble<Extension::None>(CSRR));
        unit::assert_equal(tag, "csrr	a4, mhartid",     disassemble<Extension::None>(MHARTID));
        unit::assert_equal(tag, "fence",                  disassemble<Extension::None>(FENCE));
        unit::assert_equal(tag, "ecall",                  disassemble<Extension::None>(ECALL));
        unit::assert_equal(tag, "ebreak",                 disassemble<Extension::None>(EBREAK));
        unit::assert_equal(tag, "mul	a4, t4, a7",      disassemble<Extension::M>(MUL));
        unit::assert_equal(tag, "mulh	a4, t4, a7",      disassemble<Extension::M>(MULH));
        unit::assert_equal(tag, "mulhsu	a4, t4, a7",      disassemble<Extension::M>(MULHSU));
        unit::assert_equal(tag, "mulhu	a4, t4, a7",      disassemble<Extension::M>(MULHU));
    }

    void DecoderTests(register_index_t reg, int_t imm)
    {
        register_index_t rd = reg;
        register_index_t rs1 = static_cast(reg + 5);
        register_index_t rs2 = static_cast(reg + 15);
        const register_index_t zero = ABI::zero;
        const register_index_t ra = ABI::ra;

        uint5 sha = static_cast(reg + 3);

        auto LUI    = cast<Instr>(concat(slice<31,12>(imm), rd, cast<uint7>(0b_0110111)));
        auto AUIPC  = cast<Instr>(concat(slice<31,12>(imm), rd, cast<uint7>(0b_0010111)));
        auto JAL    = cast<Instr>(concat(at<20>(imm), slice<10,1>(imm), at<11>(imm), slice<19,12>(imm), rd, cast<uint7>(0b_1101111)));
        auto JALR   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_1100111)));
        auto RET    = cast<Instr>(concat(slice<11,0>(0), ra, cast<Funct3>(0b_000), zero, cast<uint7>(0b_1100111)));
        auto BEQ    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_000), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BNE    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_001), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BLT    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_100), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BGE    = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_101), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BLTU   = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_110), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto BGEU   = cast<Instr>(concat(at<12>(imm), slice<10,5>(imm), rs2, rs1, cast<Funct3>(0b_111), slice<4,1>(imm), at<11>(imm), cast<uint7>(0b_1100011)));
        auto LB     = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0000011)));
        auto LH     = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0000011)));
        auto LW     = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0000011)));
        auto LBU    = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0000011)));
        auto LHU    = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0000011)));
        auto SB     = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_000), slice<4,0>(imm), cast<uint7>(0b_0100011)));
        auto SH     = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_001), slice<4,0>(imm), cast<uint7>(0b_0100011)));
        auto SW     = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_010), slice<4,0>(imm), cast<uint7>(0b_0100011)));
        auto ADDI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0010011)));
        auto NOP    = cast<Instr>(concat(slice<11,0>(0), zero, cast<Funct3>(0b_000), zero, cast<uint7>(0b_0010011)));
        auto SLTI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0010011)));
        auto SLTIU  = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0010011)));
        auto XORI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0010011)));
        auto ORI    = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_110), rd, cast<uint7>(0b_0010011)));
        auto ANDI   = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_111), rd, cast<uint7>(0b_0010011)));
        auto SLLI   = cast<Instr>(concat(cast<Funct7>(0b_0000000), sha, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0010011)));
        auto SRLI   = cast<Instr>(concat(cast<Funct7>(0b_0000000), sha, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0010011)));
        auto SRAI   = cast<Instr>(concat(cast<Funct7>(0b_0100000), sha, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0010011)));
        auto ADD    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011)));
        auto SUB    = cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011)));
        auto SLL    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011)));
        auto SLT    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011)));
        auto SLTU   = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011)));
        auto XOR    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0110011)));
        auto SRL    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0110011)));
        auto SRA    = cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0110011)));
        auto OR     = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_110), rd, cast<uint7>(0b_0110011)));
        auto AND    = cast<Instr>(concat(cast<Funct7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_111), rd, cast<uint7>(0b_0110011)));
        auto RDCYCLE = cast<Instr>(concat(cast<uint12>(0b_110000000000), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDCYCLEH = cast<Instr>(concat(cast<uint12>(0b_110010000000), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDTIME = cast<Instr>(concat(cast<uint12>(0b_110000000001), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDTIMEH = cast<Instr>(concat(cast<uint12>(0b_110010000001), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto RDINSTRET = cast<Instr>(concat(cast<uint12>(0b_110000000010), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto MHARTID = cast<Instr>(concat(cast<uint12>(0b_111100010100), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto CSRRS  = cast<Instr>(concat(cast<uint12>(0b_110010000011), cast<uint5>(0b_00001), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto CSRR   = cast<Instr>(concat(cast<uint12>(0b_110000000011), cast<uint5>(0b_00000), cast<Funct3>(0b_010), rd, cast<uint7>(0b_1110011)));
        auto FENCE  = cast<Instr>(concat(slice<11,0>(imm), cast<uint5>(0b_00000), cast<Funct3>(0b_000), cast<uint5>(0b_00000), cast<uint7>(0b_0001111)));
        auto ECALL  = cast<Instr>(concat(cast<uint12>(0b_000000000000), cast<uint5>(0b_00000), cast<Funct3>(0b_000), cast<uint5>(0b_00000), cast<uint7>(0b_1110011)));
        auto EBREAK = cast<Instr>(concat(cast<uint12>(0b_000000000001), cast<uint5>(0b_00000), cast<Funct3>(0b_000), cast<uint5>(0b_00000), cast<uint7>(0b_1110011)));
        auto MUL    = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011)));
        auto MULH   = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011)));
        auto MULHSU = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011)));
        auto MULHU  = cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011)));

        auto EXT_R  = cast<Instr>(concat(cast<Funct7>(0b_1011010), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0001011)));
        auto EXT_I  = cast<Instr>(concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0101011)));
        auto EXT_U  = cast<Instr>(concat(slice<31,12>(imm), rd, cast<uint7>(0b_1011011)));
        auto EXT_S  = cast<Instr>(concat(slice<11,5>(imm), rs2, rs1, cast<Funct3>(0b_010), slice<4,0>(imm), cast<uint7>(0b_1111011)));

        Instr[21] illegal = {
            cast<Instr>(concat(cast<Funct7>(0b_0100000), sha, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0010011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), sha, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0010011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), sha, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0010011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_100), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_101), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_110), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_111), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_110), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_111), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0110011))),
            cast<Instr>(concat(cast<Funct7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_011), rd, cast<uint7>(0b_0110011)))
        };

        static for (const auto i : 21)
        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(illegal[i], CustomDecode);
            assert(decoded.illegal);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(LUI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::lui);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::LUI);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<31,12>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(AUIPC, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::auipc);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::ADD);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<31,12>(imm));
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(JAL, CustomDecode);
            if (decoded.format.rd.value == ABI::zero)
            {
                assert(disasm_instr(decoded) == Instruction::j);
            }
            else
            {
                assert(disasm_instr(decoded) == Instruction::jal);
            }
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::JUMP);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<20,1>(imm));
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(JALR, CustomDecode);
            if (decoded.format.rd.value == ABI::zero && decoded.format.imm == 0)
            {
                assert(disasm_instr(decoded) == Instruction::jr);
            }
            else
            {
                assert(disasm_instr(decoded) == Instruction::jalr);
            }
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::JUMP);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(RET, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::ret);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::JUMP);
            assert(decoded.format.rd.is_valid == false);
            assert(decoded.format.rd.value == ABI::zero);
            assert(decoded.format.rs1.value == ABI::ra);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(0));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(BEQ, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::beq);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::BRANCH);
            assert(decoded.instr.control.condition == Condition::EQ);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<12,1>(imm));
            assert(!decoded.unsigned_comparison);
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(BNE, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::bne);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::BRANCH);
            assert(decoded.instr.control.condition == Condition::NE);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<12,1>(imm));
            assert(!decoded.unsigned_comparison);
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(BLT, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::blt);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::BRANCH);
            assert(decoded.instr.control.condition == Condition::LT);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<12,1>(imm));
            assert(!decoded.unsigned_comparison);
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(BGE, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::bge);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::BRANCH);
            assert(decoded.instr.control.condition == Condition::GE);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<12,1>(imm));
            assert(!decoded.unsigned_comparison);
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(BLTU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::bltu);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::BRANCH);
            assert(decoded.instr.control.condition == Condition::LT);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<12,1>(imm));
            assert(decoded.unsigned_comparison);
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(BGEU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::bgeu);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Control);
            assert(decoded.instr.control.op == ControlOp::BRANCH);
            assert(decoded.instr.control.condition == Condition::GE);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<12,1>(imm));
            assert(decoded.unsigned_comparison);
            assert(decoded.pc_plus);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(LB, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::lb);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::LOAD);
            assert(decoded.instr.mem.size == MemorySize::B);
            assert(decoded.instr.mem.sign_extend);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(LH, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::lh);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::LOAD);
            assert(decoded.instr.mem.size == MemorySize::H);
            assert(decoded.instr.mem.sign_extend);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(LW, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::lw);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::LOAD);
            assert(decoded.instr.mem.size == MemorySize::W);
            assert(decoded.instr.mem.sign_extend);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(LBU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::lbu);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::LOAD);
            assert(decoded.instr.mem.size == MemorySize::B);
            assert(!decoded.instr.mem.sign_extend);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(LHU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::lhu);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::LOAD);
            assert(decoded.instr.mem.size == MemorySize::H);
            assert(!decoded.instr.mem.sign_extend);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SB, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sb);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::STORE);
            assert(decoded.instr.mem.size == MemorySize::B);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SH, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sh);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::STORE);
            assert(decoded.instr.mem.size == MemorySize::H);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SW, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sw);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::Memory);
            assert(decoded.instr.mem.op == MemoryOp::STORE);
            assert(decoded.instr.mem.size == MemorySize::W);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(ADDI, CustomDecode);
            if (decoded.format.rs1.value == ABI::zero)
            {
                assert(disasm_instr(decoded) == Instruction::li);
            }
            else if (decoded.format.imm == 0)
            {
                assert(disasm_instr(decoded) == Instruction::mv);
            }
            else
            {
                assert(disasm_instr(decoded) == Instruction::addi);
            }
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::ADD);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(NOP, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::nop);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::ADD);
            assert(decoded.format.rd.is_valid == false);
            assert(decoded.format.rd.value == ABI::zero);
            assert(decoded.format.rs1.value == ABI::zero);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(0));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SLTI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::slti);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SLT);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SLTIU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sltiu);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.unsigned_comparison);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SLTU);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(XORI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::xori);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::XOR);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(ORI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::ori);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::OR);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(ANDI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::andi);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::AND);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SLLI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::slli);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SLL);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(cast<uint5>(decoded.format.imm) == sha);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SRLI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::srli);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SR);
            assert(!decoded.instr.compute.base.sra);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(cast<uint5>(decoded.format.imm) == sha);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SRAI, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::srai);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SR);
            assert(decoded.instr.compute.base.sra);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(cast<uint5>(decoded.format.imm) == sha);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(ADD, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::add);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::ADD);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SUB, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sub);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SUB);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SLT, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::slt);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SLT);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SLTU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sltu);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.unsigned_comparison);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SLTU);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(XOR, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::xor);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::XOR);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(OR, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::or);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::OR);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(AND, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::and);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Logical);
            assert(decoded.instr.compute.base.logical == LogicalOp::AND);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SLL, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sll);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SLL);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SRL, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::srl);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SR);
            assert(!decoded.instr.compute.base.sra);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(SRA, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::sra);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::SR);
            assert(decoded.instr.compute.base.sra);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(RDCYCLE, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::rdcycle);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_CYCLE);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(RDTIME, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::rdtime);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_CYCLE);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(RDCYCLEH, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::rdcycleh);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_CYCLEH);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(RDTIMEH, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::rdtimeh);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_CYCLEH);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(RDINSTRET, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::rdinstret);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_INSTRET);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(CSRRS, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::unknown);
            assert(decoded.illegal);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(CSRR, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::csrr);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_MHARTID);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(MHARTID, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::csrr);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::CSRRS_MHARTID);
            assert(!decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(FENCE, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::fence);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(!decoded.format.rd.is_valid);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(ECALL, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::ecall);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::ECALL);
            assert(decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(EBREAK, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::ebreak);
            assert(!decoded.illegal);
            assert(decoded.kind == InstrKind::System);
            assert(decoded.instr.system.op == SystemOp::EBREAK);
            assert(decoded.trap);
        }

        {
            auto decoded = decode_instr<Extension::M, Optimize::Area>(MUL, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::mul);
            assert(!decoded.illegal);
            assert(decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.mul.op1 == MulIn::Signed);
            assert(decoded.instr.compute.mul.op2 == MulIn::Signed);
            assert(decoded.instr.compute.mul.result == MulOut::Lower);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::M, Optimize::Area>(MULH, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::mulh);
            assert(!decoded.illegal);
            assert(decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.mul.op1 == MulIn::Signed);
            assert(decoded.instr.compute.mul.op2 == MulIn::Signed);
            assert(decoded.instr.compute.mul.result == MulOut::Upper);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::M, Optimize::Area>(MULHSU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::mulhsu);
            assert(!decoded.illegal);
            assert(decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.mul.op1 == MulIn::Signed);
            assert(decoded.instr.compute.mul.op2 == MulIn::Unsigned);
            assert(decoded.instr.compute.mul.result == MulOut::Upper);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::M, Optimize::Area>(MULHU, CustomDecode);
            assert(disasm_instr(decoded) == Instruction::mulhu);
            assert(!decoded.illegal);
            assert(decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.mul.op1 == MulIn::Unsigned);
            assert(decoded.instr.compute.mul.op2 == MulIn::Unsigned);
            assert(decoded.instr.compute.mul.result == MulOut::Upper);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(EXT_R, CustomDecode);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Custom);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.major_opcode == RVG::custom_0);
            assert(decoded.minor_opcode == 0b_001);
            assert(decoded.format.funct7 == 0b_1011010);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(EXT_I, CustomDecode);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Custom);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
            assert(decoded.major_opcode == RVG::custom_1);
            assert(decoded.minor_opcode == 0b_101);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(EXT_U, CustomDecode);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Custom);
            assert(decoded.format.rd.is_valid == (rd != 0));
            assert(decoded.format.rd.value == rd);
            assert(!decoded.format.rs2.is_valid);
            assert(decoded.format.imm == imm_slice<31,12>(imm));
            assert(decoded.major_opcode == RVG::custom_2);
        }

        {
            auto decoded = decode_instr<Extension::None, Optimize::Area>(EXT_S, CustomDecode);
            assert(!decoded.illegal);
            assert(!decoded.mul);
            assert(decoded.kind == InstrKind::Compute);
            assert(decoded.instr.compute.base.op == ComputeOp::Custom);
            assert(!decoded.format.rd.is_valid);
            assert(decoded.format.rs1.is_valid);
            assert(decoded.format.rs1.value == rs1);
            assert(decoded.format.rs2.is_valid);
            assert(decoded.format.rs2.value == rs2);
            assert(decoded.format.imm == imm_slice<11,0>(imm));
            assert(decoded.major_opcode == RVG::custom_3);
            assert(decoded.minor_opcode == 0b_010);
        }
    }

    enum Opcode : uint5
    {
        LUI, AUIPC, OR, XOR, AND, ADD, SUB, SLL, SRL, SRA, SLT, SLTU, MUL, MULH, MULHSU, MULHU, EXT
    }

    int_t ComputeTests(Opcode opcode, int_t op1, int_t op2, int_t imm, imem_addr_t pc)
    {
        core_t::Current current;
        core_t::Operands operands;
        core_t::Results results;
        Decoded decoded;

        current.hid = 0;
        current.pc = pc;
        current.next_pc = pc + 1;

        operands.op1 = op1;
        operands.add1 = op1;
        operands.sub1 = opcode == Opcode::SLTU ? cast<uint_t>(op1) : op1;

        operands.rs2 = op2;
        operands.op2 = op2;
        operands.sub2 = opcode == Opcode::SLTU ? cast<uint_t>(op2) : op2;

        decoded.format.imm = imm;

        switch (opcode)
        {
            case Opcode::LUI:
                operands.op2 = imm;
                decoded.instr.compute.base.op = ComputeOp::Logical;
                decoded.instr.compute.base.logical = LogicalOp::LUI;
                break;

            case Opcode::AUIPC:
                decoded.instr.compute.base.op = ComputeOp::ADD;
                operands.add1 = pc << 2;
                operands.op2 = (imm >> 1) << 1;
                break;

            case Opcode::OR:
                decoded.instr.compute.base.op = ComputeOp::Logical;
                decoded.instr.compute.base.logical = LogicalOp::OR;
                break;

            case Opcode::XOR:
                decoded.instr.compute.base.op = ComputeOp::Logical;
                decoded.instr.compute.base.logical = LogicalOp::XOR;
                break;

            case Opcode::AND:
                decoded.instr.compute.base.op = ComputeOp::Logical;
                decoded.instr.compute.base.logical = LogicalOp::AND;
                break;

            case Opcode::ADD:
                decoded.instr.compute.base.op = ComputeOp::ADD;
                break;

            case Opcode::SUB:
                decoded.instr.compute.base.op = ComputeOp::SUB;
                break;

            case Opcode::SLL:
                decoded.instr.compute.base.op = ComputeOp::SLL;
                break;

            case Opcode::SRL:
                decoded.instr.compute.base.op = ComputeOp::SR;
                decoded.instr.compute.base.sra = false;
                break;

            case Opcode::SRA:
                decoded.instr.compute.base.op = ComputeOp::SR;
                decoded.instr.compute.base.sra = true;
                break;

            case Opcode::SLT:
                decoded.instr.compute.base.op = ComputeOp::SLT;
                break;

            case Opcode::SLTU:
                decoded.instr.compute.base.op = ComputeOp::SLT;
                break;

            case Opcode::MUL:
                decoded.mul = true;
                decoded.instr.compute.mul = {MulIn::Signed, MulIn::Signed, MulOut::Lower};
                break;

            case Opcode::MULH:
                decoded.mul = true;
                decoded.instr.compute.mul = {MulIn::Signed, MulIn::Signed, MulOut::Upper};
                break;

            case Opcode::MULHSU:
                decoded.mul = true;
                decoded.instr.compute.mul = {MulIn::Signed, MulIn::Unsigned, MulOut::Upper};
                break;

            case Opcode::MULHU:
                decoded.mul = true;
                decoded.instr.compute.mul = {MulIn::Unsigned, MulIn::Unsigned, MulOut::Upper};
                break;

            case Opcode::EXT:
                decoded.instr.compute.base.op = ComputeOp::Custom;
                decoded.major_opcode = RVG::custom_0;
                decoded.minor_opcode = 0b_011;
                break;

            default:
                assert(false);
                break;
        }

        const auto custom = CustomExecute(current.hid, decoded.major_opcode, decoded.minor_opcode, operands.op1, operands.op2, decoded.format.imm, decoded.format.funct7);

        results = core.execute(current, decoded, operands, custom);
        return decoded.mul ? results.mul : results.compute;
    }

    enum Cond : uint3
    {
        EQ, NE, LT, GE, LTU, GEU
    }

    bool BranchConditionTests(Cond cond, int_t op1, int_t op2)
    {
        core_t::Operands operands;
        core_t::Results results;
        core_t::Current current;
        Decoded decoded;

        current.next_pc = current.pc + 1;

        decoded.control = true;
        decoded.kind = InstrKind::Control;
        decoded.instr.control.op = ControlOp::BRANCH;

        operands.op1 = op1;
        operands.op2 = op2;

        if (cond == Cond::LTU || cond == Cond::GEU)
        {
            operands.sub1 = cast<uint_t>(op1);
            operands.sub2 = cast<uint_t>(op2);
        }
        else
        {
            operands.sub1 = op1;
            operands.sub2 = op2;
        }

        switch(cond)
        {
            case Cond::EQ:
                decoded.instr.control.condition = Condition::EQ;
                break;

            case Cond::NE:
                decoded.instr.control.condition = Condition::NE;
                break;

            case Cond::LT:
                decoded.instr.control.condition = Condition::LT;
                break;

            case Cond::GE:
                decoded.instr.control.condition = Condition::GE;
                break;

            case Cond::LTU:
                decoded.instr.control.condition = Condition::LT;
                break;

            case Cond::GEU:
                decoded.instr.control.condition = Condition::GE;
                break;
        }

        results = core.execute(current, decoded, operands, 0);
        return results.branch_taken;
    }

    template <auto HARTS>
    class FetchTest
    {
        private:
            Core<HARTS, IMEM_LENGTH, DMEM_LENGTH, MMIO_LENGTH, IMEM_ORIGIN, DMEM_ORIGIN, MMIO_ORIGIN, IMEM_TCM_SIZE, memory_norep, HARTS == 2 ? Extension::None : Extension::M> core = {.external_fetch = ExternalFetch};

            uint_t[16] external_imem;

            inline optional<uint_t> ExternalFetch(index_t<HARTS> hid, imem_addr_t addr)
            {
                optional<uint_t> result;

                result.is_valid = 0 != mux(cast<uint3>(addr), 0, 1, 0, 1, 0, 0, 1, 1);
                const auto value = external_imem[addr - IMEM_TCM_SIZE];

                if (!result.is_valid)
                {
                    async_exec([addr, hid, value]
                        {
                            core.external_fetch_result(hid, addr, value);
                        });
                }
                else
                {
                    result.value = value;
                }

                return result;
            }

        public:
            template <auto INSTR, auto MICRO_OPS>
            void run(uint32 load_at, uint_t[INSTR] instr, uint_t[MICRO_OPS] expected)
            {
                print("    harts: {HARTS} at: {load_at}\n");

                uint_t[HARTS] pc;

                static for(const auto i : HARTS)
                {
                    pc[i] = load_at + i * bytesizeof uint_t;
                }

                core.initialize(pc);

                static for(const auto i : bytesizeof instr / bytesizeof instr[0])
                {
                    const imem_addr_t addr = checked_cast((load_at >> 2) + i);

                    if (addr < IMEM_TCM_SIZE)
                    {
                        core.imem_write(addr, instr[i]);
                    }
                    else
                    {
                        core.imem_write(addr, uninit_uint);
                        external_imem[i] = instr[addr - IMEM_TCM_SIZE];
                    }
                }

                pipelined_for(MICRO_OPS * HARTS * 10, [expected](index_t<MICRO_OPS * HARTS * 10> tid)
                {
                    static index_t<MICRO_OPS> i;

                    if (tid == 0) i = 0;

                    auto current = core.fetch();

                    atomic
                    {
                        if (current.hid == 0 && current.instr.is_valid && i < MICRO_OPS)
                        {
                            if (cast<uint32>(current.instr.value) != cast<uint32>(expected[i]))
                            {
                                print("Hart {current.hid} PC {current.pc} expected to fetch {expected[i]} got {current.instr.value}\n");
                                assert(false);
                            }
                            i++;
                        }
                    }
                });
            }
    }

    FetchTest<1> fetchTest1;
    FetchTest<2> fetchTest2;
    FetchTest<4> fetchTest4;

    void FetchMicroOpTests()
    {
        int_t imm = uninit_int;
        register_index_t rd = 3;
        register_index_t rs1 = 4;
        register_index_t rs2 = 5;

        auto LB   = concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0000011));
        auto LH   = concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0000011));
        auto LW   = concat(slice<11,0>(imm), rs1, cast<Funct3>(0b_010), rd, cast<uint7>(0b_0000011));
        auto ADD  = concat(cast<uint7>(0b_0000000), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011));
        auto SUB  = concat(cast<uint7>(0b_0100000), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011));
        auto MUL  = concat(cast<uint7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_000), rd, cast<uint7>(0b_0110011));
        auto MULH = concat(cast<uint7>(0b_0000001), rs2, rs1, cast<Funct3>(0b_001), rd, cast<uint7>(0b_0110011));

        uint_t[7] instr = {LB, ADD, SUB, LH, LW, SUB, ADD};
        uint_t[10] instrM = {LB, ADD, SUB, LH, LW, MUL, MULH, SUB, MUL, ADD};

        print("Fetcher tests\n");

        uint_t[25] expected1 = {
            LB, LB,
            ADD,
            SUB,
            LH, LH,
            LW, LW,
            MUL, MUL, MUL, MUL, MUL,
            MULH, MULH, MULH, MULH, MULH,
            SUB,
            MUL, MUL, MUL, MUL, MUL,
            ADD};

        fetchTest1.run(0, instrM, expected1);
        fetchTest1.run(EXTERNAL_IMEM_ADDR, instrM, expected1);

        uint_t[7] expected2 = {
            LB,
            ADD,
            SUB,
            LH,
            LW,
            SUB,
            ADD};

        fetchTest2.run(0, instr, expected2);
        fetchTest2.run(EXTERNAL_IMEM_ADDR, instr, expected2);

        uint_t[10] expected4 = {
            LB,
            ADD,
            SUB,
            LH,
            LW,
            MUL,
            MULH,
            SUB,
            MUL,
            ADD};

        fetchTest4.run(0, instrM, expected4);
        fetchTest4.run(EXTERNAL_IMEM_ADDR, instrM, expected4);
    }

    template <auto HARTS, auto DMEM_LENGTH, auto EXTENSION, auto CONFIG, bool MMIO, template <typename, auto> typename Memory = memory_norep, auto BTB_SIZE = 1024>
    class ProgramTest
    {
        private:
            const bool FAST = (CONFIG & Optimize::Fmax) == Optimize::Fmax;

            Core<HARTS, IMEM_LENGTH, DMEM_LENGTH, MMIO_LENGTH, IMEM_ORIGIN, DMEM_ORIGIN, MMIO ? MMIO_ORIGIN : 0, FAST ? IMEM_LENGTH : IMEM_TCM_SIZE, Memory, EXTENSION, CONFIG, Base::RV32I, BTB_SIZE> core = {
                .system_trap    = SystemTrap,
                .trace          = PrintTrace,
                .mmio_load      = MmioLoad,
                .mmio_store     = MmioStore,
                .external_fetch = ExternalFetch,
                .custom_decode  = CustomDecode,
                .custom_execute = CustomExecute
            };

            memory<uint_t, IMEM_LENGTH - IMEM_TCM_SIZE> external_imem;

            const uint_t breakpoint = 0x1000;

            const auto TIMEOUT = 0x10000;
            count_t<TIMEOUT> timeout;

            bool enable_trace;

            inline Format CustomDecode(RVG major_opcode)
            {
                assert(major_opcode == RVG::custom_0
                    || major_opcode == RVG::custom_1
                    || major_opcode == RVG::custom_2
                    || major_opcode == RVG::custom_3);

                return mux(cast<uint2>(major_opcode >> 3), Format::R, Format::I, Format::U, Format::S);
            }

            inline int_t CustomExecute(index_t<HARTS> hid, RVG major_opcode, Funct3 minor_opcode, int_t op1, int_t op2, int_t imm, Funct7 funct7)
            {
                static int_t[16] store;
                static int_t[HARTS] state;

                int_t result = uninit_int;

                if (major_opcode == RVG::custom_0)
                {
                    // Verify that funct7 is passed to custom_execute handler.
                    // The `minor_opcode * 2 + 3` is just an arbitrary formula used by test programs.
                    assert(funct7 == minor_opcode * 2 + 3);

                    switch(minor_opcode)
                    {
                        case 0: state[hid] = op1;
                                break;
                        case 1: state[hid] += op2;
                                break;
                        case 2: result = state[hid];
                                break;
                        case 3: result = op1 + (op2 << (hid + 1));
                                break;
                    }
                }
                else if (major_opcode == RVG::custom_2)
                {
                    //    31                                                   12 11                 7 6       0
                    //   +-------------------------------------------------------+--------------------+---------+
                    //   |                    imm[31:12]                         |         rd         | opcode  | U-type
                    //   +-------------------------------------------------------+--------------------+---------+
                    //
                    result = store[imm >> 12];
                }
                else if (major_opcode == RVG::custom_3)
                {
                    //    31                 25 24         20 19    15 14      12 11                 7 6       0
                    //   +---------------------+-------------+--------+----------+--------------------+---------+
                    //   |      imm[11:5]      |     rs2     |  rs1   |  funct3  |      imm[4:0]      | opcode  | S-type
                    //   +---------------------+-------------+--------+----------+--------------------+---------+
                    //
                    store[op1 + imm] = op2;
                }

                return result;
            }

            inline optional<imem_addr_t> SystemTrap(index_t<HARTS> hid, Trap trap, imem_addr_t pc)
            {
                sleep(16);

                optional<imem_addr_t> result = {};

                atomic
                {
                    if (handle_exception[hid].is_valid && handle_exception[hid].value == trap)
                    {
                        print("    hart {hid} transferring to ");
                        print("{trap}");
                        print(" handler at {handler[hid]}\n");
                        result = make_optional(true, cast<imem_addr_t>(handler[hid] >> 2));
                    }
                    else
                    {
                        results++;

                        if (trap != exception[hid])
                        {
                            print("    hart {hid} triggered an unexpected trap ");
                            print("{trap}");
                            print(" at {pc << 2}\n");
                            failed = true;
                        }
                        else
                        {
                            const auto a0 = core.register_get(hid, ABI::a0);

                            if (trap == Trap::EBREAK)
                            {
                                failed ||= (a0 != expected[hid] || pc != (breakpoint >> 2));

                                if (a0 != expected[hid])
                                {
                                    print("    hart {hid} break at {pc << 2} expected {expected[hid]} got {a0}\n");
                                }
                                else
                                {
                                    print("    hart {hid} got expected result {expected[hid]}\n");
                                }
                            }
                            else
                            {
                                print("    hart {hid} got expected exception ");
                                print("{exception[hid]}");
                                print("\n");
                            }
                        }
                    }
                }

                return result;
            }

            inline void PrintTrace(index_t<HARTS> hid, imem_addr_t pc, uint_t instr, Decoded decoded, optional<int_t> value)
            {
                if (enable_trace)
                {
                    print_trace(hid, pc, instr, decoded, value);
                }
            }

            inline optional<int_t> MmioLoad(index_t<HARTS> hid, uint_t addr, MemorySize size, bool sign_extend)
            {
                optional<int_t> result = {};

                result.is_valid = 0 != mux(cast<uint3>(addr), 0, 1, 0, 1, 0, 0, 1, 1);
                const int_t value = checked_cast(addr - MMIO_ORIGIN);

                if (!result.is_valid)
                {
                    async_exec([hid, addr, value]
                        {
                            const auto delay = cast<uint4>(addr);

                            sleep(delay);

                            core.mmio_load_result(hid, value);
                        });
                }
                else
                {
                    result.value = value;
                }

                return result;
            }

            inline bool MmioStore(index_t<HARTS> hid, uint_t addr, MemorySize size, int_t value)
            {
                if (addr == 0x10000000)
                {
                    start_time = value;
                }
                if (addr == 0x10000004)
                {
                    end_time = value;
                }
                if (addr == 0x10000008)
                {
                    start_instret = value;
                }
                if (addr == 0x1000000c)
                {
                    end_instret = value;
                }

                return true;
            }

            inline auto ExternalFetch(index_t<HARTS> hid, imem_addr_t addr)
            {
                optional<uint_t> result = {};

                result.is_valid = 0 != mux(cast<uint3>(addr), 0, 1, 0, 1, 0, 0, 1, 1);
                const auto instr = external_imem[addr - IMEM_TCM_SIZE];

                if (!result.is_valid)
                {
                    async_exec([instr, addr, hid]
                        {
                            const auto delay = cast<uint4>(addr);

                            sleep(delay);

                            core.external_fetch_result(hid, addr, instr);
                        });
                }
                else
                {
                    result.value = instr;
                }

                return result;
            }

        public:
            void run(unit::tag_t tag, bool trace)
            {
                enable_trace = trace;
                results = 0;
                failed = false;
                start_time = 0;
                end_time = 0;

                uint_t stack_start = DMEM_ORIGIN + DMEM_LENGTH - 0x10;

                static for(const auto hid : HARTS)
                {
                    core.register_set(hid, ABI::sp, cast<int_t>(stack_start));

                    static if ((CONFIG & Option::HartsShareDMEM) == Option::HartsShareDMEM)
                    {
                        // Set a different stack pointer for each hart.
                        stack_start -= STACK_SIZE;
                    }
                }

                core.initialize(pc);

                timeout = 0;

                pipelined_do([](index_t<64> tid)
                {
                    if (results < HARTS)
                    {
                        core.pipeline();
                    }

                    timeout++;

                    return results < HARTS && timeout < TIMEOUT;
                });

                if (results != HARTS)
                {
                    print("    expected {HARTS} results but got only {results}\n");
                    unit::assert(tag, false);
                }

                static for (const auto hid : HARTS)
                {
                    handle_exception[hid].is_valid = false;
                }

                unit::assert(tag, !failed);
            }

            void dmem_write(index_t<HARTS> hid, dmem_addr_t addr, int_t value)
            {
                core.dmem_write_aligned(hid, addr, value);
            }

            int_t dmem_read(index_t<HARTS> hid, dmem_addr_t addr)
            {
                return core.dmem_read_aligned(hid, addr);
            }

            template <auto N>
            void loadData(index_t<HARTS> hid, uint_t[N] data)
            {
                pipelined_for(N, [hid, data](index_t<N> i)
                {
                    dmem_write(hid, i * bytesizeof(data[i]), cast<int_t>(data[i]));
                });
            }

            void disassemble(index_t<IMEM_LENGTH * 4> addr, uint_t binary)
            {
                auto decoded = decode_instr<EXTENSION, CONFIG>(cast<Instr>(binary), CustomDecode);

                print("0x{addr:x8}:\t{binary:x8}\t{disasm(binary, decoded)}\n");
            }

            template <auto N>
            void loadHandler(bool disasm, index_t<HARTS> hid, uint_t[N] text, uint32 load_at, uint32 start, Trap exception)
            {
                loadProgram(disasm, hid, text, load_at, start, 0xbaddeed, exception);

                handle_exception[hid] = make_optional(true, exception);
                handler[hid] = load_at + start;
            }

            template <auto N>
            void loadProgram(bool disasm, index_t<HARTS> hid, uint_t[N] text, uint32 load_at, uint32 start, int_t expected_result, Trap expected_exception)
            {
                pipelined_for(N, [disasm, hid, text, load_at](index_t<N> i)
                {
                    const imem_addr_t addr = checked_cast((load_at >> 2) + i);

                    if (disasm)
                    {
                        disassemble(addr << 2, text[i]);
                    }

                    if (addr < (FAST ? IMEM_LENGTH : IMEM_TCM_SIZE))
                    {
                        core.imem_write(addr, text[i]);
                    }
                    else
                    {
                        core.imem_write(addr, uninit_uint);
                        external_imem[addr - IMEM_TCM_SIZE] = text[i];
                    }
                });

                const auto EBREAK = 0b_100000000000001110011;
                const auto J0     = 0b_000000000000001101111;
                const auto LI     = 0b_000000000010100010011; // li a0,0

                pc[hid] = load_at + start;
                expected[hid] = expected_result;
                exception[hid] = expected_exception;

                core.imem_write(breakpoint >> 2, EBREAK);
                core.imem_write((breakpoint >> 2) + 1, LI);
                core.imem_write((breakpoint >> 2) + 2, J0);
                if (expected_exception == Trap::EBREAK || expected_exception == Trap::ECALL)
                {
                    core.register_set(hid, ABI::ra, breakpoint);
                }
                else
                {
                    core.register_set(hid, ABI::ra, breakpoint + 4);
                }
                core.register_set(hid, ABI::a0, static_cast(uninit_int));
            }

            inline auto timeElapsed()
            {
                return end_time - start_time;
            }

            inline auto instret()
            {
                return end_instret - start_instret;
            }

        private:
            uint_t[HARTS]       pc;
            uint_t[HARTS]       handler;
            int_t[HARTS]        expected;
            Trap[HARTS]         exception;
            optional<Trap>[HARTS] handle_exception;

            count_t<HARTS>      results;
            bool                failed;
            uint_t              start_time;
            uint_t              end_time;
            uint_t              start_instret;
            uint_t              end_instret;
    }

    template <typename T, auto N>
    using memory_quad_port = [[memory, quad_port]] T[N];

    ProgramTest<1, SHARED_DMEM_LENGTH, Extension::M,    Optimize::Area, true>                                                 programTest1;
    ProgramTest<2, DMEM_LENGTH,        Extension::M,    Optimize::Area | Option::NoExceptionForCompressedEncoding, true>      programTest2;
    ProgramTest<4, DMEM_LENGTH,        Extension::M,    Optimize::Area, true>                                                 programTest4;
    ProgramTest<4, DMEM_LENGTH,        Extension::None, Optimize::Fmax, true>                                                 programTest4Fmax;
    ProgramTest<1, SHARED_DMEM_LENGTH, Extension::None, Optimize::Area, false>                                                noMMIOTest1;
    ProgramTest<2, DMEM_LENGTH,        Extension::None, Optimize::Fmax, false>                                                noMMIOTest2;
    ProgramTest<2, SHARED_DMEM_LENGTH, Extension::None, Optimize::Area | Option::HartsShareDMEM, false, memory_quad_port, 16> quadPortShared2;

    void ProgramTests()
    {
        /*
            int Do(int x)
            {
                int result = 0;

                for (int i = 0; i < x; ++i)
                {
                    for (int j = 0; j < i; ++j)
                    {
                        result += j;
                    }
                }
            }

            int main()
            {
                return Do(10);
            }
        */

        // riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -L %LibDir% -Wl,--script=processor/risc_v/util/ld-script -nostartfiles -o main main.c
        // riscv64-unknown-elf-objcopy -j .text -O binary main main.imem
        // riscv64-unknown-elf-objcopy -R .text -O binary main main.dmem

        const uint_t[42] debug = {
            /* 00000000 <Do>: */
            /*    0: */  0xfd010113, //                addi    sp,sp,-48
            /*    4: */  0x02812623, //                sw      s0,44(sp)
            /*    8: */  0x03010413, //                addi    s0,sp,48
            /*    c: */  0xfca42e23, //                sw      a0,-36(s0)
            /*   10: */  0xfe042623, //                sw      zero,-20(s0)
            /*   14: */  0xfe042423, //                sw      zero,-24(s0)
            /*   18: */  0x0400006f, //                j       58 <.L2>

            /* 0000001c <.L5>: */
            /*   1c: */  0xfe042223, //                sw      zero,-28(s0)
            /*   20: */  0x0200006f, //                j       40 <.L3>

            /* 00000024 <.L4>: */
            /*   24: */  0xfec42703, //                lw      a4,-20(s0)
            /*   28: */  0xfe442783, //                lw      a5,-28(s0)
            /*   2c: */  0x00f707b3, //                add     a5,a4,a5
            /*   30: */  0xfef42623, //                sw      a5,-20(s0)
            /*   34: */  0xfe442783, //                lw      a5,-28(s0)
            /*   38: */  0x00178793, //                addi    a5,a5,1
            /*   3c: */  0xfef42223, //                sw      a5,-28(s0)

            /* 00000040 <.L3>: */
            /*   40: */  0xfe442703, //                lw      a4,-28(s0)
            /*   44: */  0xfe842783, //                lw      a5,-24(s0)
            /*   48: */  0xfcf74ee3, //                blt     a4,a5,24 <.L4>
            /*   4c: */  0xfe842783, //                lw      a5,-24(s0)
            /*   50: */  0x00178793, //                addi    a5,a5,1
            /*   54: */  0xfef42423, //                sw      a5,-24(s0)

            /* 00000058 <.L2>: */
            /*   58: */  0xfe842703, //                lw      a4,-24(s0)
            /*   5c: */  0xfdc42783, //                lw      a5,-36(s0)
            /*   60: */  0xfaf74ee3, //                blt     a4,a5,1c <.L5>
            /*   64: */  0xfec42783, //                lw      a5,-20(s0)
            /*   68: */  0x00078513, //                mv      a0,a5
            /*   6c: */  0x02c12403, //                lw      s0,44(sp)
            /*   70: */  0x03010113, //                addi    sp,sp,48
            /*   74: */  0x00008067, //                ret

            /* 00000078 <main>: */
            /*   78: */  0xff010113, //                addi    sp,sp,-16
            /*   7c: */  0x00112623, //                sw      ra,12(sp)
            /*   80: */  0x00812423, //                sw      s0,8(sp)
            /*   84: */  0x01010413, //                addi    s0,sp,16
            /*   88: */  0x00a00513, //                li      a0,10
            /*   8c: */  0xf75ff0ef, //                jal     ra,0 <Do>
            /*   90: */  0x00050793, //                mv      a5,a0
            /*   94: */  0x00078513, //                mv      a0,a5
            /*   98: */  0x00c12083, //                lw      ra,12(sp)
            /*   9c: */  0x00812403, //                lw      s0,8(sp)
            /*   a0: */  0x01010113, //                addi    sp,sp,16
            /*   a4: */  0x00008067  //                ret
        };

        unit::test<1, 1>([debug](unit::tag_t tag)
        {
            print("Running debug\n");
            programTest1.loadProgram(true, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<2, 1>([debug](unit::tag_t tag)
        {
            print("Running debug from external IMEM\n");
            programTest1.loadProgram(false, 0, debug, EXTERNAL_IMEM_ADDR + 0x100, 0x78, 120, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<3, 1>([debug](unit::tag_t tag)
        {
            print("Running debug w/o MMIO\n");
            noMMIOTest1.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            noMMIOTest1.run(tag, false);
        });

        const uint_t[42] optimized = {
                                     //          main:
            /*   00: */  0x00000613, //                li      a2,0
            /*   04: */  0x00060693, //                mv      a3,a2
            /*   08: */  0x00a00593, //                li      a1,10
            /*   0c: */  0x00160613, //                addi    a2,a2,1
            /*   10: */  0x00000513, //                li      a0,0
            /*   14: */  0x02b60263, //                beq     a2,a1,.L17
                                     //          .L15:
            /*   18: */  0x00000793, //                li      a5,0
                                     //          .L14:
            /*   1c: */  0x00078713, //                mv      a4,a5
            /*   20: */  0x00f50533, //                add     a0,a0,a5
            /*   24: */  0x00178793, //                addi    a5,a5,1
            /*   28: */  0xfee69ae3, //                bne     a3,a4,.L14
            /*   2c: */  0x00060693, //                mv      a3,a2
            /*   30: */  0x00160613, //                addi    a2,a2,1
            /*   34: */  0xfeb612e3, //                bne     a2,a1,.L15
                                     //          .L17:
            /*   38: */  0x00008067, //                ret
                                     //            Do:
            /*   3c: */  0x00050593, //                mv      a1,a0
            /*   40: */  0x00000613, //                li      a2,0
            /*   44: */  0x00000513, //                li      a0,0
            /*   48: */  0x02b05a63, //                ble     a1,zero,.L10
            /*   4c: */  0x00060693, //                mv      a3,a2
            /*   50: */  0x00160613, //                addi    a2,a2,1
            /*   54: */  0x02c58263, //                beq     a1,a2,.L11
                                     //           .L6:
            /*   58: */  0x00000793, //                li      a5,0
                                     //           .L4:
            /*   5c: */  0x00078713, //                mv      a4,a5
            /*   60: */  0x00f50533, //                add     a0,a0,a5
            /*   64: */  0x00178793, //                addi    a5,a5,1
            /*   68: */  0xfee69ae3, //                bne     a3,a4,.L4
            /*   6c: */  0x00060693, //                mv      a3,a2
            /*   70: */  0x00160613, //                addi    a2,a2,1
            /*   74: */  0xfec592e3, //                bne     a1,a2,.L6
                                     //          .L11:
            /*   78: */  0x00008067, //                ret
                                     //          .L10:
            /*   7c: */  0x00008067  //                ret
        };

        unit::test<4, 1>([optimized](unit::tag_t tag)
        {
            print("Running optimized\n");
            programTest1.loadProgram(false, 0, optimized, 0x100, 0x00, 120, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<5, 2>([debug, optimized](unit::tag_t tag)
        {
            print("Running debug and optimized on 2 harts\n");
            programTest2.loadProgram(false, 0, debug, 0, 0x78, 120, Trap::EBREAK);
            programTest2.loadProgram(false, 1, optimized, 0x100, 0x0, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<6, 2>([debug, optimized](unit::tag_t tag)
        {
            print("Running debug from external IMEM and optimized on 2 harts\n");
            programTest2.loadProgram(false, 0, debug, EXTERNAL_IMEM_ADDR, 0x78, 120, Trap::EBREAK);
            programTest2.loadProgram(false, 1, optimized, 0x100, 0x0, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<7, 2>([debug, optimized](unit::tag_t tag)
        {
            print("Running debug and optimized from external IMEM on 2 harts\n");
            programTest2.loadProgram(false, 0, debug, EXTERNAL_IMEM_ADDR, 0x78, 120, Trap::EBREAK);
            programTest2.loadProgram(false, 1, optimized, EXTERNAL_IMEM_ADDR + 0x100, 0x0, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<8, 2>([debug, optimized](unit::tag_t tag)
        {
            print("Running debug and optimized on 2 harts w/o MMIO\n");
            noMMIOTest2.loadProgram(false, 0, debug, 0, 0x78, 120, Trap::EBREAK);
            noMMIOTest2.loadProgram(false, 1, optimized, 0x100, 0x0, 120, Trap::EBREAK);
            noMMIOTest2.run(tag, false);
        });

        /*
            int Do(int x)
            {
                int result = 0;

                for (int i = 0; i < x; ++i)
                {
                    for (int j = 0; j < i; ++j)
                    {
                        switch (i)
                        {
                            case 0: result += j;
                                break;
                            case 1: result -= j;
                                break;
                            case 2: result |= j;
                                break;
                            case 3: result ^= j;
                                break;
                            case 4: result &= j;
                                break;
                            case 5: result >>= j;
                                break;
                            case 6: result <<= j;
                                break;
                            default:
                                if (i <= 8)
                                {
                                    result += 123;
                                }
                                else
                                {
                                    result -= 321;
                                }
                                break;
                        }

                        result += j;
                    }
                }
                return result;
            }

            int main()
            {
                return Do(10);
            }
        */

        const uint_t[(0x184 >> 2) + 1] debug2 = {
            /* 00000000 */ 0xfd010113, 0x02812623, 0x03010413, 0xfca42e23,
            /* 00000010 */ 0xfe042623, 0xfe042423, 0x1200006f, 0xfe042223,
            /* 00000020 */ 0x1000006f, 0xfe842703, 0x00600793, 0x0ae7e663,
            /* 00000030 */ 0xfe842783, 0x00279713, 0x000107b7, 0x00078793,
            /* 00000040 */ 0x00f707b3, 0x0007a783, 0x00078067, 0xfec42703,
            /* 00000050 */ 0xfe442783, 0x00f707b3, 0xfef42623, 0x0a80006f,
            /* 00000060 */ 0xfec42703, 0xfe442783, 0x40f707b3, 0xfef42623,
            /* 00000070 */ 0x0940006f, 0xfec42703, 0xfe442783, 0x00f767b3,
            /* 00000080 */ 0xfef42623, 0x0800006f, 0xfec42703, 0xfe442783,
            /* 00000090 */ 0x00f747b3, 0xfef42623, 0x06c0006f, 0xfec42703,
            /* 000000a0 */ 0xfe442783, 0x00f777b3, 0xfef42623, 0x0580006f,
            /* 000000b0 */ 0xfe442783, 0xfec42703, 0x40f757b3, 0xfef42623,
            /* 000000c0 */ 0x0440006f, 0xfe442783, 0xfec42703, 0x00f717b3,
            /* 000000d0 */ 0xfef42623, 0x0300006f, 0xfe842703, 0x00800793,
            /* 000000e0 */ 0x00e7ca63, 0xfec42783, 0x07b78793, 0xfef42623,
            /* 000000f0 */ 0x0100006f, 0xfec42783, 0xebf78793, 0xfef42623,
            /* 00000100 */ 0x00000013, 0xfec42703, 0xfe442783, 0x00f707b3,
            /* 00000110 */ 0xfef42623, 0xfe442783, 0x00178793, 0xfef42223,
            /* 00000120 */ 0xfe442703, 0xfe842783, 0xeef74ee3, 0xfe842783,
            /* 00000130 */ 0x00178793, 0xfef42423, 0xfe842703, 0xfdc42783,
            /* 00000140 */ 0xecf74ee3, 0xfec42783, 0x00078513, 0x02c12403,
            /* 00000150 */ 0x03010113, 0x00008067, 0xff010113, 0x00112623,
            /* 00000160 */ 0x00812423, 0x01010413, 0x00a00513, 0xe95ff0ef,
            /* 00000170 */ 0x00050793, 0x00078513, 0x00c12083, 0x00812403,
            /* 00000180 */ 0x01010113, 0x00008067
        };

        const uint_t[7] data = {
            /* 00000000 */ 0x0000004c, 0x00000060, 0x00000074, 0x00000088,
            /* 00000010 */ 0x0000009c, 0x000000b0, 0x000000c4
        };

        unit::test<9, 1>([debug2, data](unit::tag_t tag)
        {
            print("Running debug 2\n");
            programTest1.loadData(0, data);
            programTest1.loadProgram(false, 0, debug2, 0x0, 0x158, 156358, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<10, 2>([data, debug2, optimized](unit::tag_t tag)
        {
            print("Running optimized and debug2 on 2 harts\n");
            programTest2.loadProgram(false, 0, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest2.loadData(1, data);
            programTest2.loadProgram(false, 1, debug2, 0, 0x158, 156358, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<11, 2>([data, debug, debug2](unit::tag_t tag)
        {
            print("Running debug2 and debug on 2 harts\n");
            programTest2.loadData(0, data);
            programTest2.loadProgram(false, 0, debug2, 0, 0x158, 156358, Trap::EBREAK);
            programTest2.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        const uint_t[42] optimized2 = {
            /* 00000000 */ 0x00050893, 0x00000593, 0x00000513, 0x07105c63,
            /* 00000010 */ 0x00010e37, 0x00158693, 0x00600813, 0x00800313,
            /* 00000020 */ 0x000e0e13, 0x06d88063, 0x00269613, 0x00000793,
            /* 00000030 */ 0x01c60633, 0x04d86a63, 0x00062703, 0x00070067,
            /* 00000040 */ 0x00f51733, 0x00f70533, 0x00178713, 0x02f58663,
            /* 00000050 */ 0x00070793, 0xfe1ff06f, 0x40f55733, 0xfe9ff06f,
            /* 00000060 */ 0x00f57733, 0xfe1ff06f, 0x00f54733, 0xfd9ff06f,
            /* 00000070 */ 0x00f56733, 0xfd1ff06f, 0x00068593, 0x00158693,
            /* 00000080 */ 0xfad894e3, 0x00008067, 0xebf50713, 0xfad34ce3,
            /* 00000090 */ 0x07b50713, 0xfb1ff06f, 0x00a00513, 0xf65ff06f
        };

        const uint_t[7] data2 = {
            /* 00000000 */ 0x00000088, 0x00000078, 0x00000070, 0x00000068,
            /* 00000010 */ 0x00000060, 0x00000058, 0x00000040
        };

        unit::test<12, 1>([data2, optimized2](unit::tag_t tag)
        {
            print("Running optimized 2\n");
            programTest1.loadData(0, data2);
            programTest1.loadProgram(false, 0, optimized2, 0x0, 0x98, 156358, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<13, 2>([data2, optimized2, optimized](unit::tag_t tag)
        {
            print("Running optimized2 and optimized on 2 harts\n");
            programTest2.loadData(0, data2);
            programTest2.loadProgram(false, 0, optimized2, 0, 0x98, 156358, Trap::EBREAK);
            programTest2.loadProgram(false, 1, optimized, 0x100, 0x0, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<14, 2>([debug, data2, optimized2](unit::tag_t tag)
        {
            print("Running debug and optimized2 on 2 harts\n");
            programTest2.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest2.loadData(1, data2);
            programTest2.loadProgram(false, 1, optimized2, 0, 0x98, 156358, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<15, 4>([debug, optimized, data2, optimized2](unit::tag_t tag)
        {
            print("Running debug, optimized2, optimized, optimized on 4 harts\n");
            programTest4.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4.loadData(1, data2);
            programTest4.loadProgram(false, 1, optimized2, 0, 0x98, 156358, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<16, 4>([debug, debug2, optimized, data](unit::tag_t tag)
        {
            print("Running optimized, debug, optimized, debug2 on 4 harts\n");
            programTest4.loadProgram(false, 0, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, 0x400, 0x0, 120, Trap::EBREAK);
            programTest4.loadData(3, data);
            programTest4.loadProgram(false, 3, debug2, 0, 0x158, 156358, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<17, 4>([debug, debug2, optimized, data](unit::tag_t tag)
        {
            print("Running optimized, debug, optimized from external IMEM, debug2 on 4 harts\n");
            programTest4.loadProgram(false, 0, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, EXTERNAL_IMEM_ADDR + 0x400, 0x0, 120, Trap::EBREAK);
            programTest4.loadData(3, data);
            programTest4.loadProgram(false, 3, debug2, 0, 0x158, 156358, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<18, 4>([debug, debug2, optimized, data](unit::tag_t tag)
        {
            print("Running optimized, debug, optimized from external IMEM, debug2 from IMEM on 4 harts\n");
            programTest4.loadProgram(false, 0, optimized, EXTERNAL_IMEM_ADDR + 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, EXTERNAL_IMEM_ADDR + 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, EXTERNAL_IMEM_ADDR + 0x400, 0x0, 120, Trap::EBREAK);
            programTest4.loadData(3, data);
            programTest4.loadProgram(false, 3, debug2, 0, 0x158, 156358, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<19, 4>([debug, optimized, optimized2, data2](unit::tag_t tag)
        {
            print("Running debug, optimized2, optimized, optimized on 4 harts fast\n");
            programTest4Fmax.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4Fmax.loadData(1, data2);
            programTest4Fmax.loadProgram(false, 1, optimized2, 0, 0x98, 156358, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.run(tag, false);
        });

        unit::test<20, 4>([debug, debug2, optimized, data](unit::tag_t tag)
        {
            print("Running optimized, debug, optimized, debug2 on 4 harts fast\n");
            programTest4Fmax.loadProgram(false, 0, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 2, optimized, 0x400, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.loadData(3, data);
            programTest4Fmax.loadProgram(false, 3, debug2, 0, 0x158, 156358, Trap::EBREAK);
            programTest4Fmax.run(tag, false);
        });

        /*
            unsigned g;

            int main()
            {
                volatile unsigned* io_start = (unsigned*)0x10000000;
                volatile unsigned* io_end = (unsigned*)0x10000004;
                volatile unsigned* x = &g;

                asm volatile ("csrr %0, time" : "=r"(*io_start));
                *x += 1;
                asm volatile ("csrr %0, time" : "=r"(*io_end));

                return 0;
            }

        00000000 <main>:
               0:   c01027f3                rdtime  a5
               4:   10000737                lui     a4,0x10000
               8:   00f72023                sw      a5,0(a4) # 10000000 <__global_pointer$+0xffef800>
               c:   000106b7                lui     a3,0x10
              10:   0006a783                lw      a5,0(a3) # 10000 <__DATA_BEGIN__>
              14:   00178793                addi    a5,a5,1
              18:   00f6a023                sw      a5,0(a3)
              1c:   c01027f3                rdtime  a5
              20:   00000513                li      a0,0
              24:   00f72223                sw      a5,4(a4)
              28:   00008067                ret
         */

        const uint_t[42] rdtime = {
            /* 00000000 */ 0xc01027f3, 0x10000737, 0x00f72023, 0x000106b7,
            /* 00000010 */ 0x0006a783, 0x00178793, 0x00f6a023, 0xc01027f3,
            /* 00000020 */ 0x00000513, 0x00f72223, 0x00008067
        };

        // Note that there is a subtle issues with tests of rdtime instruction.
        // The instruction returns number of cycles and thus is sensitive to
        // branch mis-prediction. The code between rdtime instructions doesn't
        // have any branches, however the core is reused through many tests,
        // and it is possible that branch prediction buffer is "polluted" and
        // it predicts a jump from non branch instruction. This is not a
        // correctness problem because the prediction will be proven incorrect,
        // but it would affect the rdtime result. To avoid this problem we load
        // the program at address that hasn't been used by earlier programs so
        // that the branch prediction buffer will not have any spurious entries
        // for addresses in that range.
        unit::test<21, 1>([rdtime](unit::tag_t tag)
        {
            print("Running MMIO/rdtime\n");
            programTest1.loadProgram(false, 0, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest1.run(tag, true);
            unit::assert(tag, programTest1.timeElapsed() == 8 || opt::stall != 0);
        });

        // BTB poisoning must be only temporary, i.e. an incorrect branch
        // prediction based on some leftover values in BTB from running
        // previous program must not persist and must self-cure.
        unit::test<22, 1>([rdtime, debug](unit::tag_t tag)
        {
            print("Poisoning BTB with debug\n");
            programTest1.loadProgram(false, 0, debug, 0x500, 0x78, 120, Trap::EBREAK);
            programTest1.run(tag, false);

            programTest1.loadProgram(false, 0, debug, 0x500, 0x78, 120, Trap::EBREAK);
            programTest1.run(tag, false);

            print("Running rdtime with poisoned BTB\n");
            programTest1.loadProgram(false, 0, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest1.run(tag, false);
            unit::assert(tag, programTest1.timeElapsed() != 8);

            programTest1.loadProgram(false, 0, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest1.run(tag, false);

            print("Running rdtime after BTB poisoning cured\n");
            programTest1.loadProgram(false, 0, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest1.run(tag, false);
            unit::assert(tag, programTest1.timeElapsed() == 8 || opt::stall != 0);

            programTest1.loadProgram(false, 0, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest1.run(tag, false);
            unit::assert(tag, programTest1.timeElapsed() == 8 || opt::stall != 0);
        });

        unit::test<23, 4>([rdtime, debug, optimized](unit::tag_t tag)
        {
            print("Running debug, MMIO/rdtime, optimized, optimized on 4 harts\n");
            programTest4.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.run(tag, false);
            unit::assert(tag, programTest4.timeElapsed() == 28 || opt::stall != 0);
        });

        unit::test<24, 4>([rdtime, debug, optimized](unit::tag_t tag)
        {
            print("Running debug from external IMEM, MMIO/rdtime, optimized, optimized on 4 harts\n");
            programTest4.loadProgram(false, 0, debug, EXTERNAL_IMEM_ADDR + 0x100, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.run(tag, false);
            unit::assert(tag, programTest4.timeElapsed() == 28 || opt::stall != 0);
        });

        unit::test<25, 4>([rdtime, debug, optimized](unit::tag_t tag)
        {
            print("Running debug, MMIO/rdtime, optimized, optimized on 4 harts fast\n");
            programTest4Fmax.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 1, rdtime, 0x500, 0x0, 0, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.run(tag, false);
            unit::assert(tag, programTest4.timeElapsed() == 28 || opt::stall != 0);
        });

        /*
            unsigned g;

            int main()
            {
                volatile unsigned* io_instret_start = (unsigned*)0x10000008;
                volatile unsigned* io_instret_end = (unsigned*)0x1000000c;
                volatile unsigned* x = &g;

                *x = 12;
                *x += 1;

                asm volatile ("csrr %0, instret" : "=r"(*io_instret_start));

                if (*x != 13)
                {
                    *x += 1;
                }

                asm volatile ("csrr %0, instret" : "=r"(*io_instret_end));

                return 0;
            }

        00000000 <main>:
               0:   000107b7                lui     a5,0x10
               4:   00c00713                li      a4,12
               8:   00e7a023                sw      a4,0(a5) # 10000 <__DATA_BEGIN__>
               c:   0007a703                lw      a4,0(a5)
              10:   00170713                addi    a4,a4,1
              14:   00e7a023                sw      a4,0(a5)
              18:   c02026f3                rdinstret       a3
              1c:   10000737                lui     a4,0x10000
              20:   00d72423                sw      a3,8(a4) # 10000008 <__global_pointer$+0xffef808>
              24:   0007a683                lw      a3,0(a5)
              28:   00d00713                li      a4,13
              2c:   00e68863                beq     a3,a4,3c <main+0x3c>
              30:   0007a703                lw      a4,0(a5)
              34:   00170713                addi    a4,a4,1
              38:   00e7a023                sw      a4,0(a5)
              3c:   c0202773                rdinstret       a4
              40:   100007b7                lui     a5,0x10000
              44:   00e7a623                sw      a4,12(a5) # 1000000c <__global_pointer$+0xffef80c>
              48:   00000513                li      a0,0
              4c:   00008067                ret
         */

        const uint_t[42] optimized4 = {
            /* 00000000 */ 0x000107b7, 0x00c00713, 0x00e7a023, 0x0007a703,
            /* 00000010 */ 0x00170713, 0x00e7a023, 0xc02026f3, 0x10000737,
            /* 00000020 */ 0x00d72423, 0x0007a683, 0x00d00713, 0x00e68863,
            /* 00000030 */ 0x0007a703, 0x00170713, 0x00e7a023, 0xc0202773,
            /* 00000040 */ 0x100007b7, 0x00e7a623, 0x00000513, 0x00008067
        };

        unit::test<26, 1>([optimized4](unit::tag_t tag)
        {
            print("Running MMIO/rdinstret\n");
            programTest1.loadProgram(false, 0, optimized4, 0x100, 0x0, 0, Trap::EBREAK);
            programTest1.run(tag, false);
            unit::assert(tag, programTest1.instret() == 6);
        });

        unit::test<27, 4>([debug, optimized, optimized4](unit::tag_t tag)
        {
            print("Running debug, MMIO/rdinstret, optimized, optimized on 4 harts\n");
            programTest4.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, optimized4, 0, 0x0, 0, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.run(tag, false);
            unit::assert(tag, programTest4.instret() == 6);
        });

        unit::test<28, 4>([debug, optimized, optimized4](unit::tag_t tag)
        {
            print("Running debug, MMIO/rdinstret, optimized, optimized from external IMEM on 4 harts\n");
            programTest4.loadProgram(false, 0, debug, EXTERNAL_IMEM_ADDR + 0x100, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, optimized4, EXTERNAL_IMEM_ADDR + 0, 0x0, 0, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, EXTERNAL_IMEM_ADDR + 0x200, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 3, optimized, EXTERNAL_IMEM_ADDR + 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.run(tag, false);
            unit::assert(tag, programTest4.instret() == 6);
        });

        unit::test<29, 4>([debug, optimized, optimized4](unit::tag_t tag)
        {
            print("Running debug, MMIO/rdinstret, optimized, optimized on 4 harts fast\n");
            programTest4Fmax.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 1, optimized4, 0, 0x0, 0, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.run(tag, false);
            unit::assert(tag, programTest4.instret() == 6);
        });

        /*
            unsigned g;

            int main()
            {
                volatile unsigned* io_instr_start = (unsigned*)0x10000000;
                volatile unsigned* io_instr_end = (unsigned*)0x10000004;
                volatile unsigned* io_instret_start = (unsigned*)0x10000008;
                volatile unsigned* io_instret_end = (unsigned*)0x1000000c;

                volatile unsigned* x = &g;

                *x = 20;

                int result = 0;

                asm volatile ("csrr %0, time" : "=r"(*io_instr_start));
                asm volatile ("csrr %0, instret" : "=r"(*io_instret_start));

                if (*x > 10)
                {
                    asm volatile ("csrr %0, time" : "=r"(*io_instr_end));
                    asm volatile ("csrr %0, instret" : "=r"(*io_instret_end));

                    return *x;
                }

                return 0;
            }

        00000000 <main>:
               0:   00010737                lui     a4,0x10
               4:   01400793                li      a5,20
               8:   00f72023                sw      a5,0(a4) # 10000 <__DATA_BEGIN__>
               c:   c01026f3                rdtime  a3
              10:   100007b7                lui     a5,0x10000
              14:   00d7a023                sw      a3,0(a5) # 10000000 <__global_pointer$+0xffef800>
              18:   c02026f3                rdinstret       a3
              1c:   00d7a423                sw      a3,8(a5)
              20:   00072603                lw      a2,0(a4)
              24:   00a00693                li      a3,10
              28:   00000513                li      a0,0
              2c:   00c6e463                bltu    a3,a2,34 <main+0x34>
              30:   00008067                ret
              34:   c01026f3                rdtime  a3
              38:   00d7a223                sw      a3,4(a5)
              3c:   c02026f3                rdinstret       a3
              40:   00d7a623                sw      a3,12(a5)
              44:   00072503                lw      a0,0(a4)
              48:   00008067                ret
        */

        unit::test<30, 2>([](unit::tag_t tag)
        {
            const uint_t[42] branch_misprediction = {
                /* 00000000 */ 0x00010737, 0x01400793, 0x00f72023, 0xc01026f3,
                /* 00000010 */ 0x100007b7, 0x00d7a023, 0xc02026f3, 0x00d7a423,
                /* 00000020 */ 0x00072603, 0x00a00693, 0x00000513, 0x00c6e463,
                /* 00000030 */ 0x00008067, 0xc01026f3, 0x00d7a223, 0xc02026f3,
                /* 00000040 */ 0x00d7a623, 0x00072503, 0x00008067
            };

            print("Running branch misprediction test\n");
            programTest2.loadProgram(false, 0, branch_misprediction, 0x600, 0x0, 20, Trap::EBREAK);
            programTest2.loadProgram(false, 1, branch_misprediction, 0x600, 0x48, uninit_int, Trap::EBREAK);
            programTest2.run(tag, false);
            // Note: -1 because there are 2 instruction between rdtime and
            // rdinstret for start read and only 1 instruction for end read.
            const auto misprediction_penalty = programTest2.timeElapsed() / 2 - programTest2.instret() - 1;

            /*
                unsigned g;

                int main()
                {
                    volatile unsigned* io_instr_start = (unsigned*)0x10000000;
                    volatile unsigned* io_instr_end = (unsigned*)0x10000004;
                    volatile unsigned* io_instret_start = (unsigned*)0x10000008;
                    volatile unsigned* io_instret_end = (unsigned*)0x1000000c;

                    volatile unsigned* x = &g;

                    *x = 20;

                    int result = 0;

                    asm volatile ("csrr %0, time" : "=r"(*io_instr_start));
                    asm volatile ("csrr %0, instret" : "=r"(*io_instret_start));

                    for (int i = 0; i < *x; ++i)
                        for (int j = 0; j < *x; ++j)
                            result++;

                    asm volatile ("csrr %0, time" : "=r"(*io_instr_end));
                    asm volatile ("csrr %0, instret" : "=r"(*io_instret_end));

                    return result;
                }

            00000000 <main>:
                   0:   000106b7                lui     a3,0x10
                   4:   01400793                li      a5,20
                   8:   00f6a023                sw      a5,0(a3) # 10000 <__DATA_BEGIN__>
                   c:   c0102773                rdtime  a4
                  10:   100007b7                lui     a5,0x10000
                  14:   00e7a023                sw      a4,0(a5) # 10000000 <__global_pointer$+0xffef800>
                  18:   c0202773                rdinstret       a4
                  1c:   00e7a423                sw      a4,8(a5)
                  20:   0006a783                lw      a5,0(a3)
                  24:   00000593                li      a1,0
                  28:   00000513                li      a0,0
                  2c:   02078863                beqz    a5,5c <main+0x5c>
                  30:   0006a703                lw      a4,0(a3)
                  34:   00000793                li      a5,0
                  38:   00150613                addi    a2,a0,1
                  3c:   00070a63                beqz    a4,50 <main+0x50>
                  40:   0006a703                lw      a4,0(a3)
                  44:   00f60533                add     a0,a2,a5
                  48:   00178793                addi    a5,a5,1
                  4c:   fee7eae3                bltu    a5,a4,40 <main+0x40>
                  50:   0006a783                lw      a5,0(a3)
                  54:   00158593                addi    a1,a1,1
                  58:   fcf5ece3                bltu    a1,a5,30 <main+0x30>
                  5c:   c0102773                rdtime  a4
                  60:   100007b7                lui     a5,0x10000
                  64:   00e7a223                sw      a4,4(a5) # 10000004 <__global_pointer$+0xffef804>
                  68:   c0202773                rdinstret       a4
                  6c:   00e7a623                sw      a4,12(a5)
                  70:   00008067                ret
            */

            const uint_t[42] nested_loop = {
                /* 00000000 */ 0x000106b7, 0x01400793, 0x00f6a023, 0xc0102773,
                /* 00000010 */ 0x100007b7, 0x00e7a023, 0xc0202773, 0x00e7a423,
                /* 00000020 */ 0x0006a783, 0x00000593, 0x00000513, 0x02078863,
                /* 00000030 */ 0x0006a703, 0x00000793, 0x00150613, 0x00070a63,
                /* 00000040 */ 0x0006a703, 0x00f60533, 0x00178793, 0xfee7eae3,
                /* 00000050 */ 0x0006a783, 0x00158593, 0xfcf5ece3, 0xc0102773,
                /* 00000060 */ 0x100007b7, 0x00e7a223, 0xc0202773, 0x00e7a623,
                /* 00000070 */ 0x00008067
            };

            // Note: that the program must be loaded at address above 1024 to get
            // the same results for RTL simulator and emulator. This is due to
            // different initial values of btb memory.
            print("Running nested loop branch prediction test\n");
            programTest2.loadProgram(false, 0, nested_loop, 0x2000, 0x0, 20*20, Trap::EBREAK);
            programTest2.loadProgram(false, 1, nested_loop, 0x2000, 0x70, uninit_int, Trap::EBREAK);
            programTest2.run(tag, false);
            // Test validates that nested loop mispredicts only once per inner loop
            unit::assert(tag, programTest2.timeElapsed() / 2 - programTest2.instret() == 24 * misprediction_penalty || opt::stall != 0);
        });

        /*
            #include <stdint.h>

            int main()
            {
                volatile uint64_t uop1 = 0xab583532;
                volatile uint64_t uop2 = 0x59234da8;
                volatile int64_t  sop1 = 0xdeadbeef;
                volatile int64_t  sop2 = 0xbeefcafe;

                int r1 = uop1 * uop2;
                int r2 = (sop1 * uop2) >> 32;
                int r3 = (uop1 * sop2) >> 32;
                int r4 = (sop1 * sop2) >> 32;

                int r5 = r1 * r2 * r3 * r4;

                return r5;
            }

        00000000 <main>:
               0:   ab583737                lui     a4,0xab583
               4:   fd010113                addi    sp,sp,-48
               8:   53270713                addi    a4,a4,1330 # ab583532 <__global_pointer$+0xab572d32>
               c:   00e12023                sw      a4,0(sp)
              10:   00000793                li      a5,0
              14:   59235737                lui     a4,0x59235
              18:   00f12223                sw      a5,4(sp)
              1c:   da870713                addi    a4,a4,-600 # 59234da8 <__global_pointer$+0x592245a8>
              20:   00e12423                sw      a4,8(sp)
              24:   00000793                li      a5,0
              28:   deadc737                lui     a4,0xdeadc
              2c:   00f12623                sw      a5,12(sp)
              30:   eef70713                addi    a4,a4,-273 # deadbeef <__global_pointer$+0xdeacb6ef>
              34:   00e12823                sw      a4,16(sp)
              38:   00000793                li      a5,0
              3c:   beefd737                lui     a4,0xbeefd
              40:   00f12a23                sw      a5,20(sp)
              44:   afe70713                addi    a4,a4,-1282 # beefcafe <__global_pointer$+0xbeeec2fe>
              48:   00e12c23                sw      a4,24(sp)
              4c:   00000793                li      a5,0
              50:   00f12e23                sw      a5,28(sp)
              54:   00012503                lw      a0,0(sp)
              58:   00412583                lw      a1,4(sp)
              5c:   00812703                lw      a4,8(sp)
              60:   00c12783                lw      a5,12(sp)
              64:   01012303                lw      t1,16(sp)
              68:   01412383                lw      t2,20(sp)
              6c:   00812603                lw      a2,8(sp)
              70:   00c12683                lw      a3,12(sp)
              74:   02e50533                mul     a0,a0,a4
              78:   00012e03                lw      t3,0(sp)
              7c:   00412e83                lw      t4,4(sp)
              80:   01812703                lw      a4,24(sp)
              84:   01c12783                lw      a5,28(sp)
              88:   02812623                sw      s0,44(sp)
              8c:   02912423                sw      s1,40(sp)
              90:   01012403                lw      s0,16(sp)
              94:   01412483                lw      s1,20(sp)
              98:   01812803                lw      a6,24(sp)
              9c:   02668fb3                mul     t6,a3,t1
              a0:   01c12883                lw      a7,28(sp)
              a4:   02c385b3                mul     a1,t2,a2
              a8:   02c33333                mulhu   t1,t1,a2
              ac:   01f585b3                add     a1,a1,t6
              b0:   03c786b3                mul     a3,a5,t3
              b4:   006585b3                add     a1,a1,t1
              b8:   02ee8333                mul     t1,t4,a4
              bc:   03c73633                mulhu   a2,a4,t3
              c0:   006686b3                add     a3,a3,t1
              c4:   02b50533                mul     a0,a0,a1
              c8:   00c686b3                add     a3,a3,a2
              cc:   030487b3                mul     a5,s1,a6
              d0:   02812483                lw      s1,40(sp)
              d4:   02888633                mul     a2,a7,s0
              d8:   03043733                mulhu   a4,s0,a6
              dc:   02c12403                lw      s0,44(sp)
              e0:   00c787b3                add     a5,a5,a2
              e4:   03010113                addi    sp,sp,48
              e8:   02d50533                mul     a0,a0,a3
              ec:   00e787b3                add     a5,a5,a4
              f0:   02f50533                mul     a0,a0,a5
              f4:   00008067                ret

        */

        const uint_t[62] multiplication = {
            /* 00000000 */ 0xab583737, 0xfd010113, 0x53270713, 0x00e12023,
            /* 00000010 */ 0x00000793, 0x59235737, 0x00f12223, 0xda870713,
            /* 00000020 */ 0x00e12423, 0x00000793, 0xdeadc737, 0x00f12623,
            /* 00000030 */ 0xeef70713, 0x00e12823, 0x00000793, 0xbeefd737,
            /* 00000040 */ 0x00f12a23, 0xafe70713, 0x00e12c23, 0x00000793,
            /* 00000050 */ 0x00f12e23, 0x00012503, 0x00412583, 0x00812703,
            /* 00000060 */ 0x00c12783, 0x01012303, 0x01412383, 0x00812603,
            /* 00000070 */ 0x00c12683, 0x02e50533, 0x00012e03, 0x00412e83,
            /* 00000080 */ 0x01812703, 0x01c12783, 0x02812623, 0x02912423,
            /* 00000090 */ 0x01012403, 0x01412483, 0x01812803, 0x02668fb3,
            /* 000000a0 */ 0x01c12883, 0x02c385b3, 0x02c33333, 0x01f585b3,
            /* 000000b0 */ 0x03c786b3, 0x006585b3, 0x02ee8333, 0x03c73633,
            /* 000000c0 */ 0x006686b3, 0x02b50533, 0x00c686b3, 0x030487b3,
            /* 000000d0 */ 0x02812483, 0x02888633, 0x03043733, 0x02c12403,
            /* 000000e0 */ 0x00c787b3, 0x03010113, 0x02d50533, 0x00e787b3,
            /* 000000f0 */ 0x02f50533, 0x00008067};

        const int_t expected_multiplication_result = static_cast(0xbcd74d00);

        unit::test<31, 4>([debug, optimized, multiplication, expected_multiplication_result](unit::tag_t tag)
        {
            print("Running optimized, debug, optimized and multiply on 4 harts\n");
            programTest4.loadProgram(false, 0, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, optimized, 0x400, 0x0, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 3, multiplication, 0, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<32, 1>([multiplication, expected_multiplication_result](unit::tag_t tag)
        {
            print("Running multiply on 1 harts\n");
            programTest1.loadProgram(false, 0, multiplication, 0, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<33, 2>([debug, multiplication, expected_multiplication_result](unit::tag_t tag)
        {
            print("Running multiply and debug on 2 harts\n");
            programTest2.loadProgram(false, 0, multiplication, 0, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest2.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<34, 2>([multiplication, expected_multiplication_result](unit::tag_t tag)
        {
            print("Running multiply and multiply on 2 harts\n");
            programTest2.loadProgram(false, 0, multiplication, 0, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest2.loadProgram(false, 1, multiplication, 0x200, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<35, 1>([multiplication, expected_multiplication_result](unit::tag_t tag)
        {
            print("Running multiply from external IMEM on 1 harts\n");
            programTest1.loadProgram(false, 0, multiplication, EXTERNAL_IMEM_ADDR, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<36, 2>([debug, multiplication, expected_multiplication_result](unit::tag_t tag)
        {
            print("Running multiply and debug from external IMEM on 2 harts\n");
            programTest2.loadProgram(false, 0, multiplication, EXTERNAL_IMEM_ADDR, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest2.loadProgram(false, 1, debug, EXTERNAL_IMEM_ADDR + 0x200, 0x78, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<37, 4>([debug, optimized, multiplication](unit::tag_t tag)
        {
            print("Running debug, ,ultiplication, optimized, optimized on 4 harts fast\n");
            programTest4Fmax.loadProgram(false, 0, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 1, multiplication, 0, 0x0, 0, Trap::IllegalInstruction);
            programTest4Fmax.loadProgram(false, 2, optimized, 0x200, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 3, optimized, 0x300, 0x0, 120, Trap::EBREAK);
            programTest4Fmax.run(tag, false);
        });

        /*
            int main()
            {
                volatile unsigned* p = (unsigned*)0x10000000;

                unsigned x = 0;

                for (int i = 0; i < 10; ++i)
                {
                    x += *(p++);
                }

                return x;
            }
        */

        /*
        00000000 <main>:
           0:   10000737                lui     a4,0x10000
           4:   00072783                lw      a5,0(a4) # 10000000 <__global_pointer$+0xffef800>
           8:   00472603                lw      a2,4(a4)
           c:   00872683                lw      a3,8(a4)
          10:   00c72583                lw      a1,12(a4)
          14:   00c787b3                add     a5,a5,a2
          18:   01072603                lw      a2,16(a4)
          1c:   00d787b3                add     a5,a5,a3
          20:   01472683                lw      a3,20(a4)
          24:   00b787b3                add     a5,a5,a1
          28:   01872503                lw      a0,24(a4)
          2c:   00c787b3                add     a5,a5,a2
          30:   01c72603                lw      a2,28(a4)
          34:   00d787b3                add     a5,a5,a3
          38:   02072683                lw      a3,32(a4)
          3c:   00a787b3                add     a5,a5,a0
          40:   02472503                lw      a0,36(a4)
          44:   00c787b3                add     a5,a5,a2
          48:   00d787b3                add     a5,a5,a3
          4c:   00f50533                add     a0,a0,a5
          50:   00008067                ret
        */

        const uint_t[42] mmio_load_optimized = {
            /* 00000000 */ 0x10000737, 0x00072783, 0x00472603, 0x00872683,
            /* 00000010 */ 0x00c72583, 0x00c787b3, 0x01072603, 0x00d787b3,
            /* 00000020 */ 0x01472683, 0x00b787b3, 0x01872503, 0x00c787b3,
            /* 00000030 */ 0x01c72603, 0x00d787b3, 0x02072683, 0x00a787b3,
            /* 00000040 */ 0x02472503, 0x00c787b3, 0x00d787b3, 0x00f50533,
            /* 00000050 */ 0x00008067};

        unit::test<38, 1>([mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load optimized on 1 harts\n");
            programTest1.loadProgram(false, 0, mmio_load_optimized, 0, 0x0, 180, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<39, 1>([mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load optimized from external IMEM on 1 harts\n");
            programTest1.loadProgram(false, 0, mmio_load_optimized, EXTERNAL_IMEM_ADDR, 0x0, 180, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        /*
        00000000 <main>:
           0:   fe010113                addi    sp,sp,-32
           4:   00812e23                sw      s0,28(sp)
           8:   02010413                addi    s0,sp,32
           c:   100007b7                lui     a5,0x10000
          10:   fef42623                sw      a5,-20(s0)
          14:   fe042423                sw      zero,-24(s0)
          18:   fe042223                sw      zero,-28(s0)
          1c:   02c0006f                j       48 <main+0x48>
          20:   fec42783                lw      a5,-20(s0)
          24:   00478713                addi    a4,a5,4 # 10000004 <__global_pointer$+0xffef804>
          28:   fee42623                sw      a4,-20(s0)
          2c:   0007a783                lw      a5,0(a5)
          30:   fe842703                lw      a4,-24(s0)
          34:   00f707b3                add     a5,a4,a5
          38:   fef42423                sw      a5,-24(s0)
          3c:   fe442783                lw      a5,-28(s0)
          40:   00178793                addi    a5,a5,1
          44:   fef42223                sw      a5,-28(s0)
          48:   fe442703                lw      a4,-28(s0)
          4c:   00900793                li      a5,9
          50:   fce7d8e3                bge     a5,a4,20 <main+0x20>
          54:   fe842783                lw      a5,-24(s0)
          58:   00078513                mv      a0,a5
          5c:   01c12403                lw      s0,28(sp)
          60:   02010113                addi    sp,sp,32
          64:   00008067                ret
        */

        const uint_t[42] mmio_load_debug = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0x100007b7,
            /* 00000010 */ 0xfef42623, 0xfe042423, 0xfe042223, 0x02c0006f,
            /* 00000020 */ 0xfec42783, 0x00478713, 0xfee42623, 0x0007a783,
            /* 00000030 */ 0xfe842703, 0x00f707b3, 0xfef42423, 0xfe442783,
            /* 00000040 */ 0x00178793, 0xfef42223, 0xfe442703, 0x00900793,
            /* 00000050 */ 0xfce7d8e3, 0xfe842783, 0x00078513, 0x01c12403,
            /* 00000060 */ 0x02010113, 0x00008067};

        unit::test<40, 1>([mmio_load_debug](unit::tag_t tag)
        {
            print("Running mmio load debug on 1 harts\n");
            programTest1.loadProgram(false, 0, mmio_load_debug, 0, 0x0, 180, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<41, 2>([mmio_load_debug, mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load debug and optimized on 2 harts\n");
            programTest2.loadProgram(false, 0, mmio_load_debug, 0x0, 0x0, 180, Trap::EBREAK);
            programTest2.loadProgram(false, 1, mmio_load_optimized, 0x100, 0x0, 180, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<42, 2>([mmio_load_debug, mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load debug and optimized  from external IMEM on 2 harts\n");
            programTest2.loadProgram(false, 0, mmio_load_debug, EXTERNAL_IMEM_ADDR, 0x0, 180, Trap::EBREAK);
            programTest2.loadProgram(false, 1, mmio_load_optimized, EXTERNAL_IMEM_ADDR + 0x100, 0x0, 180, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<43, 4>([debug, multiplication, expected_multiplication_result, mmio_load_debug, mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load debug, debug, mmio load optimized and multiply on 4 harts\n");
            programTest4.loadProgram(false, 0, mmio_load_debug, 0x300, 0x0, 180, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, mmio_load_optimized, 0x400, 0x0, 180, Trap::EBREAK);
            programTest4.loadProgram(false, 3, multiplication, 0, 0x0, expected_multiplication_result, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        /*
            #include <stdint.h>

            int main()
            {
                volatile unsigned* p = (unsigned*)0x10000000;

                unsigned x = 0;

                if (*p != 0)
                    x = p[4];
                else
                    x = p[8];

                return x;
            }
        */

        /* 00000000 <main>:
           0:   100007b7                lui     a5,0x10000
           4:   0007a703                lw      a4,0(a5) # 10000000 <__global_pointer$+0xffef800>
           8:   00070663                beqz    a4,14 <main+0x14>
           c:   0107a503                lw      a0,16(a5)
          10:   00008067                ret
          14:   0207a503                lw      a0,32(a5)
          18:   00008067                ret
        */

        const uint_t[42] mmio_load_branch = {
            /* 00000000 */ 0x100007b7, 0x0007a703, 0x00070663, 0x0107a503,
            /* 00000010 */ 0x00008067, 0x0207a503, 0x00008067};

        unit::test<44, 1>([mmio_load_branch](unit::tag_t tag)
        {
            print("Running mmio load with branch on 1 harts\n");
            programTest1.loadProgram(false, 0, mmio_load_branch, 0, 0x0, 32, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<45, 4>([debug, mmio_load_branch, mmio_load_debug, mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load debug, debug, mmio load optimized and mmio load with branch on 4 harts\n");
            programTest4.loadProgram(false, 0, mmio_load_debug, 0x300, 0x0, 180, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, mmio_load_optimized, 0x400, 0x0, 180, Trap::EBREAK);
            programTest4.loadProgram(false, 3, mmio_load_branch, 0, 0x0, 32, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<46, 4>([debug, mmio_load_branch, mmio_load_debug, mmio_load_optimized](unit::tag_t tag)
        {
            print("Running mmio load debug, debug, mmio load optimized and mmio load with branch from external IMEM on 4 harts\n");
            programTest4.loadProgram(false, 0, mmio_load_debug, 0x300, 0x0, 180, Trap::EBREAK);
            programTest4.loadProgram(false, 1, debug, 0x200, 0x78, 120, Trap::EBREAK);
            programTest4.loadProgram(false, 2, mmio_load_optimized, 0x400, 0x0, 180, Trap::EBREAK);
            programTest4.loadProgram(false, 3, mmio_load_branch, EXTERNAL_IMEM_ADDR, 0x0, 32, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        unit::test<47, 2>([debug, optimized](unit::tag_t tag)
        {
            print("Running debug and optimized on 2 harts quad-port shared\n");
            quadPortShared2.loadProgram(false, 0, debug, 0, 0x78, 120, Trap::EBREAK);
            quadPortShared2.loadProgram(false, 1, optimized, 0x100, 0x0, 120, Trap::EBREAK);
            quadPortShared2.run(tag, false);
        });

        /*
            int main()
            {
                volatile unsigned* p = (unsigned*)0x10000;

                while (*p == 0);

                return *p;
            }

        00000000 <main>:
               0:   fe010113                addi    sp,sp,-32
               4:   00812e23                sw      s0,28(sp)
               8:   02010413                addi    s0,sp,32
               c:   000107b7                lui     a5,0x10
              10:   fef42623                sw      a5,-20(s0)
              14:   00000013                nop
              18:   fec42783                lw      a5,-20(s0)
              1c:   0007a783                lw      a5,0(a5) # 10000 <__BSS_END__>
              20:   fe078ce3                beqz    a5,18 <main+0x18>
              24:   fec42783                lw      a5,-20(s0)
              28:   0007a783                lw      a5,0(a5)
              2c:   00078513                mv      a0,a5
              30:   01c12403                lw      s0,28(sp)
              34:   02010113                addi    sp,sp,32
              38:   00008067                ret
        */

        const uint_t[42] wait_for_mem = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0x000107b7,
            /* 00000010 */ 0xfef42623, 0x00000013, 0xfec42783, 0x0007a783,
            /* 00000020 */ 0xfe078ce3, 0xfec42783, 0x0007a783, 0x00078513,
            /* 00000030 */ 0x01c12403, 0x02010113, 0x00008067};

        const uint_t[2] mem_init = {0x0, 0x13};

        /*
            int main()
            {
                volatile unsigned* p = (unsigned*)0x10000;
                volatile unsigned* r = (unsigned*)0x10004;

                for (int i = 0; i < 10; ++i)
                {
                    *r *= 2;
                }

                *p = *r;

                return *p;
            }

        00000000 <main>:
               0:   fe010113                addi    sp,sp,-32
               4:   00812e23                sw      s0,28(sp)
               8:   02010413                addi    s0,sp,32
               c:   000107b7                lui     a5,0x10
              10:   fef42423                sw      a5,-24(s0)
              14:   000107b7                lui     a5,0x10
              18:   00478793                addi    a5,a5,4 # 10004 <__BSS_END__+0x4>
              1c:   fef42223                sw      a5,-28(s0)
              20:   fe042623                sw      zero,-20(s0)
              24:   0240006f                j       48 <main+0x48>
              28:   fe442783                lw      a5,-28(s0)
              2c:   0007a783                lw      a5,0(a5)
              30:   00179713                slli    a4,a5,0x1
              34:   fe442783                lw      a5,-28(s0)
              38:   00e7a023                sw      a4,0(a5)
              3c:   fec42783                lw      a5,-20(s0)
              40:   00178793                addi    a5,a5,1
              44:   fef42623                sw      a5,-20(s0)
              48:   fec42703                lw      a4,-20(s0)
              4c:   00900793                li      a5,9
              50:   fce7dce3                bge     a5,a4,28 <main+0x28>
              54:   fe442783                lw      a5,-28(s0)
              58:   0007a703                lw      a4,0(a5)
              5c:   fe842783                lw      a5,-24(s0)
              60:   00e7a023                sw      a4,0(a5)
              64:   fe842783                lw      a5,-24(s0)
              68:   0007a783                lw      a5,0(a5)
              6c:   00078513                mv      a0,a5
              70:   01c12403                lw      s0,28(sp)
              74:   02010113                addi    sp,sp,32
              78:   00008067                ret
        */

        const uint_t[42] set_mem = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0x000107b7,
            /* 00000010 */ 0xfef42423, 0x000107b7, 0x00478793, 0xfef42223,
            /* 00000020 */ 0xfe042623, 0x0240006f, 0xfe442783, 0x0007a783,
            /* 00000030 */ 0x00179713, 0xfe442783, 0x00e7a023, 0xfec42783,
            /* 00000040 */ 0x00178793, 0xfef42623, 0xfec42703, 0x00900793,
            /* 00000050 */ 0xfce7dce3, 0xfe442783, 0x0007a703, 0xfe842783,
            /* 00000060 */ 0x00e7a023, 0xfe842783, 0x0007a783, 0x00078513,
            /* 00000070 */ 0x01c12403, 0x02010113, 0x00008067};

        unit::test<48, 2>([mem_init, wait_for_mem, set_mem](unit::tag_t tag)
        {
            print("Running memory set and wait on 2 harts quad-port shared\n");
            quadPortShared2.loadData(0, mem_init);
            quadPortShared2.loadProgram(false, 0, wait_for_mem, 0x0, 0x0, 19456, Trap::EBREAK);
            quadPortShared2.loadProgram(false, 1, set_mem, 0x100, 0x0, 19456, Trap::EBREAK);
            quadPortShared2.run(tag, false);
        });

        /*
            int main()
            {
                volatile unsigned* r = (unsigned*)0x10004;

                for (int i = 0; i < 10; ++i)
                {
                    *r *= 2;
                }

                return *r;
            }

        00000000 <main>:
               0:   fe010113                addi    sp,sp,-32
               4:   00812e23                sw      s0,28(sp)
               8:   02010413                addi    s0,sp,32
               c:   000107b7                lui     a5,0x10
              10:   00478793                addi    a5,a5,4 # 10004 <__BSS_END__+0x4>
              14:   fef42423                sw      a5,-24(s0)
              18:   fe042623                sw      zero,-20(s0)
              1c:   0240006f                j       40 <main+0x40>
              20:   fe842783                lw      a5,-24(s0)
              24:   0007a783                lw      a5,0(a5)
              28:   00179713                slli    a4,a5,0x1
              2c:   fe842783                lw      a5,-24(s0)
              30:   00e7a023                sw      a4,0(a5)
              34:   fec42783                lw      a5,-20(s0)
              38:   00178793                addi    a5,a5,1
              3c:   fef42623                sw      a5,-20(s0)
              40:   fec42703                lw      a4,-20(s0)
              44:   00900793                li      a5,9
              48:   fce7dce3                bge     a5,a4,20 <main+0x20>
              4c:   fe842783                lw      a5,-24(s0)
              50:   0007a783                lw      a5,0(a5)
              54:   00078513                mv      a0,a5
              58:   01c12403                lw      s0,28(sp)
              5c:   02010113                addi    sp,sp,32
              60:   00008067                ret

        */

        const uint_t[42] set_mem2 = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0x000107b7,
            /* 00000010 */ 0x00478793, 0xfef42423, 0xfe042623, 0x0240006f,
            /* 00000020 */ 0xfe842783, 0x0007a783, 0x00179713, 0xfe842783,
            /* 00000030 */ 0x00e7a023, 0xfec42783, 0x00178793, 0xfef42623,
            /* 00000040 */ 0xfec42703, 0x00900793, 0xfce7dce3, 0xfe842783,
            /* 00000050 */ 0x0007a783, 0x00078513, 0x01c12403, 0x02010113,
            /* 00000060 */ 0x00008067};

        unit::test<49, 2>([mem_init, wait_for_mem, set_mem2](unit::tag_t tag)
        {
            print("Running memory set2 and wait on 2 harts quad-port shared\n");
            quadPortShared2.loadData(0, mem_init);
            quadPortShared2.loadProgram(false, 0, set_mem2, 0x0, 0x0, 19456, Trap::EBREAK);
            quadPortShared2.loadProgram(false, 1, wait_for_mem, 0x100, 0x0, 1234, Trap::EBREAK);

            async_exec([]
            {
                do; while (quadPortShared2.dmem_read(0, 4) != 19456);
                quadPortShared2.dmem_write(0, 0, 1234);
            });

            quadPortShared2.run(tag, false);
        });

        /*

        int main()
        {
            int x = 0;

            for (int i = 0; i < 10; ++i)
            {
                asm(".insn r CUSTOM_0, %[funct3], %[funct7], %[rd], %[rs1], %[rs2]"
                    : [rd] "=r" (x)
                    : [funct3] "i" (3),
                      [funct7] "i" (3 * 2 + 3),
                      [rs1] "r" (x),
                      [rs2] "r" (i));
            }

            return x;
        }

        00000000 <main>:
           0:   fe010113                addi    sp,sp,-32
           4:   00812e23                sw      s0,28(sp)
           8:   02010413                addi    s0,sp,32
           c:   fe042623                sw      zero,-20(s0)
          10:   fe042423                sw      zero,-24(s0)
          14:   0200006f                j       34 <main+0x34>
          18:   fec42783                lw      a5,-20(s0)
          1c:   fe842703                lw      a4,-24(s0)
          20:   12e7b78b                0x12e7b78b
          24:   fef42623                sw      a5,-20(s0)
          28:   fe842783                lw      a5,-24(s0)
          2c:   00178793                addi    a5,a5,1
          30:   fef42423                sw      a5,-24(s0)
          34:   fe842703                lw      a4,-24(s0)
          38:   00900793                li      a5,9
          3c:   fce7dee3                bge     a5,a4,18 <main+0x18>
          40:   fec42783                lw      a5,-20(s0)
          44:   00078513                mv      a0,a5
          48:   01c12403                lw      s0,28(sp)
          4c:   02010113                addi    sp,sp,32
          50:   00008067                ret

        */

        const uint_t[42] custom = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0xfe042623,
            /* 00000010 */ 0xfe042423, 0x0200006f, 0xfec42783, 0xfe842703,
            /* 00000020 */ 0x12e7b78b, 0xfef42623, 0xfe842783, 0x00178793,
            /* 00000030 */ 0xfef42423, 0xfe842703, 0x00900793, 0xfce7dee3,
            /* 00000040 */ 0xfec42783, 0x00078513, 0x01c12403, 0x02010113,
            /* 00000050 */ 0x00008067};

        unit::test<50, 2>([custom](unit::tag_t tag)
        {
            print("Running custom extension on 2 harts\n");
            programTest2.loadProgram(false, 0, custom, 0x0, 0x0, 90, Trap::EBREAK);
            programTest2.loadProgram(false, 1, custom, 0x100, 0x0, 180, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        /*

        int main()
        {
            int x = 0;

            asm volatile (".insn r CUSTOM_0, 0, 0 * 2 + 3, x0, %[rs1], x0"
                : : [rs1] "r" (x));

            for (int i = 0; i < 10; ++i)
            {
                asm volatile (".insn r CUSTOM_0, 1, 1 * 2 + 3, x0, x0, %[rs2]"
                    : : [rs2] "r" (i));
            }

            asm volatile (".insn r CUSTOM_0, 2, 2 * 2 + 3, %[rd], x0, x0"
                : [rd] "=r" (x) :);

            return x;
        }

        00000000 <main>:
           0:   fe010113                addi    sp,sp,-32
           4:   00812e23                sw      s0,28(sp)
           8:   02010413                addi    s0,sp,32
           c:   fe042423                sw      zero,-24(s0)
          10:   fe842783                lw      a5,-24(s0)
          14:   0607800b                0x607800b
          18:   fe042623                sw      zero,-20(s0)
          1c:   0180006f                j       34 <main+0x34>
          20:   fec42783                lw      a5,-20(s0)
          24:   0af0100b                0xaf0100b
          28:   fec42783                lw      a5,-20(s0)
          2c:   00178793                addi    a5,a5,1
          30:   fef42623                sw      a5,-20(s0)
          34:   fec42703                lw      a4,-20(s0)
          38:   00900793                li      a5,9
          3c:   fee7d2e3                bge     a5,a4,20 <main+0x20>
          40:   0e00278b                0xe00278b
          44:   fef42423                sw      a5,-24(s0)
          48:   fe842783                lw      a5,-24(s0)
          4c:   00078513                mv      a0,a5
          50:   01c12403                lw      s0,28(sp)
          54:   02010113                addi    sp,sp,32
          58:   00008067                ret

        */

        const uint_t[42] custom_state = {
            /* 00000000 */ 0xfe010113, 0x00812e23, 0x02010413, 0xfe042423,
            /* 00000010 */ 0xfe842783, 0x0607800b, 0xfe042623, 0x0180006f,
            /* 00000020 */ 0xfec42783, 0x0af0100b, 0xfec42783, 0x00178793,
            /* 00000030 */ 0xfef42623, 0xfec42703, 0x00900793, 0xfee7d2e3,
            /* 00000040 */ 0x0e00278b, 0xfef42423, 0xfe842783, 0x00078513,
            /* 00000050 */ 0x01c12403, 0x02010113, 0x00008067};

        unit::test<51, 2>([custom_state](unit::tag_t tag)
        {
            print("Running custom extension with state on 2 harts\n");
            programTest2.loadProgram(false, 0, custom_state, 0x0, 0x0, 45, Trap::EBREAK);
            programTest2.loadProgram(false, 1, custom_state, 0x100, 0x0, 45, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        /*
        int main()
        {
            for (int i = 0; i < 5; ++i)
            {
                int j = i * i;

                asm(".insn s CUSTOM_3, 2, %[rs2], 4(%[rs1])"
                        :
                        : [rs1] "r" (i),
                          [rs2] "r" (j));
            }

            int x = 0;

            asm(".insn u  CUSTOM_2, %[rd], 4 + 3"
                    : [rd] "=r" (x));

            return x;
        }

        00000000 <main>:
           0:   fe010113                addi    sp,sp,-32
           4:   00112e23                sw      ra,28(sp)
           8:   00812c23                sw      s0,24(sp)
           c:   02010413                addi    s0,sp,32
          10:   fe042623                sw      zero,-20(s0)
          14:   0300006f                j       44 <main+0x44>
          18:   fec42583                lw      a1,-20(s0)
          1c:   fec42503                lw      a0,-20(s0)
          20:   054000ef                jal     ra,74 <__mulsi3>
          24:   00050793                mv      a5,a0
          28:   fef42223                sw      a5,-28(s0)
          2c:   fec42783                lw      a5,-20(s0)
          30:   fe442703                lw      a4,-28(s0)
          34:   00e7a27b                0xe7a27b
          38:   fec42783                lw      a5,-20(s0)
          3c:   00178793                addi    a5,a5,1
          40:   fef42623                sw      a5,-20(s0)
          44:   fec42703                lw      a4,-20(s0)
          48:   00400793                li      a5,4
          4c:   fce7d6e3                bge     a5,a4,18 <main+0x18>
          50:   fe042423                sw      zero,-24(s0)
          54:   000077db                0x77db
          58:   fef42423                sw      a5,-24(s0)
          5c:   fe842783                lw      a5,-24(s0)
          60:   00078513                mv      a0,a5
          64:   01c12083                lw      ra,28(sp)
          68:   01812403                lw      s0,24(sp)
          6c:   02010113                addi    sp,sp,32
          70:   00008067                ret

        00000074 <__mulsi3>:
          74:   00050613                mv      a2,a0
          78:   00000513                li      a0,0
          7c:   0015f693                andi    a3,a1,1
          80:   00068463                beqz    a3,88 <__mulsi3+0x14>
          84:   00c50533                add     a0,a0,a2
          88:   0015d593                srli    a1,a1,0x1
          8c:   00161613                slli    a2,a2,0x1
          90:   fe0596e3                bnez    a1,7c <__mulsi3+0x8>
          94:   00008067                ret

        */

        const uint_t[42] custom_store = {
            /* 00000000 */ 0xfe010113, 0x00112e23, 0x00812c23, 0x02010413,
            /* 00000010 */ 0xfe042623, 0x0300006f, 0xfec42583, 0xfec42503,
            /* 00000020 */ 0x054000ef, 0x00050793, 0xfef42223, 0xfec42783,
            /* 00000030 */ 0xfe442703, 0x00e7a27b, 0xfec42783, 0x00178793,
            /* 00000040 */ 0xfef42623, 0xfec42703, 0x00400793, 0xfce7d6e3,
            /* 00000050 */ 0xfe042423, 0x000077db, 0xfef42423, 0xfe842783,
            /* 00000060 */ 0x00078513, 0x01c12083, 0x01812403, 0x02010113,
            /* 00000070 */ 0x00008067, 0x00050613, 0x00000513, 0x0015f693,
            /* 00000080 */ 0x00068463, 0x00c50533, 0x0015d593, 0x00161613,
            /* 00000090 */ 0xfe0596e3, 0x00008067};

        unit::test<52, 1>([custom_store](unit::tag_t tag)
        {
            print("Running custom store on 1 hart\n");
            programTest1.loadProgram(false, 0, custom_store, 0x0, 0x0, 9, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        const uint_t[2] illegal = {
            /* 00000000 */ concat(RVG::reserved0, 0b11), 0x00008067 };

        unit::test<53, 1>([illegal](unit::tag_t tag)
        {
            print("Running illegal on 1 hart\n");
            programTest1.loadProgram(true, 0, illegal, 0x0, 0x0, 0, Trap::IllegalInstruction);
            programTest1.run(tag, false);
        });

        const uint_t[2] illegal2 = {
            /* 00000000 */ 0x00000000, 0x00008067 };

        unit::test<54, 1>([illegal2](unit::tag_t tag)
        {
            print("Running illegal2 on 1 hart\n");
            programTest1.loadProgram(true, 0, illegal2, 0x0, 0x0, 0, Trap::IllegalInstruction);
            programTest1.run(tag, false);
        });

        // programTest2 has NoExceptionForCompressedEncoding option
        unit::test<55, 2>([illegal2, debug](unit::tag_t tag)
        {
            print("Running illegal2 and debug on 2 hart\n");
            programTest2.loadProgram(true, 0, illegal2, 0x0, 0x0, uninit_int, Trap::AccessFault);
            programTest2.loadProgram(false, 1, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<56, 2>([illegal, debug](unit::tag_t tag)
        {
            print("Running illegal and debug on 2 harts\n");
            programTest2.loadProgram(false, 0, illegal, 0x0, 0x0, 0, Trap::IllegalInstruction);
            programTest2.loadProgram(false, 1, debug, 0x100, 0x78, 120, Trap::EBREAK);
            programTest2.run(tag, false);
        });

        unit::test<57, 2>([illegal](unit::tag_t tag)
        {
            print("Running illegal on 2 harts\n");
            programTest2.loadProgram(false, 0, illegal, 0x0, 0x0, 0, Trap::IllegalInstruction);
            programTest2.loadProgram(false, 1, illegal, 0x100, 0x0, 0, Trap::IllegalInstruction);
            programTest2.run(tag, false);
        });

        /*

        int main()
        {
            asm volatile ("li ra, 2");

            return 0;
        }

        00000000 <main>:
           0:   ff010113                addi    sp,sp,-16
           4:   00812623                sw      s0,12(sp)
           8:   01010413                addi    s0,sp,16
           c:   00200093                li      ra,2
          10:   00000793                li      a5,0
          14:   00078513                mv      a0,a5
          18:   00c12403                lw      s0,12(sp)
          1c:   01010113                addi    sp,sp,16
          20:   00008067                ret


         */

        const uint_t[42] missaligned = {
            /* 00000000 */ 0xff010113, 0x00812623, 0x01010413, 0x00200093,
            /* 00000010 */ 0x00000793, 0x00078513, 0x00c12403, 0x01010113,
            /* 00000020 */ 0x00008067};

        unit::test<58, 1>([missaligned](unit::tag_t tag)
        {
            print("Running missaligned on 1 hart\n");
            programTest1.loadProgram(false, 0, missaligned, 0x0, 0x0, 0, Trap::InvalidInstructionAddress);
            programTest1.run(tag, false);
        });

        unit::test<59, 2>([missaligned, illegal](unit::tag_t tag)
        {
            print("Running missaligned and illegal on 2 harts\n");
            programTest2.loadProgram(false, 0, missaligned, 0x0, 0x0, 0, Trap::InvalidInstructionAddress);
            programTest2.loadProgram(false, 1, illegal, 0x100, 0x0, 0, Trap::IllegalInstruction);
            programTest2.run(tag, false);
        });

        /*

        int main()
        {
            const unsigned DMEM_ORIGIN = 0x10000;
            const unsigned DMEM_LENGTH = 0x2000;

            volatile char* p = (char*)(DMEM_ORIGIN - 1);

            return *p;
        }

        00000000 <main>:
           0:   000107b7                lui     a5,0x10
           4:   fff7c503                lbu     a0,-1(a5) # ffff <STACK_SIZE+0xefff>
           8:   0ff57513                andi    a0,a0,255
           c:   00008067                ret
        */

        const uint_t[4] dmem_lower_bound = {
            /* 00000000 */ 0x000107b7, 0xfff7c503, 0x0ff57513, 0x00008067};

        unit::test<60, 1>([dmem_lower_bound](unit::tag_t tag)
        {
            print("Running dmem access fault on lower bound on 1 hart\n");
            programTest1.loadProgram(false, 0, dmem_lower_bound, 0x0, 0x0, 0, Trap::AccessFault);
            programTest1.run(tag, false);
        });

        /*

        int main()
        {
            const unsigned DMEM_ORIGIN = 0x10000;
            const unsigned DMEM_LENGTH = 0x2000;

            volatile char* p = (char*)(DMEM_ORIGIN + DMEM_LENGTH);

            *p = 0;

            return 0;
        }

        00000000 <main>:
           0:   000127b7                lui     a5,0x12
           4:   00078023                sb      zero,0(a5) # 12000 <__global_pointer$+0x1800>
           8:   00000513                li      a0,0
           c:   00008067                ret
        */

        const uint_t[4] dmem_upper_bound = {
            /* 00000000 */ 0x000127b7, 0x00078023, 0x00000513, 0x00008067};

        const uint_t[4] dmem_upper_bound_shared = {
            /* 00000000 */ 0x000137b7, 0x00078023, 0x00000513, 0x00008067};

        unit::test<61, 2>([dmem_lower_bound, dmem_upper_bound](unit::tag_t tag)
        {
            print("Running dmem access fault on lower and upper bound on 2 harts\n");
            programTest2.loadProgram(false, 0, dmem_lower_bound, 0x0, 0x0, 0, Trap::AccessFault);
            programTest2.loadProgram(false, 1, dmem_upper_bound, 0x100, 0x0, 0, Trap::AccessFault);
            programTest2.run(tag, true);
        });

        unit::test<71, 2>([dmem_lower_bound, dmem_upper_bound_shared](unit::tag_t tag)
        {
            print("Running dmem access fault on lower and upper bound with shared DMEM on 2 harts\n");
            quadPortShared2.loadProgram(false, 0, dmem_lower_bound, 0x0, 0x0, 0, Trap::AccessFault);
            quadPortShared2.loadProgram(false, 1, dmem_upper_bound_shared, 0x100, 0x0, 0, Trap::AccessFault);
            quadPortShared2.run(tag, false);
        });

        unit::test<62, 4>([dmem_lower_bound, debug, missaligned, illegal](unit::tag_t tag)
        {
            print("Running missaligned, illegal, dmem access fault on lower bound and debug on 4 harts\n");
            programTest4.loadProgram(false, 0, missaligned, 0x0, 0x0, 0, Trap::InvalidInstructionAddress);
            programTest4.loadProgram(false, 1, illegal, 0x100, 0x0, 0, Trap::IllegalInstruction);
            programTest4.loadProgram(false, 2, dmem_lower_bound, 0x200, 0x0, 0, Trap::AccessFault);
            programTest4.loadProgram(false, 3, debug, 0x300, 0x78, 120, Trap::EBREAK);
            programTest4.run(tag, false);
        });

        /*

        int main()
        {
            const unsigned DMEM_ORIGIN = 0x10000;
            const unsigned DMEM_LENGTH = 0x2000;

            volatile int* p = (int*)(DMEM_ORIGIN);
            volatile int* end = (int*)(DMEM_ORIGIN + DMEM_LENGTH);

            while (p < end)
            {
                *(p++) = 0xdeadbeef;
            }

            return 42;
        }

        00000000 <main>:
           0:   deadc6b7                lui     a3,0xdeadc
           4:   000107b7                lui     a5,0x10
           8:   eef68693                addi    a3,a3,-273 # deadbeef <__global_pointer$+0xdeacb6ef>
           c:   00012637                lui     a2,0x12
          10:   00d7a023                sw      a3,0(a5) # 10000 <__BSS_END__>
          14:   00478793                addi    a5,a5,4
          18:   fec79ce3                bne     a5,a2,10 <main+0x10>
          1c:   02a00513                li      a0,42
          20:   00008067                ret

        */

        const uint_t[9] dmem_sweep = {
            /* 00000000 */ 0xdeadc6b7, 0x000107b7, 0xeef68693, 0x00012637,
            /* 00000010 */ 0x00d7a023, 0x00478793, 0xfec79ce3, 0x02a00513,
            /* 00000020 */ 0x00008067};

        const uint_t[9] dmem_sweep_shared = {
            /* 00000000 */ 0xdeadc6b7, 0x000107b7, 0xeef68693, 0x00013637,
            /* 00000010 */ 0x00d7a023, 0x00478793, 0xfec79ce3, 0x02a00513,
            /* 00000020 */ 0x00008067};

        unit::test<63, 1>([dmem_sweep](unit::tag_t tag)
        {
            print("Running dmem sweep on 1 hart\n");
            programTest1.loadProgram(false, 0, dmem_sweep, 0x0, 0x0, 42, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        unit::test<72, 1>([dmem_sweep_shared](unit::tag_t tag)
        {
            print("Running dmem sweep with shared DMEM on 1 hart\n");
            programTest1.loadProgram(false, 0, dmem_sweep_shared, 0x0, 0x0, 42, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        /*

        int main()
        {
            asm volatile ("li ra, 65536");

            return 0;
        }

        00000000 <main>:
           0:   000100b7                lui     ra,0x10
           4:   00000513                li      a0,0
           8:   00008067                ret
        */

        const uint_t[42] imem_upper_bound = {
            /* 00000000 */ 0x000100b7, 0x00000513, 0x00008067};

        unit::test<64, 1>([imem_upper_bound](unit::tag_t tag)
        {
            print("Running imem access fault on upper bound on 1 hart\n");
            programTest1.loadProgram(false, 0, imem_upper_bound, 0x0, 0x0, 0, Trap::InvalidInstructionAddress);
            programTest1.run(tag, false);
        });

        /*

        00000000 <main>:
           0:   000100b7                lui     ra,0x10
           4:   00000513                li      a0,0
        */

        const uint_t[2] imem_wrap = {
            /* 00000000 */ 0x000100b7, 0x00000513};

        unit::test<65, 1>([imem_wrap](unit::tag_t tag)
        {
            print("Running imem wrap exception on 1 hart\n");
            programTest1.loadProgram(false, 0, imem_wrap, IMEM_LENGTH * 4 - bytesizeof imem_wrap, 0x0, 0, Trap::InvalidInstructionAddress);
            programTest1.run(tag, false);
        });

        unit::test<66, 1>([debug](unit::tag_t tag)
        {
            print("Running debug at end of IMEM\n");
            programTest1.loadProgram(false, 0, debug, IMEM_LENGTH * 4 - bytesizeof debug, 0x78, 120, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        /*

        int main()
        {
            asm volatile ("ecall");

            return 0;
        }

        00000000 <main>:
           0:   00000073                ecall
           4:   00000513                li      a0,0
           8:   00008067                ret

        */

        const uint_t[42] ecall = {
            /* 00000000 */ 0x00000073, 0x00000513, 0x00008067};

        unit::test<67, 1>([optimized, ecall](unit::tag_t tag)
        {
            print("Running ecall and optimized handler on 1 hart\n");
            programTest1.loadHandler(false, 0, optimized, 0x1100, 0x0, Trap::ECALL);
            programTest1.loadProgram(false, 0, ecall, 0x0, 0x0, 120, Trap::EBREAK);
            programTest1.run(tag, false);
        });

        /*
        00000000 <main>:
           0:   10000737                lui     a4,0x10000
           4:   01872503                lw      a0,24(a4)
        */

        const uint_t[2] mmio_load_trap = {
            /* 00000000 */ 0x10000737, 0x01872503};

        /*
        00000000 <main>:
           0:   00000073                ecall
           4:   0000006f                j0
           8:   00008067                ret
        */

        const uint_t[3] trap_handler = {
            /* 00000000 */ 0x00000073, 0x0000006f, 0x00008067};

        unit::test<68, 1>([trap_handler, mmio_load_trap](unit::tag_t tag)
        {
            print("Running mmio_load_trap on 1 hart\n");
            programTest1.loadHandler(true, 0, trap_handler, 0, 8, Trap::InvalidInstructionAddress);
            programTest1.loadProgram(true, 0, mmio_load_trap, IMEM_LENGTH * 4 - bytesizeof mmio_load_trap, 0, 24, Trap::EBREAK);
            programTest1.run(tag, true);
        });

        unit::test<69, 2>([optimized, trap_handler, mmio_load_trap](unit::tag_t tag)
        {
            print("Running mmio_load_trap on 2 harts\n");
            programTest2.loadProgram(false, 0, optimized, 0x100, 0, 120, Trap::EBREAK);
            programTest2.loadHandler(false, 1, trap_handler, 0, 8, Trap::InvalidInstructionAddress);
            programTest2.loadProgram(false, 1, mmio_load_trap, IMEM_LENGTH * 4 - bytesizeof mmio_load_trap, 0, 24, Trap::EBREAK);
            programTest2.run(tag, false);
        });


        unit::test<70, 4>([optimized, trap_handler, mmio_load_trap](unit::tag_t tag)
        {
            print("Running mmio_load_trap on 4 harts Fmax\n");
            programTest4Fmax.loadProgram(false, 0, optimized, 0x100, 0, 120, Trap::EBREAK);
            programTest4Fmax.loadHandler(false, 1, trap_handler, 0, 8, Trap::InvalidInstructionAddress);
            programTest4Fmax.loadProgram(false, 1, mmio_load_trap, IMEM_LENGTH * 4 - bytesizeof mmio_load_trap, 0, 24, Trap::EBREAK);
            programTest4Fmax.loadProgram(false, 2, optimized, 0x300, 0, 120, Trap::EBREAK);
            programTest4Fmax.loadHandler(false, 3, trap_handler, 0, 8, Trap::InvalidInstructionAddress);
            programTest4Fmax.loadProgram(false, 3, mmio_load_trap, IMEM_LENGTH * 4 - bytesizeof mmio_load_trap, 0, 24, Trap::EBREAK);
            programTest4Fmax.run(tag, false);
        });
    }

    template <auto HARTS>
    class StressTest
    {
        private:
            using core_t = Core<HARTS, IMEM_LENGTH, DMEM_LENGTH, MMIO_LENGTH, IMEM_ORIGIN, DMEM_ORIGIN, MMIO_ORIGIN, 0>;
            using hart_index_t = core_t::hart_index_t;

            core_t core = {
                .system_trap    = SystemTrap,
                .trace          = PrintTrace,
                .mmio_load      = MmioLoad,
                .mmio_store     = MmioStore,
                .external_fetch = ExternalFetch
            };

            optional<int_t>[HARTS] t1;

            bool enable_trace;

            memory<uint_t, IMEM_LENGTH>[HARTS] external_mem;
            uint10 _random;

            inline uint_t read_instr(hart_index_t hid, imem_addr_t addr)
            {
                uint_t[HARTS] instr;

                static for(const auto i : HARTS)
                {
                    instr[i] = external_mem[i][addr];
                }

                return instr[hid];
            }

            inline optional<imem_addr_t> SystemTrap(hart_index_t hid, Trap trap, imem_addr_t pc)
            {
                assert(trap != Trap::EBREAK);

                const auto delay = reinterpret_cast<uint4>(pc);

                sleep(delay);

                return {};
            }

            inline void PrintTrace(hart_index_t hid, imem_addr_t pc, uint_t instr, Decoded decoded, optional<int_t> value)
            {
                assert(reinterpret_cast<uint_t>(read_instr(hid, pc)) == instr || pc == 0);

                if (enable_trace)
                {
                    print_trace(hid, pc, instr, decoded, value);
                }
            }

            inline optional<int_t> MmioLoad(hart_index_t hid, uint_t addr, MemorySize size, bool sign_extend)
            {
                assert(addr >= MMIO_ORIGIN);

                _random = lfsr(_random);

                optional<int_t> result;
                int_t value = cast<int12>(_random) << 1;

                if (addr - MMIO_ORIGIN > 512)
                {
                    async_exec([hid, value]
                        {
                            const auto delay = reinterpret_cast<uint4>(_random);

                            sleep(delay);

                            core.mmio_load_result(hid, value);
                        });

                    result.is_valid = false;
                }
                else
                {
                    result.is_valid = true;
                    result.value = value;
                }

                t1[hid] = make_optional(true, value);

                return result;
            }

            inline bool MmioStore(hart_index_t hid, uint_t addr, MemorySize size, int_t value)
            {
                assert(value == t1[hid].value || !t1[hid].is_valid);
                assert(size == MemorySize::W);

                return true;
            }

            inline auto ExternalFetch(hart_index_t hid, imem_addr_t addr)
            {
                const auto valid = reinterpret_cast<bool>(_random ^ addr);
                const auto instr = read_instr(hid, addr);

                if (!valid)
                {
                    async_exec([instr, addr, hid]
                    {
                        const auto delay = reinterpret_cast<uint4>(addr);

                        sleep(delay);

                        core.external_fetch_result(hid, addr, instr);
                    });
                }
                return make_optional(valid,  instr);
            }

        public:
            void run(auto iter, bool trace)
            {
                enable_trace = trace;

                const auto LUI = cast<int_t>(concat(slice<31,12>(cast<uint_t>(MMIO_ORIGIN)), cast<register_index_t>(ABI::a4), cast<uint7>(0b_0110111)));

                print("    harts: {HARTS} iterations {iter}\n");

                static for(const auto i : HARTS)
                {
                    t1[i].is_valid = false;
                }

                _random = 1;

                pipelined_for(HARTS, [](hart_index_t hid)
                {
                    pipelined_for(IMEM_LENGTH, [hid](imem_addr_t addr)
                    {
                        uint10 random;

                        atomic
                        {
                            random = lfsr(_random);
                            _random = random;
                        }

                        const auto choice = reinterpret_cast<uint2>(random);

                        const int_t imm = cast<int12>(random << 2);
                        const auto JALR = concat(slice<11,0>(imm), cast<register_index_t>(ABI::t1), cast<Funct3>(0b_000), cast<register_index_t>(ABI::zero), cast<uint7>(0b_1100111));
                        const auto LB   = concat(slice<11,0>(imm), cast<register_index_t>(ABI::sp), cast<Funct3>(0b_000), cast<register_index_t>(ABI::t0), cast<uint7>(0b_0000011));
                        const auto LW   = concat(slice<11,0>(imm), cast<register_index_t>(ABI::a4), cast<Funct3>(0b_010), cast<register_index_t>(ABI::t1), cast<uint7>(0b_0000011));
                        const auto SW   = concat(slice<11,5>(imm), cast<register_index_t>(ABI::t1), cast<register_index_t>(ABI::a4), cast<Funct3>(0b_010), slice<4,0>(imm), cast<uint7>(0b_0100011));

                        // In addition to asynchronous fetch that stalls the pipeline, specific instruction can cause following stalls:
                        // JALR: branch mis-prediction, invalid address exception
                        // LB:   micro-op, invalid access exception
                        // LW:   micro-op, asynchronous mmio load result, invalid access exception
                        // SW:   invalid access exception
                        const auto value = mux(choice, LB, JALR, LW, SW);
                        external_mem[hid][addr] = value;

                        assert(reinterpret_cast<uint_t>(read_instr(hid, addr)) == reinterpret_cast<uint_t>(value));
                    });
                });

                core.initialize({});

                pipelined_for(iter, [](decltype(iter) tid)
                {
                    core.pipeline();
                });
            }
    }

    StressTest<1> stressTest1;
    StressTest<2> stressTest2;
    StressTest<4> stressTest4;

    void StressTests(bool trace)
    {
        print("Running external fetch stress test\n");
        stressTest1.run(0x5000, trace);
        stressTest2.run(0x5000, trace);
        stressTest4.run(0x5000, trace);
    }

    void RunTests()
    {
        const auto inputs = 6;

        const int_t[inputs] input = {uninit_int, -2, -1, 0, 1, 2};

        unit::test<1>([input, inputs](unit::tag_t tag)
        {
            for (const auto reg : 1 << bitsizeof register_index_t)
            {
                for (const auto i : inputs)
                {
                    DecoderTests(reg, input[i]);
                }
            }
        });

        unit::test<2>([input, inputs](unit::tag_t tag)
        {
            imem_addr_t pc = 0x1234;

            for (const auto i : inputs)
            {
                for (const auto j : inputs)
                {
                    for (const auto k : inputs)
                    {
                        int_t op1 = input[i];
                        int_t op2 = input[j];
                        int_t imm = input[k];

                        const auto check = [tag, op1, op2, imm, pc](Opcode opcode, auto expected_in)
                        {
                            // Only use lower 32-bits of the input expected value
                            int_t expected = static_cast(expected_in);

                            auto result = ComputeTests(opcode, op1, op2, imm, pc);
                            if (result != expected)
                            {
                                print("Expected:  {expected}  got:  {result} {opcode} {op1} {op2} {imm} {pc}\n");
                                unit::assert(tag, false);
                            }
                        };

                        check(Opcode::LUI,    imm);
                        check(Opcode::AUIPC,  ((pc << 1) + (imm >> 1)) << 1);
                        check(Opcode::OR,     op1 | op2);
                        check(Opcode::XOR,    op1 ^ op2);
                        check(Opcode::AND,    op1 & op2);
                        check(Opcode::ADD,    op1 + op2);
                        check(Opcode::SUB,    op1 - op2);
                        check(Opcode::SLL,    op1 << cast<uint5>(op2));
                        check(Opcode::SRA,    op1 >> cast<uint5>(op2));
                        check(Opcode::SRL,    cast<uint_t>(op1) >> cast<uint5>(op2));
                        check(Opcode::SLT,    cast<uint1>(op1 < op2));
                        check(Opcode::SLTU,   cast<uint1>(cast<uint_t>(op1) < cast<uint_t>(op2)));
                        check(Opcode::MUL,    op1 * op2);
                        check(Opcode::MULH,   (op1 * op2) >> bitsizeof int_t);
                        check(Opcode::MULHSU, (op1 * cast<uint_t>(op2)) >> bitsizeof int_t);
                        check(Opcode::MULHU,  (cast<uint_t>(op1) * cast<uint_t>(op2)) >> bitsizeof int_t);
                        check(Opcode::EXT,    (op1 * 2) | (op2 / 2));

                        pc++;
                    }
                }
            }
        });

        unit::test<3>([input, inputs](unit::tag_t tag)
        {
            for (const auto i : inputs)
            {
                for (const auto j : inputs)
                {
                    int_t op1 = input[i];
                    int_t op2 = input[j];

                    const auto check = [tag, op1, op2](Cond cond, bool expected)
                    {
                        auto result = BranchConditionTests(cond, op1, op2);
                        if (result != expected)
                        {
                            print("Expected:  {expected}  got:  {result} {cond} {op1} {op2}\n");
                            unit::assert(tag, false);
                        }
                    };

                    check(Cond::EQ,  op1 == op2);
                    check(Cond::NE,  op1 != op2);
                    check(Cond::LT,  op1 < op2);
                    check(Cond::GE,  op1 >= op2);
                    check(Cond::LTU, cast<uint_t>(op1) < cast<uint_t>(op2));
                    check(Cond::GEU, cast<uint_t>(op1) >= cast<uint_t>(op2));
                }
            }
        });

        unit::test<4>([](unit::tag_t tag)
        {
            FetchMicroOpTests();
        });

        unit::test<5>([](unit::tag_t tag)
        {
            StressTests(false);
        });

        unit::test<6>(DisasmTest);

        ProgramTests();
    }
}

inline void test_main()
{
    static Tests rv32i;

    rv32i.RunTests();
}
