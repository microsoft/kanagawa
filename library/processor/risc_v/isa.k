// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
RISC-V ISA types, constants and helper functions.
*/
module processor.risc_v.isa
    { // * RISC-V ISA definitions
      Base
    , Extension
    , Types
      // ** Instruction Subfields
    , RVG
    , Funct3
    , Funct7
    , Op
    , MulDiv
    , Branch
    , LoadStore
    , System
    , MiscMem
      // ** Base Instruction Formats
    , R_type
    , I_type
    , S_type
    , B_type
    , U_type
    , J_type
    , Instr
    , Format
    , Reg
      // ** Decoding
    , decode_format
      // ** Standard calling convention ABI
    , ABI
      // ** Helper functions
    , sign_extend
    }

//| Base ISA identifier
enum Base : uint2
{
    RV32I,
    RV32E,
    RV64I
}

//| Standard ISA extension identifier
enum Extension : uint1
{
    None = 0,
    M = 1
}

using Funct7 = uint7;

//| Data types for specified base ISA
template <Base ISA>
class Types
{
    const auto int_size  = ISA == Base::RV64I ? 64 : 32;
    const auto registers = ISA == Base::RV32E ? 16 : 32;

public:
    //| Signed XLEN bits wide integer
    using int_t = int<int_size>;

    //| Unsigned XLEN bits wide integer
    using uint_t = uint<int_size>;

    //| Register index
    using register_index_t = index_t<registers>;

    //| Decoded instruction fields
    struct Decoded
    {
        //| Destination register
        optional<register_index_t> rd;

        //| First operand register
        optional<register_index_t> rs1;

        //| Second operand register
        optional<register_index_t> rs2;

        //| Immediate operand (not valid for R-type instructions)
        int_t imm;

        //| funct7 field (valid only for R-type instructions)
        Funct7 funct7;
    }
}

//| General-purpose ISA major opcodes
enum RVG : uint5
{
// inst[4:2]   000         001         010          011        100         101         110         111
// inst[6:5]
/*   00   */   LOAD,       LOAD_FP,    custom_0,   MISC_MEM,   OP_IMM,     AUIPC,      OP_IMM_32,  long0,

/*   01   */   STORE,      STORE_FP,   custom_1,   AMO,        OP,         LUI,        OP_32,      long1,

/*   10   */   MADD,       MSUB,       NMSUB,      NMADD,      OP_FP,      reserved0,  custom_2,   long2,

/*   11   */   BRANCH,     JALR,       reserved1,  JAL,        SYSTEM,     reserved2,  custom_3,   long3
}

struct Opcode
{
    uint2   inst_1_0;
    RVG     opcode;
}

// Funct3 (minor opcode)
using Funct3 = uint3;

//| OP/OP_IMM minor opcodes
enum Op : Funct3
{
    ADD          = 0b_000,
    SLL          = 0b_001,
    SLT          = 0b_010,
    SLTU         = 0b_011,
    XOR          = 0b_100,
    SR           = 0b_101,
    OR           = 0b_110,
    AND          = 0b_111
}

//| "M" standard extension minor opcodes
enum MulDiv : Funct3
{
    MUL          = 0b_000,
    MULH         = 0b_001,
    MULHSU       = 0b_010,
    MULHU        = 0b_011,
    DIV          = 0b_100,
    DIVU         = 0b_101,
    REM          = 0b_110,
    REMU         = 0b_111
}

//| BRANCH minor opcodes
enum Branch : Funct3
{
    BEQ          = 0b_000,
    BNE          = 0b_001,
    BLT          = 0b_100,
    BGE          = 0b_101,
    BLTU         = 0b_110,
    BGEU         = 0b_111
}

//| LOAD/STORE minor opcodes
enum LoadStore : Funct3
{
    B            = 0b_000,
    H            = 0b_001,
    W            = 0b_010,
    BU           = 0b_100,
    HU           = 0b_101
}

//| SYSTEM minor opcodes
enum System : Funct3
{
    PRIV         = 0b_000,
    CSRRW        = 0b_001,
    CSRRS        = 0b_010,
    CSRRC        = 0b_011,
    CSRRWI       = 0b_101,
    CSRRSI       = 0b_110,
    CSRRCI       = 0b_111
}

//| MISC_MEM mino opcodes
enum MiscMem : Funct3
{
    FENCE        = 0b_000
}

using Reg = uint5;

//| Mnemonics for integer registers in the standard calling convention
enum ABI : Reg
{ zero = 0   //< Hard-wired zero
, ra   = 1   //< Return address
, sp   = 2   //< Stack pointer
, gp   = 3   //< Global pointer
, tp   = 4   //< Thread pointer
, t0   = 5   //< Temporary/alternate link register
, t1   = 6   //< Temporary
, t2   = 7   //< Temporary
, s0   = 8   //< Frame pointer
, s1   = 9   //< Saved register
, a0   = 10  //< Function arguments/return values
, a1   = 11  //< Function arguments/return values
, a2   = 12  //< Function arguments
, a3   = 13  //< Function arguments
, a4   = 14  //< Function arguments
, a5   = 15  //< Function arguments
, a6   = 16  //< Function arguments
, a7   = 17  //< Function arguments
, s2   = 18  //< Saved register
, s3   = 19  //< Saved register
, s4   = 20  //< Saved register
, s5   = 21  //< Saved register
, s6   = 22  //< Saved register
, s7   = 23  //< Saved register
, s8   = 24  //< Saved register
, s9   = 25  //< Saved register
, s10  = 26  //< Saved register
, s11  = 27  //< Saved register
, t3   = 28  //< Temporary
, t4   = 29  //< Temporary
, t5   = 30  //< Temporary
, t6   = 31  //< Temporary
}

//| Sign-extend an integer to XLEN bits
template <auto N>
inline int32 sign_extend(uint<N> x)
{
    return cast<int32>(cast<int<N>>(x));
}

//| R-type instruction format
// @@
//    31                 25 24         20 19    15 14      12 11                 7 6       0
//   +---------------------+-------------+--------+----------+--------------------+---------+
//   |        funct7       |     rs2     |  rs1   |  funct3  |         rd         | opcode  |
//   +---------------------+-------------+--------+----------+--------------------+---------+
// @@
struct R_type
{
    Opcode  opcode;
    Reg     rd;
    Funct3  funct3;
    Reg     rs1;
    Reg     rs2;
    Funct7  funct7;
}

//| I-type instruction format
// @@
//    31                               20 19    15 14      12 11                 7 6       0
//   +-----------------------------------+--------+----------+--------------------+---------+
//   |           imm[11:0]               |  rs1   |  funct3  |         rd         | opcode  |
//   +-----------------------------------+--------+----------+--------------------+---------+
// @@
struct I_type
{
    Opcode  opcode;
    Reg     rd;
    Funct3  funct3;
    Reg     rs1;
    uint12  imm_11_0;
}

inline int32 I_immediate(I_type i)
{
    return sign_extend(i.imm_11_0);
}

//| S-type instruction format
// @@
//    31                 25 24         20 19    15 14      12 11                 7 6       0
//   +---------------------+-------------+--------+----------+--------------------+---------+
//   |      imm[11:5]      |     rs2     |  rs1   |  funct3  |      imm[4:0]      | opcode  |
//   +---------------------+-------------+--------+----------+--------------------+---------+
// @@
struct S_type
{
    Opcode  opcode;
    uint5   imm_4_0;
    Funct3  funct3;
    Reg     rs1;
    Reg     rs2;
    uint7   imm_11_5;
}

inline int32 S_immediate(S_type s)
{
    return sign_extend(concat(s.imm_11_5, s.imm_4_0));
}

//| B-type instruction format
// @@
//    31        30       25 24         20 19    15 14      12 11       8         7 6       0
//   +---------+-----------+-------------+--------+----------+----------+---------+---------+
//   | imm[12] | imm[10:5] |     rs2     |  rs1   |  funct3  | imm[4:1] | imm[11] | opcode  |
//   +---------+-----------+-------------+--------+----------+----------+---------+---------+
// @@
struct B_type
{
    Opcode  opcode;
    uint1   imm_11;
    uint4   imm_4_1;
    Funct3  funct3;
    Reg     rs1;
    Reg     rs2;
    uint6   imm_10_5;
    uint1   imm_12;
}

inline int32 B_immediate(B_type b)
{
    const uint1 zero_0 = 0;
    return sign_extend(concat(b.imm_12, b.imm_11, b.imm_10_5, b.imm_4_1, zero_0));
}

//| U-type instruction format
// @@
//    31                                                   12 11                 7 6       0
//   +-------------------------------------------------------+--------------------+---------+
//   |                    imm[31:12]                         |         rd         | opcode  |
//   +-------------------------------------------------------+--------------------+---------+
// @@
struct U_type
{
    Opcode  opcode;
    Reg     rd;
    uint20  imm_31_12;
}

inline int32 U_immediate(U_type u)
{
    const uint12 zero_11_0 = 0;
    return sign_extend(concat(u.imm_31_12, zero_11_0));
}

//| J-type instruction format
// @@
//    31        30           21        20 19               12 11                 7 6       0
//   +---------+---------------+---------+-------------------+--------------------+---------+
//   | imm[20] |   imm[10:1]   | imm[11] |     imm[19:12]    |         rd         | opcode  |
//   +---------+---------------+---------+-------------------+--------------------+---------+
// @@
struct J_type
{
    Opcode  opcode;
    Reg     rd;
    uint8   imm_19_12;
    uint1   imm_11;
    uint10  imm_10_1;
    uint1   imm_20;
}

inline int32 J_immediate(J_type j)
{
    const uint1 zero_0 = 0;
    return sign_extend(concat(j.imm_20, j.imm_19_12, j.imm_11, j.imm_10_1, zero_0));
}

static assert(bitsizeof(R_type) == 32);
static assert(bitsizeof(I_type) == 32);
static assert(bitsizeof(S_type) == 32);
static assert(bitsizeof(B_type) == 32);
static assert(bitsizeof(U_type) == 32);
static assert(bitsizeof(J_type) == 32);

union Instr
{
    R_type r;
    I_type i;
    S_type s;
    B_type b;
    U_type u;
    J_type j;
}

static assert(bitsizeof(Instr) == 32);

enum Format : uint3
{
    R, I, S, B, U, J, Invalid
}

//| Decode instruction word in given instruction format
template <Base ISA>
inline auto decode_format
    ( Instr instr   //< Instruction word
    , Format format //< Instruction format
    )
{
    Types<ISA>::Decoded decoded;

    static assert(bitoffsetof(R_type, rd) == bitoffsetof(I_type, rd));
    static assert(bitoffsetof(R_type, rd) == bitoffsetof(U_type, rd));
    static assert(bitoffsetof(R_type, rd) == bitoffsetof(J_type, rd));

    const bool rd_valid = format != Format::B && format != Format::S;
    decoded.rd = make_optional(rd_valid && instr.r.rd != ABI::zero, cast<Types<ISA>::register_index_t>(instr.r.rd));

    static assert(bitoffsetof(R_type, rs1) == bitoffsetof(I_type, rs1));
    static assert(bitoffsetof(R_type, rs1) == bitoffsetof(S_type, rs1));
    static assert(bitoffsetof(R_type, rs1) == bitoffsetof(B_type, rs1));

    const bool rs1_valid = format == Format::R || format == Format::S || format == Format::B || format == Format::I;
    decoded.rs1 = make_optional(rs1_valid, cast<Types<ISA>::register_index_t>(instr.r.rs1));

    static assert(bitoffsetof(R_type, rs2) == bitoffsetof(S_type, rs2));
    static assert(bitoffsetof(R_type, rs2) == bitoffsetof(B_type, rs2));

    const bool rs2_valid = format == Format::R || format == Format::S || format == Format::B;
    decoded.rs2 = make_optional(rs2_valid, cast<Types<ISA>::register_index_t>(instr.r.rs2));

    const auto dont_care = I_immediate(instr.i);

    decoded.imm = mux(format,
            dont_care,
            I_immediate(instr.i),
            S_immediate(instr.s),
            B_immediate(instr.b),
            U_immediate(instr.u),
            J_immediate(instr.j),
            dont_care,
            dont_care);

    decoded.funct7 = instr.r.funct7;

    return decoded;
}

