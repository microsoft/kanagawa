// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import helper.external_class_template
import test.unit as unit
import test.runner

template<auto Width, auto C0, auto C1 = 0, auto Operation = "Add">
class AluTemplate
{
public:
    uint<Width> Exec(uint<Width> a);

    [[latency(1)]] uint<Width> ExecFixed(uint<Width> a);
}

extern AluTemplate;

inline void TestAdd(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 i)
    {
        static AluTemplate<32, 6> _alu;

        uint32 result = _alu.Exec(i);

        unit::assert_equal(tag, i + 6, result);
    });
}

inline void TestAddFixed(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 i)
    {
        static AluTemplate<32, 7> _alu;

        uint32 result = _alu.ExecFixed(i);

        unit::assert_equal(tag, i + 7, result);
    });
}

inline void TestMul(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 i)
    {
        static AluTemplate<32, 3, 0, "Mul"> _alu;

        uint32 result = _alu.Exec(i);

        unit::assert_equal(tag, i * 3, result);
    });
}

inline void TestMad(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 i)
    {
        static AluTemplate<32, 8, -3, "Mad"> _alu;

        uint32 result = _alu.Exec(i);

        unit::assert_equal(tag, cast<uint32>((i * 8) - 3), result);
    });
}

// Test for an external class declared in a module
inline void TestInvert(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint16 i)
    {
        static Inverter<0x0f0f> _invert;

        uint16 result = _invert.Invert(i);

        unit::assert_equal(tag, ~(cast<uint16>(i & 0x0f0f)), result);
    });
}

// Test for a class exported with [[name()]]
inline void TestNamedClassAdd(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 i)
    {
        static Computer<32, 6> _alu;

        uint32 result = _alu.Exec(i);

        unit::assert_equal(tag, i + 6, result);
    });
}

inline void TestNamedClassAddFixed(unit::tag_t tag)
{
    pipelined_for(32, [tag](uint32 i)
    {
        static Computer<32, 7> _alu;

        uint32 result = _alu.ExecFixed(i);

        unit::assert_equal(tag, i + 7, result);
    });
}

inline void test_main()
{
    unit::test<1>(TestAdd);
    unit::test<2>(TestAddFixed);
    unit::test<3>(TestMul);
    unit::test<4>(TestMad);
    unit::test<5>(TestInvert);
    unit::test<6>(TestNamedClassAdd);
    unit::test<7>(TestNamedClassAddFixed);
}
