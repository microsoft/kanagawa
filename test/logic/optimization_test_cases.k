// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.array
import data.counter
import data.random.lfsr

import test.unit as unit
import test.runner

inline uint32 GetRandomValue(uint32 tid)
{
    static lfsr_random<32> _lfsr;

    return _lfsr.next(make_optional<uint32>(tid == 0, 1));
}

// Test FifoEncode optimization
inline void TestFifoEncode(unit::tag_t tag)
{
    pipelined_for (32, [tag](uint32 tid)
    {
        // x is known to be constant in this basic block
        uint24 x = 4;

        // y and z are known to be equal in this basic block
        uint32 y = tid;
        uint32 z = y;

        // a and b are known to be equal in this basic block. In this test case,
        // they are not yet initialized ('x in SystemVerilog simulation).
        static uint32 uninitialized_variable;
        uint32 a = uninitialized_variable;
        uint32 b = uninitialized_variable;

        // For the fifo at the entry of this loop
        // There is no need to store x.
        // There is no need to store both y and z.
        // There is no need to store both a and b.
        for (const auto i : tid)
        {
            x++;
            y += 2;
            z += 3;
        }

        print("{x} {y} {z} {a} {b} {tid}\n");

        unit::assert(tag, x == (4 + tid));
        unit::assert(tag, y == (tid + (tid * 2)));
        unit::assert(tag, z == (tid + (tid * 3)));
    });
}

// Decomposing a wide select operation into smaller ones
inline void DecomposeWideSelect(unit::tag_t tag)
{
    pipelined_for(16, [tag](index_t<16> tid)
    {
        uint32[16] ary;

        static for (const auto i : 16)
        {
            ary[i] = tid * i;
        }

        uint32 actual = ary[tid];

        uint32[16] expected_results =
        {
            0x00000000,
            0x00000001,
            0x00000004,
            0x00000009,
            0x00000010,
            0x00000019,
            0x00000024,
            0x00000031,
            0x00000040,
            0x00000051,
            0x00000064,
            0x00000079,
            0x00000090,
            0x000000a9,
            0x000000c4,
            0x000000e1
        };

        uint32 expected = expected_results[tid];

        unit::assert(tag, actual == expected);
    });
}

// Algebraic identies for op(x, 0) and op(0, x)
inline void TestZero(unit::tag_t tag)
{
    pipelined_for(128, [tag](index_t<128> tid)
    {
        uint32 x = GetRandomValue(tid);

        // Test sign extension
        int32 y = x - 100;
        int64 z = y;

        // op(x, 0) == x
        unit::assert(tag, x == (x | 0));
        unit::assert(tag, z == (y | 0));
        unit::assert(tag, x == (x ^ 0));
        unit::assert(tag, z == (y ^ 0));
        unit::assert(tag, x == (x + 0));
        unit::assert(tag, z == (y + 0));
        unit::assert(tag, x == (x - 0));
        unit::assert(tag, z == (y - 0));
        unit::assert(tag, x == (x << 0));
        unit::assert(tag, z == (y << 0));
        unit::assert(tag, x == (x >> 0));
        unit::assert(tag, z == (y >> 0));

        // op(0, x) == x
        unit::assert(tag, x == (0 | x));
        unit::assert(tag, z == (0 | y));
        unit::assert(tag, x == (0 ^ x));
        unit::assert(tag, z == (0 ^ y));
        unit::assert(tag, x == (0 + x));
        unit::assert(tag, z == (0 + y));

        // op(x, 0) == 0
        unit::assert(tag, 0 == (x & 0));
        unit::assert(tag, 0 == cast<int64>(y & 0));
        unit::assert(tag, 0 == lutmul(x, 0));
        unit::assert(tag, 0 == cast<int64>(lutmul(y, 0)));

        // op(0, x) == 0
        unit::assert(tag, 0 == (0 & x));
        unit::assert(tag, 0 == cast<int64>(0 & y));
        unit::assert(tag, 0 == (0 << cast<uint8>(x)));
        unit::assert(tag, 0 == cast<int64>(0 << cast<int8>(y)));
        unit::assert(tag, 0 == (0 >> x));
        unit::assert(tag, 0 == cast<int64>(0 >> y));
        unit::assert(tag, 0 == lutmul(0, x));
        unit::assert(tag, 0 == cast<int64>(lutmul(0, y)));
    });
}

// Algebraic identities for op(x, 1) and op(1, x)
class TestOne
{
public:
    noinline void run(unit::tag_t tag)
    {
        pipelined_for(128, [tag](index_t<128> tid)
        {
            uint32 x = GetRandomValue(tid);

            // Test sign extension
            int32 y = x - 100;
            int64 z = y;

            unit::assert(tag, x == lutmul(x, 1));
            unit::assert(tag, z == lutmul(y, 1));
            unit::assert(tag, x == lutmul(1, x));
            unit::assert(tag, z == lutmul(1, y));
        });
    }
}

// Algebraqic identities for op(false, x) and op(x, false)
inline void TestFalse(unit::tag_t tag)
{
    pipelined_for(2, [tag](uint1 x)
    {
        bool b = cast<bool>(x);
        bool z;
        z = false || b;
        unit::assert(tag, z == b);
        z = false ^^ b;
        unit::assert(tag, z == b);
        z = false && b;
        unit::assert(tag, z == false);
        z = b || false;
        unit::assert(tag, z == b);
        z = b ^^ false;
        unit::assert(tag, z == b);
        z = b && false;
        unit::assert(tag, z == false);
    });
}

// Algebraqic identities for op(true, x) and op(x, true)
inline void TestTrue(unit::tag_t tag)
{
    pipelined_for(2, [tag](uint1 x)
    {
        bool b = cast<bool>(x);
        bool z;
        z = true || b;
        unit::assert(tag, z == true);
        z = true ^^ b;
        unit::assert(tag, z == !b);
        z = true && b;
        unit::assert(tag, z == b);
        z = b || true;
        unit::assert(tag, z == true);
        z = b ^^ true;
        unit::assert(tag, z == !b);
        z = b && true;
        unit::assert(tag, z == b);
    });
}

// Algebraic identities for op(x, all 1s) and op(1, all 1s)
class TestAllOne
{
public:
    noinline void run(unit::tag_t tag)
    {
        pipelined_for(128, [tag](uint32 tid)
        {
            const uint32 x = GetRandomValue(tid);

            const uint16 allOnes16 = 0xffff;
            const uint32 allOnes32 = 0xffffffff;
            const uint64 allOnes64 = 0xffffffffffffffff;

            // x and 1s match bit width
            unit::assert(tag, x == (x & allOnes32));
            unit::assert(tag, allOnes32 == (x | allOnes32));
            unit::assert(tag, ~x == (x ^ allOnes32));
            unit::assert(tag, x == (allOnes32 & x));
            unit::assert(tag, allOnes32 == (allOnes32 | x));
            unit::assert(tag, ~x == (allOnes32 ^ x));

            // x is more bits than 1s
            uint16 x_hi = x >> 16;
            uint16 x_lo = x & 0xffff;
            unit::assert(tag, x_lo == (x & allOnes16));
            unit::assert(tag, x_lo == (allOnes16 & x));
            unit::assert(tag, concat(x_hi, allOnes16) == (x | allOnes16));
            unit::assert(tag, concat(x_hi, allOnes16) == (allOnes16 | x));
            unit::assert(tag, concat(x_hi, ~x_lo) == (x ^ allOnes16));
            unit::assert(tag, concat(x_hi, ~x_lo) == (allOnes16 ^ x));

            // x is less bits than 1s
            unit::assert(tag, x == (x & allOnes64));
            unit::assert(tag, x == (allOnes64 & x));
            unit::assert(tag, allOnes64 == (x | allOnes64));
            unit::assert(tag, allOnes64 == (allOnes64 | x));
            unit::assert(tag, concat(allOnes32, ~x) == (x ^ allOnes64));
            unit::assert(tag, concat(allOnes32, ~x) == (allOnes64 ^ x));

            // Test sign extension
            int32 allOnesSigned32 = cast<int32>(allOnes32);
            int64 allOnesSigned64 = cast<int64>(allOnes64);
            int32 y = x - 100;
            int64 z = y;
            unit::assert(tag, z == (y & allOnesSigned32));
            unit::assert(tag, z == (allOnesSigned32 & y));
            unit::assert(tag, allOnesSigned64 == (y | allOnesSigned32));
            unit::assert(tag, allOnesSigned64 == (allOnesSigned32 | y));
            unit::assert(tag, ~z == (y ^ allOnesSigned32));
            unit::assert(tag, ~z == (allOnesSigned32 ^ y));

            // Signed allOnes that is fewer bits than other source
            int1 allOnesSigned1 = 1;
            unit::assert(tag, z == (y & allOnesSigned1));
            unit::assert(tag, z == (allOnesSigned1 & y));
            unit::assert(tag, allOnesSigned64 == (y | allOnesSigned1));
            unit::assert(tag, allOnesSigned64 == (allOnesSigned1 | y));
            unit::assert(tag, ~z == (y ^ allOnesSigned1));
            unit::assert(tag, ~z == (allOnesSigned1 ^ y));

        });
    }
}

inline void TestSelect(unit::tag_t tag)
{
    pipelined_for(128, [tag](uint32 tid)
    {
        const uint32 x = GetRandomValue(tid);

        uint32 y;

        // Known select
        unit::assert(tag, x == mux(cast<uint2>(0), x, 0, 0, 0));
        unit::assert(tag, x == mux(cast<uint2>(1), 0, x, 0, 0));
        unit::assert(tag, x == mux(cast<uint2>(2), 0, 0, x, 0));
        unit::assert(tag, x == mux(cast<uint2>(3), 0, 0, 0, x));

        for (const uint2 sel : 4)
        {
            // Choices are all same register
            unit::assert(tag, x == mux(sel, x, x, x, x));

            // Test sign extension
            int32 y = x - 100;
            int64 z = y;
            unit::assert(tag, z == mux(sel, y, y, y, y));
        }
    });

    for (const uint2 sel : 4)
    {
        // Choices are all the same literal
        const uint32 val = 15;
        unit::assert(tag, val == mux(sel, val, val, val, val));
        // Choices are all the same value with different bit widths
        unit::assert(tag, 1 == mux(sel, cast<uint1>(1), cast<uint2>(1), cast<uint8>(1), cast<uint16>(1)));
        // Choices are all the same value after widen with sign extension
        unit::assert(tag, cast<int16>(0xffff) == mux(sel, cast<int1>(1), cast<int2>(3), cast<int8>(0xff), cast<int16>(0xffff)));
        // Choices == sel
        unit::assert(tag, sel == mux(sel, 0, 1, 2, 3));
        // Choices == ~sel
        unit::assert(tag, ~sel == mux(sel, 3, 2, 1, 0));
        // signed choices == sel
        unit::assert(tag, sel == mux(sel, cast<int8>(0), cast<int8>(1), cast<int8>(2), cast<int8>(3)));
        // signed choices == ~sel
        unit::assert(tag, cast<int2>(~sel) == mux(sel, cast<int2>(3), cast<int2>(2), cast<int2>(1), cast<int2>(0)));
    }
}

inline void ConstProp(unit::tag_t tag)
{
    const int32 a = 0xbeafcafe;
    const int32 b = -1;

    // Unary ops
    int32 y = ~a;
    unit::assert(tag, y == 0x41503501);

    int32 z = -a;
    unit::assert(tag, z == 0x41503502);

    // Binary op
    int32 w = a + b;
    unit::assert(tag, cast<uint32>(w) == 0xbeafcafd);

    // cast
    struct S
    {
        uint16 a;
        uint8 b;
        uint8 c;
    }

    S s = { 0xb3a8, 0xd5, 0x28 };

    uint32 u = cast<uint32>(s);
    unit::assert(tag, u == 0x28d5b3a8);

    S s2 = cast<S>(u);
    unit::assert(tag, s2.a == 0xb3a8);
    unit::assert(tag, s2.b == 0xd5);
    unit::assert(tag, s2.c == 0x28);

    // cast with signed values
    struct SignedStruct
    {
        int8 x;
        int8 y;
    }

    SignedStruct ss = { -2, -7 };
    uint16 ss16 = cast<uint16>(ss);

    unit::assert(tag, ss16 == 0xF9FE);

    SignedStruct ss2 = cast<SignedStruct>(ss16);
    unit::assert(tag, ss2.x == -2);
    unit::assert(tag, ss2.y == -7);
}

// Repro for a bug where select index is a signed integer (out of bounds)
// that constant propgation later determines is known at compile time
class SelectConstIndexSize
{
private:
    noinline uint32 Helper(uint32[8] data)
    {
        auto a = 12;
        auto b = 2;

        // Index = 10
        // wrapping should return element[2]
        return data[a - b];
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 5 == Helper({3, 4, 5, 6, 7, 8, 9, 10}));
    }
}

// Test optimization that narrows literals
inline void TestCompareShift(unit::tag_t tag)
{
    pipelined_for(4, [tag](index_t<4> tid)
    {
        const uint16 Threshold = 8;

        uint32 x = tid * 127;

        // The type of 1 << Threshold is very wide
        // however, the actual value can be represented with fewer bits
        bool actual = x < (1 << Threshold);

        bool expected = mux(tid, true, true, true, false);

        unit::assert(tag, actual == expected);
    });

    pipelined_for(4, [tag](index_t<4> tid)
    {
        const uint16 Threshold = 8;

        int32 x = tid * -127;

        // The type of 1 << Threshold is very wide
        // however, the actual value can be represented with fewer bits
        bool actual = x < (-1 << Threshold);

        bool expected = mux(tid, false, false, false, true);

        unit::assert(tag, actual == expected);
    });

    // signed, positive value
    pipelined_for(4, [tag](index_t<4> tid)
    {
        const uint16 Threshold = 8;

        int32 x = tid * 127;

        // The type of 1 << Threshold is very wide
        // however, the actual value can be represented with fewer bits
        bool actual = x < (cast<int2>(1) << Threshold);

        bool expected = mux(tid, true, true, true, false);

        unit::assert(tag, actual == expected);
    });
}

// Test for the optimization that removes
// per-function semaphores when the max thread limit cannot be possibly exceeded
class RemoveSemaphoreTest
{
private:
    counter<512, 0> threads_in_flight;

    [[max_threads(32)]] noinline void SingleDeepPipeline(uint32 x)
    {
        atomic
        {
            assert(threads_in_flight.count() <= 32);
            threads_in_flight.add(1);
        }
        barrier;

        uint32 result = x;

        // This will result in a pipeline that is deeper than the maximum thread count
        static for (const auto i : 48)
        {
            result++;
            stages<1>();
        }

        assert(result == (x + 48));

        barrier;
        threads_in_flight.decrement();
    }

    // Function with limit thread count has a context saver
    noinline uint32 Inner(uint32 y)
    {
        uint32 result = y;

        // This will result in a pipeline that is deeper than the maximum thread count
        static for (const auto i : 48)
        {
            result++;
            stages<1>();
        }

        assert(result == (y + 48));

        return result;
    }

    [[max_threads(16)]] noinline void Outer(uint32 x)
    {
        atomic
        {
            assert(threads_in_flight.count() <= 16);
            threads_in_flight.add(1);
        }
        barrier;

        uint32 actual = Inner(x);
        assert(actual == (x + 48));

        barrier;
        threads_in_flight.decrement();
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(128, SingleDeepPipeline);

        pipelined_for(128, Outer);
    }
}

inline void LiteralGlobalWriteTest(unit::tag_t tag)
{
    static uint32 _x;
    static int32 _y; // test for sign extension

    for (const auto i : 2)
    {
        _x = 4;
        unit::assert(tag, _x == 4);

        _y = -1;
        unit::assert(tag, cast<uint32>(_y) == 0xffffffff);

        if (i == 1)
        {
            _x = 5;
            unit::assert(tag, _x == 5);

            _y = -2;
            unit::assert(tag, cast<uint32>(_y) == 0xfffffffe);
        }
    }
}

// Test for individual bits being constant
class BitLevelConstantProp
{
private:
    bool _bitLevelConstantPropGate = false;

public:
    void run(unit::tag_t tag)
    {
        _bitLevelConstantPropGate = true;

        pipelined_for(8, [tag](index_t<8> tid)
        {
            // the LSB is known to be 0
            uint32 x = (tid ^ 0xdeadbeef) & (0xfffffffe);

            atomic do; while(!_bitLevelConstantPropGate);

            uint32 expected = mux(tid, 0xdeadbeee, 0xdeadbeee, 0xdeadbeec, 0xdeadbeec, 0xdeadbeea, 0xdeadbeea, 0xdeadbee8, 0xdeadbee8);

            unit::assert(tag, x == expected);
        });
    }
}

inline void BitLevelConstantPropLoop(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> tid)
    {
        // Many bits are known at compile time
        uint32 x = (tid ^ 0xdeadbeef) & (0xfffffffe);

        uint4 iteration = 0;

        do
        {
            x = (x ^ iteration) & 0xffffeffe;
            iteration++;

        } while(iteration < 4);

        uint32 expected = mux(tid, 0xdeadaeee, 0xdeadaeee, 0xdeadaeec, 0xdeadaeec, 0xdeadaeea, 0xdeadaeea, 0xdeadaee8, 0xdeadaee8);

        unit::assert(tag, x == expected);
    });
}

// Test for optimization that transforms (x & '1) into x
// Cases that should not be optimized
inline void AndOneTest(unit::tag_t tag)
{
    pipelined_for(2, [tag](uint1 i)
    {
        uint1 x = 1;
        int1 y = cast<int1>(i);
        uint4 actual = x & y;
        uint4 expected = i == 0 ? 0 : 1;
        unit::assert(tag, actual == expected);
    });

    // Swap operand order
    pipelined_for(2, [tag](uint1 i)
    {
        uint1 x = 1;
        int1 y = cast<int1>(i);
        uint4 actual = y & x;
        uint4 expected = i == 0 ? 0 : 1;
        unit::assert(tag, actual == expected);
    });
}

inline void TestLutConstantProp(unit::tag_t tag)
{
    uint32[8] result = pipelined_map<8>(8, [](index_t<8> i)
    {
        uint32 x = (i << 2) + 1;

        return x;
    });

    static for (const auto i : 8)
    {
        unit::assert(tag, result[i] == (i * 4) + 1);
    }
}

// Optimization that removes unused operands from gather
inline void TestReduceCastWidth(unit::tag_t tag)
{
    uint4[8] result = pipelined_map<8>(8, [](uint8 i)
    {
        struct S
        {
            uint4 a;
            uint4 b;
        };

        S s = { i, 0 };

        uint8 x = cast<uint8>(s);

        return cast<uint4>(x);
    });

    static for (const auto i : 8)
    {
        unit::assert(tag, result[i] == i);
    }
}

// Optimizations that detect shift by a constant shift amount
// which end up shifting out all bits
inline void TestShiftAllBits(unit::tag_t tag)
{
    pipelined_for(8, [tag](uint3 i)
    {
        uint3 shlResult = i << 3;
        unit::assert(tag, 0 == shlResult);

        unit::assert(tag, 0 == (i >> 3));

        int3 signedI = i;
        const int3 expected = signedI < 0 ? -1 : 0;

        unit::assert(tag, expected == (signedI >> 3));
    });
}

// Regression test for a case
// where an inline assigns a value
// to a parameter
// and that function is called inside of multiple conditionals
inline void ConditionalAssignToParam(unit::tag_t tag)
{
    inline uint32 helper(uint32 a)
    {
        a = 3;

        return a;
    }

    for (const auto i : 4)
    {
        bool a = (i < 3);
        bool b = (i < 4);

        if (a)
        {
            if (b)
            {
                uint32 result = helper(i);

                unit::assert_equal(tag, 3, result);
            }
        }
    }
}

inline void DoubleSelect(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> i)
    {
        uint32[8] a1 = { i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7 };
        uint32 b = a1[i];

        uint32[8] a2 = { i + 10, ~cast<uint32>(i), i & 7, b, i + 14, i + 15, i + 16, i + 17 };
        uint32 c = a2[i];

        uint32 expected = mux(i, 10, 0xfffffffe, 2, 6, 18, 20, 22, 24);

        unit::assert_equal(tag, expected, c);
    });
}

inline void ArrayWriteThenRead(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> i)
    {
        uint32[8] a = {};

        a[i] = i + 1;

        uint32 b = a[i];

        unit::assert_equal(tag, i + 1, b);
    });
}

inline void AddToOr(unit::tag_t tag)
{
    pipelined_for(16, [tag](index_t<16> i)
    {
        static for (const auto j : 16)
        {
            uint32 x1 = (i * 16) + j;
            uint32 x2 = j + (i * 16);
            uint32 y = (i * 16) | j;

            unit::assert_equal(tag, y, x1);
            unit::assert_equal(tag, y, x2);
        }
    });

    pipelined_for(16, [tag](index_t<16> i)
    {
        uint8 actual = (i * 16) + i;
        uint8 expected = mux(i,
            0b00000000,
            0b00010001,
            0b00100010,
            0b00110011,
            0b01000100,
            0b01010101,
            0b01100110,
            0b01110111,
            0b10001000,
            0b10011001,
            0b10101010,
            0b10111011,
            0b11001100,
            0b11011101,
            0b11101110,
            0b11111111);
        unit::assert_equal(tag, expected, actual);
    });
}

inline void ConstSelectBits(unit::tag_t tag)
{
    pipelined_for(16, [tag](index_t<16> outer)
    {
        pipelined_for(64, [tag, outer](index_t<64> i)
        {
            uint32[64] src_data = map_indices([outer](index_t<64> i)
            {
                return static_cast<uint32>(i + outer);
            });

            index_t<64> select_index = (i << 3) | 0b101;

            uint32 result = src_data[select_index];

            uint32 expected = select_index + outer;

            unit::assert_equal(tag, expected, result);
        });
    });
}

// To test the optimization which removes unused shared variables
class TwoUseSite
{
private:
    uint32 _value = 0;

    void foo()
    {
        atomic
        {
            // If no other code were to access _value, then
            // this read-modify-write would be optimized away
            _value++;
        }
    }

    uint32 bar()
    {
        return _value;
    }
public:
    void run(unit::tag_t tag)
    {
        foo();

        unit::assert_equal(tag, 1, bar());
    }
}

inline void test_main()
{
    unit::test<1>(TestFifoEncode);
    unit::test<2>(DecomposeWideSelect);
    unit::test<3>(TestZero);
    unit::test<4>(unit::fixture<TestOne>());
    unit::test<5>(TestFalse);
    unit::test<6>(TestTrue);
    unit::test<7>(unit::fixture<TestAllOne>());
    unit::test<8>(TestSelect);
    unit::test<9>(ConstProp);
    unit::test<10>(unit::fixture<SelectConstIndexSize>());
    unit::test<11>(TestCompareShift);
    unit::test<12>(unit::fixture<RemoveSemaphoreTest>());
    unit::test<13>(LiteralGlobalWriteTest);
    unit::test<14>(unit::fixture<BitLevelConstantProp>());
    unit::test<14>(BitLevelConstantPropLoop);
    unit::test<15>(AndOneTest);
    unit::test<16>(TestLutConstantProp);
    unit::test<17>(TestReduceCastWidth);
    unit::test<18>(TestShiftAllBits);
    unit::test<19>(ConditionalAssignToParam);
    unit::test<20>(DoubleSelect);
    unit::test<21>(ArrayWriteThenRead);
    unit::test<22>(AddToOr);
    unit::test<23>(ConstSelectBits);
    unit::test<24>(unit::fixture<TwoUseSite>());
}
