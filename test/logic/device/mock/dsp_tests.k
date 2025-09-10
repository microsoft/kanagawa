// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module dsp_tests
    { DSPTestCases
    }

import test.unit as unit

inline void CompareFloat(unit::tag_t tag, float32 actual, uint32 expected)
{
    uint32 result = cast<uint32>(actual);

    unit::assert_equal(tag, expected, result);
}

// repro for DSP output register scheduling bug
struct DecodedFP16
{
    uint27 mantissa;
    uint1 exponent; // 0 means the exponent is -14.  1 means the exponent is 3
}

inline void MultiplyFP16ToFixed(unit::tag_t tag, DecodedFP16 a, DecodedFP16 b)
{
    // Multiply mantissas
    uint54 product = umul27(a.mantissa, b.mantissa);

    // Shift of mantissa bits to convert to 16.48
    // Choose from among 3 possible shift values

    // Each FP mantissa is a fixed point number with 10 fractional bits
    // so the product has 20 fractional bits.
    // The 64-bit accumulator has c_fixedPrecision fractional bits
    //
    // Also computed is a mask that determines if the shift will overflow
    // It is set computed with:
    // const uint32_t maxMantissaProductBitsSet = 63 - shiftAmount;
    // const uint64_t overflowMask = ~((1ull << maxMantissaProductBitsSet) - 1);
    uint2 shiftAmountIndex = a.exponent | (b.exponent << 1);

    // Values for shiftAmountIndex = 1 or 2
    int64 result = product << 17;

    if (shiftAmountIndex == 0)
    {
        result = product;
    }

    unit::assert(tag, result == product);
}

inline void TestCase4(unit::tag_t tag)
{
    DecodedFP16 a;
    DecodedFP16 b;

    a.mantissa = 810496;
    a.exponent = 0;
    b.mantissa = 3416;
    b.exponent = 0;

    MultiplyFP16ToFixed(tag, a, b);
}

inline void TestCase12(unit::tag_t tag)
{
    unit::assert(tag, 117 == umul27(13, 9));
}

// dsp expression that has a child dsp expression
inline void TestCase527(unit::tag_t tag)
{
    float x = 2.0;
    float y = 4.0;

    float a = 5.0;

    a += (x * y);

    unit::assert(tag, cast<uint32>(a) == cast<uint32>(13.0));
}

// literal floats - written as integers
const float32 f528 = cast<float32>(cast<uint32>(0x40200000)); // 2.5

inline void TestCase528(unit::tag_t tag)
{
    uint32 result = cast<uint32>(f528 + 1.0);

    unit::assert(tag, result == cast<uint32>(3.5));
}

// fmad32
class TestCase532
{
    float32[4] _results532;

    [[pipelined]] void Foo532(uint32 idx)
    {
        auto x = mux(cast<uint2>(idx), 2.0, 3.125, -2.5, 1.75);
        auto y = mux(cast<uint2>(idx), 5.0, 1.25, -3.0, -2.0);
        auto z = mux(cast<uint2>(idx), 4.0, 2.125, 1.5, -1.0);

        auto result = fmad32(x, y, z);

        _results532[idx] = result;
    }

public:
    inline void run(unit::tag_t tag)
    {
        static for (const auto x : 4)
        {
            _results532[x] = 0.0;
        }

        Foo532(4);

        CompareFloat(tag, _results532[0], 0x41b00000); // 22.0
        CompareFloat(tag, _results532[1], 0x40b90000); // 5.78125
        CompareFloat(tag, _results532[2], 0xc0e00000); // -7
        CompareFloat(tag, _results532[3], 0x40700000); // 3.75
    }
}

// Test for a dsp and memory reference in the rhs of a dynamic array write
// multiple dsps/memories should not be instantiated
class TestCase533
{
    [[memory]] float[256] biases533;

    float compute_hash_533(uint32 row)
    {
        float[128] acc = {};

        acc[row] = fadd32(1.0, biases533[row]);

        return acc[row];
    }

public:
    inline void run(unit::tag_t tag)
    {
        biases533[3] = 2.0;

        float result = compute_hash_533(3);

        unit::assert(tag, cast<uint32>(3.0) == cast<uint32>(result));
    }
}

// DSP used in a single-threaded basic block within a loop
// there was a bug with the optimization to use the enable bit in single-threaded functions
// which would cause the DSP output to change value when it should not
class TestCase536
{
    inline void Compare536(unit::tag_t tag, uint54 actual, uint54 expected)
    {
        unit::assert_equal(tag, expected, actual);
    }

    inline void Foo536(unit::tag_t tag, uint27 a)
    {
        uint54 expected = 0;

        for (const uint32 i : 4)
        {
            uint54 result = umul27(a, i);

            stages<4>();

            Compare536(tag, result, expected);

            expected += a;
        }
    }

    [[pipelined]] void Helper536(uint32 threadId, unit::tag_t tag)
    {
        Foo536(tag, threadId);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Helper536(16, tag);
    }
}

// unsigned lutmul
inline void TestCase537(unit::tag_t tag)
{
    range_for (0, 256, 11, [tag](uint32 a)
    {
        range_for (1, 256, 17, [tag, a](uint32 b)
        {
            uint32 actual = lutmul(a, b);

            uint32 expected = umul27(a, b);

            unit::assert_equal(tag, expected, actual);
        });
    });
}

// signed lutmul
inline void TestCase538(unit::tag_t tag)
{
    range_for (-256, 256, 13, [tag](int10 a)
    {
        range_for (-255, 256, 7, [tag, a](int10 b)
        {
            int20 actual = lutmul(a, b);

            int20 expected = imul27(a, b, false);

            unit::assert_equal(tag, expected, actual);
        });
    });
}

// returns getting scheduled too early (when a block has multiple call sites, some returns would be scheduled too soon)
class TestCase543
{
    float32 _val543 = 0.0;

    bool _isFirstCall543 = true;

    uint32 _dummy543 = 0;

    void UpdateVal543()
    {
        bool clearAccumulator;

        atomic
        {
            clearAccumulator = _isFirstCall543;

            _isFirstCall543 = false;
        }

        float32 result = fmac32(1.0, 1.0, !clearAccumulator);

        // in the bug, 1 return "instruction" would be scheduled here (before this unrolled loop)
        // waste time to make the bug easier to be hit
        static for(const auto i : 32)
        {
            _dummy543++;
        }

        _val543 = result;
    }

public:
    inline void run(unit::tag_t tag)
    {
        UpdateVal543();
        unit::assert(tag, cast<uint32>(_val543) == cast<uint32>(1.0));

        UpdateVal543();
        unit::assert(tag, cast<uint32>(_val543) == cast<uint32>(2.0));
    }
}

// Unsigned integer multiplication - comparing against a manual implementation
class TestCase561
{
    uint54[2] _561Operands;

    [[pipelined]] void Helper561(uint32 tid, unit::tag_t tag)
    {
        uint54[2] srcOperands;

        // Generate psuedo-random numbers
        atomic
        {
            if (0 == tid)
            {
                srcOperands[0] = 0xabcdef0123;
                srcOperands[1] = 0x98746543ab;
            }
            else
            {
                srcOperands[0] = _561Operands[0] + 75432797865;
                srcOperands[1] = _561Operands[1] - 1000159765413;
            }

            _561Operands = srcOperands;
        }

        // Manually compute expected result
        uint27[2] src0Parts = cast<uint27[2]>(srcOperands[0]);
        uint27[2] src1Parts = cast<uint27[2]>(srcOperands[1]);

        uint108 expected =
            (umul27(src0Parts[0], src1Parts[0]) << 0) +
            (umul27(src0Parts[0], src1Parts[1]) << 27) +
            (umul27(src0Parts[1], src1Parts[0]) << 27) +
            (umul27(src0Parts[1], src1Parts[1]) << 54);

        uint108 actual = srcOperands[0] * srcOperands[1];

        unit::assert_equal(tag, expected, actual);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Helper561(1024, tag);
    }
}

// Unsigned integer multiplication - wide * narrow
// comparing against a manual implementation
class TestCase562
{
    uint64[2] _562Operands;

    [[pipelined]] void Helper562(uint32 tid, unit::tag_t tag)
    {
        uint64[2] srcOperands;

        // Generate psuedo-random numbers
        atomic
        {
            if (0 == tid)
            {
                srcOperands[0] = 0xabcdef0123;
                srcOperands[1] = 0x98746543ab;
            }
            else
            {
                srcOperands[0] = _562Operands[0] + 75432797865;
                srcOperands[1] = _562Operands[1] - 1000159765413;
            }

            _562Operands = srcOperands;
        }

        // Only look at bottom few bits of operand 1
        uint64 src0 = srcOperands[0];
        uint17 src1 = srcOperands[1];

        // Manually compute expected result
        uint27[3] src0Parts = cast<uint27[3]>(cast<uint81>(src0));

        uint81 expected =
            (umul27(src0Parts[0], src1) << 0) +
            (umul27(src0Parts[1], src1) << 27) +
            (umul27(src0Parts[2], src1) << 54);

        uint81 actual = src0 * src1;

        unit::assert_equal(tag, expected, actual);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Helper562(1024, tag);
    }
}

// Predicated umul27
class TestCase254
{
public:
    bool _b254 = true;

    inline void run(unit::tag_t tag)
    {
        uint54 result = 8;

        if (_b254)
        {
            result = umul27(2, 6);
        }

        unit::assert(tag, result == 12);
    }
}

// Predicated umul27
class TestCase255
{
    bool _b255 = false;

public:
    inline void run(unit::tag_t tag)
    {
        uint54 result = 8;

        if (_b255)
        {
            result = umul27(2, 6);
        }

        unit::assert(tag, result == 8);
    }
}

class TestCase287
{
    // DSP input and output registers must be respected by logic/register ratio setting
    [[pipelined]] void Foo287(uint32 idx, unit::tag_t tag)
    {
        uint32 expected = idx << 2;

        uint32 actual = umul27(4, idx);

        unit::assert_equal(tag, expected, actual);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Foo287(10, tag);
    }
}

// DSP input and output registers must be respected by logic/register ratio setting
// This applies to both the input and the output
class TestCase289
{
    [[pipelined]] void Foo289(uint32 idx, unit::tag_t tag)
    {
        uint32 expected = (idx + 1) << 2;

        uint32 actual = umul27(4, idx + 1);

        unit::assert_equal(tag, expected, actual);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Foo289(10, tag);
    }
}

// imul27
inline void TestCase323(unit::tag_t tag)
{
    unit::assert(tag, 6 == imul27(2, 3, false));

    // Most negative * most negative
    unit::assert(tag, 4503599627370496 == imul27(-67108864, -67108864, false));

    // Most positive * most positive
    unit::assert(tag, 4503599493152769 == imul27(67108863, 67108863, false));

    // Most positive * most negative
    unit::assert(tag, -4503599560261632 == imul27(67108863, -67108864, false));

    // All of the above, with negate bit

    unit::assert(tag, -6 == imul27(2, 3, true));

    unit::assert(tag, -4503599627370496 == imul27(-67108864, -67108864, true));

    unit::assert(tag, -4503599493152769 == imul27(67108863, 67108863, true));

    unit::assert(tag, 4503599560261632 == imul27(67108863, -67108864, true));
}

// imul27 with signed 26-bit inputs (sign extension)
struct DecodedFP16_327
{
    int26 _signedMantissa;  // 25-bit unsigned mantissa, then sign bit is folded in
    uint1 _exponent;        // 0 means the exponent is -14.  1 means the exponent is 1
}

inline int64 MultiplyFloat16ToFixed_327(unit::tag_t tag, DecodedFP16_327 a, DecodedFP16_327 b)
{
    // Multiply signed mantissas
    int54 signedProduct = imul27(a._signedMantissa, b._signedMantissa, false);

    // Shift mantissa bits to convert to 16.48
    // Choose from among 3 possible shift values
    uint2 shiftIndex = a._exponent + b._exponent;

    int64 shifted15 = signedProduct << 15;
    int64 shifted30 = signedProduct << 30;

    unit::assert(tag, shiftIndex != 3);
    int64 result = mux(shiftIndex, signedProduct, shifted15, shifted30, 0);

    return result;
}

inline void TestCase327(unit::tag_t tag)
{
    DecodedFP16_327 a;
    a._signedMantissa = -7816;
    a._exponent = 1;

    DecodedFP16_327 b;
    b._signedMantissa = 9504;
    b._exponent = 1;

    int64 result = MultiplyFloat16ToFixed_327(tag, a, b);

    unit::assert(tag, result == -79761047380033536);
}

// Clocking of negate input
class TestCase342
{
    [[pipelined]] void Foo342(uint32 idx, unit::tag_t tag)
    {
        bool negate = (0 == (idx % 2));

        int54 expected = negate ? -(idx << 2) : idx << 2;

        int54 actual = imul27(4, idx, negate);

        unit::assert_equal(tag, expected, actual);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Foo342(10, tag);
    }
}

// fmul32
inline void TestCase380(unit::tag_t tag)
{
    float32 f = -7.125;

    float32 g = 2.0;

    float32 product = fmul32(f, g);

    float32 expected = -14.25;

    unit::assert(tag, cast<uint32>(product) == cast<uint32>(expected));
}

// fmul32 latency
class TestCase381
{
    float32[4] _products381;

    [[pipelined]] void Foo381(uint32 idx)
    {
        auto f = mux(cast<uint2>(idx), 2.0, 3.125, -2.5, 1.75);

        auto product = fmul32(2.0, f);

        _products381[idx] = product;
    }

public:
    void run(unit::tag_t tag)
    {
        static for (const auto x : 4)
        {
            _products381[x] = 0.0;
        }

        Foo381(4);

        CompareFloat(tag, _products381[0], 0x40800000);
        CompareFloat(tag, _products381[1], 0x40c80000);
        CompareFloat(tag, _products381[2], 0xc0a00000);
        CompareFloat(tag, _products381[3], 0x40600000);
    }
}

// fmac32
inline void TestCase382(unit::tag_t tag)
{
    float32 result = fmac32(2.5, -3.25, false);

    unit::assert(tag, cast<uint32>(result) == 0xc1020000);
}

// fmac32 latency
class TestCase383
{
    [[pipelined]] void Foo383(uint32 idx, unit::tag_t tag)
    {
        float32 a = mux(cast<uint2>(idx), 2.0, 3.125, -2.5, 1.75);
        float32 b = mux(cast<uint2>(idx), 0.5, 4.0, 8.0, 16.0);

        float32 result = fmac32(a, b, 1 == cast<uint1>(idx));

        float32 expected = mux(cast<uint2>(idx), 1.0, 13.5, -20.0, 8.0);

        unit::assert(tag, cast<uint32>(result) == cast<uint32>(expected));
    }

public:
    inline void run(unit::tag_t tag)
    {
        Foo383(4, tag);
    }
}

// fadd32
inline void TestCase385(unit::tag_t tag)
{
    float32 f = -7.125;

    float32 g = 2.0;

    float32 product = fadd32(f, g);

    float32 expected = -5.125;

    unit::assert(tag, cast<uint32>(product) == cast<uint32>(expected));
}

// fadd32 latency
class TestCase386
{
    float32[4] _sums386;

    [[pipelined]] void Foo386(uint32 idx)
    {
        auto f = mux(cast<uint2>(idx), 2.0, 3.125, -2.5, 1.75);

        auto sum = fadd32(2.0, f);

        _sums386[idx] = sum;
    }

public:
    void run(unit::tag_t tag)
    {
        static for (const auto x : 4)
        {
            _sums386[x] = 0.0;
        }

        Foo386(4);

        CompareFloat(tag, _sums386[0], 0x40800000);
        CompareFloat(tag, _sums386[1], 0x40a40000);
        CompareFloat(tag, _sums386[2], 0xbf000000);
        CompareFloat(tag, _sums386[3], 0x40700000);
    }
}

// fmul32 with denorm input
inline void TestCase387(unit::tag_t tag)
{
    float32 f = fmul32(cast<float32>(cast<uint32>(1)), 1.0);

    unit::assert(tag, cast<uint32>(f) == 0);
}

// fmul32 with denorm output
inline void TestCase388(unit::tag_t tag)
{
    // The smallest normalized number
    float32 smallestNorm = cast<float32>(cast<uint32>(0x00800000));

    float32 f = fmul32(smallestNorm, 0.75);

    unit::assert(tag, cast<uint32>(f) == 0);
}

// dsp writing directly into a memory - an extra pipeline stage is required because of the registered outputs and inputs of the operations
class TestCase399
{
    [[memory]] uint54[8] _mem399;

    inline void Foo399(uint32 idx, uint27 a, uint27 b)
    {
        _mem399[idx] = umul27(a, b);
    }

public:
    void run(unit::tag_t tag)
    {
        Foo399(3, 4, 5);

        unit::assert(tag, _mem399[3] == 20);
    }
}

// built-in float->float32 typedef
inline void TestCase408(unit::tag_t tag)
{
    float f = -7.125;

    float32 g = 2.0;

    float32 product = fmul32(f, g);

    float expected = -14.25;

    unit::assert(tag, cast<uint32>(product) == cast<uint32>(expected));
}

// fsub32
inline void TestCase420(unit::tag_t tag)
{
    float f = 3.5;

    float g = 1.25;

    float result = fsub32(f, g);

    unit::assert(tag, cast<uint32>(result) == cast<uint32>(2.25));
}

// testing fmac32 under predication
class TestCase477
{
    [[pipelined]] void Foo477(uint32 idx, unit::tag_t tag)
    {
        float32 a = mux(cast<uint2>(idx), 2.0, 3.125, -2.5, 1.75);
        float32 b = mux(cast<uint2>(idx), 0.5, 4.0, 8.0, 16.0);

        float32 result = 0.0;

        bool predicate = (0 == (idx % 2));

        if (predicate)
        {
            result = fmac32(a, b, 0 != idx);
        }

        float32 expected = mux(cast<uint2>(idx), 1.0, 0.0, -19.0, 0.0);

        print("{idx} {result} {expected}\n");
        unit::assert(tag, cast<uint32>(result) == cast<uint32>(expected));
    }

public:
    inline void run(unit::tag_t tag)
    {
        Foo477(4, tag);
    }
}

// repro of a bug with MovLatencyHolder
class TestCase480
{
    inline bool FloatLessThan480(float32 a, float32 b)
    {
        return cast<uint32>(a) < cast<uint32>(b);
    }

    [[pipelined]] void ReceiveBatch480(uint32 idx)
    {
        // Determine if this vector should be processed
        bool shouldProcess = true;

        bool isFinalVector = true;

        float lowestDistance480 = 1000.0;

        // Accumulate
        bool clearAccumulator = (0 == idx);

        float32 input = mux(cast<uint2>(idx), 10.0, -2.0, 4.0, -1.0 );

        float32 accumulatorValue = fmac32(1.0, input, !clearAccumulator);

        print("Accumulator value {accumulatorValue}\n");

        if (shouldProcess)
        {
            // Compare against the best so far
            atomic
            {
                // accumulatorValue represents the square of the difference
                print("Comparing {accumulatorValue} {lowestDistance480}\n");

                if (FloatLessThan480(accumulatorValue, lowestDistance480))
                {
                    print("Comparison successful\n");

                    lowestDistance480 = accumulatorValue;
                }
            }
        }

        if (isFinalVector)
        {
            // This query has gone around the treadmill
            // Return results
            print("Returning  {lowestDistance480}\n");
        }
    }

public:
    inline void run(unit::tag_t tag)
    {
        ReceiveBatch480(4);
    }
}

// double typedef
using TypeA483=float32;
using TypeB483=TypeA483;

inline void TestCase483(unit::tag_t tag)
{
    TypeB483 result = fmul32(2.0, 3.0);

    unit::assert(tag, cast<uint32>(result) == cast<uint32>(6.0));
}

// thread rate test
class TestCase490
{
    [[pipelined, thread_rate(3)]] void Foo490(uint32 threadIdx, unit::tag_t tag)
    {
        uint32 sum = 0;

        static for(const auto i : 10)
        {
            sum += threadIdx;
        }

        uint32 expected = umul27(threadIdx, 10);

        unit::assert_equal(tag, expected, sum);
    }

public:
    inline void run(unit::tag_t tag)
    {
        Foo490(100, tag);
    }
}


inline void DSPTestCases()
{
    unit::test<1, 0>(TestCase4);
    unit::test<2, 0>(TestCase12);
    unit::test<3, 0>(TestCase527);
    unit::test<4, 0>(TestCase528);
    unit::test<1, 1>(unit::fixture<TestCase532>());
    unit::test<2, 1>(unit::fixture<TestCase533>());
    unit::test<3, 1>(unit::fixture<TestCase536>());
    unit::test<4, 1>(TestCase537);
    unit::test<5, 1>(TestCase538);
    unit::test<6, 1>(unit::fixture<TestCase543>());
    unit::test<2, 2>(unit::fixture<TestCase561>());
    unit::test<3, 2>(unit::fixture<TestCase562>());
    unit::test<4, 2>(unit::fixture<TestCase254>());
    unit::test<5, 2>(unit::fixture<TestCase255>());
    unit::test<6, 2>(unit::fixture<TestCase287>());
    unit::test<7, 2>(unit::fixture<TestCase289>());
    unit::test<8, 2>(TestCase323);
    unit::test<9, 2>(TestCase327);
    unit::test<10, 2>(unit::fixture<TestCase342>());
    unit::test<1, 3>(TestCase380);
    unit::test<2, 3>(unit::fixture<TestCase381>());
    unit::test<3, 3>(TestCase382);
    unit::test<1, 4>(unit::fixture<TestCase383>());
    unit::test<2, 4>(TestCase385);
    unit::test<3, 4>(unit::fixture<TestCase386>());
    unit::test<1, 5>(TestCase387);
    unit::test<2, 5>(TestCase388);
    unit::test<3, 5>(unit::fixture<TestCase399>());
    unit::test<4, 5>(TestCase408);
    unit::test<1, 6>(TestCase420);
    unit::test<2, 6>(unit::fixture<TestCase477>());
    unit::test<3, 6>(unit::fixture<TestCase480>());
    unit::test<1, 7>(TestCase483);
    unit::test<2, 7>(unit::fixture<TestCase490>());
}

