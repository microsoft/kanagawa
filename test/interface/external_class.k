// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import helper.external_class as em
import test.unit as unit
import test.runner
import sync.atomic

// Test for an extern class with a fixed-latency callback
class ClassWithLatencyCallback
{
    [[latency(2)]] (uint32)->uint32 cb;

public:
    // entry simply forwards to cb, with 1 cycle of latency added
    [[latency(3)]] uint32 entry(uint32 x);
}

extern ClassWithLatencyCallback;

class CallbackLatency
{
private:
    [[latency(2)]] uint32 add_one(uint32 x)
    {
        return static_cast(x + 1);
    }

    ClassWithLatencyCallback _obj = {.cb = add_one};

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(8, [tag](index_t<8> i)
        {
            uint32 actual = _obj.entry(i);

            uint32 expected = i + 1;

            unit::assert_equal(tag, expected, actual);
        });
    }
}

// Test for an extern class with a [[no_backpressure]] callback
class ClassWithNoBackpressureCallback
{
    [[async]] [[no_backpressure]] (uint32)->void cb;

public:
    [[async]] [[no_backpressure]] void entry(uint32 x);
}

extern ClassWithNoBackpressureCallback;

class CallbackNoBackpressure
{
private:
    uint32 _fence = 0;

    [[async]] [[no_backpressure]] void save_fence(uint32 x)
    {
        _fence = x;
    }

    ClassWithNoBackpressureCallback _obj = {.cb = save_fence};

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(8, [tag](index_t<8> i)
        {
            _obj.entry(i + 1);

            wait([i]()
            {
                return _fence >= (i + 1);
            });

            unit::assert(tag, _fence >= (i + 1));
        });
    }
}

class EMTest_ABC
{
private:
    [[async]] ()->void Callback;

public:
    // Method body will be ignored when compiling an instantiation of this class
    uint17 XOR(uint17 a, uint17 b)
    {
        return a + b;
    }

    [[latency(3)]] uint32 Add(uint32 a, uint32 b);
    // This function tests if the compiler correctly emits a non-empty
    // pipeline.latency op when the operator doesn't have a destination operand in CIRCT.
    [[latency(8)]] void Sub(uint32 a, uint32 b);
}

// There is no Verilog module with the _ABC suffix
// But the [[name]] attribute is used to refer to a verilog
// module without the suffix
[[name("EMTest")]]
extern EMTest_ABC;

class StaticExternClass
{
    [[async]] void EMTestCallback()
    {
    }

    EMTest_ABC _emtest = {.Callback = EMTestCallback};

public:
    void run(unit::tag_t tag)
    {
        pipelined_for (8, [tag](index_t<8> i)
        {
            uint17 xor_result = _emtest.XOR(0x1234, 0x14321);

            unit::assert_equal(tag, 0x15115, xor_result);

            uint32 a = i + 1;
            uint32 b = i + 20;

            uint32 add_result = _emtest.Add(a, b);
            _emtest.Sub(a, b);

            uint32 expected = second(atomically([a, b](uint32 prev) -> uint32
            {
                return prev + a + b;
            }));

            unit::assert_equal(tag, expected, add_result);
        });
    }
}

class Adder
{
public:
    template <typename T>
    [[latency(3)]] T Add(T a, T b);

    [[latency(1)]] void SetBase(uint32 base);
}

extern Adder;

class Helper
{
private:
    Adder _adder;

    void TestCase(unit::tag_t tag, index_t<8> i)
    {
        uint32 a = i + 1;
        uint32 b = i + 20;

        if (i == 0)
        {
            _adder.SetBase(3);
        }

        uint32 result = _adder.Add(a, b);

        uint32 expected = second(atomically<uint32, 3>([a, b](uint32 prev) -> uint32
        {
            return prev + a + b;
        }));

        unit::assert_equal(tag, expected, result);
    }

public:
    void Test(unit::tag_t tag)
    {
        pipelined_for (8, [tag](index_t<8> i)
        {
            TestCase(tag, i);
        });
    }
}

inline void ExternClassAsMemeber(unit::tag_t tag)
{
    static Helper[2] _helper;

    // Testing different instances
    _helper[0].Test(tag);
    _helper[1].Test(tag);
}

struct SomeStruct
{
    uint8 _a;
    uint16 _b;
    uint8 _c;
}

class ComplexReturns
{
public:
    SomeStruct ReturnStruct(uint8 v);
    uint8[2] ReturnArray(uint8 v);
    SomeStruct[2] ReturnArrayOfStruct(uint8 v);
}

extern ComplexReturns;

inline void TestStruct(unit::tag_t tag)
{
    static ComplexReturns complex;

    auto result = complex.ReturnStruct(0x33);
    SomeStruct expected = {0x33, 0x3300, 0xcc};
    print("SomeStruct: {result}\n");

    unit::assert_equal(tag, expected, result);
}

inline void TestArray(unit::tag_t tag)
{
    static ComplexReturns complex;

    auto result = complex.ReturnArray(0x22);
    uint8[2] expected = {0x20, 0xdd};
    print("SomeArray: {result}\n");

    unit::assert_equal(tag, expected, result);
}

inline void TestArrayOfStruct(unit::tag_t tag)
{
    static ComplexReturns complex;

    auto result = complex.ReturnArrayOfStruct(0x11);
    SomeStruct[2] expected = {{0x11, 0x1100, 0xee}, {0xee, 0xee00, 0x11}};
    print("SomeArrayOfStruct: {result}\n");

    unit::assert_equal(tag, expected, result);
}

inline void test_main()
{
    unit::test<1>(unit::fixture<CallbackLatency>());
    unit::test<2>(unit::fixture<CallbackNoBackpressure>());
    unit::test<3>(unit::fixture<StaticExternClass>());
    unit::test<4>(ExternClassAsMemeber);
    unit::test<5>(unit::fixture<em::AdderWrapper>());
    unit::test<1>(TestStruct);
    unit::test<2>(TestArray);
    unit::test<3>(TestArrayOfStruct);
}
