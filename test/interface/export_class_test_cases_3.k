// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import .options as opt
import test.unit as unit
import test.runner

template<auto N>
inline uint32 AddN(uint32 x)
{
    return x + N;
}

class ClassWithNoInlineFunction
{
private:
    (uint32)->uint32 callback;

    inline uint32 Inner(uint32 x)
    {
        return callback(x);
    }

    // The compilation pass which compiles the use of this class
    // will see methods inside of the object referenced by pipelined_last an unreferenced functions.  It should not be processed
    // further.
    noinline uint32 Outer(uint32 x)
    {
        return pipelined_last(1, [x](uint32 _)
        {
            return Inner(x);
        });
    }

public:
    void Entry(unit::tag_t tag, uint32 x, uint32 expected)
    {
        unit::assert_equal(tag, expected, Outer(x));
    }
}

export ClassWithNoInlineFunction;

class OuterClass
{
private:
    ClassWithNoInlineFunction _classWithCallback = {.callback = AddN<6>};

public:
    void Test(unit::tag_t tag, uint32 i)
    {
        _classWithCallback.Entry(tag, i, static_cast(i + 6));
    }
}

class TestNoInlineFunction
{
    OuterClass _outer;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(256, [tag](index_t<256> i)
        {
            _outer.Test(tag, i);
        });
    }
}

// Test for an export class that must wait for memory initialization
// before allowing interface methods to be called
class ClassWithInitializedMemory
{
private:
    memory<uint32, 64> _mem = { 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8,
                                1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8,
                                1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8,
                                1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8 };

public:
    // Should not be callable until memory init has completed
    void Test()
    {
        pipelined_for(64, [](index_t<64> tid)
        {
            // back to front to increase the chances
            // of catching an uninitialized entry
            index_t<64> i = static_cast(64 - tid - 1);

            uint32 actual = _mem[i];
            uint32 expected = (i % 8) + 1;

            assert(expected == actual);
        });
    }
}

export ClassWithInitializedMemory;

class Outer_ClassWithInitializedMemory
{
private:
    ClassWithInitializedMemory _inner;

    // This is trying to catch the window of time
    // before memory initialization has completed
    [[reset]] void Test()
    {
        async_exec([]()
        {
            _inner.Test();
        });
    }

public:
    void run(unit::tag_t tag)
    {
        _inner.Test();
    }
}

template<auto N>
class EndTransactionOnCallbacksInner
{
    const auto NumThreads = 512;

    (unit::tag_t tag, uint32 tid, uint32 threadCount, uint32 callsite, bool isLast) -> void _callback;

    fork_join<uint32, 2, 32, 32> _fj;

    void CallCallback(unit::tag_t tag, uint32 tid, uint32 threadCount, uint32 callsite, [[last]] bool isLast)
    {
        _callback(tag, tid, threadCount, static_cast(N * 2 + callsite), isLast);
    }

    template<auto M>
    [[pipelined]] void CallSite(uint32 tid, uint32 threadCount, unit::tag_t tag)
    {
        [[transaction_size(opt::max_threads_limit)]] CallCallback(tag, tid, threadCount, M, (tid == threadCount - 1));
    }

public:
    void Run(unit::tag_t tag)
    {
        _fj.fork<0>([tag]() -> uint32 { CallSite<0>(NumThreads, NumThreads, tag); return 0; });
        _fj.fork<1>([tag]() -> uint32 { CallSite<1>(NumThreads, NumThreads, tag); return 1; });

        _fj.join({true, true});
    }
}

class EndTransactionOnCallbacksTest
{
    EndTransactionOnCallbacksInner<0> _inner0 = { ._callback = AssertNoInterleavedCalls };
    EndTransactionOnCallbacksInner<1> _inner1 = { ._callback = AssertNoInterleavedCalls };

    fork_join<uint32, 2, 32, 32> _fj;

    void AssertNoInterleavedCalls(unit::tag_t tag, uint32 tid, uint32 threadCount, uint32 callsite, [[last]] bool isLast)
    {
        static uint32 _lastTid = 0;
        static uint32 _lastCallsite;

        uint32 snappedLastTid;
        uint32 snappedLastCallSite;

        atomic
        {
            snappedLastTid = _lastTid;
            snappedLastCallSite = _lastCallsite;

            _lastTid = tid;
            _lastCallsite = callsite;
        }

        if (tid > 0)
        {
            unit::assert_equal(tag, snappedLastCallSite, callsite);
            unit::assert_equal(tag, snappedLastTid + 1, tid);
        }

        unit::assert_equal(tag, (tid == threadCount - 1), isLast);
    }

public:
    void Run(unit::tag_t tag)
    {
        _fj.fork<0>([tag]() -> uint32 { _inner0.Run(tag); return 0; });
        _fj.fork<1>([tag]() -> uint32 { _inner1.Run(tag); return 1; });

        _fj.join({true, true});
    }
}

[[name("EndTransactionOnCallbacksInner0")]]
export EndTransactionOnCallbacksInner<0>;

[[name("EndTransactionOnCallbacksInner1")]]
export EndTransactionOnCallbacksInner<1>;

inline void TestEndTransactionOnCallbacks(unit::tag_t tag)
{
    static EndTransactionOnCallbacksTest _endTransactionOnCallbacksTest;

    _endTransactionOnCallbacksTest.Run(tag);
}

class EMTest
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

extern EMTest;

// Regression test for an export class which contains an instance of an extern class
class ExportClassWithExternObject
{
private:
    [[async]] void EMTestCallback()
    {

    }

    EMTest _test = {.Callback = EMTestCallback};

public:
    void Test(unit::tag_t tag)
    {
        uint17 xor_result = _test.XOR(0x1234, 0x14321);

        unit::assert_equal(tag, 0x15115, xor_result);
    }
}

export ExportClassWithExternObject;

inline void TestExportClassWithExternObject(unit::tag_t tag)
{
    static ExportClassWithExternObject _test;

    _test.Test(tag);
}

inline void test_main()
{
    unit::test<1>(unit::fixture<TestNoInlineFunction>());
    unit::test<2>(TestEndTransactionOnCallbacks);
    unit::test<3>(TestExportClassWithExternObject);
    unit::test<4>(unit::fixture<Outer_ClassWithInitializedMemory>());
}
