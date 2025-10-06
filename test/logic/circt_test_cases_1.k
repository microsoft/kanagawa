// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import sync.atomic

inline void AddVsMux(unit::tag_t tag)
{
    pipelined_for (8, [tag](index_t<8> i)
    {
        uint16 actual = (i + 1) * 1;
        uint16 expected = mux(cast<uint3>(i * 1), 1, 2, 3, 4, 5, 6, 7, 8);

        unit::assert_equal(tag, actual, expected);
        print("hello world {actual} {expected}\n");
    });
}

inline void BinaryOpVsMux(unit::tag_t tag)
{
    const int16 j = 4;
    pipelined_for (8, [tag](index_t<8> i)
    {
        int16 actual = static_cast(mux(cast<uint3>(i * 1), -(i + 1), lutmul(i,j), (i | -1), (i ^ -1), (i >> 2), (-i >> 3), 0, 0));
        int16 expected = mux(cast<uint3>(i * 1), -1, 4, -1, -4, 1, -1, 0, 0);

        unit::assert_equal(tag, actual, expected);
    });
}

// cast
struct Foo158
{
    uint8 a;
    uint8 b;
    uint8 c;
    uint8 d;
}

inline void GatherFunc(unit::tag_t tag)
{
    uint32 x = 0xdeadbeef;

    Foo158 foo = cast<Foo158>(x);

    unit::assert(tag, foo.a == 0xef);
    unit::assert(tag, foo.b == 0xbe);
    unit::assert(tag, foo.c == 0xad);
    unit::assert(tag, foo.d == 0xde);

    uint32 y = cast<uint32>(x);

    unit::assert(tag, y == 0xdeadbeef);
}

inline void SharedVar(unit::tag_t tag)
{
    pipelined_for (8, [tag](index_t<8> i)
    {
        uint32 result = first(atomically([](uint32 prev) -> uint32
        {
            return prev + 1;
        }));

        unit::assert(tag, result == i);
    });
}

inline void Mem(unit::tag_t tag)
{
    pipelined_for (8, [tag](index_t<8> i)
    {
        static memory<uint32, 8> _mem;

        _mem[i] = i + 1;

        unit::assert(tag, _mem[i] == (i + 1));
    });
}

inline void WaitFor(unit::tag_t tag)
{
    static bool _b = false;

    atomic do; while(_b);
}

inline void Containers(unit::tag_t tag)
{
    class A
    {
        uint32 _value = 3;

    public:
        uint32 inc()
        {
            uint32 result;

            atomic
            {
                result = _value;
                _value++;
            }

            return result;
        }
    }

    class B
    {
        A _a;

    public:
        uint32 inc()
        {
            return _a.inc();
        }
    }

    pipelined_for(4, [tag](index_t<4> tid)
    {
        static B _b;

        uint32 result = _b.inc();
        unit::assert(tag, result == (3 + tid));
    });
}

class ThreadRate
{
private:
    [[thread_rate(4)]] void ThreadRateTest(index_t<8> i)
    {
        print("{cycles()} {i}\n");
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(8, [](index_t<8> i)
        {
            ThreadRateTest(i);
        });
    }
}

inline void test_main()
{
    unit::test<1>(AddVsMux);
    unit::test<2>(SharedVar);
    unit::test<3>(BinaryOpVsMux);
    unit::test<4>(GatherFunc);
    unit::test<5>(Mem);
    unit::test<6>(WaitFor);
    unit::test<7>(unit::fixture<ThreadRate>());
    unit::test<8>(Containers);
}
