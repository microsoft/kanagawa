// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// ordered while loop
class Test486
{
private:
    uint32 g_threadIdx486 = 0;

public:
    [[pipelined]] void Foo486(uint32 threadIdx, unit::tag_t tag)
    {
        uint32 i = 0;
        uint8 iterations = (~threadIdx) ^ cycles();
        uint32 sum = 0;

        for (const uint32 i : iterations)
        {
            sum += i;
        }

        uint32 expectedSum;

        if (iterations == 0)
        {
            expectedSum = 0;
        }
        else if (0 == (iterations % 2))
        {
            expectedSum = (iterations - 1) * (iterations / 2);
        }
        else
        {
            expectedSum = ((iterations - 1) * (iterations / 2)) + (iterations / 2);
        }

        unit::assert(tag, sum == expectedSum);

        uint32 snappedIdx;

        atomic
        {
            snappedIdx = g_threadIdx486;
            g_threadIdx486++;
        }

        unit::assert(tag, snappedIdx == threadIdx);
    }

    void run(unit::tag_t tag)
    {
        Foo486(1024, tag);
    }
}

// ordered for loop inside of predicate
class Test487
{
private:
    uint32 g_threadIdx487 = 0;

public:
    [[pipelined]] void Foo487(uint32 threadIdx, unit::tag_t tag)
    {
        uint8 iterations = (~threadIdx) ^ cycles();
        uint32 sum = 0;

        bool predicate = (0 == threadIdx % 2);

        if (predicate)
        {
            for (const uint32 i : iterations)
            {
                sum += i;
            }
        }

        uint32 expectedSum;

        if (iterations == 0)
        {
            expectedSum = 0;
        }
        else if (0 == (iterations % 2))
        {
            expectedSum = (iterations - 1) * (iterations / 2);
        }
        else
        {
            expectedSum = ((iterations - 1) * (iterations / 2)) + (iterations / 2);
        }

        if (!predicate)
        {
            expectedSum = 0;
        }

        unit::assert(tag, sum == expectedSum);

        uint32 snappedIdx;

        atomic
        {
            snappedIdx = g_threadIdx487;
            g_threadIdx487++;
        }

        unit::assert(tag, snappedIdx == threadIdx);
    }

    void run(unit::tag_t tag)
    {
        Foo487(1024, tag);
    }
}

// ordered do loop inside of predicate
class Test488
{
private:
    uint32 g_threadIdx488 = 0;

public:
    [[pipelined]] void Foo488(uint32 threadIdx, unit::tag_t tag)
    {
        uint8 iterations = (~threadIdx) ^ cycles();
        uint32 sum = 0;

        bool predicate = (0 == threadIdx % 2);

        if (predicate)
        {
            uint32 i = 0;

            do
            {
                sum += i;
                i++;
            } while (i < iterations);
        }

        uint32 expectedSum;

        if (iterations == 0)
        {
            expectedSum = 0;
        }
        else if (0 == (iterations % 2))
        {
            expectedSum = (iterations - 1) * (iterations / 2);
        }
        else
        {
            expectedSum = ((iterations - 1) * (iterations / 2)) + (iterations / 2);
        }

        if (!predicate)
        {
            expectedSum = 0;
        }

        unit::assert(tag, sum == expectedSum);

        uint32 snappedIdx;

        atomic
        {
            snappedIdx = g_threadIdx488;
            g_threadIdx488++;
        }

        unit::assert(tag, snappedIdx == threadIdx);
    }

    void run(unit::tag_t tag)
    {
        Foo488(1024, tag);
    }
}

// sign extension with ~ operator bug
inline void WrapAddNegate489(uint32 a, uint32 b, unit::tag_t tag)
{
    println(b);
    unit::assert(tag, b <= 0xffff);
}


inline void TestCase489(unit::tag_t tag)
{
    uint16 local = 0x0000;

    WrapAddNegate489(2, ~local, tag);
}

// template class
const auto Const491 = 91;

template<typename myType, myType myConstant1, myType myConstant2>
class Foo491
{
public:
    myType Add(myType a, myType b)
    {
        myType result = a + b;

        print("Add {a} {b} {result}\n");

        return result;
    }

    inline myType AddConstant(myType a)
    {
        return a + myConstant1;
    }

    inline myType SubtractConstant(myType a)
    {
        return a - myConstant2;
    }
}

using Foo491U32=Foo491<uint32, 4, Const491>;
using  Foo491U2=Foo491<uint2, 1, 0>;

class Test491
{
private:
    Foo491U32 g_foo491_32;
    Foo491U2 g_foo491_2;

public:
    void run(unit::tag_t tag)
    {
        uint32 result_32 = g_foo491_32.Add(3, 4);
        unit::assert(tag, result_32 == 7);

        uint2 result_2 = g_foo491_2.Add(3, 4);
        unit::assert(tag, result_2 == 3);

        uint32 c = g_foo491_32.AddConstant(10);
        unit::assert(tag, c == 14);

        uint32 d = g_foo491_32.SubtractConstant(100);
        unit::assert(tag, d == 9);
    }
}

// template class without explict typedef
const auto Const492 = 91;

template<typename myType>
class Foo492
{
public:
    myType Add(myType a, myType b)
    {
        myType result = a + b;

        print("Add {a} {b} {result}\n");

        return result;
    }
}

class Test492
{
private:
    Foo492<uint32> g_foo492_32;

public:
    void run(unit::tag_t tag)
    {
        uint32 result_32 = g_foo492_32.Add(5, 6);
        unit::assert(tag, result_32 == 11);
    }
}

// member variable of template type
const auto Const493 = 91;

template<typename myType>
class Foo493
{
public:
    myType Add(myType a, myType b)
    {
        myType result = a + b;

        print("Add {a} {b} {result}\n");

        return result;
    }
}

using Foo493U32=Foo493<uint32>;

class Bar493
{
private:
    Foo493U32 _member;

public:
    uint32 MyFunc(uint32 a)
    {
        return _member.Add(a, 4);
    }
}

class Test493
{
private:
    Bar493 g_bar493;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_bar493.MyFunc(10);

        unit::assert(tag, result == 14);
    }
}

// typedef inside of a class
class Bar494
{
public:
    using MyType=uint3;

    uint32 MyFunc(uint32 a)
    {
        MyType temp = cast<MyType>(a);

        return cast<uint32>(temp + 1);
    }
}

class Test494
{
private:
    Bar494 g_bar494;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_bar494.MyFunc(10);

        unit::assert(tag, result == 3);
    }
}

// typedef inside of a class - used as a parameter & return type
class Bar495
{
public:
    using MyType=uint3;

    MyType MyFunc(MyType a)
    {
        return a + 1;
    }
}

class Test495
{
private:
    Bar495 g_bar495;

public:
    void run(unit::tag_t tag)
    {
        uint32 result = g_bar495.MyFunc(1);

        unit::assert(tag, result == 2);
    }
}

// nested class
class Bar496
{
private:
    class Foo
    {
    private:
        uint32 _val;

    public:
        inline void Set(uint32 v)
        {
            _val = v;
        }

        uint32 Add(uint32 a)
        {
            return a + _val;
        }
    }

    Foo _foo1;

public:
    void Test(unit::tag_t tag)
    {
        _foo1.Set(3);

        uint32 result = _foo1.Add(5);

        print("result {result}\n");

        unit::assert(tag, result == 8);
    }
}

class Test496
{
private:
    Bar496 g_bar496;

public:
    void run(unit::tag_t tag)
    {
        g_bar496.Test(tag);
    }
}

// nested template class
class Bar497
{
private:
    template<typename valType>
    class Foo
    {
    private:
        valType _val;

    public:
        inline void Set(valType v)
        {
            _val = v;
        }

        uint32 Add(uint32 a)
        {
            return a + _val;
        }
    }

    using Foo2=Foo<uint2>;

    Foo2 _foo;

public:
    void Test(unit::tag_t tag)
    {
        _foo.Set(9);

        uint32 result = _foo.Add(1);

        print("result {result}\n");

        unit::assert(tag, result == 2);
    }
}

class Test497
{
private:
    Bar497 g_bar497;

public:
    void run(unit::tag_t tag)
    {
        g_bar497.Test(tag);
    }
}

// nested template class without typedef
class Bar498
{
private:
    template<typename valType>
    class Foo498
    {
    private:
        valType _val;

    public:
        inline void Set(valType v)
        {
            _val = v;
        }

        uint32 Add(uint32 a)
        {
            return a + _val;
        }
    }

    Foo498<uint2> _foo;

public:
    void Test(unit::tag_t tag)
    {
        _foo.Set(10);

        uint32 result = _foo.Add(1);

        print("result {result}\n");

        unit::assert(tag, result == 3);
    }
}

class Test498
{
private:
    Bar498 g_bar498;

public:
    void run(unit::tag_t tag)
    {
        g_bar498.Test(tag);
    }
}

inline void test_main()
{
    unit::test<486>(unit::fixture<Test486>());
    unit::test<487>(unit::fixture<Test487>());
    unit::test<488>(unit::fixture<Test488>());
    unit::test<489>(TestCase489);
    unit::test<491>(unit::fixture<Test491>());
    unit::test<492>(unit::fixture<Test492>());
    unit::test<493>(unit::fixture<Test493>());
    unit::test<494>(unit::fixture<Test494>());
    unit::test<495>(unit::fixture<Test495>());
    unit::test<496>(unit::fixture<Test496>());
    unit::test<497>(unit::fixture<Test497>());
    unit::test<498>(unit::fixture<Test498>());
}
