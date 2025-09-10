// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.fifo
import helper.static_locals
import test.unit as unit
import test.runner

// static local declared in an inline function with multiple call sites
// method on the static local is not marked inline, but should be auto-inlined
class Helper773
{
private:
    uint32 _val = 7;

public:
    uint32 GetAndInc()
    {
        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Inner773(unit::tag_t tag)
{
    static Helper773 h;

    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 7);
}

inline void Test773(unit::tag_t tag)
{
    Inner773(tag);
    Inner773(tag);
}

// static local array of objects
class Helper774
{
private:
    uint32 _val = 7;

public:
    uint32 GetAndInc()
    {
        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Inner774(unit::tag_t tag)
{
    static Helper774[4] h;

    static for (const auto i : 4)
    {
        uint32 result = h[i].GetAndInc();
        unit::assert(tag, result == 7);
    }
}

inline void Test774(unit::tag_t tag)
{
    Inner774(tag);
    Inner774(tag);
}

// 2 static variables in the same function with the same name
inline void Test775(unit::tag_t tag)
{
    class Helper775
    {
    private:
        uint32 _val = 7;

    public:
        uint32 GetAndInc()
        {
            uint32 result = _val;

            _val++;

            return result;
        }
    }

    {
        static Helper775 h;
        uint32 result = h.GetAndInc();
        unit::assert(tag, result == 7);
    }

    {
        static Helper775 h;
        uint32 result = h.GetAndInc();
        unit::assert(tag, result == 7);
    }
}

// 2 call sites to a method of a static local - it should not be auto-inlined
class Helper776
{
public:
    uint32 GetAndInc()
    {
        static uint32 _val = 7;

        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Test776(unit::tag_t tag)
{
    static Helper776 h;
    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 7);

    result = h.GetAndInc();
    unit::assert(tag, result == 8);
}

// static local object that contains another object
class Helper777_inner
{
public:
    uint32 GetAndInc()
    {
        static uint32 _val = 7;

        uint32 result = _val;

        _val++;

        return result;
    }
}

class Helper777_outer
{
private:
    Helper777_inner _inner;

public:
    uint32 GetAndIncPlusOne()
    {
        return _inner.GetAndInc() + 1;
    }

}

inline void Test777(unit::tag_t tag)
{
    static Helper777_outer h;
    uint32 result = h.GetAndIncPlusOne();
    unit::assert(tag, result == 8);

    result = h.GetAndIncPlusOne();
    unit::assert(tag, result == 9);
}

// static local object of class type declared in a module
inline void Inner778(unit::tag_t tag)
{
    static FIFO<uint32, 512, true, true> f;

    f.enqueue(4);

    uint32 result = f.dequeue();

    unit::assert(tag, result == 4);
}

inline void Test778(unit::tag_t tag)
{
    Inner778(tag);
    Inner778(tag);
}

// 2 static local objects with the same name in different functions
template<auto init_val>
class Helper779
{
public:
    uint32 GetAndInc()
    {
        static uint32 _val = init_val;

        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void Helper779_1(unit::tag_t tag)
{
    static Helper779<3> h;
    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 3);
}

inline void Helper779_2(unit::tag_t tag)
{
    static Helper779<10> h;
    uint32 result = h.GetAndInc();
    unit::assert(tag, result == 10);
}

inline void Test779(unit::tag_t tag)
{
    Helper779_1(tag);

    Helper779_2(tag);
}

// static local in an uncalled function
class Helper781
{
private:
    uint32 _val = 7;

public:
    uint32 GetAndInc()
    {
        uint32 result = _val;

        _val++;

        return result;
    }
}

inline void UncalledFunction781(unit::tag_t tag)
{
    {
        static Helper781 h;
        uint32 result = h.GetAndInc();
        unit::assert(tag, result == 7);
    }
}

// calling into a member of a member
class Inner782
{
private:
    uint32 val = 4;
public:
    uint32 Get()
    {
        return val;
    }
}

class Mid782
{
public:
    Inner782 _inner;
}

class Outer782
{
private:
    Mid782 _mid;

public:
    void Test(unit::tag_t tag)
    {
        uint32 result = _mid._inner.Get();

        unit::assert(tag, result == 4);
    }
}

class Test782
{
private:
    Outer782 g_outer782;

public:
    void run(unit::tag_t tag)
    {
        g_outer782.Test(tag);
    }
}

inline bool GreaterThan7(uint32 x)
{
    return x > 7;
}

inline bool GreaterThan9(uint32 x)
{
    return x > 9;
}

inline uint32 Select8Or4(bool b)
{
    return b ? 8 : 4;
}

inline uint32 AddOne(bool b)
{
    return cast<uint1>(b) + 1;
}

class C
{
private:
    (uint32)->bool cb1;
    (bool)->uint32 cb2;

public:
    void test(unit::tag_t tag, uint32 param1, bool expected1, bool param2, uint32 expected2)
    {
        unit::assert_equal(tag, expected1, cb1(param1));
        unit::assert_equal(tag, expected2, cb2(param2));
    }
}

// static local used inside a non-inline function within a module
inline void Test788(unit::tag_t tag)
{
    TestStaticLocalWithinModule_Inlined(3, 5, tag);
    TestStaticLocalWithinModule_Inlined(3, 7, tag);
}

inline void test_main()
{
    unit::test<773>(Test773);
    unit::test<774>(Test774);
    unit::test<775>(Test775);
    unit::test<776>(Test776);
    unit::test<777>(Test777);
    unit::test<778>(Test778);
    unit::test<779>(Test779);
    unit::test<782>(unit::fixture<Test782>());
    unit::test<788>(Test788);
}
