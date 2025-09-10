// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.bits
import test.unit as unit
import test.runner

// Intentionally marked inline
// to test auto-inlining
template<auto N>
inline uint32 AddN_Inline(uint32 x)
{
    return x + N;
}

// Tests for exported class functionality

class ExportClass
{
public:
    uint32 X(uint32 y)
    {
        return y + 3;
    }
}

// An exported class
export ExportClass;

// A class that contains an instance of the exported class
class ContainsExportClass
{
public:
    inline uint32 X(uint32 x)
    {
        return _inner.X(x);
    }

private:
    ExportClass _inner;
}

class ExportClassTest
{
    ContainsExportClass _containsExportClass;

public:
    inline void run(unit::tag_t tag)
    {
        const uint32 inpVal = 14;
        const uint32 expected = inpVal + 3;
        uint32 actual = _containsExportClass.X(inpVal);

        unit::assert_equal(tag, expected, actual);
    }
}

class ClassWithCallback
{
private:
    // Intentionally not marked inline or noinline
    // to test auto-inlining
    template<auto N>
    uint32 AddN_AutoInline(uint32 x)
    {
        return x + N;
    }

    // The name abc will appear in the associated port in the generated RTL
    (uint32 abc)->uint32 callback;

    // Test for the case where a specified parameter name conflicts
    // with a synthetic parameter name
    (uint32 _param_1, uint32)->uint32 conflict_callback;

    // Callback which is never called
    (uint32)->uint32 unreferenced_callback;

    // This will not map to verify ports because it has a default value
    (uint32)->uint32 callback_with_default = AddN_AutoInline<5>;

public:
    uint32 Entry(unit::tag_t tag, uint32 x)
    {
        // test callback_with_default
        unit::assert_equal(tag, x + 5, callback_with_default(x));

        return callback(x + 1);
    }

    // Test for calling a callback via a lambda
    uint32 LambdaTest(uint32 x)
    {
        return pipelined_last(1, [x](uint32 id)
        {
            return callback(x + 2);
        });
    }
}

[[name("HasCallback")]]
export ClassWithCallback;

// fn can be inline, auto-inline, or noinline
template<(uint32)->uint32 fn>
inline void InlineAttributeTest(unit::tag_t tag)
{
    class Outer
    {
        // Intentially not marked inline
        // to test the no-inline case
        template<auto N>
        noinline uint32 AddN_NoInline(uint32 x)
        {
            return x + N;
        }

    private:
        ClassWithCallback _classWithCallback = {.callback = fn, .unreferenced_callback = AddN_NoInline<10>, .conflict_callback = bitwise_or<uint32> };

    public:
        void Test(unit::tag_t tag, uint32 i)
        {
            uint32 expected = i + 4;

            uint32 actual = _classWithCallback.Entry(tag, i);

            unit::assert_equal(tag, expected, actual);
        }
    }

    pipelined_for(8, [tag](index_t<8> i)
    {
        static Outer _outer;

        _outer.Test(tag, i);
    });
}

inline void test_main()
{
    unit::test<1>(unit::fixture<ExportClassTest>());
    unit::test<4>(InlineAttributeTest<AddN_Inline<3>>);
}
