// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.counter
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

    (uint32)->uint32 callback;

    // This will not map to verify ports because it has a default value
    (uint32)->uint32 callback_with_default = AddN_AutoInline<5>;

public:
    uint32 Entry(unit::tag_t tag, uint32 x)
    {
        // test callback_with_default
        unit::assert_equal(tag, x + 5, callback_with_default(x));

        return callback(static_cast(x + 1));
    }

    // Test for calling a callback via a lambda
    uint32 LambdaTest(uint32 x)
    {
        return pipelined_last(1, [x](uint32 id)
        {
            return callback(static_cast(x + 2));
        });
    }
}

export ClassWithCallback;

inline void CallbackToMethod(unit::tag_t tag)
{
    class Outer
    {
    private:
        uint32 _count = 0;

        uint32 Foo(uint32 x)
        {
            atomic
            {
                _count++;
            }

            return x + 1;
        }

        ClassWithCallback _classWithCallback = {.callback = Foo};

    public:
        void Test(unit::tag_t tag, uint32 i)
        {
            uint32 expected = static_cast(i + 2);

            uint32 actual = _classWithCallback.Entry(tag, i);

            unit::assert_equal(tag, expected, actual);

            unit::assert_string(tag, _count >= i, "call count not updated");
        }
    }

    pipelined_for(8, [tag](index_t<8> i)
    {
        static Outer _outer;

        _outer.Test(tag, i);
    });
}

class ClassWithAsyncCallback
{
private:
    [[async]] (uint32)->void callback;

public:
    void Entry(uint32 x)
    {
        callback(static_cast(x + 3));
    }
}

export ClassWithAsyncCallback;

inline void AsyncCallbackTest(unit::tag_t tag)
{
    class Outer
    {
    private:
        uint32 _value;

        [[async]] void Callback(uint32 x)
        {
            _value = x;
        }

        ClassWithAsyncCallback _classWithCallback = {.callback = Callback};

    public:
        void Test(unit::tag_t tag, uint32 i)
        {
            _value = 0;

            _classWithCallback.Entry(i);

            uint32 expected = static_cast(i + 3);

            atomic do; while(!(_value == expected));

            unit::assert_equal(tag, expected, _value);
        }
    }

    static Outer _outer;

    _outer.Test(tag, 4);
}

inline void CallFromLambdaTest(unit::tag_t tag)
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
        ClassWithCallback _classWithCallback = {.callback = AddN_NoInline<3>};

    public:
        void Test(unit::tag_t tag, uint32 i)
        {
            uint32 expected = static_cast(i + 5);

            uint32 actual = _classWithCallback.LambdaTest(i);

            unit::assert_equal(tag, expected, actual);
        }
    }

    pipelined_for(8, [tag](index_t<8> i)
    {
        static Outer _outer;

        _outer.Test(tag, i);
    });
}

using pair_t = pair<uint32, uint32>;

class ClassWithStructCallback
{
private:
    (pair_t)->pair_t callback;

public:
    pair_t Entry(uint32 x)
    {
        pair_t p = { x, cast<uint32>(x + 1) };

        return callback(p);
    }
}

export ClassWithStructCallback;

inline pair_t AddToPair(pair_t p)
{
    return {p.first + 1, p.second + 2};
}

inline void StructParamReturnTest(unit::tag_t tag)
{
    class Outer
    {
    private:
        ClassWithStructCallback _classWithStructCallback = {.callback = AddToPair};

    public:
        void Test(unit::tag_t tag, uint32 i)
        {
            pair_t result = _classWithStructCallback.Entry(i);

            unit::assert_equal(tag, i + 1, result.first);
            unit::assert_equal(tag, i + 3, result.second);
        }
    }

    pipelined_for(8, [tag](index_t<8> i)
    {
        static Outer _outer;

        _outer.Test(tag, i);
    });
}

// Exported class is a member of a global object
class GlobalDeclarationTestOuter
{
private:
    // Intentially not marked inline
    // to test the no-inline case
    template<auto N>
    noinline uint32 AddN_NoInline(uint32 x)
    {
        return x + N;
    }

    ClassWithCallback _classWithCallback = {.callback = AddN_NoInline<3>};

public:
    void Test(unit::tag_t tag, uint32 i)
    {
        uint32 expected = static_cast(i + 4);

        uint32 actual = _classWithCallback.Entry(tag, i);

        unit::assert_equal(tag, expected, actual);
    }
}

class ClassWithObject
{
private:
    counter<8, 0> _counter;

public:
    uint32 IncAndCount()
    {
        _counter.increment();

        return _counter.count();
    }
}

class ArrayOfClassWithObject
{
private:
    ClassWithObject[2] _objects;

public:
    uint32 Entry()
    {
        return _objects[0].IncAndCount() + _objects[1].IncAndCount();
    }
}

export ArrayOfClassWithObject;

inline void ArrayOfClassWithObjectTest(unit::tag_t tag)
{
    static ArrayOfClassWithObject _obj;

    uint32 result = _obj.Entry();

    unit::assert_equal(tag, 2, result);
}


inline void test_main()
{
    unit::test<1>(CallbackToMethod);
    unit::test<2>(AsyncCallbackTest);
    unit::test<3>(CallFromLambdaTest);
    unit::test<4>(StructParamReturnTest);
    unit::test<6>(ArrayOfClassWithObjectTest);
}
