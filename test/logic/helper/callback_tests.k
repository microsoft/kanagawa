// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.callback_tests { module helper.callback_tests }

import test.unit as unit
import data.array as ar

template<auto amount>
inline uint32 Add(uint32 x)
{
    return x + amount;
}

inline bool GreaterThan7(uint32 x)
{
    return x > 7;
}

inline bool LessThan7(uint32 x)
{
    return x < 7;
}

inline bool EqualThreeConst(const uint32 x)
{
    return x == 3;
}

// Generic method to invoke a test
template<typename T>
inline void TestObject(unit::tag_t tag)
{
    static T _helper;
    _helper.Test(tag);
}

class AssignDuringMemberDeclToInlineHelper
{
    (uint32)->bool callback = GreaterThan7;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, callback(8));
        unit::assert(tag, !callback(7));
    }
}

class AssignDuringMemberDeclToNonInlineHelper
{
    (uint32)->bool callback = LessThan7;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, !callback(8));
        unit::assert(tag, !callback(7));
        unit::assert(tag, callback(6));
    }
}

class AssignDuringMemberDeclToMemberHelper
{
    (uint32)->bool callback = GreaterThan3;

    inline bool GreaterThan3(uint32 x)
    {
        return x > 3;
    }

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, !callback(2));
        unit::assert(tag, !callback(3));
        unit::assert(tag, callback(4));
    }
}

// A callback that refers to another callback
class DoubleDereferenceHelper
{
    (uint32)->bool callback_a = GreaterThan7;
    (uint32)->bool callback_b = callback_a;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, callback_a(8));
        unit::assert(tag, !callback_b(7));
    }
}

// A callback that refers to an async function
class AsyncCallbackHelper
{
    // Note that the async attribute is not specified
    // in the callback declaration
    (uint32)->void callback = AsyncFunction;

    [[async]] void AsyncFunction(uint32 value)
    {
        atomic
        {
            _result = make_optional(true, value);
        }
    }

    optional<uint32> _result;

public:
    void Test(unit::tag_t tag)
    {
        _result = make_optional<uint32>(false, 0);

        callback(7);

        atomic do; while(!_result.is_valid);

        unit::assert_equal(tag, 7, _result.value);
    }
}

// Callback initialized to method of contained object
class CallbackInitializedToMemberInner
{
    uint32 _sum = 0;

public:
    uint32 AddToRunningSum(uint32 x, uint32 y)
    {
        uint32 s = x + y;

        uint32 result;

        atomic
        {
            _sum += s;

            result = _sum;
        }

        return result;
    }
}

class CallbackInitializedToMemberHelper
{
    (uint32, uint32)->uint32 callback_a = _inner_a.AddToRunningSum;

    CallbackInitializedToMemberInner _inner_a;
    CallbackInitializedToMemberInner _inner_b;

    (uint32, uint32)->uint32 callback_b = _inner_b.AddToRunningSum;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert_equal(tag, callback_a(2, 3), 5);
        unit::assert_equal(tag, callback_a(4, 6), 15);

        unit::assert_equal(tag, callback_b(1, 5), 6);
        unit::assert_equal(tag, callback_b(4, 1), 11);
    }
}

class CallbackInitializedToMemberArrayElementHelper
{
    (uint32, uint32)->uint32 callback_a = _inner[0].AddToRunningSum;

    CallbackInitializedToMemberInner[2] _inner;

    (uint32, uint32)->uint32 callback_b = _inner[1].AddToRunningSum;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert_equal(tag, callback_a(2, 3), 5);
        unit::assert_equal(tag, callback_a(4, 6), 15);

        unit::assert_equal(tag, callback_b(1, 5), 6);
        unit::assert_equal(tag, callback_b(4, 1), 11);
    }
}

// Nested object contains callbacks
class NestedObjectHelper
{
private:
    CallbackInitializedToMemberArrayElementHelper _inner;

public:
    void Test(unit::tag_t tag)
    {
        _inner.Test(tag);
    }
}

// Nested class contains callbacks
class NestedClassHelper
{
private:
    class InnerClass
    {
        (uint32)->bool callback = GreaterThan11;

        inline bool GreaterThan11(uint32 x)
        {
            return x > 11;
        }

    public:
        void Test(unit::tag_t tag)
        {
            unit::assert(tag, !callback(10));
            unit::assert(tag, !callback(11));
            unit::assert(tag, callback(12));
        }
    }

    InnerClass _inner;

public:
    void Test(unit::tag_t tag)
    {
        _inner.Test(tag);
    }
}

// Callback declaration has an explicit scope
class ScopeHelper
{
    (bool[4])->bool callback = ar::and<4>;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, callback({true, true, true, true}));
        unit::assert(tag, !callback({true, false, true, true}));
    }
}

// Callback parameter is a subtype of underlying function parameter
class ParamSubtypingHelper
{
    (uint8)->bool is_odd = IsOdd;

    inline bool IsOdd(uint32 x)
    {
        return 1 == (x % 2);
    }

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, is_odd(1));
        unit::assert(tag, !is_odd(2));
        unit::assert(tag, !is_odd(254));
        unit::assert(tag, is_odd(255));
    }
}

// Callback parameter is a subtype of underlying function parameter
// Callback is a method of a contained object
class ParamSubtypingNestedObjectHelper
{
    class Inner
    {
    public:
        inline bool IsOdd(uint32 x)
        {
            return 1 == (x % 2);
        }
    }

    (uint8)->bool is_odd = _inner.IsOdd;

    Inner _inner;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert(tag, is_odd(1));
        unit::assert(tag, !is_odd(2));
        unit::assert(tag, !is_odd(254));
        unit::assert(tag, is_odd(255));
    }
}

// Underlying function return type can be a subtype of callback return type
class ReturnSubtypingHelper
{
    (uint8)->uint32 times_two = TimesTwo;

    inline uint9 TimesTwo(uint8 x)
    {
        return x * 2;
    }

public:
    void Test(unit::tag_t tag)
    {
        unit::assert_equal(tag, 6, times_two(3));
        unit::assert_equal(tag, 510, times_two(255));
    }
}

// Underlying function return type can be a subtype of callback return type
// Callback is contained in a nested class
class ReturnSubtypingNestedHelper
{
    class Inner
    {
        inline uint9 TimesTwo(uint8 x)
        {
            return x * 2;
        }
    }

    (uint8)->uint32 times_two = _inner.TimesTwo;

    Inner _inner;

public:
    void Test(unit::tag_t tag)
    {
        unit::assert_equal(tag, 6, times_two(3));
        unit::assert_equal(tag, 510, times_two(255));
    }
}

class StructInOut
{
    struct S
    {
        uint32 _x;
    }

    S ModifyStruct(S s)
    {
        return { s._x + 1 };
    }

    (S)->S _callback = ModifyStruct;

public:
    void Test(unit::tag_t tag)
    {
        S input = { 3 };

        S output = _callback(input);

        unit::assert_equal(tag, 4, output._x);
    }
}

// Calling a callback from a higher order function
inline void CallbackFromHof(unit::tag_t tag)
{
    class Helper
    {
        (uint32)->uint32 callback;

    public:
        void Test(unit::tag_t tag, uint32 expected_offset)
        {
            pipelined_for(32, [tag, expected_offset](index_t<32> i)
            {
                uint32 result = callback(i);

                unit::assert_equal(tag, i + expected_offset, result);
            });
        }
    }

    static Helper _obj1 = { .callback = Add<1> };
    _obj1.Test(tag, 1);

    static Helper _obj2 = { .callback = Add<2> };
    _obj2.Test(tag, 2);
}

// Lambda (a single function node in the ast)
// which calls back to 1 of 3 function nodes in the ast
inline void LambdaDemux(unit::tag_t tag)
{
    class Inner
    {
        (uint32)->uint32 callback;

    public:
        uint32 Test(uint32 x)
        {
            return pipelined_last(1, [x](uint1 i)
            {
                return callback(x);
            });
        }
    }

    template<auto N>
    class Outer
    {
        uint32 Invert(uint32 x)
        {
            return ~x;
        }

        Inner _inner = {.callback = Invert};

    public:
        void Test(unit::tag_t tag, uint32 x)
        {
            uint32 actual = _inner.Test(x + N);
            uint32 expected = ~(x + N);

            unit::assert_equal(tag, expected, actual);
        }
    }

    static Outer<1> _o1;
    static Outer<2> _o2;
    static Outer<3> _o3;

    _o1.Test(tag, 4);
    _o2.Test(tag, 5);
    _o3.Test(tag, 6);
}

class DeclaredInModule1
{
    uint32 _val = 0;

public:
    uint32 Foo(uint32 x)
    {
        return x + 4 + _val;
    }
}

class DeclaredInModule2
{
private:
    (uint32)->uint32 cb;

public:
    void Bar(uint32 x, uint32 expected, unit::tag_t tag)
    {
        unit::assert_equal(tag, expected, cb(x));
    }
}

class DeclaredInModuleTest
{
private:
    DeclaredInModule1 _declaredInModule1;
    DeclaredInModule2 _declaredInModule2 = {.cb = _declaredInModule1.Foo};

public:
    void run(unit::tag_t tag)
    {
        _declaredInModule2.Bar(3, 7, tag);
    }
}

// Callback parameter marked const
class ConstParamHelper
{
    (const uint32)->bool callback;

public:
    void Test(unit::tag_t tag, uint32 x, bool expected)
    {
        unit::assert_equal(tag, expected, callback(x));
    }
}

inline void ConstParamTest(unit::tag_t tag)
{
    static ConstParamHelper _helper = { .callback = EqualThreeConst };

    _helper.Test(tag, 2, false);
    _helper.Test(tag, 3, true);
}

// Nested object initialized via the initializer list of the outer object
class NestedObjectInner
{
private:
    (uint32)->uint32 _cb;

public:
    uint32 Go(uint32 x)
    {
        return _cb(x);
    }
}

class NestedObjectOuter
{
private:
    NestedObjectInner _inner;

public:
    uint32 Go(uint32 x)
    {
        return _inner.Go(x);
    }
}

inline void NestedObjectInitializerTest(unit::tag_t tag)
{
    static NestedObjectOuter _helper = { ._inner = { ._cb = Add<1> } };

    uint32 result = _helper.Go(3);

    unit::assert_equal(tag, 4, result);
}

// Designated initializer refers to a member which
// which has not yet been declared
inline void InitializerBeforeDeclarationTest(unit::tag_t tag)
{
    class C
    {
    public:
        uint32 foo(uint32 x)
        {
            return x + 1;
        }
    }

    class D
    {
        (uint32)->uint32 cb;

    public:
        uint32 foo(uint32 x)
        {
            return cb(x + 1);
        }
    }

    class E
    {
        D[2] _d = {{.cb = _c.foo}, {.cb = _c.foo}};
        C _c;

    public:
        inline uint32 entry(index_t<2> idx, uint32 x)
        {
            return _d[idx].foo(x);
        }
    }

    static E _e;

    uint32 result0 = _e.entry(0, 4);
    unit::assert_equal(tag, 6, result0);

    uint32 result1 = _e.entry(1, 5);
    unit::assert_equal(tag, 7, result1);
}