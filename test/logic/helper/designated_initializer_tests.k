// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.designated_initializer_tests { module helper.designated_initializer_tests }

import helper.callbacks as hc
import test.unit as unit

inline bool GreaterThan7(uint32 x)
{
    return x > 7;
}

inline bool GreaterThan9(uint32 x)
{
    return x > 9;
}

inline bool GreaterThan10(uint32 x)
{
    return x > 10;
}

inline uint32 Select8Or4(bool b)
{
    return b ? 8 : 4;
}

inline uint32 AddOne(bool b)
{
    return cast<uint1>(b) + 1;
}

inline uint32 Add(uint32 a, uint32 b)
{
    return a + b;
}

inline uint31 AddSubType(uint33 a, uint33 b)
{
    return a + b;
}

inline uint32 Sub(uint32 a, uint32 b)
{
    return a - b;
}

inline uint32 Mul(uint32 a, uint32 b)
{
    return a * b;
}

inline void Struct(unit::tag_t tag)
{
    struct S
    {
        uint32 x;
        bool b;
    }

    {
        S s = { .b = true, .x = 3 };
        unit::assert_equal(tag, 3, s.x);
        unit::assert_equal(tag, true, s.b);

        s = { .b = false, .x = 154 };

        unit::assert_equal(tag, 154, s.x);
        unit::assert_equal(tag, false, s.b);

        s = { .x = 12 };

        unit::assert_equal(tag, 12, s.x);
        unit::assert_equal(tag, false, s.b);
    }

    {
        S s = { .b = true };
        unit::assert_equal(tag, 0, s.x);
        unit::assert_equal(tag, true, s.b);
    }

    {
        uint8 y = 12;

        S s = { .x = y };
        unit::assert_equal(tag, 12, s.x);
        unit::assert_equal(tag, false, s.b);
    }

    {
        uint8 y = 12;

        S s = { .x = y + 5 };
        unit::assert_equal(tag, 17, s.x);
        unit::assert_equal(tag, false, s.b);
    }

    {
        static S _shared1 = { .x = 14, .b = false };
        unit::assert_equal(tag, 14, _shared1.x);
        unit::assert_equal(tag, false, _shared1.b);
    }

    {
        static S _shared2 = { .b = true, .x = 11 };
        unit::assert_equal(tag, 11, _shared2.x);
        unit::assert_equal(tag, true, _shared2.b);
    }

    {
        static S _shared3 = { .b = true  };
        unit::assert_equal(tag, 0, _shared3.x);
        unit::assert_equal(tag, true, _shared3.b);
    }
}

inline void Union(unit::tag_t tag)
{
    union U
    {
        uint8 u8;
        uint32 u32;
        bool b;
    };

    {
        U u = { .u8 = 4 };

        unit::assert_equal(tag, 4, u.u8);
        unit::assert_equal(tag, 4, u.u32);
        unit::assert_equal(tag, false, u.b);

        u = { .u8 = 1 };

        unit::assert_equal(tag, 1, u.u8);
        unit::assert_equal(tag, 1, u.u32);
        unit::assert_equal(tag, true, u.b);

        u = { .u32 = 0x1234abcd };

        unit::assert_equal(tag, 0xcd, u.u8);
        unit::assert_equal(tag, 0x1234abcd, u.u32);
        unit::assert_equal(tag, true, u.b);
    }

    {
        U u = { .u32 = 0xcafebeef };

        unit::assert_equal(tag, 0xef, u.u8);
        unit::assert_equal(tag, 0xcafebeef, u.u32);
        unit::assert_equal(tag, true, u.b);
    }

    {
        U u = { .b = true };

        unit::assert_equal(tag, 1, u.u8);
        unit::assert_equal(tag, 1, u.u32);
        unit::assert_equal(tag, true, u.b);
    }

    {
        static U _u = { .u8 = 7 };

        unit::assert_equal(tag, 7, _u.u8);
        unit::assert_equal(tag, 7, _u.u32);
        unit::assert_equal(tag, true, _u.b);
    }
}

inline void UnionInStruct(unit::tag_t tag)
{
    union U
    {
        uint8 u8;
        uint32 u32;
    }

    struct S
    {
        uint32 x;
        U u;
        bool b;
    }

    {
        S s = { .b = true, .x = 3, .u = {} };
        unit::assert_equal(tag, 3, s.x);
        unit::assert_equal(tag, true, s.b);
        unit::assert_equal(tag, 0, s.u.u8);
        unit::assert_equal(tag, 0, s.u.u32);
    }

    {
        S s = { .x = 5, .u = {.u8 = 0xf6} };
        unit::assert_equal(tag, 5, s.x);
        unit::assert_equal(tag, false, s.b);
        unit::assert_equal(tag, 0xf6, s.u.u8);
        unit::assert_equal(tag, 0x000000f6, s.u.u32);
    }

    {
        S s = { .b = false, .u = {.u32 = 0xabcd1234} };
        unit::assert_equal(tag, 0, s.x);
        unit::assert_equal(tag, false, s.b);
        unit::assert_equal(tag, 0x34, s.u.u8);
        unit::assert_equal(tag, 0xabcd1234, s.u.u32);
    }

    {
        static S _s = { .b = false, .x = 7, .u = {.u32 = 0x1234abcd} };
        unit::assert_equal(tag, 7, _s.x);
        unit::assert_equal(tag, false, _s.b);
        unit::assert_equal(tag, 0xcd, _s.u.u8);
        unit::assert_equal(tag, 0x1234abcd, _s.u.u32);
    }
}

inline void StructInUnion(unit::tag_t tag)
{
    struct S
    {
        uint16 x;
        uint8  y;
        uint8  z;
    }

    union U
    {
        S      s;
        uint32 u32;
        uint8  u8;
    }

    {
        U u = {.s = {.z = 0xfe, .x = 0x4321, .y = 0x18}};

        unit::assert_equal(tag, 0x4321, u.s.x);
        unit::assert_equal(tag, 0x18, u.s.y);
        unit::assert_equal(tag, 0xfe, u.s.z);
        unit::assert_equal(tag, 0xfe184321, u.u32);
        unit::assert_equal(tag, 0x21, u.u8);
    }

    {
        U u = {.u32 = 0x9876abcd};

        unit::assert_equal(tag, 0xabcd, u.s.x);
        unit::assert_equal(tag, 0x76, u.s.y);
        unit::assert_equal(tag, 0x98, u.s.z);
        unit::assert_equal(tag, 0x9876abcd, u.u32);
        unit::assert_equal(tag, 0xcd, u.u8);
    }

    {
        static U _u = {.s = {.z = 0xfe, .x = 0x4321, .y = 0x18}};

        unit::assert_equal(tag, 0x4321, _u.s.x);
        unit::assert_equal(tag, 0x18, _u.s.y);
        unit::assert_equal(tag, 0xfe, _u.s.z);
        unit::assert_equal(tag, 0xfe184321, _u.u32);
        unit::assert_equal(tag, 0x21, _u.u8);
    }

    {
        static U _u = {.u32 = 0x9876abcd};

        unit::assert_equal(tag, 0xabcd, _u.s.x);
        unit::assert_equal(tag, 0x76, _u.s.y);
        unit::assert_equal(tag, 0x98, _u.s.z);
        unit::assert_equal(tag, 0x9876abcd, _u.u32);
        unit::assert_equal(tag, 0xcd, _u.u8);
    }
}

inline void ArrayInUnion(unit::tag_t tag)
{
    union U
    {
        uint8[2] u8x2;
        uint32   u32;
    }

    {
        U u = { .u32 = 0x1234abcd };
        unit::assert_equal(tag, 0x1234abcd, u.u32);
        unit::assert_equal(tag, 0xcd, u.u8x2[0]);
        unit::assert_equal(tag, 0xab, u.u8x2[1]);
    }

    {
        U u = { .u8x2 = { 0x1a, 0xbd } };
        unit::assert_equal(tag, 0x0000bd1a, u.u32);
        unit::assert_equal(tag, 0x1a, u.u8x2[0]);
        unit::assert_equal(tag, 0xbd, u.u8x2[1]);
    }

    {
        static U _u = { .u32 = 0x1234abcd };
        unit::assert_equal(tag, 0x1234abcd, _u.u32);
        unit::assert_equal(tag, 0xcd, _u.u8x2[0]);
        unit::assert_equal(tag, 0xab, _u.u8x2[1]);
    }

    {
        static U _u = { .u8x2 = { 0x1a, 0xbd } };
        unit::assert_equal(tag, 0x0000bd1a, _u.u32);
        unit::assert_equal(tag, 0x1a, _u.u8x2[0]);
        unit::assert_equal(tag, 0xbd, _u.u8x2[1]);
    }
}

inline void StructInStruct(unit::tag_t tag)
{
    struct Inner
    {
        uint32   a;
        uint8    b;
    }

    struct Outer
    {
        uint4 a;
        Inner i;
        uint32 b;
    }

    {
        Outer o = { .b = 0x1234dbac, .i = { .b = 4, .a = 0xabcdabcd }, .a = 0xe };
        unit::assert_equal(tag, 0xe, o.a);
        unit::assert_equal(tag, 0xabcdabcd, o.i.a);
        unit::assert_equal(tag, 4, o.i.b);
        unit::assert_equal(tag, 0x1234dbac, o.b);
    }

    {
        Outer o = { .b = 0x1234dbac, .i = { .a = 0xabcdabcd } };
        unit::assert_equal(tag, 0, o.a);
        unit::assert_equal(tag, 0xabcdabcd, o.i.a);
        unit::assert_equal(tag, 0, o.i.b);
        unit::assert_equal(tag, 0x1234dbac, o.b);
    }

    {
        static Outer _o = { .b = 0x1234dbac, .i = { .b = 4, .a = 0xabcdabcd }, .a = 0xe };
        unit::assert_equal(tag, 0xe, _o.a);
        unit::assert_equal(tag, 0xabcdabcd, _o.i.a);
        unit::assert_equal(tag, 4, _o.i.b);
        unit::assert_equal(tag, 0x1234dbac, _o.b);
    }

    {
        static Outer _o = { .b = 0x1234dbac, .i = { .a = 0xabcdabcd } };
        unit::assert_equal(tag, 0, _o.a);
        unit::assert_equal(tag, 0xabcdabcd, _o.i.a);
        unit::assert_equal(tag, 0, _o.i.b);
        unit::assert_equal(tag, 0x1234dbac, _o.b);
    }
}

inline void DuplicateFields(unit::tag_t tag)
{
    struct S
    {
        uint32 x;
        uint32 y;
    }

    S s = {.x = 3, .y = 5, .x = 6};

    unit::assert_equal(tag, 6, s.x);
    unit::assert_equal(tag, 5, s.y);
}

template< template <typename, auto> typename Container >
inline void StructInArrayOrMemory(unit::tag_t tag)
{
    class Helper
    {
        struct S
        {
            bool    b;
            uint8   u;
        }

        Container<S, 4> _container = { {.b = true, .u = 254}, {.u = 127}, {.b = true, .u = 17}, {} };

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for (4, [tag](index_t<4> i)
            {
                uint8[4] expected_u = { 254, 127, 17, 0 };
                bool[4] expected_b = { true, false, true, false };

                S s = _container[i];

                unit::assert_equal(tag, expected_u[i], s.u);
                unit::assert_equal(tag, expected_b[i], s.b);
            });
        }
    }

    static Helper _helper;

    _helper.test(tag);
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

inline void StaticObjectGlobalFunctions(unit::tag_t tag)
{
    static C _c = { .cb2 = Select8Or4, .cb1 = GreaterThan9 };

    _c.test(tag, 8, false, true, 8);
}

inline void StaticArrayOfObjects(unit::tag_t tag)
{
    static C[2] _a =
        {
            { .cb2 = Select8Or4, .cb1 = GreaterThan7 },
            { .cb2 = AddOne, .cb1 = GreaterThan9 }
        };

    _a[0].test(tag, 8, true,  false, 4);
    _a[1].test(tag, 8, false, false, 1);
}

class Inner
{
private:
    (uint32, uint32)->uint32 cb;

public:
    void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
    {
        unit::assert_equal(tag, expected, cb(a, b));
    }
}

// Inner object callback initialized to a global function
// Outer object is static
inline void NestedObjectGlobalFunctionOuterStatic(unit::tag_t tag)
{
    class Outer
    {
    private:
        Inner _inner = {.cb = Add };

    public:
        void test(unit::tag_t tag)
        {
            _inner.test(tag, 3, 4, 7);
        }
    }

    static Outer _outer;

    _outer.test(tag);
}

// Inner object callback initialized to a global function
// Outer object is global
class OuterClass
{
private:
    Inner _inner = {.cb = Add };

public:
    void test(unit::tag_t tag)
    {
        _inner.test(tag, 3, 4, 7);
    }
}

class NestedObjectGlobalFunctionOuterTest
{
private:
    OuterClass _outerClass;

public:
    void run(unit::tag_t tag)
    {
        _outerClass.test(tag);
    }
}

inline void NestedObjectCallbackPrioritzation(unit::tag_t tag)
{
    class Outer
    {
    private:
        Inner _inner = {.cb = Add };

    public:
        void test(unit::tag_t tag)
        {
            _inner.test(tag, 6, 4, 2);
        }
    }

    // The initialization of .cb = Sub should take priority over
    // .cb = Add
    static Outer _outer = { ._inner = {.cb = Sub}};

    _outer.test(tag);
}

inline void CallbackPrioritzation(unit::tag_t tag)
{
    class C
    {
    private:
        (uint32, uint32) -> uint32 callback = Add;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            unit::assert_equal(tag, expected, callback(a, b));
        }
    }

    static C[2] _c =
    {
        {},                 // .callback not specifed, will use default Add
        {.callback = Sub}
    };

    _c[0].test(tag, 3, 2, 5);
    _c[1].test(tag, 3, 2, 1);
}

// Callback refers to method in outer (static) class
inline void CallbackToMethodStatic(unit::tag_t tag)
{
    class Outer
    {
    private:
        Inner _inner = {.cb = Mul };
        uint32 _num_mul_calls = 0;

    public:
        void test(unit::tag_t tag)
        {
            _inner.test(tag, 3, 4, 12);

            unit::assert_equal(tag, 1, _num_mul_calls);
        }

        uint32 Mul(uint32 a, uint32 b)
        {
            atomic
            {
                _num_mul_calls++;
            }

            return a * b;
        }
    }

    static Outer _outer;

    _outer.test(tag);
}

class CallbackToMethodGlobalHelper
{
private:
    Inner _inner = {.cb = And };
    uint32 _num_calls = 0;

public:
    void test(unit::tag_t tag)
    {
        _inner.test(tag, 7, 3, 3);

        unit::assert_equal(tag, 1, _num_calls);
    }

    uint32 And(uint32 a, uint32 b)
    {
        atomic
        {
            _num_calls++;
        }

        return a & b;
    }
}

class CallbackToMethodGlobalTest
{
private:
    CallbackToMethodGlobalHelper _callbackToMethodGlobalHelper;

public:
    void run(unit::tag_t tag)
    {
        _callbackToMethodGlobalHelper.test(tag);
    }
}

// Callback of an inner object is initialized to a callback of the middle object
inline void ThreeLevelNesting(unit::tag_t tag)
{
    class Inner
    {
    private:
        (uint32, uint32)->uint32 cb;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            unit::assert_equal(tag, expected, cb(a, b));
        }
    }

    class Middle
    {
    private:
        (uint32, uint32)->uint32 callback;

        Inner _inner = {.cb = callback };

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            _inner.test(tag, a, b, expected);
        }
    }

    class Outer
    {
    private:
        Middle _middle = {.callback = Add};

    public:
        void test(unit::tag_t tag)
        {
            _middle.test(tag, 4, 5, 9);
        }
    }

    static Outer _outer;

    _outer.test(tag);
}

// Some callbacks are specified on the outside, others are not
inline void SelectiveOverride(unit::tag_t tag)
{
    class C
    {
    private:
        (uint32, uint32)->uint32 cb1 = Add;
        (uint32, uint32)->uint32 cb2 = Sub;

    public:
        void test(
            unit::tag_t tag,
            uint32 a1, uint32 b1, uint32 expected1,
            uint32 a2, uint32 b2, uint32 expected2)
        {
            unit::assert_equal(tag, expected1, cb1(a1, b1));
            unit::assert_equal(tag, expected2, cb2(a2, b2));
        }
    }

    static C _c = {.cb1 = Mul};

    _c.test(
        tag,
        3, 4, 12,
        5, 4, 1);
}

inline void StaticLocalInlineImpl(unit::tag_t tag)
{
    class Inner
    {
    private:
        (uint32, uint32)->uint32 cb;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            unit::assert_equal(tag, expected, cb(a, b));
        }
    }

    class Outer
    {
    private:
        Inner _inner = {.cb = Or};
        uint32 _count = 0;

        uint32 Or(uint32 a, uint32 b)
        {
            atomic
            {
                _count++;
            }

            return a | b;
        }

    public:
        void test(unit::tag_t tag)
        {
            _inner.test(tag, 8, 9, 9);

            unit::assert_equal(tag, 1, _count);
        }
    }

    static Outer _test;

    _test.test(tag);
}

inline void StaticLocalInline(unit::tag_t tag)
{
    StaticLocalInlineImpl(tag);
    StaticLocalInlineImpl(tag);
}

// Callback assigned to a function in another module
inline void CallbackSetToModule(unit::tag_t tag)
{
    class C
    {
    private:
        (uint32)->bool cb;

    public:
        void test(unit::tag_t tag, uint32 a, bool expected)
        {
            unit::assert_equal(tag, expected, cb(a));
        }
    }

    static C _c = { .cb = hc::EqualTo3 };

    _c.test(tag, 3, true);
    _c.test(tag, 2, false);
}

// Class declared in a separate module
inline void ClassInOtherModule(unit::tag_t tag)
{
    static hc::CallbackTester _test = { .callback = GreaterThan10 };

    _test.TestValue(tag, 10, false);
    _test.TestValue(tag, 11, true);
}

// Callback assigned to method of member object
inline void SiblingMethod(unit::tag_t tag)
{
    class Inner1
    {
    private:
        uint32 _base = 4;

    public:
        uint32 Add(uint32 a)
        {
            return a + _base;
        }

        void SetBase(uint32 b)
        {
            _base = b;
        }
    }

    class Inner2
    {
    private:
        (uint32)->uint32 callback;

    public:
        void test(unit::tag_t tag, uint32 src, uint32 expected)
        {
            unit::assert_equal(tag, expected, callback(src));
        }
    }

    class Outer
    {
    private:
        Inner1 _inner1;
        Inner2 _inner2 = { .callback = _inner1.Add };

    public:
        void test(unit::tag_t tag)
        {
            _inner2.test(tag, 5, 9);

            _inner1.SetBase(10);

            _inner2.test(tag, 5, 15);
        }
    }

    static Outer _outer;

    _outer.test(tag);
}

inline void EmptyInitializerList(unit::tag_t tag)
{
    static hc::CallbackTester _obj = {};

    _obj.Test(tag);
}

// Nested classes (not objects)
inline void NestedClass(unit::tag_t tag)
{
    class Outer
    {
    private:
        bool LogicalAnd(bool a, bool b)
        {
            return a && b;
        }

        class Inner
        {
        private:
            (bool, bool)->bool _callback;

        public:
            void test(unit::tag_t tag, bool a, bool b, bool expected)
            {
                unit::assert_equal(tag, expected, _callback(a, b));
            }
        }

        Inner _inner = { ._callback = LogicalAnd };

    public:
        void test(unit::tag_t tag)
        {
            _inner.test(tag, false, false, false);
            _inner.test(tag, false, true, false);
            _inner.test(tag, true, true, true);
        }
    }

    static Outer _outer;

    _outer.test(tag);
}

inline void CallbackAssignedToCallback(unit::tag_t tag)
{
    class Foo
    {
        (uint32, uint32) -> uint32 callback1;
        (uint32, uint32) -> uint32 callback2 = callback1;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            unit::assert_equal(tag, expected, callback1(a, b));
            unit::assert_equal(tag, expected, callback2(a, b));
        }
    }

    static Foo _foo = {.callback1 = Mul};

    _foo.test(tag, 3, 4, 12);
}

inline void InnerCallbackAssignedToCallback(unit::tag_t tag)
{
    class Inner
    {
        (uint32, uint32) -> uint32 callback1;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            unit::assert_equal(tag, expected, callback1(a, b));
        }
    }

    class Outer
    {
        (uint32, uint32) -> uint32 callback1;

        Inner _inner = {.callback1 = callback1};

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            _inner.test(tag, a, b, expected);
        }
    }

    static Outer _foo = {.callback1 = Add};

    _foo.test(tag, 3, 4, 7);
}

inline void MemberArrayOfObjects(unit::tag_t tag)
{
    class Inner
    {
        (uint32, uint32) -> uint32 callback;
        uint32 _base;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32 expected)
        {
            unit::assert_equal(tag, expected, _base + callback(a, b));
        }

        void set_base(uint32 x)
        {
            _base = x;
        }
    }

    class Outer
    {
        (uint32, uint32) -> uint32 callback1;
        (uint32, uint32) -> uint32 callback2;

        Inner[2] _inner =
        {
            {.callback = callback1},
            {.callback = callback2}
        };

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32[2] base, uint32[2] expected)
        {
            static for (const auto i : 2)
            {
                _inner[i].set_base(base[i]);

                _inner[i].test(tag, a, b, expected[i]);
            }
        }
    }

    static Outer _outer =
    {
        .callback1 = Add,
        .callback2 = Sub
    };

    _outer.test(tag, 7, 4, { 6, 3 }, { 17, 6 });

    class Outer2
    {
        Inner[2] _inner;

    public:
        void test(unit::tag_t tag, uint32 a, uint32 b, uint32[2] base, uint32[2] expected)
        {
            static for (const auto i : 2)
            {
                _inner[i].set_base(base[i]);

                _inner[i].test(tag, a, b, expected[i]);
            }
        }
    }

    static Outer2 outer2 = 
        {._inner = { {.callback = Add}
                   , {.callback = Sub}
                   }
        };

    outer2.test(tag, 7, 4, { 6, 3 }, { 17, 6 });
}

// This function has the same name as callbacks in side of classes below
// The signature is different as well, to verify there are not type checking passes
// in the compiler which handle name conflicts incorrectly and thus
// succeed if the signatures match, but fail if the signatures do not.
inline uint32 name_conflict_callback(uint32 a, uint32 b)
{
    return a + b;
}

inline bool name_conflict_callback_wrapper(uint32 a)
{
    return name_conflict_callback(a, 0) > 6;
}

inline void NameConflict(unit::tag_t tag)
{
    class Inner
    {
        (uint32)->bool name_conflict_callback;

    public:
        void test(unit::tag_t tag, uint32 src, bool expected)
        {
            bool result = name_conflict_callback(src);

            unit::assert_equal(tag, expected, result);
        }
    }

    class Outer
    {
        (uint32)->bool name_conflict_callback;

        Inner _inner = {.name_conflict_callback = name_conflict_callback};

    public:
        void test(unit::tag_t tag, uint32 src, bool expected)
        {
            _inner.test(tag, src, expected);
        }
    }

    static Outer _outer = {.name_conflict_callback = name_conflict_callback_wrapper};

    _outer.test(tag, 7, true);
}

inline void CallOuterMethodFromLambda(unit::tag_t tag)
{
    class Inner
    {
        (uint32)->bool _callback;

    public:
        void test(unit::tag_t tag, uint32[4] src, bool[4] expected)
        {
            pipelined_for(4, [tag, src, expected](index_t<4> i)
            {
                bool result = _callback(src[i]);

                unit::assert_equal(tag, expected[i], result);
            });
        }
    }

    class Outer
    {
        bool GreaterThan7Method(uint32 x)
        {
            return x > 7;
        }

        Inner _inner = {._callback = GreaterThan7Method};

    public:
        void test(unit::tag_t tag)
        {
            _inner.test(tag, {6, 7, 8, 9}, {false, false, true, true});
        }
    }

    static Outer _outer;
    _outer.test(tag);
}

inline void MethodUsedBeforeObject(unit::tag_t tag)
{
    class ClassWithMethod
    {
    public:
        uint32 Method(uint32 x)
        {
            return x + 1;
        }
    }

    class ClassWithCallback
    {
        (uint32)->uint32 callback;

    public:
        void test(unit::tag_t tag, uint32 src, uint32 expected)
        {
            unit::assert_equal(tag, expected, callback(src));
        }
    }

    class Outer
    {
        // reference _cwm before _cwm is declared
        ClassWithCallback _cwc = { .callback = _cwm.Method };
        ClassWithMethod _cwm;

    public:
        void test(unit::tag_t tag)
        {
            _cwc.test(tag, 3, 4);
        }
    }

    static Outer _outer;
    _outer.test(tag);
}

class DefaultInitializer
{
private:
    default = {.cb = Add};

    // Callbacks in the same class as the default initializer
    // are unaffected by the default initializer
    (uint32, uint32)->uint32 cb = Mul;

    Inner _inner;

public:
    void run(unit::tag_t tag)
    {
        _inner.test(tag, 3, 4, 7);

        unit::assert_equal(tag, 20, cb(4, 5));
    }
}

class OuterDefaultInitializerIgnored1
{
private:
    // This one should be ignored
    default = {.cb = Sub};

    DefaultInitializer _inner;

public:
    void run(unit::tag_t tag)
    {
        _inner.run(tag);
    }
}

class InitializeFromGrandParentHelper
{
private:
    Inner _inner;

public:
    void run(unit::tag_t tag)
    {
        _inner.test(tag, 2, 4, 8);
    }
}

class InitializeFromGrandParent
{
private:
    InitializeFromGrandParentHelper _helper;

public:
    void run(unit::tag_t tag)
    {
        _helper.run(tag);
    }

private:
    // Default initialize can appear anywhere
    // Semantics do not change based on where it is in the class
    default = {.cb = Mul};
}

class OuterDefaultInitializerIgnored2Helper
{
private:
    Inner _inner = {.cb = Add};

public:
    void run(unit::tag_t tag)
    {
        _inner.test(tag, 3, 5, 8);
    }
}

class OuterDefaultInitializerIgnored2
{
private:
    // Should be ignored
    default = {.cb = Sub};

    OuterDefaultInitializerIgnored2Helper _inner;

public:
    void run(unit::tag_t tag)
    {
        _inner.run(tag);
    }
}

class DefaultInitializeToMethod
{
private:
    default = {.cb = method};

    Inner _inner;

    uint32 _val;

private:
    uint32 method(uint32 a, uint32 b)
    {
        return a + b + _val;
    }

public:
    void run(unit::tag_t tag)
    {
        _val = 3;

        _inner.test(tag, 3, 4, 10);
    }
}

class SubTyping
{
private:
    default = {.cb = AddSubType};

    Inner _inner;

public:
    void run(unit::tag_t tag)
    {
        _inner.test(tag, 3, 4, 7);
    }
}
