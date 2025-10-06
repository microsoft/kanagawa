// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

template<auto N, typename T>
inline T add_n_times(()->T fn)
{
    T result = 0;

    T val = fn();

    static for (const auto i : N)
    {
        result += val;
    }

    return result;
}

inline void store_into(uint32 val, (uint32)->void fn)
{
    fn(val);
}

inline void CaptureThisReadMemberVariableTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

    public:
        void test(unit::tag_t tag)
        {
            _x = 4;

            auto callback = []() -> uint32
            {
                return _x;
            };

            uint32 result = add_n_times<5>(callback);

            unit::assert(tag, result == 20);
        }
    }

    static Helper helper;
    helper.test(tag);
}

inline void CaptureThisWriteMemberVariableTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

    public:
        void test(unit::tag_t tag)
        {
            _x = 4;

            auto callback = [](uint32 newVal) -> void
            {
                _x = newVal;
            };

            store_into(7, callback);

            unit::assert(tag, _x == 7);
        }
    }

    static Helper helper;
    helper.test(tag);
}

inline void CaptureMemberVariableTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

    public:
        void test(unit::tag_t tag)
        {
            _x = 4;

            auto callback = [_x](uint32 newVal) -> void
            {
                _x = newVal;
            };

            store_into(7, callback);

            unit::assert(tag, _x == 4);
        }
    }

    static Helper helper;
    helper.test(tag);
}

inline void CaptureThisMethodTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

        inline uint32 GetX()
        {
            return _x;
        }

    public:
        void test(unit::tag_t tag)
        {
            _x = 3;

            uint32 result = add_n_times<5>(GetX);

            unit::assert(tag, result == 15);
        }
    }

    static Helper helper;
    helper.test(tag);
}

// Calling a non-inline method via a reference
// the method should be auto-inlined
inline void CaptureThisAutoInlinedMethodTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

        uint32 GetX()
        {
            return _x;
        }

    public:
        void test(unit::tag_t tag)
        {
            _x = 7;

            uint32 result = add_n_times<5>(GetX);

            unit::assert(tag, result == 35);
        }
    }

    static Helper helper;
    helper.test(tag);
}

// Calling a non-inline method via a reference
// the method should be auto-inlined - even if there are multiple objects
inline void CaptureThisAutoInlinedMethodMultipleObjectsTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

        uint32 GetX()
        {
            return _x;
        }

    public:
        void test(unit::tag_t tag)
        {
            _x = 7;

            uint32 result = add_n_times<5>(GetX);

            unit::assert(tag, result == 35);
        }
    }

    static Helper helper1;
    static Helper helper2;

    helper1.test(tag);
    helper2.test(tag);
}

// Calling a non-inline method via a reference
// the method should not be auto-inlined because there is a separate call site to the method
inline void CaptureThisNotAutoInlinedMethodTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;

        uint32 GetX()
        {
            return _x;
        }

    public:
        void test(unit::tag_t tag)
        {
            _x = 6;

            // The GetX here is crucial, it ensures there is another call site to GetX()
            // which will disable auto-inlining
            uint32 result = static_cast(add_n_times<5>(GetX) + GetX());

            unit::assert(tag, result == 36);
        }
    }

    static Helper helper;
    helper.test(tag);
}

// Reading a member variable of a member object
inline void CaptureThisReadMemberObjectMemberVariableTest(unit::tag_t tag)
{
    class Inner
    {
    public:
        uint32 _y;
    }

    class Helper
    {
    private:
        Inner _inner;

    public:
        void test(unit::tag_t tag)
        {
            _inner._y = 6;

            auto callback = []() -> uint32
            {
                return _inner._y;
            };

            uint32 result = add_n_times<5>(callback);

            unit::assert(tag, result == 30);
        }
    }

    static Helper helper;
    helper.test(tag);
}

// Writing a member variable of a member object
inline void CaptureThisWriteMemberObjectMemberVariableTest(unit::tag_t tag)
{
    class Inner
    {
    public:
        uint32 _y;
    }

    class Helper
    {
    private:
        Inner _inner;

    public:
        void test(unit::tag_t tag)
        {
            _inner._y = 6;

            auto callback = [](uint32 x) -> void
            {
                _inner._y = x;
            };

            store_into(9, callback);

            unit::assert(tag, _inner._y == 9);
        }
    }

    static Helper helper;
    helper.test(tag);
}

// Calling a method of a member variable
inline void CaptureThisMemberObjectMethodTest(unit::tag_t tag)
{
    class Inner
    {
    private:
        uint32 _y = 2;

    public:
        uint32 GetY() { return _y; }
    }

    class Helper
    {
    private:
        Inner _inner;

    public:
        void test(unit::tag_t tag)
        {
            auto callback = []() -> uint32
            {
                return _inner.GetY();
            };

            uint32 result = add_n_times<5>(callback);

            unit::assert(tag, result == 10);
        }
    }

    static Helper helper;
    helper.test(tag);
}

class HofCallsHelper
{
public:
    // A higher order function that passes a function to a non-inline helper function
    inline uint32 hof_calls_non_inline_fn((uint32)->uint32 fn)
    {
        using fn_t = decltype(fn);

        inline uint32 helper_function(fn_t callback)
        {
            return callback(3);
        }

        return helper_function(fn);
    }
}

// A higher order function that passes a lambda
// to a non-inline helper function.  The helper function must be instantiated multiple times
// even though it is not inline
inline void NonInlineFunctionEnumerationTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x;
        HofCallsHelper hofCallsHelper;

    public:
        void test(unit::tag_t tag)
        {
            _x = 4;

            auto callback = [](uint32 input) -> uint32
            {
                return _x + input;
            };

            uint32 result = hofCallsHelper.hof_calls_non_inline_fn(callback);

            unit::assert(tag, result == 7);
        }
    }

    static Helper helper1;
    static Helper helper2;

    helper1.test(tag);
    helper2.test(tag);
}

// Function that take multiple references parameters
inline void MultipleReferenceParamTest(unit::tag_t tag)
{
    class Helper1
    {
    public:
        uint32 _x = 5;

        inline uint32 Go((uint32)->uint32 fn)
        {
            auto callback = [fn]() -> uint32
            {
                return fn(static_cast(_x + 2));
            };

            return add_n_times<3>(callback);
        }
    }

    class Helper2
    {
    public:
        uint32 _x = 7;

        Helper1 _helper1;

        void Test(unit::tag_t tag)
        {
            auto callback = [](uint32 y) -> uint32
            {
                return cast<uint32>(_x + y);
            };

            uint32 result = _helper1.Go(callback);

            unit::assert(tag, result == 42);
        }
    }

    static Helper2 helper2;
    helper2.Test(tag);
}

// A single function that accepts 2 functions as input (each with different referenced objects)
inline uint32 add_2(()->uint32 fn_a, ()->uint32 fn_b)
{
    using fn_a_t = decltype(fn_a);
    using fn_b_t = decltype(fn_b);

    inline uint32 heler(fn_a_t fn_a, fn_b_t fn_b)
    {
        return fn_a() + fn_b();
    }

    return heler(fn_a, fn_b);
}

inline void MultipleFunctionParamTest(unit::tag_t tag)
{
    class Helper1
    {
    public:
        uint32 _x = 5;

        inline uint32 Go(()->uint32 fn)
        {
            auto callback = []() -> uint32
            {
                return _x;
            };

            return add_2(callback, fn);
        }
    }

    class Helper2
    {
    public:
        uint32 _x = 7;

        Helper1 _helper1;

        void Test(unit::tag_t tag)
        {
            auto callback = []() -> uint32
            {
                return _x;
            };

            uint32 result = _helper1.Go(callback);

            unit::assert(tag, result == 12);
        }
    }

    static Helper2 helper2;
    helper2.Test(tag);
}

// Capture this from within a lambda
inline void NestedCaptureThisTest(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 _x = 6;

    public:
        void Test(unit::tag_t tag)
        {
            const auto callback1 = []() -> uint32
            {
                const auto callback2 = []() -> uint32
                {
                    return _x;
                };

                return callback2() + 5;
            };

            uint32 result = callback1();

            unit::assert(tag, result == 11);
        }
    }

    static Helper helper;
    helper.Test(tag);
}


inline uint32 Go((uint32) -> uint32 Do, uint32 x)
{
    return Do(x) + Do(static_cast(x + 1));
}

inline uint32 DoIt(uint32 x)
{
    return x;
}

template <(uint32) -> uint32 Fn>
class FunctionTemplateParameterHelper
{
    public:
        uint32 Do(uint32 x)
        {
            return Go([](uint32 i)
            {
                return Fn(i);
            },
            x);
        }
}

inline void FunctionTemplateParameterFromLambda(unit::tag_t tag)
{
    static FunctionTemplateParameterHelper<DoIt> helper;
    auto result = helper.Do(13);
    unit::assert(tag, result == 13 + 14);
}

// Reference parameter passed to a method
inline void ReferenceInMethodInner((uint32)->uint32 fn)
{
    using fn_t = decltype(fn);

    class Helper
    {
    private:
        uint32 _value = 0;

    public:
        void Test(fn_t fn)
        {
            _value = fn(_value);
        }
    }

    static Helper helper;
    helper.Test(fn);
}

inline void ReferenceInMethod(unit::tag_t tag)
{
    class Outer
    {
    private:
        uint32 _result;

    public:
        void Run(unit::tag_t tag)
        {
            _result = 12;

            const auto fn1 = [](uint32 x) -> uint32
            {
                _result = x;
                return x + 3;
            };

            const auto fn2 = [](uint32 x) -> uint32
            {
                _result = x;
                return x + 4;
            };

            for (const auto i : 4)
            {
                // Each call site calls a separate instance
                ReferenceInMethodInner(fn1);
                unit::assert(tag, _result == (i * 3));

                ReferenceInMethodInner(fn1);
                unit::assert(tag, _result == (i * 3));

                ReferenceInMethodInner(fn2);
                unit::assert(tag, _result == (i * 4));
            }
        }
    }

    static Outer _testObj;
    _testObj.Run(tag);
}

// A static local variable in a capture list
// Regression test for a compiler bug that would cause an assertion to fire.
inline void CaptureStatic(unit::tag_t tag)
{
    pipelined_for(8, [](index_t<8> tid)
    {
        static uint32 _counter = 0;

        uint32 snapped;

        atomic
        {
            snapped = _counter;
            _counter = cycles();
        }

        async_exec([_counter, snapped]
        {
            print("{_counter} {snapped}\n");
        });
    });
}

inline void ReturnReferenceFromFunction(unit::tag_t tag)
{
    inline auto Go(() -> void fn)
    {
        decltype(fn) x = fn;

        return x;
    }

    class Foo
    {
        bool _ran = false;

    public:
        void Run(unit::tag_t tag)
        {
            unit::assert(tag, !_ran);
            auto fn = Go([]{_ran = true;});
            fn();
            unit::assert(tag, _ran);
        }
    }

    static Foo _foo;

    _foo.Run(tag);
}

inline void ReturnClosureFromFunction(unit::tag_t tag)
{
    inline auto Add(uint32 y)
    {
        return [y](uint32 x)
        {
            return x + y;
        };
    }

    auto add10 = Add(10);

    unit::assert(tag, add10(20) == 30);
    unit::assert(tag, add10(2) == 12);
}

inline void ReturnClosureFromClosure(unit::tag_t tag)
{
    inline auto Add(uint32 y)
    {
        return [y](uint32 x)
        {
            return [x, y](uint32 z)
            {
                return x + y + z;
            };
        };
    }

    auto add10 = Add(10);
    auto add10add11 = add10(11);

    unit::assert(tag, add10add11(20) == 41);
    unit::assert(tag, add10add11(2) == 23);
}

inline auto ReturnClosureFromHOF((uint32, uint32) -> uint32 fn, uint32 x)
{
    return [x, fn](uint32 y) -> uint32
    {
        return fn(x, y);
    };
}

inline auto RunClosure((uint32) -> uint32 fn, uint32 x)
{
    return fn(x);
}

inline void RunClosureReturnedFromHOF(unit::tag_t tag)
{
    uint32 a = 11;

    unit::assert(tag, a + 12 + 13 ==
        RunClosure(
            ReturnClosureFromHOF([a](uint32 x, uint32 y) -> uint32
                {
                    return a + x + y;
                },
                12)
            ,
            13));
}

inline void GenericClosureReturnedFromFunction(unit::tag_t tag)
{
    inline auto Foo()
    {
        return [](auto x)
        {
            return x;
        };
    }

    auto id = Foo();

    unit::assert(tag, true == id(true));
    unit::assert(tag, 10 == id(10));
}

inline void HigherOrderFunctionAsArgumentToHigherOrderFunction(unit::tag_t tag)
{
    inline auto Go((auto x) -> auto Do, uint32 a)
    {
        return Do([a](uint32 x)
        {
            return x > a;
        });
    }

    Go([tag](auto callback) -> auto
    {
        unit::assert(tag, callback(0) == false);
        unit::assert(tag, callback(11) == true);
    },
    10);
}

// Regression test for a bug regarding a non-inline function that has reference parameters and 2 call sites
inline void NonInlineHOFTwoCallSites(unit::tag_t tag)
{
    class helper
    {
        uint32 _val = 3;

        template<typename Closure>
        noinline uint32 noinlinefn(Closure closure)
        {
            return closure();
        }

        inline uint32 hof(()->uint32 fn)
        {
            return noinlinefn<decltype(fn)>(fn);
        }

    public:
        void test(unit::tag_t tag)
        {
            unit::assert(tag, _val == hof([]->uint32{return _val;}));
            unit::assert(tag, _val == hof([]->uint32{return _val;}));
        }
    }

    static helper _helper;
    _helper.test(tag);
}

inline void test_main()
{
    unit::test<1>(CaptureThisReadMemberVariableTest);
    unit::test<2>(CaptureThisWriteMemberVariableTest);
    unit::test<3>(CaptureMemberVariableTest);
    unit::test<4>(CaptureThisMethodTest);
    unit::test<5>(CaptureThisAutoInlinedMethodTest);
    unit::test<6>(CaptureThisAutoInlinedMethodMultipleObjectsTest);
    unit::test<7>(CaptureThisNotAutoInlinedMethodTest);
    unit::test<8>(CaptureThisReadMemberObjectMemberVariableTest);
    unit::test<9>(CaptureThisWriteMemberObjectMemberVariableTest);
    unit::test<10>(CaptureThisMemberObjectMethodTest);
    unit::test<11>(NonInlineFunctionEnumerationTest);
    unit::test<12>(MultipleReferenceParamTest);
    unit::test<13>(MultipleFunctionParamTest);
    unit::test<14>(NestedCaptureThisTest);
    unit::test<15>(FunctionTemplateParameterFromLambda);
    unit::test<16>(ReferenceInMethod);
    unit::test<17>(CaptureStatic);
    unit::test<18>(ReturnReferenceFromFunction);
    unit::test<19>(ReturnClosureFromFunction);
    unit::test<20>(ReturnClosureFromClosure);
    unit::test<21>(RunClosureReturnedFromHOF);
    unit::test<22>(GenericClosureReturnedFromFunction);
    unit::test<23>(HigherOrderFunctionAsArgumentToHigherOrderFunction);
    unit::test<24>(NonInlineHOFTwoCallSites);
}
