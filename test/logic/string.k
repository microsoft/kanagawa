// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.optional
import test.unit as unit
import test.runner

inline void Ternary(unit::tag_t tag)
{
    for (const auto i : 2)
    {
        string s = "first iteration";
        string t = "second iteration";
        string u = (i == 0) ? s : t;

        if (i == 0)
        {
            __assert_str_eq("first iteration", u);
        }
        else
        {
            __assert_str_eq("second iteration", u);
        }

        print("{i} {u}\n");
    }
}

inline void Mux(unit::tag_t tag)
{
    for (const auto i : 2)
    {
        string s = "first iteration";
        string t = "second iteration";
        string u = mux(i, s, t);

        if (i == 0)
        {
            __assert_str_eq("first iteration", u);
        }
        else
        {
            __assert_str_eq("second iteration", u);
        }

        print("{i} {u}\n");
    }
}

class UninitializedClassMember
{
private:
    string _str; // no initial value

public:
    void run(unit::tag_t tag)
    {
        _str = "test";
        __assert_str_eq("test", _str);

        _str = "bat";
        __assert_str_eq("bat", _str);
    }
}

class InitializedClassMember
{
private:
    string _str = "";

public:
    void run(unit::tag_t tag)
    {
        __assert_str_eq(_str, "");

        _str = "bat";
        __assert_str_eq("bat", _str);
    }
}

class InitializerClassMember
{
private:
    string _str = {}; // This test is for a string initialized with {}

public:
    void run(unit::tag_t tag)
    {
        __assert_str_eq(_str, "");

        _str = "bat";
        __assert_str_eq("bat", _str);
    }
}

inline void StaticLocal(unit::tag_t tag)
{
    {
        static string _str;
        __assert_str_eq(_str, "");
        _str = "bat";
        __assert_str_eq("bat", _str);
    }

    {
        static string _str = "";
        __assert_str_eq(_str, "");
        _str = "bar";
        __assert_str_eq("bar", _str);
    }

    {
        static string _str = {};
        __assert_str_eq(_str, "");
        _str = "boo";
        __assert_str_eq("boo", _str);
    }
}

inline void Struct(unit::tag_t tag)
{
    struct S
    {
        uint16 i;
        string str;
    }

    S s = {};

    s.i = 3;
    s.str = "test";

    unit::assert_equal(tag, s.i, 3);
    __assert_str_eq("test", s.str);
}

inline void InlineParam(unit::tag_t tag)
{
    inline void helper(string str)
    {
        __assert_str_eq("abcd", str);
    }

    helper("abcd");

    inline void opt_helper(optional<string> str)
    {
        __assert_str_eq("abcd", str.value);
    }

    opt_helper(just("abcd"));

    for (const auto i : 2)
    {
        // Predicated calls
        if (i == 0)
        {
            helper("abcd");

            opt_helper(just("abcd"));
        }
    }
}

inline void InlineReturn(unit::tag_t tag)
{
    inline string helper()
    {
        return "testing";
    }

    {
        string result = helper();

        __assert_str_eq("testing", result);

        // Test the case where the returned string is ignored
        helper();
    }

    inline optional<string> opt_helper()
    {
        return just("panda");
    }

    {
        optional<string> result = opt_helper();

        unit::assert(tag, result.is_valid);

        __assert_str_eq("panda", result.value);
    }

    for (const auto i : 2)
    {
        // Predicated return
        if (i == 0)
        {
            __assert_str_eq("testing", helper());

            {
                optional<string> result = opt_helper();

                unit::assert(tag, result.is_valid);

                __assert_str_eq("panda", result.value);
            }
        }
    }
}

class NonInlineParam
{
private:
    noinline void helper(string str)
    {
        __assert_str_eq("abcd", str);
    }

    noinline void opt_helper(optional<string> str)
    {
        __assert_str_eq("abcd", str.value);
    }

public:
    void run(unit::tag_t tag)
    {
        helper("abcd");

        opt_helper(just("abcd"));

        for (const auto i : 2)
        {
            // Predicated call
            if (i == 0)
            {
                helper("abcd");

                opt_helper(just("abcd"));
            }
        }
    }
}

class NonInlineReturn
{
private:
    noinline string helper()
    {
        return "testing";
    }

    noinline optional<string> opt_helper()
    {
        return just("panda");
    }

public:
    inline void run(unit::tag_t tag)
    {
        {
            string result = helper();

            __assert_str_eq("testing", result);

            // Test the case where the returned string is ignored
            helper();
        }

        {
            optional<string> result = opt_helper();

            unit::assert(tag, result.is_valid);

            __assert_str_eq("panda", result.value);
        }

        for (const auto i : 2)
        {
            // Predicated return
            if (i == 0)
            {
                string result = helper();

                __assert_str_eq("testing", result);

                {
                    optional<string> opt_result = opt_helper();

                    unit::assert(tag, opt_result.is_valid);

                    __assert_str_eq("panda", opt_result.value);
                }
            }
        }
    }
}

inline void Array(unit::tag_t tag)
{
    string[4] str;

    str[0] = "foo";
    str[1] = "bar";
    str[2] = "baz";
    str[3] = "goo";

    for (const auto i : 4)
    {
        string result = str[i];

        string expected;

        switch (i)
        {
        case 0: expected = "foo"; break;
        case 1: expected = "bar"; break;
        case 2: expected = "baz"; break;
        case 3: expected = "goo"; break;
        }

        __assert_str_eq(expected, result);
    }

    for (const auto i : 4)
    {
        string new_value;

        switch (i)
        {
        case 0: new_value = "space"; break;
        case 1: new_value = "ship"; break;
        case 2: new_value = "dog"; break;
        case 3: new_value = "cat"; break;
        }

        str[i] = new_value;
    }

    __assert_str_eq("space", str[0]);
    __assert_str_eq("ship", str[1]);
    __assert_str_eq("dog", str[2]);
    __assert_str_eq("cat", str[3]);
}

inline void PipelinedParam(unit::tag_t tag)
{
    string str = "test";

    pipelined_for(8, [str](index_t<8> i)
    {
        __assert_str_eq("test", str);
    });

    pipelined_map<8>(8, [str](index_t<8> i) -> uint32
    {
        __assert_str_eq("test", str);

        return i;
    });

    for (const auto iter_count : 4)
    {
        string[4] result = pipelined_map<4>(iter_count,  [](index_t<4> i) -> string
        {
            return "zoo";
        });

        static for (const auto i : 4)
        {
            if (i < iter_count)
            {
                __assert_str_eq("zoo", result[i]);
            }
        }
    }

    pipelined_last(8, [str](index_t<8> i) -> uint32
    {
        __assert_str_eq("test", str);

        return i;
    });
}

class Async
{
private:
    bool _done;

    noinline void check_string(string s)
    {
        __assert_str_eq("foo", s);
    }

    void impl(unit::tag_t tag)
    {
        unit::assert(tag, !_done);

        string str = "foo";

        // Code here runs after the calling thread has left
        // this function, and hence the ref held by the local variable
        // `str` has been released.
        async_exec([str]()
        {
            __assert_str_eq("foo", str);

            check_string(str);

            parallel_for(4, [str](index_t<4> i)
            {
                __assert_str_eq("foo", str);
            });

            _done = true;
        });
    }

    [[pipelined, async]] void pipelined_impl(uint32 tid, unit::tag_t tag, string str)
    {
        __assert_str_eq("food", str);

        if (tid == 15)
        {
            _done = true;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        _done = false;
        impl(tag);
        atomic do{} while (!_done);

        _done = false;
        pipelined_impl(16, tag, "food");
        atomic do{} while (!_done);
    }
}

// Uninitialized local strings should default to the empty string
inline void UninitLocal(unit::tag_t tag)
{
   string s;
   __assert_str_eq("", s);

   optional<string> os;
   __assert_str_eq("", os.value);
}

// Strings nested in other types
// should be initialized to the empty string
inline void UninitArray(unit::tag_t tag)
{
    {
        string[4] str;

        static for (const auto i : 4)
        {
            __assert_str_eq("", str[i]);
        }
    }

    {
        optional<string>[4] str;

        static for (const auto i : 4)
        {
            __assert_str_eq("", str[i].value);
        }
    }

    {
        static string[4] _str;

        static for (const auto i : 4)
        {
            __assert_str_eq("", _str[i]);
        }
    }
}

// String return value passed as argument
inline void InlineReturnAsArg(unit::tag_t tag)
{
    inline void helper1(string str)
    {
        __assert_str_eq("foo", str);
    }

    inline string helper2()
    {
        return "foo";
    }

    helper1(helper2());
}

// String return value passed as argument
class NonInlineReturnAsArg
{
private:
    noinline void helper1(string str)
    {
        __assert_str_eq("foo", str);
    }

    noinline string helper2()
    {
        return "foo";
    }

public:
    inline void run(unit::tag_t tag)
    {
        helper1(helper2());
    }
}

inline void Size(unit::tag_t tag)
{
    string str = "abc";

    // Evaluated by the middle-end
    uint32 size = bitsizeof str;
    unit::assert_equal(tag, 0, size);

    // this one is evaluated by the front-end
    unit::assert_equal(tag, 0, bitsizeof str);

    unit::assert_equal(tag, 0, bitsizeof string);
}

inline void AtomicDo(unit::tag_t tag)
{
    uint32 x = 0;

    atomic do
    {
        string s = "iteration";
        print("{s} {x}\n");
        x++;
    } while (x < 2);

    string s = "test";

    atomic do
    {
        print("{s} {x}\n");
        x++;
    } while (x < 3);
}

// Test that atomic blocks are schedulable
// with the implicit atomic block added by string allocation
inline void ParamInAtomic(unit::tag_t tag)
{
    inline void helper(unit::tag_t tag, string str, uint32 i)
    {
        unit::assert_equal(tag, 1, i);
        __assert_str_eq("test", str);
    }

    atomic
    {
        static uint32 _i = 0;
        _i++;
        helper(tag, "test", _i);
    }
}

inline void ClassInterface(unit::tag_t tag)
{
    class Helper
    {
    private:
        (string)->string cb;

    public:
        string Foo(string s)
        {
            return cb(s);
        }
    }

    inline string fn(string s)
    {
        __assert_str_eq(s, "test");
        return "bar";
    }

    static Helper _helper = {.cb = fn};

    string result = _helper.Foo("test");
    __assert_str_eq(result, "bar");
}

const string GlobalConst = "foo";

inline void GlobalConstant(unit::tag_t tag)
{
    __assert_str_eq(GlobalConst, "foo");

    string l = GlobalConst;
    __assert_str_eq(l, "foo");
}

inline void LocalConstant(unit::tag_t tag)
{
    const string LocalConst = "bar";

    __assert_str_eq(LocalConst, "bar");

    string l = LocalConst;
    __assert_str_eq(l, "bar");
}

const uint32 ConstThree = 3;
const string ConstThreeString = "{ConstThree}";

enum Color : uint4
{
    Red = 4,
    Green = 15,
    Blue = 11
}

inline void Interpolation(unit::tag_t tag)
{
    __assert_str_eq(ConstThreeString, "3");

    const auto align = 10;
    uint32 val;

    val = 0xF00;

    __assert_str_eq("{2+2=}", "(2 + 2) = 4");
    __assert_str_eq("{val}", "3840");
    __assert_str_eq("{ val }", "3840");
    __assert_str_eq(" {val} ", " 3840 ");
    __assert_str_eq("{val=}", "val = 3840");
    __assert_str_eq("{bitsizeof val=}", "bitsizeof val = 32");
    __assert_str_eq("{val:d6}", "003840");
    __assert_str_eq("{val:x}", "00000f00");
    __assert_str_eq("{val:x1}", "f00");
    __assert_str_eq("{val:x3}", "f00");
    __assert_str_eq("{0xf00:x}", "f00");
    __assert_str_eq("{0xf00u16:x}", "0f00");
    __assert_str_eq("{val:x4}", "0f00");
    __assert_str_eq("{val:X4}", "0F00");
    __assert_str_eq("{val,5:X4}", " 0F00");
    __assert_str_eq("{val,align-5:X4}", " 0F00");
    __assert_str_eq("{"0x{val:X4}",7}", " 0x0F00");
    __assert_str_eq("{"0x{val:X4}",-7}", "0x0F00 ");
    __assert_str_eq("{val,-5:X4}", "0F00 ");
    __assert_str_eq("{val,5-align:X4}", "0F00 ");
    __assert_str_eq("{val:b}", "00000000000000000000111100000000");
    __assert_str_eq("{val:b16}", "0000111100000000");
    __assert_str_eq("{val>>8:b}", "000000000000000000001111");
    __assert_str_eq("{val*2:b}", "000000000000000000001111000000000");
    __assert_str_eq("{0b111100000000:b}", "111100000000");
    __assert_str_eq("{val:b16}", "0000111100000000");
    __assert_str_eq("{val:o}", "00000007400");
    __assert_str_eq("{0o740:o}", "740");
    __assert_str_eq("{val:o5}", "07400");
    __assert_str_eq("{val*val:o}", "0000000000000070200000");
    __assert_str_eq("{val=:b}", "val = 00000000000000000000111100000000");

    const string constStr = "{val}";
    __assert_str_eq(constStr, "3840");

    val = 0;
    __assert_str_eq("{val=}", "val = 0");

    int32 signed_val = 3;
    __assert_str_eq("{signed_val}", "3");
    __assert_str_eq("{signed_val:x}", "00000003");

    signed_val = -5;
    __assert_str_eq("{signed_val}", "-5");
    __assert_str_eq("{signed_val:d1}", "-5");
    __assert_str_eq("{cast<int12>(signed_val)}", "-5");
    __assert_str_eq("{signed_val:x4}", "fffffffb");
    __assert_str_eq("{signed_val:x}", "fffffffb");

    bool bool_val = true;
    __assert_str_eq("{bool_val}", "true");
    __assert_str_eq("{bool_val,5}", " true");
    __assert_str_eq("{bool_val,-6}", "true  ");
    __assert_str_eq("{bool_val:b}", "1");
    __assert_str_eq("{bool_val,3:b}", "  1");

    bool_val = false;
    __assert_str_eq("{bool_val}", "false");
    __assert_str_eq("{bool_val,5}", "false");
    __assert_str_eq("{bool_val,-6}", "false ");
    __assert_str_eq("{bool_val:b}", "0");
    __assert_str_eq("{bool_val,-3:b}", "0  ");

    float fval = -2.3;
    __assert_str_eq("{fval}", "-2.300000");
    __assert_str_eq("{fval:d}", "3222483763");
    __assert_str_eq("{fval:x}", "c0133333");
    __assert_str_eq("{fval:o}", "30004631463");
    __assert_str_eq("{fval:b}", "11000000000100110011001100110011");

    Color c = Color::Green;
    __assert_str_eq("{c}", "Green");
    __assert_str_eq("{c:d}", "15");
    __assert_str_eq("{c:x}", "f");
    __assert_str_eq("{c:o}", "17");
    __assert_str_eq("{c:b}", "1111");
    __assert_str_eq("{c,8}", "   Green");
    __assert_str_eq("{Color::Red:b}", "0100");

    // Enum with a value that is not any one of the enumerators
    c = cast<Color>(6);
    __assert_str_eq("{c}", "6 does not represent a valid Color");

    struct S
    {
        uint32 a;
        bool b;
    }

    S s = {45, true};
    __assert_str_eq("{s}", "{{a:45, b:true}");

    uint32[4] a = { 4, 2, 9, 10 };
    __assert_str_eq("{a}", "[4, 2, 9, 10]");

    union U
    {
        uint32 a;
        bool b;
    }

    U u;
    u.b = true;

    __assert_str_eq("{u}", "{{a:1, b:true}");

    u.a = 0;
    __assert_str_eq("{u}", "{{a:0, b:false}");

    string str = "foo";
    __assert_str_eq("{str,-5}", "foo  ");

    struct LargeStruct
    {
        int16 a;
        bool b;
        Color c;
        string s;
    }

    LargeStruct[2] lsa = {{-4, true, Color::Blue, "test"}, {27, false, Color::Red, "pass"}};
    __assert_str_eq("{lsa}", "[{{a:-4, b:true, c:Blue, s:test}, {{a:27, b:false, c:Red, s:pass}]");

    union U2
    {
        S s;
        uint48 c;
    }

    U2 u2;
    u2.c = 0xaf12345678;

    __assert_str_eq("{u2}", "{{s:{{a:305419896, b:true}, c:751924696696}");

    // Testing for dynamic values for which
    // constant propagation does not apply
    for (const auto i : 2)
    {
        string str1 = "{i}";
        if (i == 0)
        {
            __assert_str_eq(str1, "0");
        }
        else
        {
            __assert_str_eq(str1, "1");
        }
    }

    // Interpolating a closure (which captures this)
    class ClosureClass
    {
    private:
        uint32 _member;

    public:
        void test(uint32 val)
        {
            const auto fn = [val]()
            {
                assert(_member == val); // this code never runs
            };

            __assert_str_eq("{fn}", "{{x1:3}");
        }
    }

    static ClosureClass _c;
    _c.test(3);

    uint32 loopCounter = 0;
    string loopResult;

    atomic do
    {
        loopResult = "{loopCounter=}";
        loopCounter++;
    } while (loopCounter < 3);

    __assert_str_eq(loopResult, "loopCounter = 2");

    pipelined_for(2, [](index_t<2> i)
    {
        pipelined_for(2, [i](index_t<2> j)
        {
            if (i == 0)
            {
                string s;

                if (j == 0)
                {
                    s = "{j}";
                }

                barrier;

                string s2 = "{s}y";

                string expected = j == 0 ? "0y" : "y";

                __assert_str_eq(s2, expected);
            }
        });
    });
}

inline void test_main()
{
    // These tests run before the first read of the string count
    // These tests do allocate strings (in shared variables) which are not freed
    // That is by design as the allocated strings are referenced by class members
    // or static locals.
    unit::test<1>(unit::fixture<UninitializedClassMember>());
    unit::test<2>(unit::fixture<InitializedClassMember>());
    unit::test<3>(unit::fixture<InitializerClassMember>());
    unit::test<4>(StaticLocal);

    // The remaining tests should not leak string handles
    // __str_cnt is used to check for leaks

    // Get the size of the string table before running the remaining test cases.
    uint64 string_count_before = __str_cnt();

    unit::test<5>(Ternary);
    unit::test<6>(Mux);
    unit::test<7>(Struct);
    unit::test<8>(InlineParam);
    unit::test<9>(InlineReturn);
    unit::test<10>(unit::fixture<NonInlineReturn>());
    unit::test<11>(unit::fixture<NonInlineParam>());
    unit::test<12>(Array);
    unit::test<14>(PipelinedParam);
    unit::test<15>(unit::fixture<Async>());
    unit::test<16>(UninitArray);
    unit::test<17>(InlineReturnAsArg);
    unit::test<18>(unit::fixture<NonInlineReturnAsArg>());
    unit::test<19>(Size);
    unit::test<20>(AtomicDo);
    unit::test<21>(ParamInAtomic);
    unit::test<22>(ClassInterface);
    unit::test<23>(GlobalConstant);
    unit::test<24>(LocalConstant);
    unit::test<25>(Interpolation);
    unit::test<26>(UninitLocal);

    // Get the size of the string table after running the tests
    uint64 string_count_after = __str_cnt();

    print("string_count_before {string_count_before} string_count_after {string_count_after}\n");

    // All tests that run after the first call to __str_cnt should free all strings that they allocate.
    // Thus the number of strings in the string table should be the unchanged before and after
    // running the tests.
    assert(string_count_after == string_count_before);
}
