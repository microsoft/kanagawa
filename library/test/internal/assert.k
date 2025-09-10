// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module test.internal.assert
    { module test.internal.assert
    }

import debug.sim_assert
import type.newtype

using tag_t = newtype<uint32>;

inline void print_failure(tag_t tag)
{
    print("Error: test case {tag.unwrap:x} failed\n");
    print("Isolate the test case by compiling with --define tag=0x{tag.unwrap:x}\n");
}

//| Fail test with the specified tag when the condition is false. Test failure
// triggers `sim_assert`, prints information about failing test, and sets
// global status to the test tag.
inline void assert(tag_t tag, bool cond)
{
    if (!cond)
    {
        print_failure(tag);
        sim_assert(false);
    }
}

//| Fail test with the specified tag when the condition is false. Test failure
// triggers `sim_assert`, prints information about failing test and the supplied
// message, and sets global status to the test tag.
inline void assert_string(tag_t tag, bool cond, const string message)
{
    if (!cond)
    {
        print(message);
        print("\n");
        print_failure(tag);
        sim_assert(false);
    }
}

//| Fail test with the specified tag when the arguments are not equal.
// Test failure triggers `sim_assert`, prints information about failing test,
// and sets global status to the test tag.
template <typename T>
inline void assert_equal
    ( tag_t tag     //< Test tag
    , T expected    //< Expected value
    , T actual      //< Actual value
    )
{
    static if (T == string)
    {
        __assert_str_eq(expected, actual);
    }
    else if (reinterpret_cast<uint<bitsizeof T>>(expected) != reinterpret_cast<uint<bitsizeof T>>(actual))
    {
        print("{expected=}\n{actual=}\n");
        print_failure(tag);
        sim_assert(false);
    }
}