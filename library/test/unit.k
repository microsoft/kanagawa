// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|

The simple unit test framework allows defining test cases that can be
conditionally included at compile time. Test cases are identified by tags and
can be selected to be built/executed using two values specified via compiler
`--define` option:

- tag - identifies a specific test to be included
- mask - specifies a bit mask of tests to be included

A test is included in the build/execution if its tag is equal to specified tag,
or if its tag is included in the bit mask. If both tag and mask are zero, which
is the default, all tests are included.

Test tags are constructed from three parts:

- 12 bit test identifier
- 4 bit group identifier
- 16 bit flags

Only the test identifier is required. Group identifier and flags are optional
and set to 0 by default.

Typical usage scenarios are:

- isolate a specific test to build/execute:

    `--define tag=5`

- build/execute tests from a specific group, for example to make
  build/simulation smaller:

    `--define mask=0x2FFF`

- build/execute a cross section of all tests with specified flag(s):

    `--define mask=0x4FFFF`

Verbosity of the test framework output is controlled by boolean value `verbose`
that can be defined via compiler `--define` command line option. With verbose
output the framework prints information about all skipped and executed test
cases. By default only information about failed test case(s) is printed.

*/
module test.unit
    { test
    , check
    , fixture
    , module test.internal.assert
    }

import test.internal.params as param
import test.internal.assert as unit

using id_t = uint12;
using group_t = uint4;
using flags_t = uint16;

template <id_t Test, group_t Group = 0, flags_t Flags = 0, typename TestResult>
inline void test_case((unit::tag_t) -> TestResult fn)
{
    static assert(Test != 0);

    const auto T = Flags << (bitsizeof id_t + bitsizeof group_t);
    const auto A = Group << (bitsizeof id_t);
    const auto G = Test;

    const uint32 tag = T | A | G;

    assert(tag == concat(cast<flags_t>(Flags), cast<group_t>(Group), cast<id_t>(Test)));

    static if (((tag & param::mask) == tag) || tag == param::tag || (param::tag == 0 && param::mask == 0))
    {
        bool succeeded;

        static if (TestResult == void)
        {
            fn({tag});
            succeeded = true;
        }
        else
        {
            succeeded = fn({tag});
            unit::assert({tag}, succeeded);
        }

        if (static(param::verbose) && succeeded)
        {
            print("Test case {tag:x} succeeded\n");
        }
    }
    else static if (param::verbose)
    {
        print("Skipping disabled test case {tag:x}\n");
    }

    barrier;
}

//| Add a test case that returns `true` for success and `false` for failure.
template
    < auto Test         //< Test id
    , auto Group = 0    //< Optional test group id, by default 0
    , auto Flags = 0    //< Optional test flags, by default 0
    >
inline void check(() -> bool fn /*< Test case function */)
{
    static assert(Test == cast<id_t>(Test));
    static assert(Group == cast<group_t>(Group));
    static assert(Flags == cast<flags_t>(Flags));

    test_case<Test, Group, Flags>([fn](unit::tag_t tag)
    {
        return fn();
    });
}

//| Add a test case that takes tag argument and uses the assert helpers to
// indicate failures.
template
    < auto Test         //< Test id
    , auto Group = 0    //< Optional test group id, by default 0
    , auto Flags = 0    //< Optional test flags, by default 0
    >
inline void test((unit::tag_t) -> void fn /*< Test case function */)
{
    static assert(Test == cast<id_t>(Test));
    static assert(Group == cast<group_t>(Group));
    static assert(Flags == cast<flags_t>(Flags));

    test_case<Test, Group, Flags>([fn](unit::tag_t tag)
    {
        return fn(tag);
    });
}

//| Use the specified `Fixture` class to implement a test. A fixture used with
// `test` function must implement a public method `void run(tag_t tag)` and use
// asserts to indicate test failure. A fixture used with `check` must implement
// a public method `bool run()` that returns `true` for success and `false` for
// failure.
//
// #### __Example__
//
// class Foo
// {
//    public:
//      void run(unit::tag_t tag)
//      {
//          unit::assert_equal(tag, 4, 2 + 2);
//      }
// }
//
// unit::test<1>(unit::fixture<Foo>());
//
template
    < typename Fixture  //< Fixture class
    >
inline auto fixture()
{
    static if (decltype(Fixture::run) == () -> bool)
    {
        return []
        {
            static Fixture test;
            return test.run();
        };
    }
    else
    {
        return [](unit::tag_t tag)
        {
            static Fixture test;
            test.run(tag);
        };
    }
}

