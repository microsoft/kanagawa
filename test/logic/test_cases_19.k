// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Unsigned integer multiplication - implemented with multiple DSPs
inline void Helper560(uint64 a, uint64 b, uint64 expected, unit::tag_t tag)
{
    uint64 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void TestCase560(unit::tag_t tag)
{
    Helper560(3, 7, 21, tag);
    Helper560(0xDEADBEEF, 0x12345678, 0xFD5BDEE5621CA08, tag);
}

// Signed integer multiplication - implemented with multiple DSPs
inline void Helper563(int64 a, int64 b, int64 expected, unit::tag_t tag)
{
    int64 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void TestCase563(unit::tag_t tag)
{
    Helper563(3, 7, 21, tag);
    Helper563(-3, 7, -21, tag);
    Helper563(3, -7, -21, tag);
    Helper563(-3, -7, 21, tag);
}

// Signed integer multiplication - large values
inline void Helper564(int64 a, int64 b, int128 expected, unit::tag_t tag)
{
    int128 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void TestCase564(unit::tag_t tag)
{
    // most negative * most negative
    Helper564(-9223372036854775808, -9223372036854775808, 85070591730234615865843651857942052864, tag);

    // one below most negative * most negative
    Helper564(-9223372036854775807, -9223372036854775808, 85070591730234615856620279821087277056, tag);

    // one below most negative * one below most negative
    Helper564(-9223372036854775807, -9223372036854775807, 85070591730234615847396907784232501249, tag);

    // most negative * -1
    Helper564(-9223372036854775808, -1, 9223372036854775808, tag);

    // most negative * -2
    Helper564(-9223372036854775808, -2, 18446744073709551616, tag);

    // most negative * most positive
    Helper564(-9223372036854775808, 9223372036854775807, -85070591730234615856620279821087277056, tag);

    // one below most negative * most positive
    Helper564(-9223372036854775807, 9223372036854775807, -85070591730234615847396907784232501249, tag);

    // one below most negative * one below most positive
    Helper564(-9223372036854775807, 9223372036854775806, -85070591730234615838173535747377725442, tag);

    // most positive * most positive
    Helper564(9223372036854775807, 9223372036854775807, 85070591730234615847396907784232501249, tag);

    // one below most positive * most positive
    Helper564(9223372036854775806, 9223372036854775807, 85070591730234615838173535747377725442, tag);

    // one below most positive * one below most positive
    Helper564(9223372036854775806, 9223372036854775806, 85070591730234615828950163710522949636, tag);

    // Random values
    Helper564(-2793956154089394880, -4391774912661651384, 12270426544606435595433815942074513920, tag);

    Helper564(-2793956154089394880, 4391774912661651384, -12270426544606435595433815942074513920, tag);

    Helper564(-3823711255420530967, -2503674716406219498, 9573329193034267396046808240308194566, tag);

    Helper564(3823711255420530967, -2503674716406219498, -9573329193034267396046808240308194566, tag);
}

inline void test_main()
{
    unit::test<560>(TestCase560);
    unit::test<563>(TestCase563);
    unit::test<564>(TestCase564);
}
