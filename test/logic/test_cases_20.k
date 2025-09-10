// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Unsigned integer multiplication - large values
inline void Helper565(uint64 a, uint64 b, uint128 expected, unit::tag_t tag)
{
    uint128 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void TestCase565(unit::tag_t tag)
{
    // most positive * most positive
    Helper565(18446744073709551615, 18446744073709551615, 340282366920938463426481119284349108225, tag);

    // one below most positive * most positive
    Helper565(18446744073709551614, 18446744073709551615, 340282366920938463408034375210639556610, tag);

    // one below most positive * one below most positive
    Helper565(18446744073709551614, 18446744073709551614, 340282366920938463389587631136930004996, tag);

    // most positive * random
    Helper565(18446744073709551615, 4391774912661651383, 81013947843207601285743061646374633545, tag);

    // random * random
    Helper565(3823711255420530967, 4391774912661651383, 16792879164817875750931360464909877361, tag);
}

// Unsigned integer * signed integer (both directions)
inline void Helper566_UnsignedSigned(uint64 a, int64 b, int128 expected, unit::tag_t tag)
{
    int128 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void Helper566_SignedUnsigned(int64 a, uint64 b, int128 expected, unit::tag_t tag)
{
    int128 actual = a * b;

    unit::assert(tag, actual == expected);
}

inline void Helper566(uint64 a, int64 b, int128 expected, unit::tag_t tag)
{
    Helper566_UnsignedSigned(a, b, expected, tag);

    Helper566_SignedUnsigned(b, a, expected, tag);
}

inline void TestCase566(unit::tag_t tag)
{
    // most positive * most positive
    Helper566(18446744073709551615, 9223372036854775807, 170141183460469231704017187605319778305, tag);

    // one below most positive * most positive
    Helper566(18446744073709551614, 9223372036854775807, 170141183460469231694793815568465002498, tag);

    // most positive * one below most positive
    Helper566(18446744073709551615, 9223372036854775806, 170141183460469231685570443531610226690, tag);

    // one below most positive * one below most positive
    Helper566(18446744073709551614, 9223372036854775806, 170141183460469231676347071494755450884, tag);

    // most positive * most negative
    Helper566(18446744073709551615, -9223372036854775808, -170141183460469231722463931679029329920, tag);

    // one below most positive * most negative
    Helper566(18446744073709551614, -9223372036854775808, -170141183460469231713240559642174554112, tag);

    // most positive * one below most negative
    Helper566(18446744073709551615, -9223372036854775807, -170141183460469231704017187605319778305, tag);

    // one below most positive * one below most negative
    Helper566(18446744073709551614, -9223372036854775807, -170141183460469231694793815568465002498, tag);

     // Random values
    Helper566(3823711255420530967, 2503674716406219498, 9573329193034267396046808240308194566, tag);

    Helper566(3823711255420530967, -2503674716406219498, -9573329193034267396046808240308194566, tag);
}

inline void test_main()
{
    unit::test<565>(TestCase565);
    unit::test<566>(TestCase566);
}
