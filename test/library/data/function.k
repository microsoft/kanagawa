// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.function
import data.array
import data.order
import data.tuple
import test.unit as unit
import test.runner

inline void test_constant(unit::tag_t tag)
{
    unit::assert_equal(tag,
            repeat<3>(false),
            map(constant(false), {1, 2, 3}));

    unit::assert_equal(tag,
            repeat<3>(3),
            map(constant(3), {1, 2, 3}));
}

inline void test_bind(unit::tag_t tag)
{
    inline auto sub(auto x, auto y)
    {
        return x - y;
    }

    auto xs = map(id, {5, 4, 3});

    unit::assert_equal(tag,
            zip_with(sub, repeat<3>(6), xs),
            map(bind1st(sub, 6), xs));

    unit::assert_equal(tag,
            zip_with(sub, xs, repeat<3>(2)),
            map(bind2nd(sub, 2), xs));
}

inline void test_curry(unit::tag_t tag)
{
    auto fn = uncurry(constant(constant(2)));

    unit::assert_equal(tag,
            2,
            fn(10, 20));

    inline auto calc(uint32 x, uint32 y)
    {
        return x + 3 * y - 9;
    }

    auto fn2 = curry(calc);
    auto fn3 = fn2(10);
    auto fn4 = uncurry(curry(calc));

    unit::assert_equal(tag,
            calc(10, 20),
            fn3(20));

    unit::assert_equal(tag,
            calc(7, 26),
            fn4(7, 26));

    auto fn5 = uncurry3(constant(curry(calc)));

    unit::assert_equal(tag,
            calc(97, 16),
            fn5(false, 97, 16));

    inline auto calc3(uint32 x, uint32 y, uint32 z)
    {
        return x + 3 * y - z;
    }

    unit::assert_equal(tag,
            calc3(4, 7, 10),
            apply(apply(apply(curry3(calc3), 4), 7), 10));

    auto fn6 = uncurry(apply(curry3(calc3), 55));

    unit::assert_equal(tag,
            calc3(55, 17, 13),
            fn6(17, 13));
}

inline void test_compose(unit::tag_t tag)
{
    inline uint32 add(uint32 x, uint32 y)
    {
        return x + y;
    }

    inline int32 neg(auto x)
    {
        return -x;
    }

    auto fn = compose(bind2nd(add, 7), bind1st(add, 3));

    unit::assert_equal(tag,
            10 + 7 + 3,
            fn(10));

    int32[2] result = {-4, -6};

    unit::assert_equal(tag,
            result,
            zip_with(compose2(neg, add), {1, 2}, {3, 4}));
}

inline void test_selecting(unit::tag_t tag)
{
    auto fn = selecting(less_than);

    unit::assert_equal(tag,
            5,
            fn(5, 6));

    unit::assert_equal(tag,
            4,
            fn(5, 4));
}

inline void test_flip(unit::tag_t tag)
{
    auto fn1 = flip(less_than);
    auto fn2 = flip(flip(less_than));

    auto inputs = zip_with(make_pair<uint4, uint4>, {1, 3, 15}, {2, 3, 7});

    pipelined_for_each(inputs, [fn1, fn2, tag](index_t<3> i, pair<uint4, uint4> x)
    {
        unit::assert_equal(tag,
                less_than(x.first, x.second),
                fn1(x.second, x.first));

        unit::assert_equal(tag,
                less_than(x.first, x.second),
                fn2(x.first, x.second));
    });
}

inline void test_apply(unit::tag_t tag)
{
    inline uint32 add(uint32 x, uint32 y)
    {
        return x + y;
    }

    unit::assert_equal(tag,
            add(2, 5),
            apply(apply(curry(add), 2), 5));

    unit::assert_equal(tag,
            false,
            apply(constant(false), true));

    unit::assert_equal(tag,
            10,
            apply(apply(constant(constant(10)), false), 3.14));
}

inline void test_compose_endo(unit::tag_t tag)
{
    inline uint8 increment(uint8 x)
    {
        return x + 1;
    }
    unit::assert(tag, 4 == apply(compose_endo<3>(increment), 1));
    unit::assert(tag, 7 == apply(compose_endo<3>(compose_endo<2>(increment)), 1));

    auto inv = [](bool x){ return !x; };
    unit::assert(tag, !apply(compose_endo<1>(inv), true));
    unit::assert(tag,  apply(compose_endo<2>(inv), true));
}

inline void test_main()
{
    unit::test<1>(test_constant);
    unit::test<2>(test_bind);
    unit::test<3>(test_curry);
    unit::test<4>(test_compose);
    unit::test<5>(test_selecting);
    unit::test<6>(test_flip);
    unit::test<7>(test_apply);
    unit::test<8>(test_compose_endo);
    unit::check<9>(bind(id, true));
    unit::check<10>(bind(constant(true), false));
}
