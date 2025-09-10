// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.optional
import numeric.int
import test.unit as unit
import test.runner

inline void test_from_optional(unit::tag_t tag)
{
    unit::assert_equal(tag, 13, from_optional(11, make_optional(true, 13)));
    unit::assert_equal(tag, 11, from_optional(11, make_optional(false, 13)));
}

inline void test_join(unit::tag_t tag)
{
    unit::assert_equal(tag, {false, 0xFF}, join({false, {false, 0xFF}}));
    unit::assert_equal(tag, {false, 0xFF}, join({false, {true,  0xFF}}));
    unit::assert_equal(tag, {false, 0xFF}, join({true,  {false, 0xFF}}));
    unit::assert_equal(tag, {true,  0xFF}, join({true,  {true,  0xFF}}));

    unit::assert_equal(tag, {true, {false, 0}}, join<optional<uint8>>({true, {true, {false, 0}}}));
}

inline void test_equal_by(unit::tag_t tag)
{
    auto both_even = [](auto x, auto y) -> bool
                     {
                         return even(x) && even(y);
                     };

    auto a = equal_by(both_even, {true, 0}, {false, 2});
    unit::assert(tag, !a);

    auto b = equal_by(both_even, {true, 0}, {true, 2});
    unit::assert(tag, b);

    auto c = equal_by(both_even, {false, 0}, {false, 3});
    unit::assert(tag, c);
}

class test_lift_optional
{
private:
    uint32 _expectedCallCount = 0;
    uint32 _actualCallCount = 0;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(256, [tag](uint8 i)
        {
            const auto convert = lift_optional([](uint8 a)
            {
                atomic
                {
                    _actualCallCount++;
                }

                return cast<uint32>(a) | cast<uint32>(a << 24);
            });

            optional<uint8> src = {};

            if (0 == (i % 4))
            {
                // value should be ignored
                src = make_optional(false, ~i);
            }
            else
            {
                src = make_optional(true, i);

                atomic
                {
                    _expectedCallCount++;
                }
            }

            optional<uint32> actual = convert(src);

            unit::assert_equal(tag, src.is_valid, actual.is_valid);

            if (actual.is_valid)
            {
                unit::assert_equal(tag, src.value | (src.value << 24), actual.value);
            }
        });

        // Verify that side-effects of the conversion function only apply
        // when is_valid == true
        unit::assert_equal(tag, _expectedCallCount, _actualCallCount);
    }
}

class test_lift2_optional
{
private:
    uint32 _expectedCallCount = 0;
    uint32 _actualCallCount = 0;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(8, [tag](uint8 i)
        {
            pipelined_for(8, [tag, i](uint8 j)
            {
                const auto convert = lift2_optional([](uint8 a, uint8 b)
                {
                    atomic
                    {
                        _actualCallCount++;
                    }

                    return cast<uint32>(a) | cast<uint32>(b << 24);
                });

                optional<uint8> src1 = {};

                if (0 == (i % 4))
                {
                    // value should be ignored
                    src1 = make_optional(false, ~i);
                }
                else
                {
                    src1 = make_optional(true, i);
                }

                optional<uint8> src2 = {};

                if (0 == (j % 8))
                {
                    // value should be ignored
                    src2 = make_optional(false, ~j);
                }
                else
                {
                    src2 = make_optional(true, j);
                }

                if (src1.is_valid && src2.is_valid)
                {
                    atomic
                    {
                        _expectedCallCount++;
                    }
                }

                optional<uint32> actual = convert(src1, src2);

                unit::assert_equal(tag, src1.is_valid && src2.is_valid, actual.is_valid);

                if (actual.is_valid)
                {
                    unit::assert_equal(tag, src1.value | (src2.value << 24), actual.value);
                }
            });
        });

        // Verify that side-effects of the conversion function only apply
        // when is_valid == true
        unit::assert_equal(tag, _expectedCallCount, _actualCallCount);
    }
}

inline void test_lift_checked_cast(unit::tag_t tag)
{
    const auto check_cast_opt = lift_optional(checked_cast<uint8>);

    auto src = make_optional<uint32>(true, 255);
    optional<uint8> result = check_cast_opt(src);
    unit::assert(tag, result.is_valid);
    unit::assert_equal(tag, 255, result.value);

    src = make_optional<uint32>(false, 256);
    result = check_cast_opt(src);
    unit::assert(tag, !result.is_valid);
}

inline void test_lift_reinterpret_cast(unit::tag_t tag)
{
    const auto reinterpret_cast_opt = lift_optional(reinterpret_cast<uint8[2]>);

    auto src = make_optional<uint8[4]>(true, {1, 2, 3, 4});
    optional<uint8[2]> result = reinterpret_cast_opt(src);
    unit::assert(tag, result.is_valid);
    unit::assert_equal(tag, 1, result.value[0]);
    unit::assert_equal(tag, 2, result.value[1]);

    src = make_optional<uint8[4]>(false, {});
    result = reinterpret_cast_opt(src);
    unit::assert(tag, !result.is_valid);
}

inline void test_main()
{
    unit::test<1>(test_from_optional);
    unit::test<2>(test_join);
    unit::test<3>(test_equal_by);
    unit::test<4>(unit::fixture<test_lift_optional>());
    unit::test<5>(unit::fixture<test_lift2_optional>());
    unit::test<6>(test_lift_checked_cast);
    unit::test<7>(test_lift_reinterpret_cast);
}
