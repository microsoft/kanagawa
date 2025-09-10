// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import control.fsm
import data.optional
import test.unit as unit
import test.runner

inline void speculate_updates_test(unit::tag_t tag)
{
    optional<index_t<4>> override = {};
    uint32[4] result = speculate_updates([](index_t<4> i){return cast<uint32>(i + 1);}, override);

    static for (const auto i : 4)
    {
        unit::assert_equal(tag, i + 1, result[i]);
    }

    override = just(cast<index_t<4>>(2));
    result = speculate_updates([](index_t<4> i){return cast<uint32>(i + 1);}, override);

    static for (const auto i : 4)
    {
        unit::assert_equal(tag, 3, result[i]);
    }
}

inline void apply_update_test(unit::tag_t tag)
{
    int8[4] speculative_updates = { 1, 2, -1, -3 };

    auto fn = apply_update(
        [](int8[4] updates, uint8 state) -> int32
        { return (state < 4) ? updates[state] : -1; },
        [](int32 update, uint8 state) -> uint8 { return cast<uint8>(state + update); },
        speculative_updates);

    pipelined_for(8, [tag, fn](uint8 prev)
    {
        uint8 result = fn(prev);

        uint8[8] expected = { 1, 3, 1, 0, 3, 4, 5, 6 };

        unit::assert_equal(tag, expected[prev], result);
    });
}

inline void test_main()
{
    unit::test<1>(speculate_updates_test);
    unit::test<2>(apply_update_test);
}
