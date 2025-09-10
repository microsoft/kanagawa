// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import numeric.int.multi_word
import numeric.int.multi_word.internal
import test.unit as unit
import test.runner

template<typename T>
inline T rng1(auto itr)
{
    return cast<T>(0xFFFFFF*itr);
}

template<typename T>
inline T rng2(auto itr)
{
    return cast<T>(0xFFF*itr);
}

template<typename T>
inline T rng3(auto itr)
{
    return cast<T>(0xFFFFFF*itr);
}

template<typename T>
inline T rng4(auto itr)
{
    return cast<T>(0xF*-1*itr);
}

template<typename T>
inline T rng5(auto itr)
{
    return cast<T>(7*itr + 1393);
}

template<typename T>
inline T rng6(auto itr)
{
    return cast<T>(31*itr + 1);
}

template<typename T>
inline T randomly_invert(auto val, auto itr)
{
    int2 sign = itr % 2 == 0 ? 1 : -1;

    return cast<T>(val * sign);
}

inline void add_with_carry_test(unit::tag_t tag)
{
    pipelined_for(16, [tag](uint8 itr){
        uint32 a = rng1(itr);
        uint32 b = rng2(itr);
        uint1 carry = itr % 2;

        auto p = add_with_carry(a, b, carry);
        unit::assert_equal(tag, p.first, cast<uint32>(a + b + carry));
        unit::assert_equal(tag, p.second, cast<uint1>((a + b + carry) >> 32));
    });

    pipelined_for(16, [tag](uint8 itr){
        int32 a = randomly_invert(rng3<int32>(itr), itr);
        int32 b = rng2(itr);
        uint1 carry = itr % 2;

        auto p = add_with_carry(a, b, carry);
        unit::assert_equal(tag, p.first,  cast<int32>(a + b + carry));
        unit::assert_equal(tag, p.second, cast<uint1>((a + b + carry) >> 32));
    });

    pipelined_for(16, [tag](uint8 itr){
        uint32 a = rng1(itr);
        int9 b = rng4(itr);
        uint1 carry = itr % 2;

        auto p = add_with_carry(a, b, carry);
        using sum_t = decltype(p.first);
        unit::assert_equal(tag, p.first,  cast<sum_t>(a + b + carry));
        unit::assert_equal(tag, p.second, cast<uint1>((a + b + carry) >> bitsizeof(sum_t)));
    });

    pipelined_for(16, [tag](uint4 a){
        pipelined_for(16, [tag, a](uint4 b) {
            pipelined_for(2, [tag, a, b](uint1 carry){
                auto p = add_with_carry(a, b, carry);
                using sum_t = decltype(p.first);
                unit::assert_equal(tag, p.first,  cast<sum_t>(a + b + carry));
                unit::assert_equal(tag, p.second, cast<uint1>((a + b + carry) >> bitsizeof(sum_t)));
            });
        });
    });

    auto p1 = add_with_carry(0xFFFFFFFF, 0xFFFFFFFF, 1);
    auto p2 = add_with_carry(-1, 1, 0);
    auto p3 = add_with_carry(-1, 1, 1);

    unit::assert_equal(tag, p1.first, cast<uint32>(0xFFFFFFFF + 0xFFFFFFFF + 1));
    unit::assert_equal(tag, p1.second, cast<uint1>((0xFFFFFFFF + 0xFFFFFFFF + 1) >> 32));

    unit::assert_equal(tag, p2.first, 0);
    unit::assert_equal(tag, p2.second, 0);

    unit::assert_equal(tag, p3.first, 1);
    unit::assert_equal(tag, p3.second, 0);
}

inline void sub_with_borrow_test(unit::tag_t tag)
{
    pipelined_for(16, [tag](uint8 itr){
        uint32 a = rng1(itr);
        uint32 b = rng2(itr);
        uint1 borrow = itr % 2;

        auto p = sub_with_borrow(a, b, borrow);
        unit::assert_equal(tag, p.first, cast<uint32>(a - b - borrow));
        unit::assert_equal(tag, p.second, cast<uint1>((a  - b - borrow) >> 32));
    });

    pipelined_for(16, [tag](uint8 itr){
        int32 a = randomly_invert(rng3<int32>(itr), itr);
        int32 b = rng2(itr);
        uint1 borrow = itr % 2;

        auto p = sub_with_borrow(a, b, borrow);
        unit::assert_equal(tag, p.first,  cast<int32>(a - b - borrow));
        unit::assert_equal(tag, p.second, cast<uint1>((a - b - borrow) >> 32));
    });

    pipelined_for(16, [tag](uint8 itr){
        uint32 a = rng1(itr);
        int9 b = rng4(itr);
        uint1 borrow = itr % 2;

        auto p = sub_with_borrow(a, b, borrow);
        using sub_t = decltype(p.first);
        unit::assert_equal(tag, p.first,  cast<sub_t>(a - b - borrow));
        unit::assert_equal(tag, p.second, cast<uint1>((a - b - borrow) >> bitsizeof(sub_t)));
    });

    pipelined_for(16, [tag](uint4 a){
        pipelined_for(16, [tag, a](uint4 b){
            pipelined_for(2, [tag, a, b](uint1 borrow){
                auto p = sub_with_borrow(a, b, borrow);
                using sub_t = decltype(p.first);
                unit::assert_equal(tag, p.first,  cast<sub_t>(a - b - borrow));
                unit::assert_equal(tag, p.second, cast<uint1>((a - b - borrow) >> bitsizeof(sub_t)));
            });
        });
    });

    auto t1 = sub_with_borrow(5, 2, 1);
    auto t2 = sub_with_borrow(0, 1, 0);
    unit::assert_equal(tag, t1.first , 2);
    unit::assert_equal(tag, t1.second, 0);

    unit::assert_equal(tag, t2.first,  1);
    unit::assert_equal(tag, t2.second, 1);
}


inline void min_with_metadata_test(unit::tag_t tag)
{
    pipelined_for(1024, [tag](uint32 itr){
        uint32 a = rng5(itr);
        uint32 b = rng6(itr);

        auto p = min_with_metadata(a, b, get_initial_comp_metadata());
        unit::assert_equal(tag, p.first, a < b ? a : b);
        unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
        unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a <= b);
    });

    pipelined_for(1024, [tag](uint32 itr){
        int32 a = randomly_invert(rng5<int32>(itr), itr);
        int32 b = rng6(itr);

        auto p = min_with_metadata(a, b, get_initial_comp_metadata());
        unit::assert_equal(tag, p.first, a < b ? a : b);
        unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
        unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a <= b);
    });

    pipelined_for(1024, [tag](uint32 itr){
        uint32 a = rng5(itr);
        int20  b = randomly_invert(rng6<int32>(itr), itr);

        auto p = min_with_metadata(a, b, get_initial_comp_metadata());
        unit::assert_equal(tag, p.first, a < b ? a : b);
        unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
        unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a <= b);
    });

    pipelined_for(16, [tag](uint4 a){
        pipelined_for(16, [tag, a](uint4 b){
            auto p = min_with_metadata(a, b, get_initial_comp_metadata());
            unit::assert_equal(tag, p.first, a < b ? a : b);
            unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
            unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a <= b);
        });
    });

    auto t1 = min_with_metadata(1, 2, get_initial_comp_metadata());
    unit::assert_equal(tag, t1.first , 1);
    unit::assert_equal(tag, t1.second.all_previous_words_eq, false);
    unit::assert_equal(tag, t1.second.select_a_words_on_prevnoteq, true);

    auto t2 = min_with_metadata(10, 10, get_initial_comp_metadata());
    unit::assert_equal(tag, t2.first , 10);
    unit::assert_equal(tag, t2.second.all_previous_words_eq, true);
}

inline void max_with_metadata_test(unit::tag_t tag)
{
    pipelined_for(1024, [tag](uint32 itr){
        uint32 a = rng5(itr);
        uint32 b = rng6(itr);

        auto p = max_with_metadata(a, b, get_initial_comp_metadata());
        unit::assert_equal(tag, p.first, a > b ? a : b);
        unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
        unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a >= b);

    });

    pipelined_for(1024, [tag](uint32 itr){
        int32 a = randomly_invert(rng5<int32>(itr), itr);
        int32 b = rng6(itr);

        auto p = max_with_metadata(a, b, get_initial_comp_metadata());
        unit::assert_equal(tag, p.first, a > b ? a : b);
        unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
        unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a >= b);
    });

    pipelined_for(1024, [tag](uint32 itr){
        uint32 a = rng5(itr);
        int20  b = randomly_invert(rng6<int20>(itr), itr);

        auto p = max_with_metadata(a, b, get_initial_comp_metadata());
        unit::assert_equal(tag, p.first, a > b ? a : b);
        unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
        unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a >= b);
    });

    pipelined_for(16, [tag](uint4 a){
        pipelined_for(16, [tag, a](uint4 b){
            auto p = max_with_metadata(a, b, get_initial_comp_metadata());
            unit::assert_equal(tag, p.first, a > b ? a : b);
            unit::assert_equal(tag, p.second.all_previous_words_eq, a == b);
            unit::assert_equal(tag, p.second.select_a_words_on_prevnoteq, a >= b);
        });
    });

    comp_metadata metadata = {true, true};
    auto t1 = max_with_metadata(1, 2, get_initial_comp_metadata());
    unit::assert_equal(tag, t1.first , 2);
    unit::assert_equal(tag, t1.second.all_previous_words_eq, false);
    unit::assert_equal(tag, t1.second.select_a_words_on_prevnoteq, false);

    auto t2 = max_with_metadata(10, 10, get_initial_comp_metadata());
    unit::assert_equal(tag, t2.first , 10);
    unit::assert_equal(tag, t2.second.all_previous_words_eq, true);
}

inline void test_main()
{
    unit::test<1>(add_with_carry_test);
    unit::test<2>(sub_with_borrow_test);
    unit::test<3>(min_with_metadata_test);
    unit::test<4>(max_with_metadata_test);
}
