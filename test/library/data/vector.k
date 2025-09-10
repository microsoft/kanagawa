// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Copyright (c), Microsoft Corporation. All rights reserved.

import .options as opt
import data.array as A
import data.bits
import data.optional
import data.optional as O
import data.tuple
import data.vector
import data.vector as V
import numeric.int
import numeric.int.operator
import test.unit as unit
import test.runner

inline void test_make_vector(unit::tag_t tag)
{
    unit::assert_equal(tag, {{0, 1, 2}, 3}, make_vector<uint8>({0, 1, 2, 0, 0}, 3));
}

inline void test_from_array(unit::tag_t tag)
{
    unit::assert_equal(tag, {{0, 1, 2}, 3}, from_array<uint8>({0, 1, 2}));
}

inline void test_from_optional(unit::tag_t tag)
{
    unit::assert_equal(tag, {{0xFF}, 1}, V::from_optional({true,  0xFF}));
    unit::assert_equal(tag, {{0xFF}, 0}, V::from_optional({false, 0xFF}));
}

inline void test_repeat(unit::tag_t tag)
{
    vector<uint8, 3> v = repeat(3, 2);
    unit::assert_equal(tag, {{3, 3, 0}, 2}, v);

    unit::assert_equal(tag, {{2, 2}, 2}, repeat<2, uint8>(2, 2));
}

inline void test_at(unit::tag_t tag)
{
    auto v = resize(from_array<uint8>({0, 1, 2, 3, 4, 5}), 4);
    unit::assert_equal(tag, 2, at(v, 2));
}

inline void test_front(unit::tag_t tag)
{
    auto v = resize(from_array<uint8>({0, 1, 2, 3, 4, 5}), 4);
    unit::assert_equal(tag, 0, front(v));
}

inline void test_back(unit::tag_t tag)
{
    auto v = resize(from_array<uint8>({0, 1, 2, 3, 4, 5}), 4);
    unit::assert_equal(tag, 3, back(v));
}

inline void test_size(unit::tag_t tag)
{
    auto v = from_array({0, 1, 2, 3});
    unit::assert(tag, 4 == size(v));
    v = pop_back(v);
    unit::assert(tag, 3 == size(v));
}

inline void test_empty(unit::tag_t tag)
{
    auto v = resize(from_array({0, 1, 2, 3, 4, 5}), 0);
    unit::assert(tag, empty(v));
}

inline void test_capacity(unit::tag_t tag)
{
    auto v = resize(from_array({0, 1, 2, 3, 4, 5}), 4);
    unit::assert_equal(tag, 6, capacity(v));
}

inline void test_reserve(unit::tag_t tag)
{
    auto a = resize(from_array<uint8>({0, 1, 2, 3}), 2);

    //expand
    vector<uint8, 6> b = reserve(a);
    unit::assert_equal(tag, {{0, 1, 2, 3, 0, 0}, 2}, b);

    //shrink
    auto c = reserve<2>(a);
    unit::assert_equal(tag, {{0, 1}, 2}, c);

    //examples
    auto d = from_array<uint8>({0, 1, 2});
    unit::assert_equal(tag, {{0, 1, 2, 0, 0}, 3}, reserve<5>(d));

    auto e = from_array<uint8>({0, 1, 2, 3, 4});
    e = resize(e, 3);
    unit::assert_equal(tag, {{0, 1, 2}, 3}, reserve<3>(e));

    //identity
    auto f = from_array<uint8>({0xFF});
    unit::assert_equal(tag, {{0xFF}, 1}, reserve<1>(f));
}

inline void test_clear(unit::tag_t tag)
{
    auto v = from_array<uint8>({0, 1, 2, 3, 4, 5});
    v = clear(v);
    unit::assert_equal(tag, 0, v.size);
}

inline void test_insert(unit::tag_t tag)
{
    auto v = clear(from_array<uint8>({0, 1, 2, 3}));
    v = insert(v, 0, 0xFF);
    unit::assert_equal(tag, {{0xFF, 1, 2, 3}, 1}, v);
    v = insert(v, 1, 0xAB);
    unit::assert_equal(tag, {{0xFF, 0xAB, 2, 3}, 2}, v);
    v = insert(v, 1, 0xCD);
    unit::assert_equal(tag, {{0xFF, 0xCD, 0xAB, 3}, 3}, v);
    v = insert(v, 3, 0xEF);
    unit::assert_equal(tag, {{0xFF, 0xCD, 0xAB, 0xEF}, 4}, v);
}

inline void test_erase(unit::tag_t tag)
{
    auto v = from_array<uint8>({0, 1, 2, 3});
    v = erase(v, 0);
    unit::assert_equal(tag, {{1, 2, 3, 3}, 3}, v);
    v = erase(v, 1);
    unit::assert_equal(tag, {{1, 3, 3, 3}, 2}, v);
    v = erase(v, 1);
    unit::assert_equal(tag, {{1, 3, 3, 3}, 1}, v);
    v = erase(v, 0);
    unit::assert_equal(tag, {{1, 3, 3, 3}, 0}, v);
}

inline void test_push_back(unit::tag_t tag)
{
    auto v = clear(from_array<uint8>({0, 1, 2, 3}));
    v = push_back(0xAB, v);
    unit::assert_equal(tag, {{0xAB, 1, 2, 3}, 1}, v);
    v = push_back(0xBC, v);
    unit::assert_equal(tag, {{0xAB, 0xBC, 2, 3}, 2}, v);
    v = push_back(0xDE, v);
    unit::assert_equal(tag, {{0xAB, 0xBC, 0xDE, 3}, 3}, v);
    v = push_back(0xFF, v);
    unit::assert_equal(tag, {{0xAB, 0xBC, 0xDE, 0xFF}, 4}, v);
}

inline void test_pop_back(unit::tag_t tag)
{
    auto v = from_array<uint8>({0, 1, 2, 3});
    v = pop_back(v);
    unit::assert_equal(tag, {{0, 1, 2, 3}, 3}, v);
    v = pop_back(v);
    unit::assert_equal(tag, {{0, 1, 2, 3}, 2}, v);
    v = pop_back(v);
    unit::assert_equal(tag, {{0, 1, 2, 3}, 1}, v);
    v = pop_back(v);
    unit::assert_equal(tag, {{0, 1, 2, 3}, 0}, v);
}

inline void test_append(unit::tag_t tag)
{
    auto a = reserve<6>(from_array<uint8>({1, 2, 3}));
    auto b = reserve<6>(from_array<uint8>({4, 5, 6}));
    unit::assert_equal(tag, {{1, 2, 3, 4, 5, 6}, 6}, append(a, b));

    auto c = reserve<4>(from_array<uint8>({1, 2}));
    auto d = reserve<4>(from_array<uint8>({3, 4}));
    unit::assert_equal(tag, {{1, 2, 3, 4}, 4}, append(c, d));

    auto e = from_array<uint8>({1, 2, 3});
    auto f = clear(from_array<uint8>({0xFF, 0xFF, 0xFF}));
    unit::assert_equal(tag, {{1, 2, 3}, 3}, append(e, f));

    //right identity
    e = pop_back(e);
    unit::assert_equal(tag, {{1, 2, 3}, 2}, append(e, f));

    //left identity
    e = clear(e);
    f = resize(f, 2);
    unit::assert_equal(tag, {{0xFF, 0xFF, 3}, 2}, append(e, f));

    e = resize(e, 3);
    f = clear(f);
    unit::assert_equal(tag, {{1, 2, 3}, 3}, append(e, f));
}

inline void test_map(unit::tag_t tag)
{
    auto v = pop_back(from_array<uint8>({1, 2, 3, 4}));
    vector<uint9, 4> actual = map([](uint8 x){ return x + 1; }, v);
    auto expected = resize(from_array<uint9>({2, 3, 4, 0}), 3);
    unit::assert_equal(tag, expected, actual);
}

inline void test_unique_by(unit::tag_t tag)
{
    auto a = from_array<optional<uint8>>({{true, 1}, {false, 1}, {true, 1}});
    unit::assert_equal
        ( tag
        , {{{true, {true, 1}}, {true, {false, 1}}, {false, {true, 1}}}, 3}
        , unique_by(O::equal<uint8>, a)
        );
}

inline void test_unique(unit::tag_t tag)
{
    auto a = from_array<uint8>({1});
    unit::assert_equal(tag, {{{true, 1}}, 1}, unique(a));

    a = clear(a);
    unit::assert_equal(tag, {{{false, 1}}, 0}, unique(a));

    auto b = from_array<uint8>({1, 2});
    unit::assert_equal(tag, {{{true, 1}, {true, 2}}, 2}, unique(b));

    auto c = from_array<uint8>({1, 1});
    unit::assert_equal(tag, {{{true, 1}, {false, 1}}, 2}, unique(c));

    c = pop_back(c);
    unit::assert_equal(tag, {{{true, 1}, {false, 1}}, 1}, unique(c));

    auto d = from_array<uint8>({1, 2, 3, 2});
    unit::assert_equal(tag, {{{true, 1}, {true, 2}, {true, 3}, {false, 2}}, 4}, unique(d));

    d = resize(d, 2);
    unit::assert_equal(tag, {{{true, 1}, {true, 2}, {false, 3}, {false, 2}}, 2}, unique(d));
}

inline void test_reduce(unit::tag_t tag)
{
    //singleton
    auto b = from_array<uint8>({2});
    unit::assert(tag, 2 == reduce(add<uint8, uint8>, b));

    //even max
    auto c = pop_back(from_array<uint8>({1, 2}));
    unit::assert(tag, 1 == reduce(add<uint8, uint8>, c));

    auto d = from_array<uint8>({1, 2});
    unit::assert(tag, 3 == reduce(add<uint8, uint8>, d));

    //odd max
    auto e = resize(from_array<uint8>({1, 2, 3}), 1);
    unit::assert(tag, 1 == reduce(add<uint8, uint8>, e));

    auto f = resize(from_array<uint8>({1, 2, 3}), 2);
    unit::assert(tag, 3 == reduce(add<uint8, uint8>, f));

    auto g = from_array<uint8>({1, 2, 3});
    unit::assert(tag, 6 == reduce(add<uint8, uint8>, g));

    //struct
    struct Foo
    {
        uint8 x;
    }
    auto h = pop_back(from_array<uint8>({2, 4, 6, 8}));
    Foo hh = map_reduce
                  ( [](uint8 byte) -> Foo
                    { Foo f; f.x = byte; return f; }
                  , [](Foo l, Foo r){ Foo f; f.x = add<uint8, uint8>(l.x, r.x); return f; }
                  , h
                  );
    unit::assert_equal(tag, {12}, hh);
}

inline void test_or(unit::tag_t tag)
{
    auto a = from_array<bool>({false, false, false, false, true});
    unit::assert(tag, or(a));
    a = pop_back(a);
    unit::assert(tag, !or(a));
    a = clear(a);
    unit::assert(tag, !or(a));
}

inline void test_and(unit::tag_t tag)
{
    auto a = from_array<bool>({true, true, true, false});
    unit::assert(tag, !and(a));
    a = pop_back(a);
    unit::assert(tag, and(a));
    a = clear(a);
    unit::assert(tag, and(a));
}

inline void test_any(unit::tag_t tag)
{
    auto a = from_array<uint8>({1, 3, 5, 6});
    unit::assert(tag, any(even, a));
    a = pop_back(a);
    unit::assert(tag, !any(even, a));
    a = clear(a);
    unit::assert(tag, !any(even, a));
}

inline void test_all(unit::tag_t tag)
{
    auto a = from_array<uint8>({1, 3, 5, 6});
    unit::assert(tag, !all(odd, a));
    a = pop_back(a);
    unit::assert(tag, all(odd, a));
    a = clear(a);
    unit::assert(tag, all(odd, a));
}

inline void test_minimum(unit::tag_t tag)
{
    auto a = from_array<uint16>({4, 64, 2});
    unit::assert(tag, 2 == minimum(a));

    a = pop_back(a);
    unit::assert(tag, 4 == minimum(a));
}

inline void test_maximum(unit::tag_t tag)
{
    auto a = from_array<int32>({5, -23, 182});
    unit::assert(tag, 182 == maximum(a));

    a = pop_back(a);
    unit::assert(tag, 5 == maximum(a));
}

inline void test_sum(unit::tag_t tag)
{
    auto a = from_array<int8>({3, -9, 7});
    unit::assert(tag, 1 == sum<int8>(a));
    a = pop_back(a);
    unit::assert(tag, -6 == sum<int8>(a));
}

inline void test_first_valid(unit::tag_t tag)
{
    auto a = from_array<optional<uint8>>({{true, 1}, {false, 2}, {true, 3}});
    unit::assert_equal(tag, {true, 1}, first_valid(a));

    a = erase(a, 0);
    unit::assert_equal(tag, {true, 3}, first_valid(a));

    a = clear(a);
    unit::assert_equal(tag, {}, first_valid(a));

    //none valid
    a = {{{false, 1}, {false, 2}, {true, 3}}, 2};
    unit::assert_equal(tag, {false, 2}, first_valid(a));

    auto b = clear(from_array<optional<uint8>>({{true, 1}, {true, 2}}));
    unit::assert_equal(tag, {false, 0}, first_valid(b));
}

inline void test_last_valid(unit::tag_t tag)
{
    auto a = from_array<optional<uint8>>({{true, 1}, {false, 2}, {true, 3}});
    unit::assert_equal(tag, {true, 3}, last_valid(a));

    a = pop_back(a);
    unit::assert_equal(tag, {true, 1}, last_valid(a));

    a = clear(a);
    unit::assert_equal(tag, {}, last_valid(a));

    //none valid
    auto b = resize(from_array<optional<uint8>>({{false, 1}, {false, 2}, {true, 3}}), 2);
    unit::assert_equal(tag, {false, 1}, last_valid(b));

    auto c = clear(from_array<optional<uint8>>({{true, 1}, {true, 2}}));
    unit::assert_equal(tag, {false, 0}, last_valid(c));
}

inline void test_map_optional(unit::tag_t tag)
{
    auto a = from_array<uint8>({1, 2, 3});
    unit::assert_equal(tag, {{1, 3, 0}, 2}, map_optional<uint8, uint8, 3>([](uint8 x){ return make_optional(odd(x), x); }, a));
    unit::assert_equal(tag, {{2, 3, 0}, 1}, map_optional<uint8, uint8, 3>([](uint8 x){ return make_optional(even(x), x); }, a));

    auto b = from_array<pair<int8, uint8>>({{-1, 1}, {-2, 2}, {-3, 3}});
    b = pop_back(b);
    auto bf = [](tuple2<int8, uint8> p){ return make_optional(even(p.second), p); };
    unit::assert_equal(tag, {{{-2, 2}, {}, {}}, 1}, map_optional(bf, b));
}

inline void test_cat_optionals(unit::tag_t tag)
{
    auto a = from_array<optional<uint8>>({{true, 0}, {true, 1}, {true, 2}});
    unit::assert_equal(tag, {{0, 1, 2}, 3}, cat_optionals(a));

    a = from_array<optional<uint8>>({{false, 0}, {true, 1}, {true, 2}});
    unit::assert_equal(tag, {{1, 2, 0}, 2}, cat_optionals(a));

    a = from_array<optional<uint8>>({{true, 0}, {false, 1}, {true, 2}});
    unit::assert_equal(tag, {{0, 2, 0}, 2}, cat_optionals(a));

    a = from_array<optional<uint8>>({{true, 0}, {true, 1}, {false, 2}});
    unit::assert_equal(tag, {{0, 1, 2}, 2}, cat_optionals(a));

    a = from_array<optional<uint8>>({{false, 0}, {false, 1}, {true, 2}});
    unit::assert_equal(tag, {{2, 0, 0}, 1}, cat_optionals(a));

    a = from_array<optional<uint8>>({{true, 0}, {false, 1}, {false, 2}});
    unit::assert_equal(tag, {{0, 2, 0}, 1}, cat_optionals(a));

    a = from_array<optional<uint8>>({{false, 0}, {true, 1}, {false, 2}});
    unit::assert_equal(tag, {{1, 2, 0}, 1}, cat_optionals(a));

    a = from_array<optional<uint8>>({{false, 0}, {false, 1}, {false, 2}});
    unit::assert_equal(tag, {{2, 0, 0}, 0}, cat_optionals(a));

    auto b = from_array<optional<bool>>({{true, true}, {true, false}, {true, true}});
    b = pop_back(b);
    unit::assert_equal(tag, {{true, false, false}, 2}, cat_optionals(b));
}

inline void test_zip_with(unit::tag_t tag)
{
    vector<uint8, 4> x = {{1, 2, 3, 4},    3};
    vector<uint8, 5> y = {{1, 2, 3, 4, 5}, 4};
    vector<uint8, 1> z;

    vector<uint8, 4> xy = zip_with(add<uint8, uint8>, x, y);
    vector<uint8, 4> expected_xy = {{2, 4, 6, 0}, 3};
    unit::assert_equal(tag, expected_xy, xy);

    vector<uint8, 1> xz = zip_with(add<uint8, uint8>, x, z);
    vector<uint8, 1> expected_xz = {{0}, 0};
    unit::assert_equal(tag, expected_xz, xz);

    vector<bool,  3> a = {{true, false, true}, 3};
    vector<int32, 2> b = {{-22,  5},           2};
    vector<optional<int32>, 2> ab = zip_with(make_optional<int32>, a, b);
    vector<optional<int32>, 2> expected_ab = {{{true, -22}, {false, 5}}, 2};
    unit::assert_equal(tag, expected_ab, ab);
}

inline void test_zip(unit::tag_t tag)
{
    vector<uint8, 2> a = {{1,    2},     2};
    vector<bool,  2> b = {{true, false}, 2};
    vector<pair<uint8, bool>, 2> ab = zip(a, b);
    vector<pair<uint8, bool>, 2> expected_ab = {{{1, true}, {2, false}}, 2};
    unit::assert_equal(tag, expected_ab, ab);

    auto c = from_array<uint8, 3>({1, 2, 3});
    vector<pair<uint8, uint8>, 2> ac = zip(a, c);
    vector<pair<uint8, uint8>, 2> expected_ac = {{{1, 1}, {2, 2}}, 2};
    unit::assert_equal(tag, expected_ac, ac);
}

inline void test_unzip_with(unit::tag_t tag)
{
    auto a = from_array<optional<uint8>>({{true, 1}, {false, 2}, {true, 3}});
    a = pop_back(a);
    pair<vector<bool, 3>, vector<uint8, 3>> aa = unzip_with<bool, uint8, optional<uint8>, 3>(optional_to_pair<uint8>, a);
    unit::assert_equal(tag, {{true, false, false}, 2}, aa.first);
    unit::assert_equal(tag, {{1,    2,     0    }, 2}, aa.second);
}

inline void test_unzip(unit::tag_t tag)
{
    auto a = from_array<pair<int32, uint64>>({{0, 0}, {-1, 1}, {-2, 2}, {-3, 3}});
    pair<vector<int32, 4>, vector<uint64, 4>> aa = unzip<int32, uint64>(a);
    unit::assert_equal(tag, {{0, -1, -2, -3}, 4}, aa.first);
    unit::assert_equal(tag, {{0,  1,  2,  3}, 4}, aa.second);

    a = pop_back(a);
    aa = unzip<int32, uint64>(a);
    unit::assert_equal(tag, {{0, -1, -2, 0}, 3}, aa.first);
    unit::assert_equal(tag, {{0,  1,  2, 0}, 3}, aa.second);

    a = clear(a);
    aa = unzip<int32, uint64>(a);
    unit::assert_equal(tag, {{0, 0, 0, 0}, 0}, aa.first);
    unit::assert_equal(tag, {{0, 0, 0, 0}, 0}, aa.second);
}

inline void test_prefix_sum(unit::tag_t tag)
{
    auto v = from_array<uint8>({0, 1, 2, 3});

    unit::assert_equal(tag, {{0, 1, 3, 6}, 4}, prefix_sum<uint8>(v));

    auto a = pop_back(v);
    unit::assert_equal(tag, {{0, 1, 3, 3}, 3}, prefix_sum<uint8>(a));

    auto b = from_array<uint8>({0, 1, 2, 3, 4});
    b = pop_back(b);
    b = pop_back(b);
    unit::assert_equal(tag, {{0, 1, 3, 0, 0}, 3}, prefix_sum<uint8>(b));

    auto c = from_array<uint8>({5});
    unit::assert_equal(tag, {{5}, 1}, prefix_sum<uint8>(c));

    c = pop_back(c);
    unit::assert_equal(tag, {{5}, 0}, prefix_sum<uint8>(c));

    auto d = from_array<uint8>({3, 2, 1});
    d = pop_back(d);
    unit::assert_equal(tag, {{3, 5, 1}, 2}, prefix_sum<uint8>(d));
}

inline void test_equal_by(unit::tag_t tag)
{

    auto v = from_array<optional<uint8>>({{true, 0x23}, {false, 0xFF}, {true, 0x19}, {false, 0x20}});
    auto w = from_array<optional<uint8>>({{true, 0x23}, {false, 0x00}, {true, 0x19}});
    unit::assert(tag, !V::equal_by(O::equal<uint8>, v, w));

    v = pop_back(v);
    unit::assert(tag, V::equal_by(O::equal<uint8>, v, w));
}

inline void test_equal(unit::tag_t tag)
{
    auto v = from_array<uint8>({1, 2, 3});
    auto w = from_array<uint8>({1, 2, 3, 4});
    unit::assert(tag, !V::equal(v, w));

    w = pop_back(w);
    unit::assert(tag, V::equal(v, w));
}

inline void test_sharded_map(unit::tag_t tag)
{
    const auto IterationCount = 1024;
    const auto N = 8;
    const auto Shards = 4;

    auto start = cycles();

    pipelined_for(IterationCount, [tag](index_t<IterationCount> iteration)
    {
        vector<uint32, N> input;

        static for (const auto i : N)
        {
            input.data[i] = static_cast((iteration + i) * 1234567);
        }

        input.size = (iteration % N) + 1;

        vector<uint64, N> output = sharded_map(
            [tag](index_t<Shards> shard_idx, uint32 val)
            {
                unit::assert_equal(tag, cast<index_t<Shards>>(val), static(shard_idx));

                uint64 result = val * 54321;

                return result;
            },
            static_cast<index_t<Shards>>,
            input);

        unit::assert_equal(tag, output.size, input.size);

        static for (const auto i : N)
        {
            if (i < input.size)
            {
                unit::assert_equal(tag, input.data[i] * 54321, output.data[i]);
            }
        }
    });

    auto end = cycles();

    if (opt::stall == 0)
    {
        unit::assert(tag, (end - start) < (IterationCount * (N / Shards)));
    }
}

class test_sharded_for_each
{
    const auto Shards = 4;
    const auto IterationCount = 1024;
    const auto N = 8;

    uint64[Shards] _actual_sum;
    uint64 _expected_sum;

public:
    inline void run(unit::tag_t tag)
    {
        _actual_sum = {};
        _expected_sum = 0;

        auto start = cycles();

        pipelined_for(IterationCount, [tag](index_t<IterationCount> iteration)
        {
            vector<uint32, N> input;

            static for (const auto i : N)
            {
                input.data[i] = static_cast((iteration + i) * 567);
            }

            input.size = (iteration % N) + 1;

            uint32 local_sum = reduce(add<uint32, uint32>, input);

            atomic
            {
                _expected_sum += local_sum;
            }

            sharded_for_each(
                [tag](index_t<Shards> shard_idx, uint32 val)
                {
                    unit::assert_equal(tag, cast<index_t<Shards>>(val), static(shard_idx));

                    atomic
                    {
                        _actual_sum[shard_idx] += val;
                    }
                },
                static_cast<index_t<Shards>>,
                input);
        });

        auto end = cycles();

        unit::assert_equal(tag, _expected_sum, A::sum<uint64>(_actual_sum));

        if (opt::stall == 0)
        {
            unit::assert(tag, (end - start) < (IterationCount * (N / Shards)));
        }
    }
}

inline bool test_scatter_gather()
{
    bool result = true;

    struct test_vector_t
    {
        bool[8] valids;
        uint8[8] expanded;
        uint8[8] compact;
    }

    const auto num_vectors = 8;
    test_vector_t[num_vectors] test_vectors = {
        {{ false, false, false, false, false, false, false, false}, {1, 2, 3, 4, 5, 6, 7, 8}, {0, 0, 0, 0, 0, 0, 0, 0}},
        {{  true, false,  true, false,  true, false,  true, false}, {1, 2, 3, 4, 5, 6, 7, 8}, {1, 3, 5, 7, 0, 0, 0, 0}},
        {{ false,  true, false,  true, false,  true, false,  true}, {1, 2, 3, 4, 5, 6, 7, 8}, {2, 4, 6, 8, 0, 0, 0, 0}},
        {{  true, false, false, false, false, false, false, false}, {8, 7, 6, 5, 4, 3, 2, 1}, {8, 0, 0, 0, 0, 0, 0, 0}},
        {{ false, false, false, false, false, false, false,  true}, {8, 7, 6, 5, 4, 3, 2, 1}, {1, 0, 0, 0, 0, 0, 0, 0}},
        {{ false, false, false,  true, false, false, false, false}, {8, 7, 6, 5, 4, 3, 2, 1}, {5, 0, 0, 0, 0, 0, 0, 0}},
        {{  true, false,  true,  true, false, false,  true,  true}, {8, 7, 6, 5, 4, 3, 2, 1}, {8, 6, 5, 2, 1, 0, 0, 0}},
        {{  true,  true,  true,  true,  true,  true,  true,  true}, {1, 2, 3, 4, 5, 6, 7, 8}, {1, 2, 3, 4, 5, 6, 7, 8}}
    };

    for (const auto i : num_vectors)
    {
        auto vector = test_vectors[i];
        uint8 valid_count = pop_count(cast<uint8>(vector.valids));

        // Test gather
        auto gather_output = gather<uint8>(vector.valids, vector.expanded);
        result = (gather_output.size == valid_count);
        if (!result)
        {
            print("Failed: {valid_count} {gather_output.size}\n");
            assert(result);
        }
        for (const auto j : 8)
        {
            if (j < valid_count)
            {
                result &&= (gather_output.data[j] == vector.compact[j]);
            }
            if (!result)
            {
                print("Failed: {vector} {gather_output.data}\n");
                assert(result);
            }
        }

        // Test scatter
        auto scatter_output = scatter(vector.valids, vector.compact);
        result = (scatter_output.size == valid_count);
        if (!result)
        {
            print("Failed: {valid_count} {scatter_output.size}\n");
            assert(result);
        }
        for (const auto j : 8)
        {
            if (vector.valids[j])
            {
                result &&= (scatter_output.data[j] == vector.expanded[j]);
            }
            if (!result)
            {
                print("Failed: {vector} {scatter_output.data}\n");
                assert(result);
            }
        }
    }

    return result;
}

inline void test_main()
{
    print("data.vector test start\n");
    unit::test< 1>(test_from_array);
    unit::test< 2>(test_from_optional);
    unit::test< 3>(test_repeat);
    unit::test< 4>(test_at);
    unit::test< 5>(test_front);
    unit::test< 6>(test_back);
    unit::test< 7>(test_size);
    unit::test< 8>(test_empty);
    unit::test< 9>(test_capacity);
    unit::test<10>(test_reserve);
    unit::test<11>(test_clear);
    unit::test<12>(test_insert);
    unit::test<13>(test_erase);
    unit::test<14>(test_push_back);
    unit::test<15>(test_pop_back);
    unit::test<16>(test_append);
    unit::test<17>(test_map);
    unit::test<18>(test_unique_by);
    unit::test<19>(test_unique);
    unit::test<20>(test_reduce);
    unit::test<21>(test_or);
    unit::test<22>(test_and);
    unit::test<23>(test_any);
    unit::test<24>(test_all);
    unit::test<25>(test_minimum);
    unit::test<26>(test_maximum);
    unit::test<27>(test_sum);
    unit::test<28>(test_first_valid);
    unit::test<29>(test_last_valid);
    unit::test<30>(test_map_optional);
    unit::test<31>(test_cat_optionals);
    unit::test<32>(test_zip_with);
    unit::test<33>(test_zip);
    unit::test<34>(test_unzip_with);
    unit::test<35>(test_unzip);
    unit::test<36>(test_prefix_sum);
    unit::test<37>(test_equal_by);
    unit::test<38>(test_equal);
    unit::check<39>(test_scatter_gather);
    unit::test<40>(test_make_vector);
    unit::test<41>(test_sharded_map);
    unit::test<42>(unit::fixture<test_sharded_for_each>());
    print("data.vector test complete\n");
}
