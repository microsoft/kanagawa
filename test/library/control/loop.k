// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.array
import data.order
import sync.atomic
import sync.atomic.init
import test.unit as unit
import test.runner

class pipelined_for_test
{
private:
    uint32[200] _a = {};
    uint32[4][100] _aa = {};

    const auto iter = bitsizeof(_a) / bitsizeof(_a[0]);

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;

        pipelined_for(iter, [x](index_t<iter> i)
        {
            _a[i] = i * x;
        });

        bool result = true;

        for (const auto i : iter)
        {
            if (_a[i] != i * x)
                result = false;
        }

        unit::assert_equal(tag, true, result);
    }
}

class pipelined_for_nested_test
{
private:
    uint32[4][100] _aa = {};

    const auto iter1 = bitsizeof(_aa) / bitsizeof(_aa[0]);
    const auto iter2 = bitsizeof(_aa[0]) / bitsizeof(_aa[0][0]);

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;

        pipelined_for(iter1, [x](index_t<iter1> i)
        {
            uint32 y = 6;

            pipelined_for(iter2, [x, y, i](index_t<iter2> j)
            {
                _aa[i][j] = i * x + j * y;
            });
        });

        bool result = true;

        for (const auto i : iter1)
        {
            uint32 y = 6;

            for (const auto j : iter2)
            {
                if (_aa[i][j] != i * x + j * y)
                    result = false;
            }
        }

        unit::assert_equal(tag, true, result);
    }
}

class pipelined_for_each_test
{
private:
    uint32[4][100] _aa = {};

    const auto iter2 = bitsizeof(_aa[0]) / bitsizeof(_aa[0][0]);

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;

        pipelined_for_each(_aa[0], [x](index_t<iter2> i, uint32 e)
        {
            _aa[1][i] = e * x;
        });

        bool result = true;

        for (const auto i : iter2)
        {
            if (_aa[1][i] != _aa[0][i] * x)
                result = false;
        }

        unit::assert_equal(tag, true, result);
    }
}

class pipelined_for_fn_test
{
private:
    uint32[200] _a = {};

    const auto iter = bitsizeof(_a) / bitsizeof(_a[0]);

    void fn(uint32 x)
    {
        _a[x] = x;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(iter, fn);

        bool result = true;

        for (const auto i : iter)
        {
            if (_a[i] != i)
                result = false;
        }

        unit::assert_equal(tag, true, result);
    }
}

class pipelined_map_equal_size_test
{
private:
    uint32[200] _a = {};

    const auto iter = bitsizeof(_a) / bitsizeof(_a[0]);

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;

        auto a = pipelined_map<iter>(iter, [x](index_t<iter> i) -> uint32
        {
            return _a[i] + x;
        });

        bool result = true;

        for (const auto i : bitsizeof(a)/bitsizeof(a[0]))
        {
            if (_a[i] != a[i] - x)
                result = false;
        }

        unit::assert_equal(tag, true, result);
    }
}

class pipelined_map_bigger_test
{
private:
    uint32[200] _a = {};

    const auto iter = bitsizeof(_a) / bitsizeof(_a[0]);

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;

        auto a = pipelined_map<iter + 100>(iter, [x](index_t<iter> i) -> uint32
        {
            return _a[i] + x;
        });

        bool result = true;

        for (const auto i : iter)
        {
            if (_a[i] != a[i] - x)
                result = false;
        }

        unit::assert_equal(tag, true, result);
    }
}

template<bool HasThreadId>
class PipelinedDoTest
{
    public:
        template <auto N>
        optional<uint10> Run(uint32[N] data, uint32 x)
        {
            i = 0;
            found.is_valid = false;

            const auto body = [data, x]()
            {
                atomic
                {
                    if (!found.is_valid)
                    {
                        found = make_optional(data[i] == x, i);
                    }
                    i++;
                }
                return !found.is_valid && i < N;
            };

            static if (HasThreadId)
            {
                pipelined_do([data, x, body](index_t<8> tid)
                {
                    return body();
                });
            }
            else
            {
                // The function passed to `pipelined_do` has no argument
                pipelined_do([data, x, body]()
                {
                    return body();
                });
            }

            return found;
        }

    private:
        uint10 i;
        optional<uint10> found;
}

template<bool HasThreadId>
inline bool pipelined_do_test()
{
    static PipelinedDoTest<HasThreadId> test;

    uint32[128] a;
    a[77] = 0x84328da3;
    a[99] = a[77];

    auto result = test.Run(a, a[77]);

    return result.is_valid && result.value == 77;
}

class pipelined_last_test
{
private:
    uint32[4][100] _aa = {};

    const auto iter1 = bitsizeof(_aa) / bitsizeof(_aa[0]);
    const auto iter2 = bitsizeof(_aa[0]) / bitsizeof(_aa[0][0]);

public:
    void run(unit::tag_t tag)
    {
        auto a = pipelined_map<iter1>(iter1, [](index_t<iter1> i) -> bool
        {
            auto x = pipelined_last(iter2, [i](index_t<iter2> j) -> uint32
            {
                static uint32 total;

                auto x = _aa[i][j];

                atomic
                {
                    if (j == 0)
                        total = x;
                    else
                        total += x;
                }

                return total;
            });

            return x == sum<uint32>(_aa[i]);
        });

        unit::assert_equal(tag, true, and(a));
    }
}

inline void pipelined_last_default_test(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> count)
    {
        uint32 actual = pipelined_last<index_t<8>, uint32, 34>(count, [](index_t<8> i)
        {
            return cast<uint32>(i + 1);
        });

        uint32 expected = (count == 0) ? 34 : count;

        unit::assert_equal(tag, expected, actual);
    });
}

inline void pipelined_inlining_tests(unit::tag_t tag)
{
    inline void go(unit::tag_t tag)
    {
        pipelined_for(1, [tag](index_t<1> i)
        {
            static init_once once;
            unit::assert(tag, !once.check());
        });

        pipelined_map<1>(1, [tag](index_t<1> i)
        {
            static init_once once;
            unit::assert(tag, !once.check());
            return i;
        });

        pipelined_last(1, [tag](index_t<1> i)
        {
            static init_once once;
            unit::assert(tag, !once.check());
            return i;
        });
    }

    static for(const auto i : 8)
    {
        go(tag);
    }
}

// Test interaction of pipelined and `wait`
class PipelinedWaitForTest
{
private:
    bool flag = false;

public:
    void Run()
    {
        // Toggle flag infinitely
        async_exec([]
        {
            do
            {
                flag = !flag;
            } while (true);
        });

        pipelined_do([](index_t<8> tid)
        {
            wait([](){return flag;});
            return false;
        });

        pipelined_for(1, [](index_t<1> i)
        {
            wait([](){return flag;});
        });

        pipelined_map<1>(1, [](index_t<1> i)
        {
            wait([](){return flag;});
            return i;
        });

        pipelined_last(1, [](index_t<1> i)
        {
            wait([](){return flag;});
            return i;
        });
    }
}

inline bool pipelined_wait_test()
{
    static PipelinedWaitForTest test;

    test.Run();

    return true;
}

inline void range_for_test(unit::tag_t tag)
{
    class Fixture
    {
        uint32 iter;

    public:
        template <auto begin, auto end, auto step>
        inline void Run(unit::tag_t tag)
        {
            iter = 0;

            range_for (begin, end, step, [tag](auto i)
            {
                unit::assert(tag, i < end);

                auto result = second(atomically([tag, i](decltype(i) prev) -> decltype(i)
                    {
                        return i == begin ? begin : prev + step;
                    }));

                unit::assert_equal(tag, result, i);

                iter++;
            });

            unit::assert_equal(tag, max(0, (end - begin + step - 1)  / step), iter);
        }
    }

    static Fixture fixture;

    fixture.Run<-10, 10, 3>(tag);
    fixture.Run<10, 10, 3>(tag);
    fixture.Run<11, 10, 1>(tag);
    fixture.Run<10, 11, 1>(tag);
    fixture.Run<10, 11, 2>(tag);
    fixture.Run<1, 10, 1>(tag);
    fixture.Run<1, 10, 10>(tag);
}

inline bool while_do_test()
{
    class Fixture
    {
        int10 x;

    public:
        void Init()
        {
            x = -2;
        }

        bool Run()
        {
            while_do(
                []
                {
                    return x < 19;
                },
                []
                {
                    x += 1;
                }
            );

            return x == 19;
        }
    }

    static Fixture fixture;

    fixture.Init();
    return fixture.Run() && fixture.Run();
}

inline void parallel_for_test(unit::tag_t tag)
{
    template<auto N>
    class helper
    {
    private:
        uint32[N] _expected_count;
        uint32[N] _actual_count;

    public:
        void test(unit::tag_t tag)
        {
            _expected_count = {};
            _actual_count = {};

            pipelined_for(1024, [tag](index_t<1024> outer_index)
            {
                count_t<N> count = outer_index % N;

                atomic
                {
                    static for (const auto i : N)
                    {
                        if (i < count)
                        {
                            _expected_count[i]++;
                        }
                    }
                }

                parallel_for<N>(count, [tag](index_t<N> i)
                {
                    // The provided index should be known at compile time
                    unit::assert(tag, static(i) == i);

                    atomic
                    {
                        _actual_count[i]++;
                    }
                });
            });

            static for (const auto i : N)
            {
                unit::assert_equal(tag, _expected_count[i], _actual_count[i]);
            }
        }
    }

    static helper<4> _helper;
    _helper.test(tag);
}

inline void parallel_for_each_test(unit::tag_t tag)
{
    template<auto N>
    class helper
    {
    private:
        uint32[N] _expected_count;
        uint32[N] _actual_count;

    public:
        void test(unit::tag_t tag)
        {
            _actual_count = {};

            pipelined_for(1024, [tag](index_t<1024> outer_index)
            {
                uint32[N] arr = map_indices<N>([outer_index](index_t<N> i){return cast<uint32>(outer_index + i);});

                parallel_for_each(arr, [tag, outer_index](index_t<N> i, uint32 val)
                {
                    unit::assert_equal(tag, outer_index + static(i), val);

                    atomic
                    {
                        _actual_count[i]++;
                    }
                });
            });

            static for (const auto i : N)
            {
                unit::assert_equal(tag, 1024, _actual_count[i]);
            }
        }
    }

    static helper<4> _helper;
    _helper.test(tag);
}

inline void parallel_map_test(unit::tag_t tag)
{
    pipelined_for(1024, [tag](index_t<1024> outer_index)
    {
        count_t<4> count = outer_index % 4;

        const auto body = [](index_t<4> i) -> uint32 {return i * 3;};

        uint32[4] actual = parallel_map(count, body);
        uint32[4] expected = map_indices<4>(body);

        static for (const auto i : 4)
        {
            if (i < count)
            {
                unit::assert_equal(tag, expected[i], actual[i]);
            }
        }
    });
}

inline void test_main()
{
    print("control.loop test start\n");

    unit::test<1>(unit::fixture<pipelined_for_test>());
    unit::test<2>(unit::fixture<pipelined_for_nested_test>());
    unit::test<3>(unit::fixture<pipelined_for_each_test>());
    unit::test<4>(unit::fixture<pipelined_for_fn_test>());
    unit::check<5>(pipelined_do_test<true>);
    unit::check<5>(pipelined_do_test<false>);
    unit::test<6>(unit::fixture<pipelined_map_equal_size_test>());
    unit::test<7>(unit::fixture<pipelined_map_bigger_test>());
    unit::test<8>(unit::fixture<pipelined_last_test>());
    unit::test<9>(pipelined_last_default_test);
    unit::test<10>(pipelined_inlining_tests);
    unit::check<11>(pipelined_wait_test);
    unit::check<12>(while_do_test);
    unit::test<13>(range_for_test);
    unit::test<14>(parallel_for_test);
    unit::test<15>(parallel_for_each_test);
    unit::test<16>(parallel_map_test);

    print("control.loop test complete\n");
}

