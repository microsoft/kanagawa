// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.array
import sync.atomic.init
import test.unit as unit
import test.runner

//
// async test
//

class async_then_test
{
private:
    bool _done = false;
    uint32 _result;
    uint32[200] _a = {};

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;
        _done = false;

        async_then(
            [x] -> uint32
            {
                return x + minimum(_a);
            },
            [](uint32 r)
            {
                _result = r;
                _done = true;
            });

        wait([](){return _done;});
        unit::assert_equal(tag, x + minimum(_a), _result);
    }
}

class async_then_chain_test
{
private:
    bool _done = false;
    uint32[200] _a = {};
    bool _bresult = false;

public:
    void run(unit::tag_t tag)
    {
        uint32 x = 3;
        _done = false;

        async_then(
            [x] -> uint32
            {
                return x + maximum(_a);
            },
            [x](uint32 r)
            {
                async_then(
                    [x, r] -> bool
                    {
                        return r - x == maximum(_a);
                    },
                    [](bool r)
                    {
                        _bresult = r;
                        _done = true;
                    });
            });

        wait([](){return _done;});
        unit::assert_equal(tag, true, _bresult);
    }
}

class async_then_void_test
{
private:
    bool _done = false;
    uint32[200] _a = {};

    const auto iter = bitsizeof(_a) / bitsizeof(_a[0]);

public:
    inline bool equal_13(uint32 x)
    {
        return x == 13;
    }

    void run(unit::tag_t tag)
    {
        _done = false;

        async_then_void(
                []
                {
                    pipelined_for(iter, [](index_t<iter> i)
                    {
                        _a[i] = 13;
                    });
                },
                []
                {
                    _done = true;
                });

        wait([](){return _done;});

        unit::assert_equal(tag, true, all(equal_13, _a));
    }
}

class async_exec_test
{
private:
    bool _done = false;
    uint32[200] _a = {};

    const auto iter = bitsizeof(_a) / bitsizeof(_a[0]);

public:
    inline bool equal_14(uint32 x)
    {
        return x == 14;
    }

    void run(unit::tag_t tag)
    {
        _done = false;

        async_exec(
                []
                {
                    pipelined_for(iter, [](index_t<iter> i)
                    {
                        _a[i] = 14;
                    });

                    _done = true;
                });

        wait([](){return _done;});

        unit::assert_equal(tag, true, all(equal_14, _a));
    }
}

//
// fork_join test
//
inline uint32 fast_task()
{
    auto t = cycles();
    wait([t]()
    {
        return cycles() >= t + 100;
    });
    return 0;
}

inline uint32 slow_task()
{
    auto t = cycles();
    wait([t]()
    {
        return cycles() >= t + 2 * 100;
    });
    return 1;
}

inline bool join_one_test(uint1 which)
{
    static fork_join<uint32, 2, 32 /* ReturnDepth */, 32 /* WaitForDepth */> fj;
    if (which == 0)
        fj.fork<0>(fast_task);
    else
        fj.fork<1>(slow_task);
    auto ret = fj.join(reinterpret_cast<bool[2]>(1 << which));
    return (ret[which] != which);
}

inline bool join_all_test()
{
    static fork_join<uint32, 2, 32 /* ReturnDepth */, 32 /* WaitForDepth */> fj;
    fj.fork<0>(fast_task);
    fj.fork<1>(slow_task);
    auto ret = fj.join({true, true});
    return (ret[0] != 0 || ret[1] != 1);
}

inline bool join_none_test()
{
    static fork_join<uint32, 2, 32 /* ReturnDepth */, 32 /* WaitForDepth */> fj;
    fj.join({false, false});
    return false;
}

class fork_join_test
{
private:
    bool[3] fj_result;

public:
    void run(unit::tag_t tag)
    {
        print("fork_join test starting\n");

        static for(const auto i : bitsizeof(fj_result))
        {
            fj_result[i] = false;
        }

        // Call each join variant many times to ensure we exceed the depth of
        // fork_join's internal return FIFO and make sure no deadlock occurs
        pipelined_for(64,
            [](index_t<64> tid)
            {
                fj_result[0] ||= join_one_test(tid % 2);
                assert(!fj_result[0]);
            }
        );

        pipelined_for(64,
            [](index_t<64> tid)
            {
                fj_result[1] ||= join_all_test();
                assert(!fj_result[1]);
            }
        );

        pipelined_for(64,
            [](index_t<64> tid)
            {
                fj_result[2] ||= join_none_test();
                assert(!fj_result[2]);
            }
        );

        print("fork_join test complete\n");

        unit::assert_equal(tag, true, !or(fj_result));
    }
}

class async_inlining_test
{
private:
    bool _done = false;

public:
    void run(unit::tag_t tag)
    {
        inline void go(unit::tag_t tag)
        {
            _done = false;

            async_then(
                [tag] -> uint32
                {
                    static init_once once;
                    unit::assert(tag, !once.check());
                    return 0;
                },
                [tag](uint32 x) -> void
                {
                    static init_once once;
                    unit::assert(tag, !once.check());
                    _done = true;
                });

            wait([](){return _done;});

            _done = false;

            async_then_void(
                [tag]
                {
                    static init_once once;
                    unit::assert(tag, !once.check());
                },
                [tag] -> void
                {
                    static init_once once;
                    unit::assert(tag, !once.check());
                    _done = true;
                });

            wait([](){return _done;});

            _done = false;

            async_exec(
                [tag]
                {
                    static init_once once;
                    unit::assert(tag, !once.check());
                    _done = true;
                });

            wait([](){return _done;});
        }

        static for(const auto i : 8)
        {
            go(tag);
        }
    }
}

inline void test_main()
{
    print("control.async test start\n");

    unit::test<1>(unit::fixture<async_then_test>());
    unit::test<2>(unit::fixture<async_then_chain_test>());
    unit::test<3>(unit::fixture<async_then_void_test>());
    unit::test<4>(unit::fixture<async_exec_test>());
    unit::test<5>(unit::fixture<fork_join_test>());
    unit::test<6>(unit::fixture<async_inlining_test>());

    print("control.async test finish\n");
}

