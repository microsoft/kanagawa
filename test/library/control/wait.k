// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.runner
import test.unit as unit

class TestWaitUntil
{
private:
    uint32 _wait_gate;
    uint32 _wait_finished_count;

public: 
    void run(unit::tag_t tag)
    {
        _wait_gate = 0;
        _wait_finished_count = 0;

        // Kick off thread that will increment _wait_gate
        async_exec([]
        {
            for (const auto i : 512)
            {
                atomic
                {
                    _wait_gate++;
                }
            }
        });

        pipelined_for(512, [tag](uint32 tid)
        {
            uint32 val = ~tid;

            const auto callback = [val, tid, tag]
            {
                optional<uint32> result = make_optional(tid < _wait_gate, val);

                if (result.is_valid)
                {
                    unit::assert_equal(tag, tid, _wait_finished_count);
                    _wait_finished_count = _wait_finished_count + 1;
                }

                return result;
            };

            uint32 final_result = wait_until(callback);

            unit::assert_equal(tag, val, final_result);
        });

        unit::assert_equal(tag, 512, _wait_finished_count);
    }
}

// Predicated `wait` calls can result in threads exiting the `wait` out of
// order. Threads with a false predicate will continue to enter the do-while
// loop inside `wait` and exit the loop immediately, even if a previous thread
// is still stuck inside this loop.  Below, one in every 4 threads will bypass
// the wait statement -- make sure that this thread stays in order
inline void TestPredicatedWaitUntil(unit::tag_t tag)
{
    pipelined_for(32,
        [tag](uint32 tid)
        {
            if (tid % 4 != 1)
            {
                wait_until([tid]
                    {
                        bool ret = true;
                        if (tid == 0)
                        {
                            static bool first = true;
                            ret = !first;
                            first = false;
                        }
                        return ret;
                    }
                );
            }
            atomic
            {
                static uint32 expected_tid = 0;
                unit::assert_equal(tag, expected_tid, tid);
                expected_tid++;
            }
        }
    );
}

class TestWaitBool
{
private:
    uint32 _wait_gate;
    uint32 _wait_finished_count;

public: 
    void run(unit::tag_t tag)
    {
        _wait_gate = 0;
        _wait_finished_count = 0;

        // Kick off thread that will increment _wait_gate
        async_exec([]
        {
            for (const auto i : 512)
            {
                atomic
                {
                    _wait_gate++;
                }
            }
        });

        pipelined_for(512, [tag](uint32 tid)
        {
            const auto callback = [tid, tag]
            {
                bool result = tid < _wait_gate;

                if (result)
                {
                    unit::assert_equal(tag, tid, _wait_finished_count);
                    _wait_finished_count = _wait_finished_count + 1;
                }

                return result;
            };

            wait(callback);
        });

        unit::assert_equal(tag, 512, _wait_finished_count);
    }
}

class TestWait
{
private:
    uint32 _wait_gate;
    uint32 _wait_finished_count;

public: 
    void run(unit::tag_t tag)
    {
        _wait_gate = 0;
        _wait_finished_count = 0;

        // Kick off thread that will increment _wait_gate
        async_exec([]
        {
            for (const auto i : 512)
            {
                atomic
                {
                    _wait_gate++;
                }
            }
        });

        pipelined_for(512, [tag](uint32 tid)
        {
            uint32 val = ~tid;

            const auto callback = [val, tid, tag]
            {
                optional<uint32> result = make_optional(tid < _wait_gate, val);

                if (result.is_valid)
                {
                    unit::assert_equal(tag, tid, _wait_finished_count);
                    _wait_finished_count = _wait_finished_count + 1;
                }

                return result;
            };

            uint32 final_result = wait(callback);

            assert(final_result == val);
            unit::assert_equal(tag, val, final_result);
        });

        unit::assert_equal(tag, 512, _wait_finished_count);
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<TestWaitUntil>());
    unit::test<2>(TestPredicatedWaitUntil);
    unit::test<3>(unit::fixture<TestWaitBool>());
    unit::test<4>(unit::fixture<TestWait>());
}
