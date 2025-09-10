// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import sync.atomic
import sync.atomic.init
import data.buffer
import data.array
import test.unit as unit
import test.runner

//
// init_once test
//
class test_basic
{
private:
    init_once initializer;

public:
    void run(unit::tag_t tag)
    {
        bool pass = true;
        // First call should return false
        if (initializer.check())
        {
            pass = false;
        }
        // All subsequent calls should return true
        else
        {
            for (const auto i : 10)
            {
                if (!initializer.check())
                {
                    pass = false;
                }
            }
        }
        unit::assert_equal(tag, true, pass);
    }
}

// Test multiple async calls to init_once
class test_async
{
private:
    const auto NUM_ASYNC_CALLS = 10;

    init_once async_initializer;
    uint32 async_completed_counter;
    uint32 async_ret_counter;

    [[async]] void async_check()
    {
        bool ret = async_initializer.check();
        if (!ret)
        {
            atomic
            {
                async_ret_counter++;
            }
        }
        atomic
        {
            async_completed_counter++;
        }
    }

public:
    void run(unit::tag_t tag)
    {
        async_ret_counter = 0;
        async_completed_counter = 0;

        // Spawn multiple calls to init_once.check()
        static for (const auto i : NUM_ASYNC_CALLS)
        {
            async_check();
        }

        // Wait for all async threads to complete
        wait([]()
        {
            return async_completed_counter == NUM_ASYNC_CALLS;
        });

        // Only one async call should have seen "not initialized"
        unit::assert_equal(tag, true, async_ret_counter == 1);
    }
}

//
// atomic_var test
//
inline uint32 AddOne(uint32 val)
{
    return val + 1;
}

// Class: Unit tests for atomic_var class.
class AtomicVarTests
{
private:
    const auto InitialValue = 1234;
    bool _error = false;

    uint32 ReadModifyWrite()
    {
        return first(atomically<uint32, InitialValue>(AddOne));
    }

public:

    [[pipelined]] void CallReadModifyWrite(uint32 tid)
    {
        ReadModifyWrite();
    }

    bool Run()
    {
        print("atomic_var tests start\n");

        _error = false;

        uint32 val;

        val = ReadModifyWrite();
        if (val != InitialValue)
        {
            _error = true;
            assert(false);
        }
        else
        {
            CallReadModifyWrite(10);
            val = ReadModifyWrite();

            if (val != (InitialValue+11))
            {
                _error = true;
                assert(false);
            }
        }

        print("atomic_var tests complete\n");
        return !_error;
    }
}


// Class: Unit tests for atomic_var_param1 classes.
class AtomicVarParam1Tests
{
private:
    const auto InitialValue = 1234;
    bool _error = false;

    uint32 ReadModifyWrite(uint32 n)
    {
        return first(atomically<uint32, InitialValue>([n](uint32 x) -> uint32
        {
            return n + x;
        }));
    }

public:

    [[pipelined]] void CallReadModifyWrite(uint32 tid, uint32 addend)
    {
        ReadModifyWrite(addend);
    }

    bool Run()
    {
        bool error = false;

        print("atomic_var_param1 tests start\n");

        uint32 val;

        val = ReadModifyWrite(1);
        if (val != InitialValue)
        {
            error = true;
            assert(false);
        }
        else
        {
            CallReadModifyWrite(10, 1);
            val = ReadModifyWrite(1);

            if (val != (InitialValue+11))
            {
                _error = true;
                assert(false);
            }
        }

        print("atomic_var_param1 tests complete\n");
        return !error;
    }
}

// Function: entry point for atomic_var unit tests in this file.
//
// Returns - 0 if test passed, test-dependent non-zero value if test failed.
class test_atomic_var
{
private:
    AtomicVarTests avar_tests;
    AtomicVarParam1Tests avar_param1_tests;

public:
    void run(unit::tag_t tag)
    {
        bool retVal;

        retVal = avar_tests.Run();
        if (retVal)
        {
            retVal = avar_param1_tests.Run();
        }

        unit::assert_equal(tag, true, retVal);
    }
}

class test_atomically_one_of
{
private:
    memory<uint32, 80> inputs;

public:
    void run(unit::tag_t tag)
    {
        const auto N = bitsizeof(inputs)/bitsizeof(inputs[0]);

        static assert(0 == N % 4);

        pipelined_for(N, [](index_t<N> i)
        {
            inputs[i] = i;
        });

        const auto x = pipelined_last(N, [](index_t<N> i)
        {
            uint32 input = inputs[i];
            auto accu = second(atomically_one_of<4>(i % 4, [input](uint32 x) -> uint32
            {
                return input >= 7 ? x + 1 : x;
            }));

            return sum<uint32>(second(serial_in_parallel_out<4>(accu)));
        });

        unit::assert_equal(tag, true, x == N - 7);
    }
}

inline void test_init_generationl(unit::tag_t tag)
{
    const bool[8] inputs =
    {
        true,
        false,
        false,
        true,
        true,
        false,
        true,
        false
    };

    const pair<bool, uint<2>>[8] expecteds =
    {
        { true,  1 },
        { false, 1 },
        { false, 1 },
        { false, 2 },
        { false, 3 },
        { false, 3 },
        { true,  1 },
        { false, 1 }
    };

    pipelined_for(8, [tag, inputs, expecteds](index_t<8> i)
    {
        const auto result = init_generational<2>(inputs[i]);

        const auto expected = expecteds[i];

        unit::assert_equal(tag, expected.first, result.first);
        unit::assert_equal(tag, expected.second, result.second);
    });
}

inline void test_main()
{
    print("sync.atomic test start\n");

    unit::test<1>(unit::fixture<test_basic>());

    // Check multiple asynchronous calls
    unit::test<2>(unit::fixture<test_async>());

    unit::test<3>(unit::fixture<test_atomic_var>());

    unit::test<4>(unit::fixture<test_atomically_one_of>());

    unit::test<5>(test_init_generationl);

    print("sync.atomic test complete\n");
}
