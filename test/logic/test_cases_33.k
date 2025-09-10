// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// memory with an array of memories
// ensures that the replicated mode is used with these memories (no conflicts)
class Helper550
{
private:
    uint32 _valid = 0;
    uint32 _invalid = 0;

    (uint32, uint2)->uint32 read_mem;

public:
    [[async, pipelined]] void Read(uint32 tid)
    {
        uint32 localValid = 0;
        uint32 localInvalid = 0;

        static for(const auto i : 4)
        {
            uint32 val = read_mem(tid, i);

            if (val == (tid + 2 + i))
            {
                localValid++;
            }
            else
            {
                localInvalid++;
            }
        }

        atomic
        {
            _valid += localValid;
            _invalid += localInvalid;
        }
    }

    inline uint32 GetValid()
    {
        return _valid;
    }

    inline uint32 GetInvalid()
    {
        return _invalid;
    }

    inline void Wait(uint32 expected)
    {
        atomic do; while((_valid + _invalid) != expected);
    }
}

class Test550
{
private:
    memory<uint32, 512>[4] _mem550;

    [[pipelined]] void Init550(uint32 tid)
    {
        static for(const auto i : 4)
        {
            _mem550[i][tid] = tid + 2 + i;
        }
    }

    inline uint32 ReadMemory(uint32 addr, uint2 i)
    {
        return _mem550[i][addr];
    }

    Helper550[2] g_helper550 =
    {
        {.read_mem = ReadMemory},
        {.read_mem = ReadMemory}
    };

public:
    void run(unit::tag_t tag)
    {
        // fill the memory
        Init550(512);

        //  Start reads from both helpers
        static for (const auto i : 2)
        {
            g_helper550[i].Read(512);

            // ensure the reads start at different times
            static for(const auto j : 10)
            {
                stages<2>();
            }
        }

        // Wait for all operations to finish
        static for (const auto i : 2)
        {
            g_helper550[i].Wait(2048);
        }

        uint32 totalValid = 0;
        uint32 totalInvalid = 0;

        static for (const auto i : 2)
        {
            print("Instance: {i} valid: {g_helper550[i].GetValid()} invalid: {g_helper550[i].GetInvalid()}\n");

            totalValid += g_helper550[i].GetValid();
            totalInvalid += g_helper550[i].GetInvalid();
        }

        // assert after print
        barrier;

        // Replicated memory is used, so there should never be conflicts
        unit::assert(tag, totalValid == 4096);
        unit::assert(tag, totalInvalid == 0);
    }
}

// predicated read of [[memory, non_replicated]]
class Helper551
{
private:
    uint32 _valid;
    uint32 _invalid;

    (uint32)->uint32 read_mem;

public:
    void Reset()
    {
        _valid = 0;
        _invalid = 0;
    }

    [[async, pipelined]] void Read(uint32 tid, bool flag)
    {
        uint32 val = 0;

        if (flag)
        {
            val = read_mem(tid);
        }

        if (val == (tid + 2))
        {
            atomic
            {
                _valid++;
            }
        }
        else
        {
            atomic
            {
                _invalid++;
            }
        }
    }

    inline uint32 GetValid()
    {
        return _valid;
    }

    inline uint32 GetInvalid()
    {
        return _invalid;
    }

    inline void Wait(uint32 expected)
    {
        atomic do; while((_valid + _invalid) != expected);
    }
}

class Test551
{
private:
    [[memory, non_replicated]] uint32[512] _mem551;

    [[pipelined]] void Init551(uint32 tid)
    {
        _mem551[tid] = tid + 2;
    }

    inline uint32 ReadMemory(uint32 addr)
    {
        return _mem551[addr];
    }

    Helper551[2] g_helper551 =
    {
        {.read_mem = ReadMemory},
        {.read_mem = ReadMemory}
    };

public:
    void run(unit::tag_t tag)
    {
        // fill the memory
        Init551(512);

        // test both memories
        // to ensure that this test is useful no matter which one is chosen as the "winner"
        for (const uint32 testIndex : 2)
        {
            // Clear state
            static for (const auto i : 2)
            {
                g_helper551[i].Reset();
            }

            // Start reads from both helpers
            // reads from 1 helper are predicated off
            static for (const auto i : 2)
            {
                g_helper551[i].Read(512, (i == testIndex));

                // ensure the reads start at different times
                static for(const auto j : 10)
                {
                    stages<2>();
                }
            }

            // Wait for all operations to finish
            static for (const auto i : 2)
            {
                g_helper551[i].Wait(512);
            }

            static for (const auto i : 2)
            {
                print("Instance: {i} valid: {g_helper551[i].GetValid()} invalid: {g_helper551[i].GetInvalid()}\n");
            }

            if (0 == testIndex)
            {
                // helper[0] should see all valid
                unit::assert(tag, g_helper551[0].GetValid() == 512);
                unit::assert(tag, g_helper551[0].GetInvalid() == 0);

                unit::assert(tag, g_helper551[1].GetValid() == 0);
                unit::assert(tag, g_helper551[1].GetInvalid() == 512);
            }
            else
            {
                // helper[1] should see all valid
                unit::assert(tag, g_helper551[0].GetValid() == 0);
                unit::assert(tag, g_helper551[0].GetInvalid() == 512);

                unit::assert(tag, g_helper551[1].GetValid() == 512);
                unit::assert(tag, g_helper551[1].GetInvalid() == 0);
            }
        }
    }
}

// atomic on array of memories
class Test552
{
private:
    memory<uint32, 512>[4] g_mem552;

public:
    [[pipelined]] void Helper552(uint32 tid)
    {
        uint16[4] indices;

        indices[0] = 0;
        indices[1] = tid % 2;
        indices[2] = tid % 4;
        indices[3] = tid % 8;

        atomic
        {
            uint32[4] values;
            static for(const auto i : 4)
            {
                values[i] = g_mem552[i][indices[i]];
            }
            static for(const auto i : 4)
            {
                values[i] = values[i] + 1;
            }
            static for(const auto i : 4)
            {
                g_mem552[i][indices[i]] = values[i];
            }
        }
    }

    [[pipelined]] void Clear552(uint32 tid)
    {
        static for(const auto i : 4)
        {
            g_mem552[i][tid] = 0;
        }
    }

    void run(unit::tag_t tag)
    {
        // Set all elements to 0
        Clear552(512);

        // do some atomic operations
        Helper552(16);

        unit::assert(tag, g_mem552[0][0] == 16);

        unit::assert(tag, g_mem552[1][0] == 8);
        unit::assert(tag, g_mem552[1][1] == 8);

        unit::assert(tag, g_mem552[2][0] == 4);
        unit::assert(tag, g_mem552[2][1] == 4);
        unit::assert(tag, g_mem552[2][2] == 4);
        unit::assert(tag, g_mem552[2][3] == 4);

        unit::assert(tag, g_mem552[3][0] == 2);
        unit::assert(tag, g_mem552[3][1] == 2);
        unit::assert(tag, g_mem552[3][2] == 2);
        unit::assert(tag, g_mem552[3][3] == 2);
        unit::assert(tag, g_mem552[3][4] == 2);
        unit::assert(tag, g_mem552[3][5] == 2);
        unit::assert(tag, g_mem552[3][6] == 2);
        unit::assert(tag, g_mem552[3][7] == 2);
    }
}

// atomic - testing write delay functionality works
class Test553
{
private:
    [[memory]] uint32[512] g_mem553;

    uint32 g_counter_553 = 0;
    uint32 g_index0_553 = 0;
    uint32 g_rate553 = 1;

public:
    [[pipelined]] void Helper553(uint32 tid)
    {
        uint32 index;

        atomic
        {
            index = g_counter_553;

            if (index < (g_rate553 - 1))
            {
                g_counter_553 = index + 1;
            }
            else
            {
                g_counter_553 = 0;
            }
        }

        if (index == 0)
        {
            atomic
            {
                g_index0_553++;
            }
        }

        atomic
        {
            uint32 val = g_mem553[index];
            val++;
            g_mem553[index] = val;
        }
    }

    void run(unit::tag_t tag)
    {
        // bypass bugs occur with a particular modulus
        // for example, some bugs occur when the index access patern is:
        // 0, 0, 0, 0
        // while others repro with
        // 0, 1, 2, 0, 1, 2
        for (const auto i : 7)
        {
            g_rate553 = i + 1;
            print("Iteration {g_rate553}\n");

            g_index0_553 = 0;

            g_counter_553 = 0;

            g_mem553[0] = 7;

            Helper553(500);

            unit::assert(tag, g_mem553[0] == (g_index0_553 + 7));
        }
    }
}

// predicated read of [[memory, non_replicated]]
class Helper555
{
private:
    uint32 _valid;
    uint32 _invalid;

    [[memory]] uint32[512] g_mem555;

public:
    [[pipelined]] void Init555(uint32 tid)
    {
        g_mem555[tid] = tid + 2;
    }

    void Reset()
    {
        _valid = 0;
        _invalid = 0;
    }

    [[async, pipelined]] void Read(uint32 tid)
    {
        uint32 val = 0;

        // back to back reads where every even address
        // should be a valid predicated read
        if (tid%2 == 0)
        {
            val = g_mem555[tid];
        }

        if (val == (tid + 2))
        {
            atomic
            {
                _valid++;
            }
        }
        else
        {
            atomic
            {
                _invalid++;
            }
        }
    }

    inline uint32 GetValid()
    {
        return _valid;
    }

    inline uint32 GetInvalid()
    {
        return _invalid;
    }

    inline void Wait(uint32 expected)
    {
        atomic do; while((_valid + _invalid) != expected);
    }
}

class Test555
{
private:
    Helper555 g_helper555;

public:
    void run(unit::tag_t tag)
    {
        // fill the memory
        g_helper555.Init555(512);

        // Clear state
        g_helper555.Reset();

        // Start reads (predication occurs within Read based on tid)
        // Goal - every other read should happen because predicate is true
        g_helper555.Read(512);

        // Wait for all operations to finish
        g_helper555.Wait(512);

        print("valid: {g_helper555.GetValid()} invalid: {g_helper555.GetInvalid()}\n");

        unit::assert(tag, g_helper555.GetValid() == 256);
        unit::assert(tag, g_helper555.GetInvalid() == 256);
    }
}

// Test pipelined calls with thread count == 1
class Test558
{
private:
    uint32 g_count558 = 0;

public:
    [[pipelined]] void Helper558(uint32 tid)
    {
        atomic
        {
            g_count558++;
        }
    }

    void run(unit::tag_t tag)
    {
        for (const uint32 threadCount : 10)
        {
            g_count558 = 0;

            Helper558(threadCount);

            unit::assert(tag, g_count558 == threadCount);
        }
    }
}


// ternary operator with void type
class Test559
{
private:
    bool Helper559_1_Called = false;
    bool Helper559_2_Called = false;

public:
    void Helper559_1()
    {
        Helper559_1_Called = true;
    }

    void Helper559_2()
    {
        Helper559_2_Called = true;
    }

    void run(unit::tag_t tag)
    {
        bool b = false;

        b ? Helper559_1() : Helper559_2();

        unit::assert(tag, Helper559_1_Called);
        unit::assert(tag, Helper559_2_Called);
    }
}

inline void test_main()
{
    unit::test<550>(unit::fixture<Test550>());
    unit::test<551>(unit::fixture<Test551>());
    unit::test<552>(unit::fixture<Test552>());
    unit::test<553>(unit::fixture<Test553>());
    unit::test<555>(unit::fixture<Test555>());
    unit::test<558>(unit::fixture<Test558>());
    unit::test<559>(unit::fixture<Test559>());
}
