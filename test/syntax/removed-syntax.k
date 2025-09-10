// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    [[atomic]] uint32 Foo()
    {
        return 0;
    }

expected:1

    atomic inline uint32 Foo()
    {
        return 0;
    }

expected:1

    inline atomic uint32 Foo()
    {
        return 0;
    }

expected:1

    pipelined uint32 Foo()
    {
        return 0;
    }

expected:1

    unordered uint32 Foo()
    {
        return 0;
    }

expected:1

    [[memory]] uint32[128] mem;

    uint32 Foo()
    {
        atomic_mem(auto x, mem[4])
        {
            x++;
        }
        return 0;
    }

expected:1

    memory uint32[128] mem;

expected:1

    memory_norep uint32[128] mem;

expected:1

    inline void main()
    {
        [call_rate:1] Bar();
    }

expected:1

    extern module clock(0) MyModule
    {
        uint24 Add(uint24 a, uint24 b);
    }

expected:1

    extern(latency:4) uint32 Foo();

expected:1

    typedef bool Foo;

expected:1

    inline void main()()
    {
        unrolled_for (const auto x : 10){}
    }

expected:1

    inline void main()([[end_transaction]] bool l)
    {
    }

expected:1

    [[memory]] uint32[4][512] a;

expected:1
