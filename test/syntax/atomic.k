// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // Atomic
    inline void globalFunc(uint32 x)
    {
        uint32 result;

        atomic
        {
            result = 4;
        }
    }

expected:0

    // atomic memory load index coming from another memory
    class Test
    {
        [[memory]] uint32[512] g_mem1;
        [[memory]] uint32[512] g_mem2;

    public:
        void main(uint32 idx)
        {
            atomic
            {
                g_mem1[g_mem2[idx]]++;
            }
        }
    }

    export Test;

expected:64

    // atomic memory load index coming from a global
    class Test
    {
        uint9 g_idx = 0;

        [[memory]] uint32[512] g_mem;

    public:
        void main()
        {
            atomic
            {
                g_mem[g_idx]++;
            }
        }
    }

    export Test;

expected:0

    // atomic memory load index coming from a global + constant
    class Test
    {
        uint9 g_idx = 0;

        [[memory]] uint32[512] g_mem;

     public:
        void main()
        {
            atomic
            {
                g_mem[g_idx + 1]++;
            }
        }
    }

    export Test;

expected:0

    // control flow inside atomic block
    class Test
    {
    public:
        void Go(uint32 x)
        {
            atomic
            {
                for (const auto i : x)
                {
                    println(i);
                }
            }
        }
    }

    export Test;

expected:34

    // Unrolled for inside atomic (legal)
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                static for (const auto x : 5)
                {
                    uint32 y = x;
                }
            }
        }
    }

    export Main;

expected:0

    // Return inside of atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
            return;
            }
        }
    }

    export Main;

expected:21

    // flat if inside of atomic
    class Main
    {
    public:
        void globalFunc(uint32 x)
        {
            atomic
            {
            if (x > 0)
            {
            }
            }
        }
    }

    export Main;

expected:0

    // atomic block that stores to a memory and then reads from it
    class Main
    {
        [[memory]] uint32[512] g_mem;

    public:
        void Test(uint32 x)
        {
            atomic
            {
                g_mem[x] = x;
                println(g_mem[x]);
            }
        }
    }

    export Main;

expected:34

    // atomic block that compute a load address inside of the atomic block
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void Test(uint32 x)
        {
            atomic
            {
                println(g_mem[x+1]);
            }
        }
    }

    export Main;

expected:0

    // memory write inside of atomic
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex;

            atomic
            {
                auto val = g_mem[slotIndex];
                g_mem[1] = 3;
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:64

    // atomic mem with bad rmw variable type
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex;

            atomic
            {
                uint33 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:0

    // atomic mem with bad rmw variable type
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex;

            atomic
            {
                uint31 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:0

    // atomic mem with bad index type
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            int32 slotIndex;

            atomic
            {
                uint32 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:0

    // atomic mem with bad index type
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            bool slotIndex;

            atomic
            {
                uint32 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:9

    // atomic on an nonarray
    class Main
    {
        uint32 g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex = 0;

            atomic
            {
                uint32 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:1

    // atomic on an array
    class Main
    {
        uint32[512] g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex = 0;

            atomic
            {
                uint32 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:0

    // atomic inside of atomic
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex = 0;

            atomic
            {
                uint32 val = g_mem[slotIndex];
                atomic
                {
                }
                g_mem[slotIndex] = val;
            }
        }
    }

    export Main;

expected:0

    // atomic inside of atomic
    class Main
    {
        [[memory]] uint32[512] g_mem;
    
    public:
        void main()
        {
            uint32 slotIndex = 0;

            atomic
            {
                atomic
                {
                    uint32 val = g_mem[slotIndex];
                    g_mem[slotIndex] = val;
                }
            }
        }
    }

    export Main;

expected:0

    import hardware.dsp

    // fmac32 in atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                auto f = fmac32(3.0, 4.0, false);
                println(f);
            }
        }
    }

    export Main;

expected:64

    import hardware.dsp

    // fadd32 in atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                auto f = fadd32(3.0, 4.0);
                println(f);
            }
        }
    }

    export Main;

expected:64

    import hardware.dsp

    // fmul32 in atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                auto f = fmul32(3.0, 4.0);
                println(f);
            }
        }
    }

    export Main;

expected:64

    import hardware.dsp

    // umul27 inside atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                uint54 x = umul27(3, 4);
                println(x);
            }
        }
    }

    export Main;

expected:64

    import base.system

    // stage inside of atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                stages<1>();
            }
        }
    }

    export Main;

expected:34

    // barrier inside of atomic
    class Main
    {
    public:
        void main()
        {
            atomic
            {
                barrier;
            }
        }
    }

    export Main;

expected:34

    // fan_out in atomic
    class Main
    {
    public:
        void main()
        {
            uint32 x = 2;

            atomic
            {
                uint32[4] f = fan_out<4>(x);
            }
        }
    }

    export Main;

expected:34

    // Read memory in atomic
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        void TestFunction()
        {
            atomic
            {
                assert(1 == g_mem[2]);
            }
        }
    }

    export Main;

expected:0

    // Write memory in atomic
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        void TestFunction()
        {
            atomic
            {
                g_mem[4] = 1;
            }
        }
    }

    export Main;

expected:0

    // Read after write of a global variable in an atomic block
    class Main
    {
        uint32 g = 5;
    
    public:
        void TestFunction()
        {
            uint32 x = 0;

            atomic
            {
                g = 7;

                x = g;
            }

            assert(x == 7);
        }
    }

    export Main;

expected:0

// nested atomic
class Main
{
public:
    uint32 Run(uint32 x)
    {
        atomic
        {
            atomic
            {
                x++;
            }
        }
        return x;
    }
}

export Main;
expected:0

    // Nested atomic via inline function
    inline void Foo()
    {
        atomic
        {
        }
    }

    class Main
    {
    public:
        void main()
        {
            atomic
            {
                Foo();
            }
        }
    }

    export Main;

expected:0

// nested less restrictive atomic
class Main
{
public:
    uint32 Run(uint32 x)
    {
        [[schedule(3)]]
        {
            x++;
            [[schedule(4)]]
            {
                x++;
            }
            x++;
        }
        return x;
    }
}

export Main;
expected:0

// nested more restrictive atomic
class Main
{
public:
    uint32 Run(uint32 x)
    {
        [[schedule(3)]]
        {
            x++;
            atomic
            {
                x++;
            }
            x++;
        }
        return x;
    }
}

export Main;
expected:0

// multiple nestings
class Main
{
public:
    uint32 Run(uint32 x)
    {
        [[schedule(5)]]
        {
            atomic
            {
                x++;
            }
            [[schedule(2)]]
            {
                atomic
                {
                    x++;
                }
            }
            [[schedule(2)]]
            {
                x++;
            }
        }
        return x;
    }
}

export Main;
expected:0

    // multiple nestings with global
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run()
        {
            [[schedule(5)]]
            {
                atomic
                {
                    g_var++;
                }
                [[schedule(2)]]
                {
                    atomic
                    {
                        g_var++;
                    }
                }
                [[schedule(2)]]
                {
                    g_var++;
                }
            }
            return g_var;
        }
    }

    export Main;

expected:0

    // multiple nestings with memory
    class Main
    {
        [[memory]] uint32[32] g_var;
    
    public:
        uint32 Run()
        {
            [[schedule(10)]]
            {
                // This becomes a set of 3 chained atomics where the middle atomic
                // has an update rate of 1. 3 pipeline stages are needed.
                atomic
                {
                    g_var[0]++;
                }
                [[schedule(3)]]
                {
                    // This becomes a set of 3 chained atomics where the middle
                    // atomic has an update rate of 1. 3 pipeline stages are
                    // needed.
                    atomic
                    {
                        g_var[0]++;
                    }
                }
                // This becomes a set of 3 chained atomics where the middle atomic
                // has an update rate of 2. 4 pipeline stages are needed.
                [[schedule(2)]]
                {
                    g_var[0]++;
                }
            }
            return g_var[0];
        }
    }

    export Main;

expected:0

    // global read before nested atomic
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                x = g_var;
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:0

    // global write before nested atomic
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                g_var = x;
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:0

    // global read after nested atomic
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                x = g_var;
            }
            return x;
        }
    }

    export Main;

expected:0

    // global write after nested atomic
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                g_var = x;
            }
            return x;
        }
    }

    export Main;

expected:0

    // global read between nested atomics
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                x = g_var;
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:0

    // global write between nested atomics
    class Main
    {
        uint32 g_var;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                g_var = x;
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:0

    // memory read before nested atomic
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                x = g_mem[0];
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:34

    // memory write before nested atomic
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                g_mem[0] = x;
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:34

    // memory read after nested atomic
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                x = g_mem[0];
            }
            return x;
        }
    }

    export Main;

expected:34

    // memory write after nested atomic
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                g_mem[0] = x;
            }
            return x;
        }
    }

    export Main;

expected:34

    // memory read between nested atomics
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                x = g_mem[0];
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:34

    // memory write between nested atomics
    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        uint32 Run(uint32 x)
        {
            [[schedule(3)]]
            {
                atomic
                {
                    x++;
                }
                g_mem[0] = x;
                atomic
                {
                    x++;
                }
            }
            return x;
        }
    }

    export Main;

expected:34

// More logic than schedulable with register ratio before nested atomic
class Main
{
public:
    uint32 Run(uint32 x)
    {
        [[schedule(2)]]
        {
            static for (const auto i : 100)
            {
                x += x;
            }
            atomic
            {
                x += x;
            }
        }
        return x;
    }
}

export Main;
expected:0

// More logic than schedulable with register ratio after nested atomic
class Main
{
public:
    uint32 Run(uint32 x)
    {
        [[schedule(2)]]
        {
            atomic
            {
                x += x;
            }
            static for (const auto i : 100)
            {
                x += x;
            }
        }
        return x;
    }
}

export Main;
expected:0

    class Main
    {
        [[memory]] uint32[32] g_mem;
    
    public:
        void main(uint32 addr)
        {
            [[schedule(4)]]
            {
                [[schedule(1)]]
                {
                    // Address computation in nested atomic
                    static for (const auto i : 2)
                    {
                        addr++;
                    }

                    g_mem[addr] = g_mem[addr] + 1;
                }
            }
        }
    }

    export Main;

expected:0
