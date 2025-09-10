// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    class Test
    {
    public:
        void main()
        {
            atomic
            {
                do
                {
                } while (false)
            }
        }
    }

    export Test;

expected:0

    class Test
    {
    public:
        void main()
        {
            atomic do {} while (false)
        }
    }

    export Test;

expected:0

    class Test
    {
    public:
        void main()
        {
            // Do not merge with previous basic block
            barrier;

            atomic do {} while (false)
        }
    }

    export Test;

expected:0

    class Test
    {
        inline void DoWhileLoop()
        {
            do
            {

            } while (false)
        }

    public:
        void main()
        {
            atomic
            {
                DoWhileLoop();
            }
        }
    }

    export Test;

expected:34

    class Test
    {
    public:
        void main()
        {
            atomic
            {
                for (const auto i : 15)
                {
                }
            }
        }
    }

    export Test;

expected:34

    class Test
    {
    public:
        void main()
        {
            atomic for (const auto i : 15)
            {
            }
        }
    }

    export Test;

expected:34

    class Test
    {
        bool g_var;

    public:
        void main()
        {
            atomic do {} while (!g_var)
        }
    }

    export Test;

expected:0

    // Local variable for done
    class Test
    {
    public:
        void main(uint8 flag)
        {
            bool done;
            atomic do 
            {
                done = flag != 0;
            } while (!done)
        }
    }

    export Test;

expected:0

    class Test
    {
        bool g_var;

        // Operation outside of do-while
    public:
        void main()
        {
            atomic
            {
                bool done;
                do
                {
                    done = g_var;
                } while (!done)
            }
        }
    }

    export Test;

expected:34

    // atomic inside of atomic do
    class Test
    {
        inline bool helper()
        {
            bool b = true;

            atomic
            {
                b = false;
            }

            return b;
        }

        inline void main()
        {
            atomic do {} while (!helper())
        }
    }

    export Test;

expected:0

    // atomic do with a memory
    class Test
    {
        [[memory]] uint32[16] _headPointers;
        [[memory]] uint32[16] _tailPointers;

    public:
        void main(uint32 qid)
        {
            atomic do {} while (_tailPointers[qid] == _headPointers[qid]+1)
        }
    }

    export Test;

expected:34

    // atomic inside atomic do
    class Test
    {
        [[memory]] uint32[512] g_mem;

        inline bool Foo()
        {
            uint32 slotIndex = 0;

            atomic
            {
                uint32 val = g_mem[slotIndex];
                g_mem[slotIndex] = val;
            }

            return true;
        }

    public:
        void Bar()
        {
            atomic do {} while (!Foo())
        }
    }

    export Test;

expected:34

    // atomic do without boolean
    class Test
    {
        int32 i = 0;

        inline void main()
        {
            atomic do {} while (i)
        }
    }

    export Test;

expected:25

    // atomic do inside of atomic
    class Test
    {
    public:
        void main()
        {
            atomic
            {
                atomic do {} while (false)
            }
        }
    }

    export Test;

expected:34

    // Synchronous function call inside of atomic do
    class Test
    {
        bool Foo()
        {
            return true;
        }

    public:
        void main()
        {
            atomic do {} while (!Foo())
        }
    }

    export Test;

expected:25

    // Previous BB is empty
    inline void main()
    {
        for (const auto i : 2)
        {
            atomic
            {
                [[fifo_depth(64)]] do {} while (true)
            }
        }
    }

expected:0

    // Previous BB is not empty
    inline void main()
    {
        print("Ensure BB is not empty\n");
        atomic
        {
            [[fifo_depth(64)]] do {} while (true)
        }
    }

expected:0

    // more than one fifo_depth (previous BB is empty)
    inline void main()
    {
        atomic
        {
            [[fifo_depth(64), fifo_depth(64)]] do {} while (true)
        }
    }

expected:65

    // more than one fifo_depth (previous BB is not empty)
    inline void main()
    {
        print("Ensure BB is not empty\n");
        atomic
        {
            [[fifo_depth(64), fifo_depth(64)]] do {} while (true)
        }
    }

expected:65

    // more than one fifo_depth (previous BB is empty)
    inline void main()
    {
        for (const auto i : 2)
        {
            atomic
            {
                [[fifo_depth(64), fifo_depth(64)]] do {} while (true)
            }
        }
    }

expected:65

    // Start condition
    class Test
    {
        bool _flag;

    public:
        void main()
        {
            atomic do {} while (!_flag)
        }
    }

    export Test;

expected:0

    // Start condition with inline function
    class Test
    {
        bool _flag;

        inline bool test_and_set_flag()
        {
            bool result = false;
            if (!_flag)
            {
                result = true;
                _flag = true;
            }
            return result;
        }

    public:
        void main()
        {
            atomic do {} while (!test_and_set_flag())
        }
    }

    export Test;

expected:0

// atomic do inside atomic do
    class Test
    {
    public:
        void main()
        {
            atomic do
            {
                atomic do
                {

                } while (false)
            } while (false)
        }
    }

    export Test;

expected:34

    // Statements in atomic, outside do-while
    class Test
    {
    public:
        void main()
        {
            uint32 x;

            atomic
            {
                x++;

                do
                {

                } while (false)
            }
        }
    }

    export Test;

expected:34

    class Test
    {
        uint32 g_var;

        // Statements in atomic, outside do-while
    public:
        void main()
        {
            uint32 x;

            atomic
            {
                do
                {

                } while (false)

                x++;
            }
        }
    }

    export Test;

expected:34

    // Schedule with atomic do
    class Test
    {
    public:
        void main()
        {
            [[schedule(2)]]
            {
                do {}  while (false)
            }
        }
    }

    export Test;

expected:34

    // Nested atomic with do while
    class Test
    {
    public:
        void main()
        {
            [[schedule(2)]]
            {
                atomic
                {

                }
                do {} while (false)
            }
        }
    }

    export Test;

expected:34

    class Test
    {
    public:
        void main()
        {
            go();
        }
    private:
        [[thread_rate(2)]] void go()
        {
            atomic { do {} while (false) }
        }
    }

    export Test;

expected:0

    // atomic do while in nested atomic
    class Test
    {
    public:
        void main()
        {
            [[schedule(2)]]
            {
                atomic
                {
                    do {} while (false)
                }
            }
        }
    }

    export Test;

expected:34

    class Test
    {
        uint32 g_var = 0;

    public:
        void main()
        {
            uint32 x = 1;
            atomic do
            {
                // Read from local variable
                g_var += x;
            } while (g_var < 10);
        }
    }

    export Test;

expected:0

    class Test
    {
        uint32 g_var = 0;

    public:
        void main()
        {
            uint32 x = 1;
            atomic do
            {
                // Write to local variable
                x = g_var;
            } while (g_var < 10);
        }
    }

    export Test;

expected:0

    class Test
    {
        uint32 g_var;

    public:
        void main()
        {
            atomic do
            {
                // Read-modify-write to variable declared in atomic do
                uint32 x = 0;
                x++;
                g_var += x;
            } while (g_var < 10);
        }
    }

    export Test;

expected:0
