// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    class Main
    {
        // inline and noinline together
        inline noinline uint32 X()
        {
            return 3;
        }

        uint32 main()
        {
            return X();
        }
    }
expected:1

    class C
    {
    public:
        [[no_backpressure]] void main(uint32 x)
        {
            for (const uint32 i : x)
            {

            }
        }
    }

    export C;

expected:18

    class C
    {
    public:
        void main()
        {
            go();
        }
    private:
        [[no_backpressure, max_threads(32)]] void go()
        {
        }
    }

    export C;

expected:18

    class C
    {
    public:
        void main()
        {
            go();
        }
    private:
        [[no_backpressure, thread_rate(2)]] void go()
        {
        }
    }

    export C;

expected:18

    class C
    {
        [[async]] void F()
        {
            print("here\n");
        }

    public:
        [[no_backpressure]] void main()
        {
            F();
        }
    }

    export C;

expected:18

    class C
    {
    public:
        [[no_backpressure]] void main()
        {
        }
    }

    export C;

expected:0

    // extern [[no_backpressure]] requires [[async]]
    class C
    {
    public:
        [[no_backpressure]] void main()
        {
        }
    }

    extern C;

    class D
    {
        C _c;

    public:
        void main()
        {

        }
    }

    export D;

expected:18

    class C
    {
    public:
        [[latency(2)]] void main()
        {
        }
    }
    export C;

expected:0

    class C
    {
        [[reset]]
        void foo()
        {
        }
    }
    export C;

expected:0

    class C
    {
    public:
        [[reset]]
        void foo()
        {
        }
    }
    export C;

expected:0

    class C
    {
    public:
        [[reset]]
        bool foo()
        {
            return true;
        }
    }
    export C;

expected:1

    class C
    {
    public:
        [[reset]]
        void foo(uint32 x)
        {
        }
    }
    export C;

expected:1

    class C
    {
        [[reset]]
        auto foo()
        {
            return 3;
        }
    }

expected:1

    inline void bar()
    {
    }

    class C
    {
    public:
        [[reset]]
        auto foo()
        {
            return bar();
        }
    }
    export C;

expected:0

    class C
    {
    public:
        auto bar()
        {
            return true;
        }

        [[reset]]
        auto foo()
        {
            return bar();
        }
    }

expected:1

    import control.loop
    import type.stdtype
    import data.memory

    class Foo
    {
    private:
        [[reset]] void init()
        {
            pipelined_for(32, [](index_t<32> i)
            {
                mem[i] = 0;
            });
        }

        memory<uint32, 32> mem;
    }

expected:0
    // [[pipelined]] inline not supported
    [[pipelined]] inline uint32 foo(uint32 tid)
    {
        return tid + 1;
    }

    class C
    {
    public:
        uint32[1] main()
        {
            return foo(1);
        }
    }

    export C;

expected:18

    template <typename T>
    inline void Foo()
    {
    }

    inline void main()
    {
        Foo<[[thread_rate(10)]] () -> void>();
    }

expected:0

    class C
    {
    public:
        void main()
        {
            f();
        }
    private:
        // --max-threads-limit exceeded
        [[max_threads(1024)]] void f()
        {

        }
    }

    export C;

expected:18


    // [[pure]] only allowed on external functions
    class C
    {
    public:
        [[pure]] uint32 foo(uint32 x)
        {
            return x + 1;
        }
    }

    export C;
expected:18

    // call into a function with [[latency]] from ordinary code
    class C
    {
        [[latency(1)]] uint32 f(uint32 x)
        {
            return x + 1;
        }

    public:
        uint32 entry(uint32 x)
        {
            return f(x);
        }
    }

    export C;

expected:27

    // [[latency]] & inline
    [[latency(1)]] inline uint32 f(uint32 x)
    {
        return x + 1;
    }

expected:18

    // [[no_backpressure]] & inline
    [[no_backpressure]] inline uint32 f(uint32 x)
    {
        return x + 1;
    }

expected:18
