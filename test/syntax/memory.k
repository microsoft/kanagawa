// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    import data.array

    class Main
    {
        const memory_ecc<uint32, 16> mem = { 1, 2, 3, 4 };

    public:
        void main()
        {
            println(mem[0]);
        }
    }

    export Main;

expected:1

    inline uint32 make_ecc_wrong_data_type(bool error, bool valid, uint31 value)
    {
        return value;
    }

    class Main
    {
        [[memory, ecc(make_ecc_wrong_data_type)]] uint32[512] _mem;

    public:
        void main()
        {
            println(_mem[0]);
        }
    }

    export Main;

expected:69

    inline uint32 make_ecc_wrong_valid_type(bool error, uint1 valid, uint32 value)
    {
        return value;
    }

    class Main
    {
        [[memory, ecc(make_ecc_wrong_valid_type)]] uint32[512] _mem;

    public:
        void main()
        {
            println(_mem[0]);
        }
    }

    export Main;

expected:69

    inline uint32 make_ecc_wrong_error_type(uint1 error, bool valid, uint32 value)
    {
        return value;
    }

    class Main
    {
        [[memory, ecc(make_ecc_wrong_error_type)]] uint32[512] _mem;

    public:
        void main()
        {
            println(_mem[0]);
        }
    }

    export Main;

expected:69

    inline uint32 make_ecc_extra_param(bool error, bool valid, uint32 value, uint32 extra)
    {
        return value;
    }

    class Main
    {
        [[memory, ecc(make_ecc_extra_param)]] uint32[512] _mem;

    public:
        void main()
        {
            println(_mem[0]);
        }
    }

    export Main;

expected:69

    class Main
    {
        memory_ecc<uint32, 16> m;
    }

    export Main;

expected:0

    class Main
    {
        memory_ecc_strict<uint32, 16> m;
    }

    export Main;

expected:0

    inline void main()
    {
        check_return_type<uint32, optional, read_mem<uint32, memory>>();
    }

expected:1

    inline void main()
    {
        check_scalar_return_type<uint32, read_mem<uint32, memory>>();
    }

expected:0

    import data.array

    inline void main()
    {
        check_scalar_return_type<uint32, read_mem<uint32, array>>();
    }

expected:0

    inline void main()
    {
        check_return_type<uint32, optional, read_mem<uint32, memory_ecc>>();
    }

expected:1

    inline void main()
    {
        check_return_type<uint32, ecc, read_mem<uint32, memory_ecc_strict>>();
    }

expected:1

    inline void main()
    {
        check_return_type<uint32, ecc, read_mem<uint32, memory_ecc>>();
    }

expected:0

    inline void main()
    {
        check_return_type<uint32, optional, read_mem<uint32, memory_ecc_strict>>();
    }

expected:0

    inline auto read_data()
    {
        auto x = read_mem<bool, memory_ecc>();

        return x.data;
    }

    inline void main()
    {
        check_return_type<bool, optional, read_data>();
    }

expected:0

    inline auto read_error()
    {
        auto x = read_mem<bool, memory_ecc>();

        return x.error;
    }

    inline void main()
    {
        check_scalar_return_type<bool, read_error>();
    }

expected:0

    inline auto read_value()
    {
        auto x = read_mem<bool, memory_ecc_strict>();

        return x.value;
    }

    inline void main()
    {
        check_scalar_return_type<bool, read_value>();
    }

expected:0

    import data.array

    class Main
    {
        memory_ecc<array<uint32, 10>, 16> mem;

    public:
        auto read()
        {
            return mem[0];
        }

        void main()
        {
            static assert(decltype(read()) == ecc<uint32[10]>);
        }
    }

    export Main;

expected:0

    import data.array

    class Main
    {
        array<memory_ecc<array<uint32, 10>, 16>, 3> mem;

    public:
        auto read()
        {
            return mem[0][0];
        }

        void main()
        {
            static assert(decltype(read()) == ecc<uint32[10]>);
        }
    }

    export Main;

expected:0

    class Main
    {
        memory_ecc<uint32, 16> mem;

    public:
        void main()
        {
            mem[0] += 10;
        }
    }

    export Main;

expected:1

    struct foo
    {
    }

    template <typename T, auto N>
    using invalid_ecc_memory = [[memory, ecc(foo)]] T[N];

    inline void main()
    {
        auto x = read_mem<uint32, invalid_ecc_memory>();
    }

expected:1

    template <typename T>
    inline void foo()
    {
        inline optional<T> fn(bool error, bool valid, T value)
        {
            return {valid, value};
        }

        static [[memory, ecc(fn)]] T[16] m;

        m[0] = 10;
    }

    inline void main()
    {
        foo<uint32>();
    }

expected:0

    template <typename T>
    inline void Foo()
    {
    }

    class Main
    {
        uint32 make_ecc(bool error, bool valid, uint32 value)
        {
            return value;
        }

        inline void main()
        {
            Foo<[[memory, ecc(make_ecc)]] uint32[512]>();
        }
    }

expected:0

    using t = uint32[10];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = t[2];

        t2[4] c;

    public:
        void main()
        {
            uint32[2][10] x = a;
            uint32[4][2][10] y = b;
            uint32[4][2][10] z = c;
        }
    }

    export Main;

expected:0

    using t = uint32[11];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = t[2];

        t2[4] c;

    public:
        void main()
        {
            uint32[2][10] x = a;
            uint32[4][2][10] y = b;
            uint32[4][2][10] z = c;
        }
    }

    export Main;

expected:1

    using t = [[memory]] uint32[10];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = t[2];

        t2[4] c;

    public:
        void main()
        {
            uint32[2][10] x = a;
            uint32[4][2][10] y = b;
            uint32[4][2][10] z = c;
        }
    }

    export Main;

expected:1

    using t = uint32[10];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = [[memory]] t[2];

        t2[4] c;

    public:
        void main()
        {
            uint32[2][10] x = a;
            uint32[4][2][10] y = b;
            uint32[4][2][10] z = c;
        }
    }

    export Main;

expected:1

    using t = uint32[10];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = t[2];

        [[memory]] t2[4] c;

    public:
        void main()
        {
            uint32[2][10] x = a;
            uint32[4][2][10] y = b;
            uint32[4][2][10] z = c;
        }
    }

    export Main;

expected:1

    using t = uint32[10];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = t[2];

        t2[4] c = {{.foo = 10}};
    }

    export Main;

expected:1

    using t = [[memory]] uint32[10];

    class Main
    {
        t[2] a;
        t[4][2] b;

        using t2 = t[2];

        t2[4] c = {{.foo = 10}};
    }

    export Main;

expected:1

    template <typename T, auto N>
    using memory_init = [[memory, initialize]] T[N];

    class Main
    {
        memory_init<uint32, 200> mem;
    }

    export Main;

expected:0
