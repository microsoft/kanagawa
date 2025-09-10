// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        static T1<S2> t;
        S2<uint32> s;
        t.x = s;
    }

expected:0

    inline void main()
    {
        static T1<S3> t;
        S3<uint32> s;
        t.x = s;
    }

expected:0

    inline void main()
    {
        static T3<5, S3> t;
        S3<uint32, 5> s;
        t.x = s;
    }

expected:0

    inline void main()
    {
        static T3<5, S2> t;
    }

expected:1

    inline void main()
    {
        static T1<> t;
        S1<uint32> s;
        t.x = s;
    }

expected:0

    inline void main()
    {
        static T1<> t;
        S2<uint32> s;
        t.x = s;
    }

expected:14

    inline void main()
    {
        static T1<S2> t;
        S1<uint32> s;
        t.x = s;
    }

expected:14

    inline void main()
    {
        static T1<S1> t;
        S1<bool> s;
        t.x = s;
    }

expected:14

    template <template <typename> typename TT>
    class Foo
    {
    public:
        T1<TT> x;
    }

    inline void main()
    {
        static Foo<S2> foo;
        S2<uint32> s;
        foo.x.x = s;
    }

expected:0

    inline void main()
    {
        static T2<bool, S2> t;
        S2<bool> s;
        t.x = s;
    }

expected:0

    inline void main()
    {
        static T2<bool> t;
        S1<bool> s;
        t.x = s;
    }

expected:0

    inline void main()
    {
        static T2<bool, S2> t;
        S2<uint32> s;
        t.x = s;
    }

expected:14

    inline void main()
    {
        static T2<bool> t;
        S1<uint32> s;
        t.x = s;
    }

expected:14

    import data.memory

    inline void main()
    {
        static T3<16, memory> mem1;
        static T3<16, memory_norep> mem2;
        mem1.x[5] = mem2.x[3];
    }

expected:0

    import data.memory

    inline void main()
    {
        static T3<15, memory> mem1;
    }

expected:0

    template <bool B>
    struct Foo
    {
        uint<B ? 32 : 64> x;
    }

    inline void main()
    {
        static T4<bool, Foo> t;
        static assert(bitsizeof t.x.x == 64);
    }

expected:0

    template <bool B>
    struct Foo
    {
        uint<B ? 32 : 64> x;
    }

    inline void main()
    {
        static T4<uint32, Foo> t;
    }

expected:1

    template <uint32 B>
    struct Foo
    {
        uint<B> x;
    }

    inline void main()
    {
        static T4<uint32, Foo> t;
    }

expected:1

    template <typename T, (T) -> uint32 Fn, template <(T) -> uint32> typename TT>
    class Foo
    {
    public:
        auto Go(T t)
        {
            return x.Go(t);
        }

        TT<Fn> x;
    }

    template <(bool) -> uint32 Fn>
    class Bar
    {
    public:
        auto Go(bool b)
        {
            return Fn(b);
        }
    }

    inline uint32 Do(bool b)
    {
        return b ? 10 : 11;
    }

    inline void main()
    {
        static Foo<bool, Do, Bar> foo;
        foo.Go(false);
    }

expected:0

    inline void main()
    {
        static T1<bool> t;
    }

expected:1

    inline void main()
    {
        static T1<10> t;
    }

expected:1

    import data.memory

    inline void main()
    {
        static T1<memory> t;
    }

expected:1

    template <auto N>
    class Foo{}

    inline void main()
    {
        static T1<Foo> t;
    }

expected:1

    using X = uint32;

    template <template <typename> typename TT = X>
    class Foo
    {
    public:
    }

expected:1

    template <typename T>
    inline T Fn(T x)
    {
        return x;
    }

    inline void main()
    {
        static T1<Fn> t;
    }

expected:1

    template <typename T, uint<10> N>
    class Bar
    {
        T[N] x;
    }

    template <auto N, template <typename, uint<N>> typename TT>
    class Foo
    {
    public:
        TT<uint32, N> x;
    }

    inline void main()
    {
        static Foo<10, Bar> foo;
    }

expected:0

    inline void main()
    {
        Fn1();
    }

expected:0

    inline void main()
    {
        Fn1<S2>();
    }

expected:0

    inline void main()
    {
        Fn1<S3>();
    }

expected:0

    import data.memory

    inline void main()
    {
        Fn1<memory>();
    }

expected:1

    template <template <typename> typename TT>
    class Foo
    {
    public:
        TT<uint32, bool> x;
    }

    inline void main()
    {
        static Foo<S1> foo;
    }

expected:1

    inline S1<uint32> Do()
    {
        return {};
    }

    inline void main()
    {
        FnFn<uint32, S1, Do>();
    }

expected:0

    inline S1<uint32> Do()
    {
        return {};
    }

    inline void main()
    {
        FnFn<uint32, S2, Do>();
    }

expected:1

    inline S1<uint32> Do()
    {
        return {};
    }

    inline void main()
    {
        FnFn<uint32, bool, Do>();
    }

expected:1

    inline S1<uint32> Do()
    {
        return {};
    }

    template <template <typename> typename TT>
    inline void Go()
    {
        FnFn<uint32, TT, Do>();
    }

    inline void main()
    {
        Go<S1>();
    }

expected:0

    inline S1<uint32> Do()
    {
        return {};
    }

    template <template <typename> typename TT>
    inline void Go()
    {
        FnFn<uint32, TT, Do>();
    }

    inline void main()
    {
        Go<S2>();
    }

expected:1

    inline S1<uint32> Do()
    {
        return {};
    }

    template <typename T>
    inline void Go()
    {
        FnFn<uint32, T, Do>();
    }

    inline void main()
    {
        Go<S1<uint32>>();
    }

expected:1

    inline S1<uint32> Do()
    {
        return {};
    }

    template <typename T>
    inline void Go()
    {
        FnFn<uint32, T, Do>();
    }

    inline void main()
    {
        Go<uint32>();
    }

expected:1

    template <typename T, template <typename> typename TT, () -> TT<T, uint32> fn>
    inline void Go()
    {
    }
    
    inline S1<uint32> Do()
    {
        return {};
    }

    inline void main()
    {
        Go<uint32, S1, Do>();
    }

expected:1
