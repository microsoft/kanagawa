// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    const auto fn = Foo;

    inline void main()
    {
        fn(true);
    }

expected:0

    const auto fn = Foo1;

    inline void main()
    {
        fn(true);
    }

expected:1

    const auto fn1 = Foo;
    const auto fn = fn1;

    inline void main()
    {
        fn(true);
    }

expected:0

    const auto fn1 = Foo;
    const auto fn = fn2;

    inline void main()
    {
        fn(true);
    }

expected:1

    const auto fn1 = Foo1;
    const auto fn = fn1;

    inline void main()
    {
        fn(true);
    }

expected:1

    const auto fn = Bar<uint32>;

    inline void main()
    {
        fn(10);
    }

expected:0

    const auto fn = Bar<bool>;

    inline void main()
    {
        fn(true);
    }

expected:0

    const auto fn = Bar<uint32>;

    inline void main()
    {
        T1<fn>(10);
    }

expected:0

    const auto fn1 = Bar<uint32>;
    const auto fn = fn1;

    inline void main()
    {
        T1<fn>(10);
    }

expected:0

    const auto fn1 = Bar<uint3>;
    const auto fn = fn1;

    inline void main()
    {
        T1<fn>(10);
    }

expected:1

    const (bool) -> uint32 fn = Foo;

expected:0

    const (bool) -> uint32 fn = Foo;
    const auto fn2 = fn;

expected:0

    const auto fn = Foo;
    const (bool) -> uint32 fn2 = fn;

expected:0

    const auto fn = Foo;
    const (bool) -> uint3 fn2 = fn;

expected:1

    inline void mian()
    {
        (bool) -> uint32 fn = Foo;
    }

expected:1

    const (bool) -> uint3 fn = Foo;

expected:1

    const (uint32) -> uint32 fn = Foo;

expected:1

    const (uint3) -> uint3 fn = Bar<uint3>;

expected:0

    const (uint3 foo) -> uint3 fn = Bar<uint3>;

expected:0

    const (uint4) -> uint3 fn = Bar<uint3>;

expected:1

    const (uint32) -> uint32 fn = 10;

expected:1

    const (uint32) -> uint32 fn;

expected:1

    template <typename T>
    class C
    {
    public:
        const (T) -> uint32 f = Foo;
    }

    inline void main()
    {
        static C<bool> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        const (T x) -> uint32 f = Foo;
    }

    inline void main()
    {
        static C<bool> c;
    }

expected:0

    template <typename T>
    class C
    {
    public:
        const (T) -> uint32 f = Foo;
    }

    inline void main()
    {
        static C<uint32> c;
    }

expected:1

    inline void main()
    {
        F3<Foo>();
    }

expected:0

    inline void main()
    {
        F3<FooB>();
    }

expected:1

    inline void main()
    {
        static C1<Foo> c1;

        c1.Run();
    }

expected:0

    inline void main()
    {
        static C2<bool, uint32, Foo> c2;

        c2.Run();
    }

expected:0

    inline void main()
    {
        static C2<uint32, bool, FooB> c2;

        c2.Run();
    }

expected:1

    const auto fn1 = Foo;

    inline void main()
    {
        static C2<bool, uint32, fn1> c2;
        c2.Run();
    }

expected:0
    
    const T_to_R_t<uint32, uint32> fn = T_to_R<uint32, uint32>; 

expected:0
    
    const T_to_R_t<uint32, int32> fn = T_to_R<uint32, uint31>; 

expected:0
    
    const T_to_R_t<uint32, int32> fn = T_to_R<int33, uint31>; 

expected:0
    
    const T_to_R_t<uint32, uint32> fn = T_to_R<uint32, int32>; 

expected:1
    
    class Main
    {
        template <auto N, typename T, typename R>
        [[max_threads(N)]]
        R max_threads_N_T_to_R(T x)
        {
           return x;
        }

        const max_threads_N_T_to_R_t<2, uint32, int32> fn = max_threads_N_T_to_R<2, int33, uint31>; 
    }

expected:0
    
    class Main
    {
        template <auto N, typename T, typename R>
        [[max_threads(N)]]
        R max_threads_N_T_to_R(T x)
        {
           return x;
        }

        const max_threads_N_T_to_R_t<1, uint32, int32> fn = max_threads_N_T_to_R<2, int33, uint31>; 
    }

expected:1
    
    class Main
    {
        template <auto N, typename T, typename R>
        [[max_threads(N)]]
        R max_threads_N_T_to_R(T x)
        {
           return x;
        }

        const T_to_R_t<uint32, int32> fn = max_threads_N_T_to_R<2, int33, uint31>; 
    }

expected:0
    
    const max_threads_N_T_to_R_t<1, uint32, int32> fn = T_to_R<int33, uint31>; 

expected:1

