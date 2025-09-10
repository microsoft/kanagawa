// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

expected:0

    class Foo
    {
    public:
        void Do();

    private:
        () -> void callback;
    }

    extern Foo;

    inline void Go()
    {
    }

    inline void main()
    {
        static Foo foo = {.callback = Go};
        
        foo.Do();
    }

expected:0

    class Foo
    {
    public:
        void Do()
        {
        }

    private:
        () -> void callback;
    }

    extern Foo;

    inline void Go()
    {
    }

    inline void main()
    {
        static Foo foo = {.callback = Go};
        
        foo.Do();
    }

expected:0

    class Foo
    {
    public:
        void Do();

    public:
        () -> void callback;
    }

    extern Foo;

expected:1

    class Foo
    {
    public:
        void Do();

    public:
        uint32 x;
    }

    extern Foo;

expected:1

    class Foo
    {
    public:
        inline void Do();
    }

    extern Foo;

expected:1

    class Bar
    {
    public:
        () -> void callback;
    }

    class Foo
    {
    public:
        void Do();

        Bar[2] bars;
    }

    extern Foo;

expected:1

    import helper.export_class

    extern PublicMethodTemplate;

expected:0

    import helper.export_class

    extern PublicInlineMethod;

expected:1

    import helper.export_class

    extern PublicCallback;

expected:1

    import helper.export_class

    extern PublicData;

expected:1

    import helper.export_class

    extern PublicHOF;

expected:0

    class Bar
    {
    private:
        () -> void callback;
    }

    class Foo
    {
    public:
        void Do();

        Bar[2] bars;
    }

    extern Foo;

expected:1

    inline void main()
    {
        class Foo
        {
        public:
            void Do();

        public:
            () -> void callback;
        }

        extern Foo;
    }

expected:1

    class Foo
    {
    public:
        inline void Do();
    }

expected:1

    class Foo
    {
    public:
        void Do(auto x);
    }

expected:0

    class Foo
    {
    public:
        auto Do();
    }

expected:1

    class Foo
    {
    public:
        template <typename T>
        void Do();
    }

expected:0

   void Do();

expected:1

    extern Class;

    inline void Go() { }

    inline void main()
    {
        static Class obj = {.callback = Go };
    }

expected:0

    [[name("MyClass")]]
    extern Class;

    inline void Go() { }

    inline void main()
    {
        static Class obj = {.callback = Go };
    }

expected:0

    extern Class_T;

    class Foo
    {
        void Go(uint32 x)
        {
        }

        Class_T<uint32> obj = {.callback = Go};
    }

    export Foo;

expected:79

    extern Class_T<uint32>;

    class Foo
    {
        void Go(uint32 x)
        {
        }

        Class_T<uint32> obj = {.callback = Go};
    }

    export Foo;

expected:79

    extern Class_N;

    class Foo
    {
        void Go(uint10 x)
        {
        }

        Class_N<10> obj = {.callback = Go};
    }

    export Foo;

expected:0

    extern Class_N<10>;

    class Foo
    {
        void Go(uint10 x)
        {
        }

        Class_N<10> obj = {.callback = Go};
    }

    export Foo;

expected:0

    extern Class_N<10>;

    class Foo
    {
        void Go(int10 x)
        {
        }

        Class_N<10> obj = {.callback = Go};
    }

    export Foo;

expected:1

    template <typename T>
    class Foo
    {
    public:
        void Do();

    private:
        (T x) -> void callback;
    }

    extern Foo<uint32>;

    class Main
    {
        void Go(uint32 x)
        {
        }

        Foo<uint32> foo = {.callback = Go};

        inline void main()
        {
            foo.Do();
        }
    }

    export Main;

expected:79

    template <typename T>
    class Foo
    {
    public:
        void Do()
        {
            bar.Go();
        }

    private:
        class Bar
        {
        public:
            T Go(T x)
            {
                return x;
            }
        }

        extern Bar;

        Bar bar;
    }

expected:0

    inline void main()
    {
        class Helper
        {
            public:
                void Go()
                {
                }
        }

        extern Helper;

        static Helper helper;
    }

expected:0

    struct S{};

    extern S;

expected:1

    union U{bool x};

    extern U;

expected:1

    enum E{};

    extern E;

expected:1

    // Extern class callback connected to an inline function
    class Foo
    {
    public:
        void Do();

    private:
        () -> void callback;
    }

    extern Foo;

    class Main
    {
        inline void Go()
        {
        }

        Foo foo = {.callback = Go};

        inline void main()
        {
            foo.Do();
        }
    }

    export Main;
    
expected:65

    // Function that is connected to an export class callback
    // and has other call sites
    class C
    {
    private:
        ()->void cb;

    public:
        void foo()
        {
            cb();
        }
    }

    extern C;

    class Main
    {
        C _c = {.cb = bar};
        void bar()
        {
        }

    public:
        void main()
        {
            _c.foo();

            bar();
        }
    }

    export Main;

expected:75

    // Function that is connected to multiple an export class callbacks
    class C
    {
    private:
        ()->void cb;

    public:
        void foo()
        {
            cb();
        }
    }

    extern C;

    class Main
    {
        C _c1 = {.cb = bar};
        C _c2 = {.cb = bar};

        void bar()
        {
        }
     public:
        void main()
        {
            _c1.foo();
            _c2.foo();
        }
    }
    export Main;

expected:75

    class A
    {
    private:
        ()->void cb;

    public:
        void entry()
        {
            cb();
        }
    }

    extern A;

    class B
    {
    private:
        void foo()
        {
        }

        A _a = {.cb = foo};

    public:
        void entry()
        {
            _a.entry();
        }
    }

    export B;

expected:0

    class Foo
    {
    public:
        template <typename T>
        void Do(T x);
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;

        foo.Do(10);
    }

expected:0

    class Foo
    {
    public:
        void Do(auto x);
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;

        foo.Do(10);
    }

expected:0

    class Foo
    {
    public:
        template <typename T>
        T Do();
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;

        uint32 x = foo.Do();
    }

expected:0

    class Foo
    {
    public:
        template <typename T>
        void Do(T x);
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;

        foo.Do<uint32>(10);
    }

expected:0

    class Foo
    {
    public:
        template <auto N>
        [[latency(N)]] void Do();
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;

        foo.Do<3>();
    }

expected:0

    import helper.invalid_extern_class

expected:21

    import helper.invalid_extern_class

    extern Invalid;

expected:0

    class InvalidWithTemplate
    {
    public:    
        template <typename T>
        T Do(T x)
        {
        }
    }

    extern InvalidWithTemplate;

    inline void main()
    {
        static InvalidWithTemplate foo;
        
        foo.Do(10);
    }

expected:0

    import helper.invalid_extern_class

    static if (true)
    {
        extern Invalid;
    }

expected:0

    import helper.invalid_extern_class

    static if (true) extern Invalid;

expected:0

    import helper.invalid_extern_class

    static if (false) extern Invalid;

expected:21

    class InvalidWithTemplate
    {
    public:    
        template <typename T>
        T Do(T x)
        {
        }
    }

    static if (false) extern InvalidWithTemplate;

    inline void main()
    {
        static InvalidWithTemplate foo;
        
        foo.Do(10);
    }

expected:21

    import helper.invalid_extern_class

    template <typename T>
    class Foo
    {
        extern Invalid;
    }

expected:21

    import helper.invalid_extern_class

    template <typename T>
    struct Foo
    {
        T x;
    }

    static if (bitsizeof Foo<uint32> == 32) extern Invalid;

expected:21

    template <auto N>
    class InvalidTemplate
    {
    public:    
        uint32 Do(uint32 x)
        {
        }
    }

    inline void main()
    {
        static InvalidTemplate<32> foo;
    }

expected:21

    template <auto N>
    class InvalidTemplate
    {
    public:    
        uint32 Do(uint32 x)
        {
        }
    }

    extern InvalidTemplate<32>;

expected:21

    template <auto N>
    class InvalidTemplate
    {
    public:    
        uint32 Do(uint32 x)
        {
        }
    }

    extern InvalidTemplate;

    inline void main()
    {
        static InvalidTemplate<32> foo;
    }

expected:21

    import data.closure

    inline void Go(() -> void Do)
    {
        Do();
    }

    class Foo
    {
    public:    
        void Do()
        {
        }

        noinline uint32 Test(uint32 x)
        {
            Go(Do);
            return 0;
        }
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;
        foo.Test(10);
    }  

expected:0

    import data.closure

    inline void Go(() -> void Do)
    {
        Do();
    }

    class Foo
    {
    public:    
        void Do()
        {
        }

        uint32 Test(uint32 x)
        {
            return TestImpl(x);
        }

    private:    
        inline uint32 TestImpl(uint32 x)
        {
            Go(Do);
            return 0;
        }
    }

    extern Foo;

    inline void main()
    {
        static Foo foo;
        foo.Test(10);
    }  

expected:0
