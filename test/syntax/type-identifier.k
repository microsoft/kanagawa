// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        static Foo foo;
    }

    class Foo
    {}

expected:0

    inline void main()
    {
        static Foo foo;
    }

    struct Foo
    {}

expected:0

    inline void main()
    {
        static Foo_t foo;
    }

    using Foo_t = Foo;

    struct Foo
    {}

expected:0

    const auto x = bitsizeof Foo;

    static assert(x == 1);

    inline void main()
    {
        static Foo foo;
    }

    struct Foo
    {
        bool x;
    }

expected:0

    const auto x = bitsizeof Foo[10];

    static assert(x == 1);

    inline void main()
    {
        static Foo foo;
    }

    struct Foo
    {
        bool x;
    }

expected:1

    const auto x = bitsizeof Foo_t;

    static assert(x == 1);

    inline void main()
    {
        static Foo_t foo;
    }

    using Foo_t = Foo;

    struct Foo
    {
        bool x;
    }

expected:0

    const auto x = bitsizeof  typename Foo_t[10];

    static assert(x == 10);

    inline void main()
    {
        static Foo_t foo;
    }

    using Foo_t = Foo;

    struct Foo
    {
        bool x;
    }

expected:0

    using Foo_t = Foo;

    const auto x = bitsizeof  typename Foo_t[10];

    static assert(x == 10);

    inline void main()
    {
        static Foo_t foo;
    }

    struct Foo
    {
        bool x;
    }

expected:0

    class Foo
    {
    public:
        using X = uint32;
    }

    inline void main()
    {
        static D<Foo> c;
    }

expected:0

    enum E : uint32
    {
        One = 1
    }

    inline void main()
    {
        static C<E> c;
    }

expected:0

    template <typename T>
    inline T cast1(auto x)
    {
        return cast<T>(x);
    }

    template <typename T>
    class Foo
    {
        public:
            using X = T;
    }

    template <typename T>
    using u = Foo<T>;


    inline void main()
    {
        uint32 y = cast1<typename u<uint32>::X>(10);
    }

expected:0

    inline void main()
    {
        static Otter outer;
    }

expected:1

    struct Other{};

    uint32 otter;

    inline void main()
    {
        static Otter outer;
    }

expected:1

    inline void main()
    {
        static Otter::Inner inner;
    }

expected:1

    inline void main()
    {
        static Outer::Inne inner;
    }

expected:1

    inline void main()
    {
        static Outer::Inner::alis value;
    }

expected:1

    using Value = Outer::Inner::alias;
   
    inline void main()
    {
        static Value value;
        
        auto x = values;
    }

expected:1

    using Value = Outer::Inner::alias;
   
    inline void main()
    {
        static Value value;
        
        inline void valeus()
        {
        }

        auto x = values;
    }

expected:1

    inline void main()
    {
        static C<Otter> c;
    }

expected:1

    inline void main()
    {
        static C<typename Otter> c;
    }

expected:1

    inline void main()
    {
        static C<Outer::Inne> c;
    }

expected:1

    struct ali{};

    class Foo
    {
        public:
            using dahlia = uint32;

            inline void Run()
            {
                alia x;

                class Foo
                {
                    using alias = uint32;
                }
            }
    } 

expected:1

    struct ali{};

    class Foo
    {
        public:
            class Bar
            {
                public:
                    using dahlia = uint32;
            }

            inline void Run()
            {
                Bar::alia x;

                class Foo
                {
                    using alias = uint32;
                }
            }
    } 

expected:1

    template <typename T>
    class Foo
    {
    public:
        inline void Go()
        {
        }
    }

    inline void main()
    {
        static Foo<decltype(foo)> foo;
        
        foo.Go();
    }

expected:1
