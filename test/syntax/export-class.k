// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    class C
    {
    }

    export C;

    class D
    {
        C _obj;
    }

     export D;

expected:0

    template <typename T>
    class Foo
    {
    }

    export Foo<uint32>;

expected:0

    inline void main()
    {
        using Int = int32;

        class Foo
        {
        public:
            void Bar()
            {
            }
        }

        export Foo;
        export Int;

        static Foo foo;
    }

expected:0

    using Int = int3;
    export Int;

    class Foo
    {
    public:
        using Int = int32;
        export Int;
    }

expected:0

    export Exportable;

expected:0

    export PublicMethodTemplate;

expected:0

    class NotExportable
    {
    public:
        PublicMethodTemplate field;
    }

    export NotExportable;

expected:1

    export PublicInlineMethod;

expected:1

    export PublicCallback;

expected:1

    export PublicData;

expected:1

    export PublicHOF;

expected:0

    export PrivateMethodTemplate;

expected:0

    export PrivateInlineMethod;

expected:0

    export PrivateCallback;

expected:0

    export PrivateData;

expected:0

    export PrivateHOF;

expected:0

    export PublicTypes;

expected:0

    class Foo
    {
    public:
        PublicMethodTemplate[2] x;
    }

    class Bar
    {
    public:
        Foo[2] foo;
    }

    export Bar;

expected:1

    class Foo
    {
    public:
        PublicInlineMethod x;
    }

    class Bar
    {
    public:
        Foo[2] foo;
    }

    export Bar;

expected:1

    inline void main()
    {
        class Foo
        {
        public:
            PublicInlineMethod x;
        }

        class Bar
        {
        public:
            Foo[2] foo;
        }

        export Bar;
    }

expected:1

    class Foo
    {
    public:
        PublicCallback x;
    }

    class Bar
    {
    public:
        Foo[2] foo;
    }

    export Bar;

expected:1

    class Foo
    {
    public:
        PublicData x;
    }

    class Bar
    {
    public:
        Foo foo;
    }

    export Bar;

expected:1

    class Foo
    {
    }

    class Bar
    {
    private:
        Foo foo;
    }

    export Bar;

expected:0

    class Foo
    {
    private:
        PublicData x;
    }

    class Bar
    {
    public:
        Foo foo;
    }

    export Bar;

expected:1

    template <typename T>
    class Foo
    {
        export Foo;
    }

    template <typename T>
    class Bar
    {
        Foo<uint32> foo;
        Foo<T> foo1;
    }

    inline void main()
    {
        static Bar<bool> bar;
    }

expected:0

    template <typename T>
    class Foo
    {
    }

    template <typename T>
    class Bar
    {
        export Foo;
        Foo<uint32> foo;
        Foo<T> foo1;
    }

    inline void main()
    {
        static Bar<bool> bar;
    }

expected:1

    template <typename T>
    class Foo
    {
        public:
            uint32 x;
            export Foo;
    }

    using Foo32 = Foo<uint32>;

expected:1

    class Foo
    {
    public:
        template <typename T>
        T Go(T x)
        {
          return x;
        }

    private:
        void noop()
        {
          const auto x = Go<uint32>;
          const auto y = Go<bool>;
        }
    }


    export Foo;

expected:0
