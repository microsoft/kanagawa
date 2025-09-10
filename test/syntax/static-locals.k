// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // exported method form within a static local ojbect
    class F
    {
    public:
        uint32 a()
        {
            return 3;
        }
    }

    class Main
    {
    public:
        void main()
        {
            static F f;
        }
    }

    export Main;

expected:63

    // static local object within a method
    class F
    {
    public:
        inline void run()
        {
            static Foo f;
        }
    }

expected:63

    // static local in a lambda
    inline void F()
    {
        const auto lamb = []()
        {
            static Foo f;
        };
    }

expected:0

    // static local in a local function
    inline void F()
    {
        inline void G()
        {
            static Foo f;
        }

        inline void H()
        {
            static Foo f;
        }
    }

expected:0

    // static local in higher order function
    inline void G()
    {
        const auto lamb = []()
        {
            return cast<uint32>(34);
        };

        F(lamb);
    }

    inline void F(()->uint32 fn)
    {
        static Foo f;
    }

expected:0

    // static local of type specified as template parameter
    template<typename T>
    inline void F()
    {
        static T t;
    }

    inline void G()
    {
        F<Foo>();
    }

expected:0
