// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // String stored in a union (automatic reference counting not supported)
    union U
    {
        int32 i;
        string s;
    }
expected:53

    // String stored in a memory (automatic reference counting not supported)
    class C
    {
    private:
        [[memory]] string[32] _mem;
    }
expected:53

    // Shared variable initialized to a non-empty string
    class C
    {
    private:
        string s = "test";
    }
expected:53

    // String as parameter of public method of an exported class
    class C
    {
    public:
        void foo(string str)
        {
        }
    }

    export C;
expected:53

    // String as parameter of private method of an exported class
    class C
    {
    private:
        void foo(string str)
        {
        }
    }

    export C;
expected:0

    // String as return value of public method of an exported class
    class C
    {
    public:
        string foo()
        {
            return "test";
        }
    }

    export C;
expected:53

    // String as return value of private method of an exported class
    class C
    {
    private:
        string foo()
        {
            return "test";
        }
    }

    export C;
expected:0

    // String as parameter of callback contained in an exported class
    class C
    {
    private:
        (string)->void cb;
    }

    export C;
expected:53

    // String return type on callback contained in an exported class
    class C
    {
    private:
        ()->string cb;
    }

    export C;
expected:53

    // array of strings
    inline void Helper(const string[4] s)
    {
        println(s[1]);
    }
expected:0

    // string in struct
    struct D
    {
        string s;
    }
expected:0

    // string local variable
    inline void Main()
    {
        string s;
    }
expected:0

    // string passed to non inline function
    inline void Foo(const string s)
    {
        println(s);
    }

    inline void Main()
    {
        Foo("blake");
    }
expected:0

    // string passed to non inline function
    inline void Foo(string s)
    {
        println(s);
    }

    inline void Main()
    {
        Foo("blake");
    }
expected:0

    // string parameter modified
    inline void Foo(string s)
    {
        s = "changed";

        println(s);
    }

    inline void Main()
    {
        Foo("blake");
    }
expected:0

    // Ternary operator with strings
    inline void Helper544(bool b)
    {
        println(b ? "true" : "false");
    }

    inline void main()
    {
        Helper544(true);
        Helper544(false);
    }
expected:0

    inline auto foo(){return "string";}
expected:0

    inline void foo(auto str)
    {
    }

    inline void bar()
    {
        foo("test");
    }
expected:0

    template <typename T> inline void foo(T x){}

    inline void bar()
    {
        foo("bus");
    }
expected:0

    const auto x = "";
expected:0

    inline void foo(uint1 i)
    {
        auto x = mux(i, "abc", "def");
    }
expected:0

    static assert (str_foo == also_str_foo);

expected:0

    static assert (str_foo != str_Foo);

expected:0

    static assert (str_foo == "foo");

expected:0

    static assert ("foo" == also_str_foo);

expected:0

    static assert ("foo" != also_str_foo);

expected:1

    static if (str_foo == "foo")
    {
        static assert(true);
    }
    else
    {
        static assert(false);
    }

expected:0

    inline void main()
    {
        const auto str = str_foo;

        static if (str == str_foo)
        {
            const string s = str_Foo;
        }

        static assert (s == str_Foo);
    }

expected:0

    inline void fn1()
    {
    }

    inline void fn2()
    {
    }

    class Foo
    {
        () -> void callback;
    }

    inline void main()
    {
        static Foo foo = { .callback = str_foo == "foo" ? fn1 : fn2 };
    }

expected:0
