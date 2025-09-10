// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // Auto-inlining of functions with 0 call sites
    inline void foo()
    {

    }

    inline void bar()
    {
        atomic
        {
            foo(); // no error here, because foo should be auto-inlined
        }
    }
expected:0

    // Function with 2 return statements
    inline uint32 foo()
    {
        return 3;
        return 5;
    }
expected:21

    inline void Run(bool x)
    {
        uint32 boo;

        boo();
    }

expected:1

    // Direct recursion (inline function)
    inline void foo()
    {
        foo();
    }

    class C
    {
    public:
        void entry()
        {
            foo();
        }
    };
    export C;

expected:37

    // Indirect recursion (inline functions)
    inline void foo()
    {
        bar();
    }

    inline void bar()
    {
        foo();
    }

    class C
    {
    public:
        void entry()
        {
            foo();
        }
    };
    export C;

expected:37
