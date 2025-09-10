// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // unordered and reorder_by_looping modifiers
    inline void F()
    {
        [[unordered, reorder_by_looping]]
        for (const uint32 x : 34)
        {
            println(x);
        }
    }
expected:25
    
    // range-for, modify induction variable
    inline void F()
    {
        for (const uint32 x : 34)
        {
            x = 4;
        }
    }
expected:1
    
    // range-for, induction variable is not const
    inline void F()
    {
        for (uint32 x : 34)
        {

        }
    }
expected:1
    
    // range-for, induction variable is not an integer
    inline void F()
    {
        struct S { uint32 y; }

        for (const S s : 10)
        {
        }
    }
expected:16

    // range-for, bound is not an integer
    inline void F()
    {
        struct S { uint32 y; }

        S s;

        for (const auto x : s)
        {
        }
    }
expected:16

    // range-for, induction variable is signed
    inline void F()
    {
        for (const int32 x : 10)
        {
        }
    }
expected:16

    // range-for, bound is signed
    inline void F()
    {
        int32 bound = 4;

        for (const uint32 x : bound)
        {
        }
    }
expected:16

    class Main
    {
    public:
        // range-for, unordered
        [[async]] void F()
        {
            [[unordered]] for (const auto i : 10)
            {
                println(i);
            }
        }
    }
    
    export Main;

expected:0

    class Main
    {
    public:
        [[async]] void F()
        {
            [[unordered]] for (const uint4 i : 10)
            {
                println(i);
            }
        }
    }
    
    export Main;

expected:0

    class Main
    {
    public:
        [[async]] void F()
        {
            [[unordered]] do
            {
            }
            while (true);
        }
    }
    
    export Main;

expected:0

    inline void F()
    {
        [[fifo_depth(128)]]
        for (const uint32 x : 34)
        {
            println(x);
        }
    }

expected:0

    // more than one fifo_depth
    inline void F()
    {
        [[fifo_depth(128), fifo_depth(128)]]
        for (const uint32 x : 34)
        {
            println(x);
        }
    }
expected:65

    inline void F()
    {
        uint32 x = 0;
        [[reorder_by_looping]]
        do
        {
            x++;
        } while (x < 32);
    }
expected:0

    inline void F()
    {
        [[reorder_by_looping]]
        for (const auto i : 32)
        {
            println(i);
        }
    }
expected:65

    inline void F()
    {
        uint32 x = 0;
        // assume_ordered has been removed and replaced with reorder_by_looping
        [[assume_ordered]]
        do
        {
            x++;
        } while (x < 32);
    }
expected:1
