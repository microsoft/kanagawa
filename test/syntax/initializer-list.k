// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // Initializer list used with global union
    inline void main()
    {
        static Bump g_bump = { 3 };
    }

expected:0

    // Initializer list used with local union
    inline void Main()
    {
        Bump b = { 3 };
    }

expected:0

    // Invalid initializer list used with array of memories
    inline void main()
    {
        static memory<uint32, 512>[4] g_mem = { 1 };
    }

expected:1

    import data.memory

    // Invalid initializer list used with memory of arrays
    inline void main()
    {
        static memory<uint32[4], 512> g_mem = { 1 };
    }

expected:1

    import data.memory

    // Initializer list used with array of memories
    inline void main()
    {
        static memory<uint32, 512>[4] g_mem = { {1}, {2}, {3}, {4} };
    }

expected:0

    import data.memory

    // Initializer list used with memory of arrays
    inline void main()
    {
        static memory<uint32[4], 512> g_mem = { {1, 2, 3, 4} };
    }

expected:0

    import data.memory

    // Initializer list used with array of memory of arrays
    inline void main()
    {
        static memory<uint32[4], 512>[2] g_mem = { { {1, 2, 3, 4} }, { {1, 2, 3, 4} } };
    }

expected:0

    // Initializer list used in assignment without declaration
    inline void Main()
    {
        uint32[4] x;

        x = { 1, 2, 3, 4 };
    }

expected:0

    // Type checking of initializer list elements (struct)
    struct S
    {
        uint32 x;
        uint32 y;
    }

    inline void Main()
    {
        S s = { 1, false };
    }

expected:1

    // Type checking of initializer list elements (array)
    inline void Main()
    {
        uint32[4] x = { 1, false };
    }

expected:1

    // Initializer list with too many elements (struct)
    struct S
    {
        uint32 x;
        uint32 y;
    }
    inline void Main()
    {
        S s = { 1, 2, 3 };
    }

expected:1

    // Initializer list with too many elements (array)
    inline void Main()
    {
        uint32[4] x = { 1, 2, 3, 4, 5 };
    }

expected:1

    // Non-empty initializer for int
    inline void Main()
    {
        uint32 x = { 3 };
    }

expected:1

    // Initializer list does not support type inference
    inline void Main()
    {
        auto x = { 3 };
    }

expected:1

    // Initializer list does not support type inference
    inline void Main()
    {
        auto x = {};
    }

expected:1

    inline uint32[4] Foo()
    {
        return {0, 1, 2, 3};
    }

expected:0

    inline uint32[4] Foo()
    {
        return {0, 1, 2, 3, 4};
    }

expected:1

    inline uint32[4] Foo()
    {
        return {false, true, false, true};
    }

expected:1

    inline bool[4] Foo()
    {
        return {false, true, false, true};
    }

expected:0

    inline bool[4] Foo()
    {
        bool a;
        bool b;
        bool c;
        return {a, b, a, true};
    }

expected:0

    inline void Foo(uint32[4] x)
    {
    }

    inline void Do()
    {
        Foo ({0, 1, 2, 3});
    }

expected:0

    inline void Foo(uint32[4] x)
    {
    }

    inline void Do()
    {
        uint32 a;
        uint32 b;
        uint32 c;
        Foo ({a, b, c, 3});
    }

expected:0

    inline void Foo(uint32[4] x)
    {
    }

    inline void Do()
    {
        Foo ({0, 1, 2, 3, 4});
    }

expected:1

    inline Bar Foo()
    {
        return {0, 1};
    }

    inline void Do()
    {
        Bar bar = Foo();
    }

expected:0

    inline Bar Foo()
    {
        return {0, 1};
    }

    inline void Do()
    {
        auto bar = Foo();
    }

expected:0

    inline Bar Foo(bool x)
    {
        return x ? {0, 1} : {3, 4};
    }

expected:0

    inline Bar Foo(bool x)
    {
        return x ? {true, 1} : {3, 4};
    }

expected:1

    inline Bar Foo(bool x)
    {
        uint32 a;
        uint32 b;
        return x ? {a, b} : {b, a};
    }

expected:0

    inline Bar Foo()
    {
        return {0, 1, 2};
    }

expected:1

    inline void Foo(Bar x)
    {
    }

    inline void Do()
    {
        Foo ({2, 3});
    }

expected:0

    inline void Foo(Bar x)
    {
    }

    inline void Do()
    {
        uint32 x;
        uint32 y;
        Foo ({x, y});
    }

expected:0

    inline void Foo(Bar x)
    {
    }

    inline void Do()
    {
        Foo ({2, 3, 4});
    }

expected:1

    inline void main()
    {
        {10, 20}
    }

expected:1

    inline void main()
    {
        {10;}
    }

expected:0

    inline void main()
    {
        Bar bar = {1, 2};
    }

expected:0

    inline void main()
    {
        Bar bar = {1, 2} + 4;
    }

expected:1

    inline void main()
    {
        Bar[3] x = {{0, 1}, {2, 3}, {4, 5}};
    }

expected:0

    inline void main()
    {
        Bar[3] x = {{false, 1}, {2, 3}, {4, 5}};
    }

expected:1

    inline void main()
    {
        Bar[3] x = {{0, 1}, {2, 3}, {4, false}};
    }

expected:1

    inline void main()
    {
        Bar[3] x = {{0, 1}, {2, 3}, {4, 5}, {0, 1}};
    }

expected:1

    inline void main()
    {
        uint32[2][3] x = {{1, 2, 3}, {4, 5, 6}};
    }

expected:0

    inline void main()
    {
        FnT<uint32[2][3]>({{1, 2, 3}, {4, 5, 6}});
    }

expected:0

    import data.array

    inline void main()
    {
        FnT<array<uint32[3], 2>>({{1, 2, 3}, {4, 5, 6}});
    }

expected:0

    inline void main()
    {
        uint32[2][3] x = {{false, 2, 3}, {4, 5, 6}};
    }

expected:1

    inline void main()
    {
        FnT<uint32[2][3]>({{false, 2, 3}, {4, 5, 6}});
    }

expected:1

    inline void main()
    {
        uint32[2][3] x = {{0, 1, 2}, {4, 5, 6}, {7, 8, 9}};
    }

expected:1

    inline void main()
    {
        FnT<uint32[2][3]>({{0, 1, 2}, {4, 5, 6}, {7, 8, 9}});
    }

expected:1

    inline void main()
    {
        uint32[2][3] x = {{0, 2, 3, 4}, {4, 5, 6}};
    }

expected:1

    template <auto N>
    struct Foo
    {
        uint32 x;
    }

    inline void main()
    {
        Foo<{0}> foo;
    }

expected:1

    inline void main()
    {
        Bux<bool, uint32> x = {true, 10};
    }

expected:0

    inline void main()
    {
        Bux<bool, uint32> x = {10, true};
    }

expected:1

    inline void main()
    {
        Bux<Bar, bool> x = { {0, 1}, false };
    }

expected:0

    inline Bux<Bar, bool> Do()
    {
        Bar bar;
        bool b;

        return {bar, b};
    }

expected:0

    inline void Do(Bux<Bar, bool> x)
    {
    }

    inline void Go()
    {
        Bar bar;
        bool b;

        Do({bar, b});
    }

expected:0

    inline void Do(Bux<Bar, bool> x)
    {
    }

    inline void Go()
    {
        Bar bar;
        bool b;

        Do({b, bar});
    }

expected:1

    inline void main()
    {
        Bux<Bar, bool> x = { {0, 1}, 10 };
    }

expected:1

    inline void main()
    {
        Bux<Bar, bool> x = { {false, 1}, false };
    }

expected:1

    inline void main()
    {
        Bux<Bar, bool> x = { {0, 1}, 10 , 20};
    }

expected:1

    inline void main()
    {
        Bux<Bar, bool> x = { {false, 1, 3}, false };
    }

expected:1

    inline void Do()
    {
        Bux<Bar, bool> x;
        
        x.x = {10, 10};
        x.y = true;
    }

expected:0

    inline void Do()
    {
        Bux<Bar, bool> x;
        
        uint32 i = 10;
        x.x = {i, 10};
        x.y = true;
    }

expected:0

    inline void Do()
    {
        Bux<Bar, bool> x;
        
        x.x = {false, 10};
    }

expected:14

    inline void Do()
    {
        Bux<Bar, bool> x;

        x.x = {20, 10, 10};
    }

expected:62

    inline void Do()
    {
        Bux<Bar, bool> x;
        
        auto i = 20;
        x.x = {i, 10, 10};
    }

expected:62

    template <Bar bar>
    class C
    {
    }

    inline void main()
    {
        const Bar bar = {13, 10};
        static C<{10, 12}> d1;
        static C<bar>      d2;
    }

expected:0

    inline void Foo(bool b)
    {}

    inline void main()
    {
        Foo({});
    }

expected:0

    inline void main()
    {
        for (const auto i : 10)
        {
        };
    }

expected:0

    inline void main()
    {
        {};
    }

expected:0

    inline void Run(bool c)
    {
        uint32 bar;

        auto x = c ? bar : {};
        static assert(bitsizeof x == bitsizeof bar);
    }

expected:0

    inline void Run(bool c)
    {
        bool bar;

        auto x = c ? bar : {};
        static assert(bitsizeof x == bitsizeof bar);
    }

expected:0

    inline void Run(bool c)
    {
        Bar bar;

        auto x = c ? bar : {};
        auto y = x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar bar;

        auto x = c ? {10} : bar;
        auto y = x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar bar;

        auto x = c ? bar : {10, 11};
        auto y = x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar bar;

        auto x = c ? bar : {10, false};
        auto y = x.y;
    }

expected:1

    inline void Run(bool c)
    {
        Bar bar;

        auto x = c ? {.x = 10, .y = 11} : bar;
        auto y = x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar bar;

        auto x = c ? bar : {.x = 10, .z = 11};
        auto y = x.y;
    }

expected:1

    inline void Run(bool c)
    {
        Bux<Bar, uint4> bux;

        auto x = c ? bux : {10, 11};
        auto y = x.x.y;
    }

expected:1

    inline void Run(bool c)
    {
        Bux<Bar, bool> bux;

        auto x = c ? bux : {{10, 11}};
        auto y = x.x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bux<Bar, uint4> bux;

        auto x = c ? {{10, 11}, false} : bux;
        auto y = x.x.y;
    }

expected:1

    inline void Run(bool c)
    {
        Bux<Bar, bool> bux;

        auto x = c ? {{10, 11}, false} : bux;
        auto y = x.x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bux<Bar, bool> bux;

        auto x = c ? bux : {{10, false}, false};
        auto y = x.x.y;
    }

expected:1

    inline void Run(bool c)
    {
        Bar[2] bar;

        auto x = c ? bar : {{10, 11}, {1, 2}};
        auto y = x[1].y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar[2] bar;

        auto x = c ? bar : {{.x = 10, .y = 11}};
        auto y = x[1].y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar[2] bar;

        auto x = c ? bar : {{10, 11}, {1, 2}, {2, 3}};
        auto y = x[1].y;
    }

expected:1

    inline void Run(uint2 c)
    {
        Bar bar;

        auto x = mux(c, {}, {10}, bar, {10, 11});
        auto y = x.y;
    }

expected:0

    inline void Run(uint2 c)
    {
        Bar bar;

        auto x = mux(c, {10, 11, 12}, {0}, bar, {10, 11});
        auto y = x.y;
    }

expected:1

    inline void Run(bool c)
    {
        Bar bar;

        Bar x = c ? {0, 1} : {10, 11};
        auto y = x.y;
    }

expected:0

    inline void Run(bool c)
    {
        Bar bar;

        Bar x = c ? {0, false} : {10, 11};
        auto y = x.y;
    }

expected:1
