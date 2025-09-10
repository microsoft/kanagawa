// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void Foo(uint32 x)
    {
    }

    inline void main()
    {
        int32 x;
        Foo(x);
    }

warning:conversion

    inline void Foo(uint32[4] x)
    {
    }

    inline void main()
    {
        int32 x;
        Foo({x, 1, 2});
    }

warning:conversion

    struct S
    {
        int32 x;
        bool y;
    }

    inline void Foo(S x)
    {
    }

    inline void main()
    {
        uint32 x;
        Foo({x, false});
    }

warning:conversion

    struct S
    {
        int32 x;
        bool y;
    }

    inline void Foo(S x)
    {
    }

    inline void main()
    {
        int32 x;
        Foo({x, false});
    }

warning:0

    enum E : uint3
    {
        X = 0b111
    }

    inline void Foo(uint2 x)
    {
    }

    inline void main()
    {
        Foo(E::X);
    }

warning:conversion

    enum E : uint2
    {
        X = 0b11
    }

    inline void Foo(uint2 x)
    {
    }

    inline void main()
    {
        Foo(E::X);
    }

warning:0

    enum E : uint2
    {
        X = 0b11
    }

    struct S
    {
        int2 x;
        bool y;
    }

    inline void Foo(S x)
    {
    }

    inline void main()
    {
        Foo({E::X, false});
    }

warning:conversion

    inline void main()
    {   
        uint32 x = 10;
        uint32 y = 2;

        uint8 z = x + y;
    }  

warning:conversion

    const uint32 x = 10;
    const uint32 y = 2;

    inline void main()
    {   
        uint8 z = x + y;
    }  

warning:0

    const auto x = 10;
    const auto y = 2;

    inline void main()
    {   
        uint8 z = x + y;
    }  

warning:0

    inline void main()
    {
        auto x = -101 % 5;
    }
    
warning:0

    inline void main()
    {
        auto x = -101 % 5;
    }

warning:0

    inline void main()
    {
        auto x = -101 / 5;
    }

warning:0

    inline void main()
    {
        auto x = -101 / 5;
    }

warning:0

    enum Foo : uint16
    {
        One = 1
    }

    inline void main()
    {
        uint3 x = Foo::One;
    }

warning:0

    enum Foo : uint16
    {
        One = 1
    }

    inline void Do(uint3 x)
    {
    }

    inline void main()
    {
        Do(Foo::One);
    }

warning:0

    enum Foo : uint16
    {
        One = 1
    }

    inline void main()
    {
        auto x = Foo::One;

        uint3 y = x;
    }

warning:conversion

    enum Foo : uint16
    {
        One = 1
    }

    inline void Do(uint3 x)
    {
    }

    inline void main()
    {
        auto x = Foo::One;
        Do(x);
    }

warning:conversion

    enum Foo : uint16
    {
        One = 1
    }

    template <auto N>
    class Bar
    {
        const uint3 x = Foo::One;
    }

warning:0

// Argument conversion
inline uint32 foo(uint32 x, uint4[8] y)
{
    return x;
}

inline uint32 main()
{
    int8 x;
    uint4[8] y;
    return foo(x, y);
}
warning:conversion
