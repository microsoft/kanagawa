// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        static for (const auto x : 8)
        {
            BinaryOpsLiteral<x - 4>();
        }
    }

warning:0

    inline void main()
    {
        BinaryOpsLiteral<32>();
    }

warning:0

    inline void main()
    {
        BinaryOpsLiteral<33>();
    }

warning:0

    inline void main()
    {
        BinaryOpsLiteral<64>();
    }

warning:0

    inline void main()
    {
        BinaryOpsLiteral<-16>();
    }

warning:0

    inline void main()
    {
        BinaryOpsLiteral<-17>();
    }

warning:0

    inline void main()
    {
        BinaryOpsLiteral<-32>();
    }

warning:0

    inline void main()
    {
        const auto x = 10;
        const auto y = -20;

        const uint32 a = 10;
        const int16 b = -20;

        const auto r = a;
        const auto s = b;

        const auto u = x;
        const auto v = y;

        static assert(decltype(x * 1) == uint4);
        static assert(decltype(y * 1) == int6);

        static assert(decltype(a * 1) == uint32);
        static assert(decltype(b * 1) == int16);

        static assert(decltype(r * 1) == uint32);
        static assert(decltype(s * 1) == int16);

        static assert(decltype(u * 1) == uint4);
        static assert(decltype(v * 1) == int6);
    }

warning:0

    inline void main()
    {
        const auto x = 10;
        const auto y = -20;

        const uint32 a = 10;
        const int16 b = -20;

        {
            const auto o = 10 * (-20);
            const auto p = 10 * y;
            const auto r = x * y;
            const auto s = a * b;
            const auto t = b * a;
            const auto u = b * x;
            const auto v = b * 10;

            static assert(decltype(o) == decltype(-200));
            static assert(decltype(p) == decltype(-200));
            static assert(decltype(r) == decltype(-200));
            static assert(decltype(s) == int48);
            static assert(decltype(s) == decltype(t));
            static assert(decltype(u) == int20);
            static assert(decltype(u) == decltype(v));
        }
    }

warning:0

    inline void main()
    {
        const auto x = ELEVEN * 2;

        static assert(decltype(x) == decltype(22));
    }

warning:0

    inline void main()
    {
        const auto x = (0 + Num::Four) + ((Num::Seven * 1) & Num::Three);

        static assert(x == 7);
        static assert(decltype(x) == decltype(7));
    }

warning:0

    inline void main()
    {
        uint1 one = 1;
        const auto x = Num::Four + ((Num::Seven * one) & Num::Three);

        static assert(decltype(x) == uint5);
    }

warning:0
