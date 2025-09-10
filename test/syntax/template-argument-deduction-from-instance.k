// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline bool main()
    {
        auto x = First(MakeTuple(false, 10));

        return x;
    }

expected:0

    inline bool main()
    {
        auto x = First(MakePair(false, 10));

        return x;
    }

expected:0

    inline bool main()
    {
        auto x = First(Second(MakePair(MakePair(123, 100), MakeTuple(false, 10))));

        return x;
    }

expected:0

    inline bool main()
    {
        auto x = First(Second(MakeTuple(MakePair(123, 100), MakePair(false, 10))));

        return x;
    }

expected:0

    inline bool main()
    {
        auto x = First(Second(MakeDuo(MakePair(false, 100), MakePair(false, 101))));

        return x;
    }

expected:0

    inline void main()
    {
        Go(First<bool, uint32>);
    }

expected:0

    inline void main()
    {
        Go(Second<bool, uint32>);
    }

expected:1

    inline void main()
    {
        Go1(First<bool, uint4>, MakePair(false, 10));
    }

expected:0

    inline void main()
    {
        Go1(First<bool, uint4>, MakePair(false, 100));
    }

expected:1

    import type.stdtype

    template <auto N>
    inline void Foo(index_t<N> i)
    {
    }

    inline void main()
    {
        index_t<10> i;

        Foo(i);
    }

expected:0

    template <typename T, auto N>
    inline auto Foo(Tuple2<bool, T>[N] a)
    {
        static assert(N == 3);
        return Second(a[0]);
    }

    inline float32 Run()
    {
        Pair<bool, float32>[3] a = {MakePair(true, 3.14)};

        return Foo(a);
    }

expected:0
    
    import data.optional

    template <typename T = uint1>
    inline void Foo(optional<T> x)
    {
    }

    inline void main()
    {
        Foo(make_optional(true, 100));

        Foo(just(9));

        Foo({});
    }

expected:0
