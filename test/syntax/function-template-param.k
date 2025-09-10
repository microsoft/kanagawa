// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline bool uint3_to_bool(uint3 x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<uint3_to_bool> t1;
    }

expected:0

    template <auto N>
    inline bool uintN_to_bool(uint<N> x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<uintN_to_bool<3>> t1;
    }

expected:0

    const auto Y = 5 - bitsizeof(int2);

    inline bool uint3_to_bool(uint<Y> x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<uint3_to_bool> t1;
    }

expected:0

    inline bool uint3_to_bool(UInt3 x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<uint3_to_bool> t1;
    }

expected:0

    using unsigned3=UInt3;
    inline bool uint3_to_bool(unsigned3 x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<uint3_to_bool> t1;
    }

expected:0

    inline bool uint3_to_bool(uint<3> x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<uint3_to_bool> t1;
    }

expected:0

    inline void main()
    {
        static T1<T_to_R<bool, uint3, true>> t1;
    }

expected:0

    inline void main()
    {
        static T1<T_to_R<bool, uint2, true>> t1;
    }

expected:1

    inline void main()
    {
        static T3<bool, uint3, T_to_R<bool, uint3, true>> t1;
    }

expected:0

    inline void main()
    {
        static T3<bool, uint<3>, T_to_R<bool, uint3, true>> t1;
    }

expected:0

    inline void main()
    {
        static T3<bool, UInt3, T_to_R<bool, uint3, true>> t1;
    }

expected:0

    inline void main()
    {
        static T3<bool, UInt<3>, T_to_R<bool, uint3, true>> t1;
    }

expected:0

    inline void main()
    {
        static T3<bool, uint3, T_to_R<bool, UInt<3>, true>> t1;
    }

expected:0

    inline void main()
    {
        static T3<bool, uint3, T_to_R<bool, uint<3>, true>> t1;
    }

expected:0

    inline void main()
    {
        static T3<bool, uint3, T_to_R<bool, UInt3, true>> t1;
    }

expected:0

    using UINT2 = uint2;
    
    inline void main()
    {
        static T3<bool, UINT2, T_to_R<bool, uint3, true>> t1;
    }

expected:0

    using UINT2 = uint2;
    
    inline void main()
    {
        static T3<bool, UINT2, T_to_R<bool, uint1, true>> t1;
    }

expected:1

    inline bool Bar(int3 x, int2 y, int1 z)
    {
        return true;
    }

    inline void main()
    {
        static T2<Bar> t2;
    }

expected:0

    inline void main()
    {
        static T2<T_to_R<bool, int3, true>> t1;
    }

expected:1

    inline uint32 Do()
    {
        return Foo<T_to_R<uint32, uint32, 0>>(123);
    }

expected:0

    inline uint32 Do()
    {
        return Foo<T_to_R<int32, uint32, 0>>(123);
    }

expected:1

    class Bar
    {
    public:
        bool F(uint3 x)
        {
            return true;
        }

        T1<F> t1;
    }

expected:1

    struct Bar
    {
        uint32 x;
    }

    inline bool Do(Bar bar)
    {
        return bar.x == 3;
    }

    inline void main()
    {
        static T3<bool, Bar, Do> t3;
    }

expected:0

    template <typename T>
    struct Bar
    {
        T x;
    }

    template <typename T>
    inline bool Do(T bar)
    {
        return bar.x == 3;
    }

    inline void main()
    {
        static T3<bool, Bar<uint32>, Do<Bar<uint32>>> t3;
    }

expected:0

    template <typename T>
    struct Bar
    {
        T x;
    }

    template <typename T>
    inline bool Do(T bar)
    {
        return bar.x == 3;
    }

    inline void main()
    {
        static T3<bool, Bar<int32>, Do<Bar<uint32>>> t3;
    }

expected:1

    template <auto N, (uint<N-3>) -> uint<N-3> F>
    class C
    {
        using T = uint<N>;

        T Foo(T x)
        {
            return F(x);
        }
    }

    inline uint32 uint32_to_uint32(uint32 x)
    {
        return x;
    }

    inline void main()
    {
        static C<35, uint32_to_uint32> c;
    }

expected:0

    inline bool Bar(uint3 x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<Bar> t2;
    }

expected:0

    inline bool Bar(uint3 x)
    {
        return x == 1;
    }

    inline void main()
    {
        static T1<Bar> t2;
    }

expected:0

    inline void main()
    {
        S<uint32> x;
        From<S<uint32>, FromS<uint32>>(x);
    }

expected:0

    inline void main()
    {
        S<S<uint32>> x;
        From<S<S<uint32>>, FromS<S<uint32>>>(x);
    }

expected:0

    inline void main()
    {
        S<uint32> x;
        From<S<uint32>, FromS<S<uint32>>>(x);
    }

expected:1

    inline void main()
    {
        S<uint32> x;
        From<S<uint32>, FromS<uint33>>(x);
    }

expected:1

    inline S<uint32> Run()
    {
        return To<S<uint32>, ToS<uint32>>();
    }

expected:0

    inline S<S<uint32>> Run()
    {
        return To<S<S<uint32>>, ToS<S<uint32>>>();
    }

expected:0

    inline S<uint32> Run()
    {
        return To<S<uint32>, ToS<S<uint32>>>();
    }

expected:1

    inline S<uint32> Run()
    {
        return To<S<uint32>, ToS<uint33>>();
    }

expected:1

    template <typename T, (T) -> T Fn>
    class C
    {}

    inline uint32 Fn(uint32 x)
    {
        return x;
    }

    inline void main()
    {
        static C<uint32, Fn> c;
    }

expected:0
