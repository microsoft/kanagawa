// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    const A<uint32> x = {};

expected:0

    const A x = {};

expected:1

    const A<uint32>::A1_t x = 10;

expected:0

    const A<uint32>::A1_t x = false;

expected:1

    const A<uint32[2]>::A1_t x = {1, 2};

expected:0

    const A<uint32[2]>::A1_t x = {1, 2, 3};

expected:1

    const A<bool>::A2_t<2> x = {false, true};

expected:0

    const A<bool>::A2_t<2, 3> x = {false, true};

expected:1

    const B<bool>::A<uint32>::A1_t x = 0;

expected:0

    const B<uint32>::A<bool>::A1_t x = 0;

expected:1

    const B<bool>::A<uint32>::A2_t<2> x = {0, 1};

expected:0

    const B<bool>::A<uint32>::A2_t<2> x = {0, 1, 2};

expected:1

    const B<bool>::A<uint32>::A2_t<2> x = {false, true};

expected:1

    const B<bool>::A<uint32>::A3_t x = {false, 10};

expected:0

    const B<bool>::A<uint32>::A3_t x = {.x = false, .y = 10};

expected:0

    const B<bool>::A<uint32>::A3_t x = {10, false};

expected:1

    const B<bool>::A<uint32>::A3_t x = {.x = 10, .y = false};

expected:1

    const A<A<bool>::A1_t>::A1_t x = true;

expected:0

    const A<A<bool>::A2_t<2>>::A1_t x = {false, true};

expected:0

    const A<A<bool>::A2_t<2>>::A1_t x = {false, 10};

expected:1

    const B<bool>::A<B<bool>::A<uint32>::A3_t>::A1_t x = {true, 10};

expected:0

    const B<bool>::A<B<bool>::A<uint32>::A3_t>::A1_t x = {true, false};

expected:1

    import helper.template_instance_identifiers as N

    const N::B<bool>::A<N::B<bool>::A<uint32>::A3_t>::A1_t x = {true, 10};

expected:0

    import helper.template_instance_identifiers as N

    const N::B<bool>::A<N::B<bool>::A<uint32>::A3_t>::A1_t x = {.x = true, .y = 10};

expected:0

    import helper.template_instance_identifiers as N

    const B<bool>::A<N::B<bool>::A<uint32>::A3_t>::A1_t x = {true, false};

expected:1

    import helper.template_instance_identifiers as N

    const N::B<B<bool>::A<uint32>::A3_t>::A<N::B<float32>::A<uint5>::A2_t<2>>::A3_t x = {};

    inline Foo<Foo<bool, uint32>, uint5[2]> Do()
    {
        return x;
    }

expected:0

    import helper.template_instance_identifiers as N

    const N::B<B<bool>::A<uint32>::A3_t>::A<N::B<float32>::A<uint5>::A2_t<2>>::A3_t x = {};

    inline Foo<Foo<bool, uint32>, uint5[1]> Do()
    {
        return x;
    }

expected:1

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    inline N::A<uint32>::A1_t Do(B<float32>::A<bool>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do>();
    }

expected:0

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    inline N::A<uint3>::A1_t Do(B<float32>::A<bool>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do>();
    }

expected:0

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    inline N::A<uint33>::A1_t Do(B<float32>::A<bool>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do>();
    }

expected:1

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    inline N::A<uint32>::A1_t Do(B<float32>::A<uint1>::A1_t x)
    {
        return x == 0 ? 1 : 0;
    }

    inline void main()
    {
        Go<Do>();
    }

expected:1

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    template <typename T1, typename T2>
    inline N::A<T1>::A1_t Do(B<float32>::A<T2>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do<uint32, bool>>();
    }

expected:0

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    template <typename T1, typename T2>
    inline N::A<T1>::A1_t Do(B<float32>::A<T2>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do<uint32, uint4>>();
    }

expected:1

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    template <typename T1, typename T2>
    inline N::A<T1>::A1_t Do(B<float32>::A<T2>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do<A<uint32>::A1_t, bool>>();
    }

expected:0

    import helper.template_instance_identifiers as N

    template <(bool) -> uint32 Fn>
    inline void Go()
    {
    }

    template <typename T1, typename T2>
    inline N::A<T1>::A1_t Do(B<float32>::A<T2>::A1_t x)
    {
        return x ? 1 : 0;
    }

    inline void main()
    {
        Go<Do<uint32, A<uint4>::A1_t>>();
    }

expected:1

    using Bbool = B<bool>;
    const Bbool::A<uint32>::A3_t x = {false, 10};

expected:0

    using Bbool = B<bool>;
    const Bbool::A<uint32>::A3_t x = {.x = false, .y = 10};

expected:0

    using Bbool = B<bool>;
    const Bbool::A<uint32>::A3_t x = {.x = false, .y = -10};

expected:1

    using Bbool = B<bool>;
    const Bbool::A<uint32>::A3_t x = {10, 10};

expected:1

    using BboolAuint32 = B<bool>::A<uint32>;
    const BboolAuint32::A3_t x = {false, 10};

expected:0

    using BboolAuint32 = B<bool>::A<uint32>;
    const BboolAuint32::A3_t x = {10, 10};

expected:1
