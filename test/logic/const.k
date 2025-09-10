// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Using a parameter of an inline function to index into an array of objects
inline void InlineParamObjectIndex(unit::tag_t tag)
{
    class C
    {
    public:
        uint32 Foo(uint32 x)
        {
            return x + 1;
        }
    }

    inline uint32 Helper(uint32 x)
    {
        // Test constexpr evaluation at IR generation time
        static_assert(x == 1);

        // Track constexpr evaluation at function instance enumeration time
        static C[2] _objs;
        return _objs[x].Foo(3);
    }

    unit::assert_equal(tag, 4, Helper(1));
}

// Parameter of an inline function is modified by that function
inline void InlineParamModified(unit::tag_t tag)
{
    inline uint32 Helper(uint32 idx)
    {
        const uint32[4] a = { 1, 2, 3, 4 };

        idx++;

        return a[idx];
    }

    unit::assert_equal(tag, 3, Helper(1));
}

inline void test_main()
{
    unit::test<1>(InlineParamObjectIndex);
    unit::test<2>(InlineParamModified);
}
