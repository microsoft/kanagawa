// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

struct simple_struct
{
    uint5 _a;
    int17 _b;
};

template <typename T, auto N>
union simple_union
{
    T _a;
    int<N> _b;
};

union union_type
{
    uint7 _primitive;
    uint3[3] _array;
    simple_struct _struct;
    simple_union<uint5, 7> _union;
};

struct struct_with_union
{
    uint2 _front;
    union_type _union;
    uint3 _back;
};

inline union_type construct_union_p(uint7 p)
{
    union_type u;
    u._primitive = p;
    return u;
}

inline union_type construct_union_a(uint3 a, uint3 b, uint3 c)
{
    union_type u;
    u._array[0] = a;
    u._array[1] = b;
    u._array[2] = c;
    return u;
}

inline union_type construct_union_s(uint5 a, int17 b)
{
    union_type u;
    u._struct._a = a;
    u._struct._b = b;
    return u;
}

inline void assert_primitive(union_type u, uint7 p, unit::tag_t tag)
{
    unit::assert(tag, u._primitive == p);
}

inline void assert_array(union_type u, uint3 a, uint3 b, uint3 c, unit::tag_t tag)
{
    unit::assert(tag, u._array[0] == a);
    unit::assert(tag, u._array[1] == b);
    unit::assert(tag, u._array[2] == c);
}

inline void assert_struct(union_type u, uint5 a, int17 b, unit::tag_t tag)
{
    unit::assert(tag, u._struct._a == a);
    unit::assert(tag, u._struct._b == b);
}


class Test737
{
private:
    union_type g_union;
    [[memory]] union_type[2] m_union;

public:
    void run(unit::tag_t tag)
    {
        static assert(bitsizeof(union_type) == bitsizeof(simple_struct));

        // Data stored in a union could be read back.
        // Bits not belonging to the stored data are NOT guaranteed to be intact.
        atomic {
            g_union._primitive = 1;
        }
        assert_primitive(g_union, 1, tag);
        atomic {
            g_union._array[1] = 3;
            unit::assert(tag, g_union._array[1] == 3);
        }
        atomic {
            g_union._struct._a = 5;
            unit::assert(tag, g_union._struct._a == 5);
        }
        atomic {
            g_union._union._a = 7;
            unit::assert(tag, g_union._union._a == 7);
        }

        union_type local_union;
        local_union._primitive = 1;
        assert_primitive(local_union, 1, tag);
        local_union = construct_union_a(2, 3, 4);
        assert_array(local_union, 2, 3, 4, tag);
        local_union = construct_union_s(4, 5);
        assert_struct(local_union, 4, 5, tag);
        local_union._union._a = 0;
        local_union._union._b = -1;
        unit::assert(tag, local_union._union._a != 0);

        // modifying an union in a struct shouldn't affect other member fields.
        struct_with_union s;
        s._front = 0;
        s._back = 0;
        s._union = construct_union_s(0x1F, -1);
        unit::assert(tag, s._front == 0);
        assert_struct(s._union, 0x1F, -1, tag);
        unit::assert(tag, s._back == 0);

        s._front = 3;
        s._back = 7;
        s._union = construct_union_p(0);
        unit::assert(tag, s._front == 3);
        assert_primitive(s._union, 0, tag);
        unit::assert(tag, s._back == 7);

        // Elements of an array of unions should be independent.
        union_type[2] array;
        array[0] = construct_union_p(1);
        array[1] = construct_union_a(2, 3, 4);
        assert_primitive(array[0], 1, tag);
        assert_array(array[1], 2, 3, 4, tag);
        array[0] = construct_union_s(5, 6);
        assert_struct(array[0], 5, 6, tag);
        assert_array(array[1], 2, 3, 4, tag);
        array[1]._union._b = 7;
        assert_struct(array[0], 5, 6, tag);
        unit::assert(tag, array[1]._union._b == 7);

        // Unions is allowed in memories
        for (const auto i: 2)
        {
            union_type value;
            if (i % 2 == 0)
                value = construct_union_p(3);
            else
                value = construct_union_a(2, 1, 0);
            m_union[i] = value;
        }
        assert_primitive(m_union[0], 3, tag);
        assert_array(m_union[1], 2, 1, 0, tag);
    }
}

template<typename T>
struct S738
{
    uint32 x;
    T t;
}

inline S738<uint16> Helper738(S738<uint32> src)
{
    S738<uint16> result;

    result.x = src.x + 1;
    result.t = src.t - 1;

    return result;
}

inline void Test738(unit::tag_t tag)
{
    S738<uint32> a;

    a.x = 4;
    a.t = 7;

    S738<uint16> b = Helper738(a);

    unit::assert(tag, b.x == 5);
    unit::assert(tag, b.t == 6);
}

inline void test_main()
{
    unit::test<737>(unit::fixture<Test737>());
    unit::test<738>(Test738);
}
