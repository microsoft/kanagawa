// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Array of structures, writing to 1 structure element should only 
// update that element, not a read-modify-write of the entire struct
class ArrayOfStructMemberUpdateStaticHelper
{
public:
    struct element_t
    {
        uint32 x;
        uint32 y;
    }

    element_t[1] _aos = {};

    void Test(unit::tag_t tag)
    {
        pipelined_for(128, [tag](index_t<128> i)
        {
            atomic
            {
                _aos[0].x++;
            }

            barrier;

            atomic
            {
                _aos[0].y++;
            }
        });

        unit::assert(tag, _aos[0].x == 128);
        unit::assert(tag, _aos[0].y == 128);
    }
}

inline void ArrayOfStructStaticMemberUpdateTest(unit::tag_t tag)
{
    static ArrayOfStructMemberUpdateStaticHelper helper;
    helper.Test(tag);
}

// Array of structures, writing to 1 structure element should only 
// update that element, not a read-modify-write of the entire struct
class ArrayOfStructMemberUpdateDynamicHelper
{
public:
    struct element_t
    {
        uint32 x;
        uint32 y;
    }

    element_t[2] _aos = {};

    void Test(unit::tag_t tag)
    {
        pipelined_for(128, [](index_t<128> i)
        {
            index_t<2> index = i;

            atomic
            {
                _aos[index].x++;
            }

            barrier;

            atomic
            {
                _aos[index].y++;
            }
        });

        unit::assert(tag, _aos[0].x == 64);
        unit::assert(tag, _aos[0].y == 64);

        unit::assert(tag, _aos[1].x == 64);
        unit::assert(tag, _aos[1].y == 64);
    }
}

inline void ArrayOfStructDynamicMemberUpdateTest(unit::tag_t tag)
{
    static ArrayOfStructMemberUpdateDynamicHelper helper;
    helper.Test(tag);
}

inline void test_main()
{
    unit::test<1>(ArrayOfStructStaticMemberUpdateTest);
    unit::test<2>(ArrayOfStructDynamicMemberUpdateTest);
}
