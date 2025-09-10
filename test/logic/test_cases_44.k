// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// array read with signed index
class Test734
{
private:
    uint32 Helper734(int32 idx)
    {
        uint32[7] data;

        static for (const auto i : 7)
        {
            data[i] = i + 1;
        }

        return data[idx];
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, 1 == Helper734(0));
        unit::assert(tag, 2 == Helper734(1));
        unit::assert(tag, 3 == Helper734(2));
        unit::assert(tag, 4 == Helper734(3));
        unit::assert(tag, 5 == Helper734(4));
        unit::assert(tag, 6 == Helper734(5));
        unit::assert(tag, 7 == Helper734(6));
        unit::assert(tag, 0 == Helper734(7)); // array size is logically rounded up to the next power of 2 (zero-padded)
        unit::assert(tag, 1 == Helper734(8)); // wrapping

        unit::assert(tag, 0 == Helper734(-1)); // should be the same as 7
        unit::assert(tag, 7 == Helper734(-2)); // should be the same as 6
        unit::assert(tag, 6 == Helper734(-3)); // should be the same as 5
        unit::assert(tag, 5 == Helper734(-4)); // should be the same as 4
        unit::assert(tag, 4 == Helper734(-5)); // should be the same as 3
        unit::assert(tag, 3 == Helper734(-6)); // should be the same as 2
        unit::assert(tag, 2 == Helper734(-7)); // should be the same as 1
    }
}

inline void TestCase735(unit::tag_t tag)
{
    bool b = true;
    unit::assert(tag, cast<uint4>(cast<uint1>(b)) == 1);
    unit::assert(tag, cast<int4>(cast<uint1>(b)) == 1);

    const int4 extendedResult = 0xF;
    unit::assert(tag, cast<uint4>(cast<int1>(b)) == 0xF);
    unit::assert(tag, cast<int4>(cast<int1>(b)) == extendedResult);
}

class Test736
{
private:
    uint121 v736;

public:
    [[pipelined]] void set_with_wide_thread_id(uint121 i)
    {
        v736 = i;
    }

    void run(unit::tag_t tag)
    {
        const uint121 result = 17;
        v736 = ~result;
        set_with_wide_thread_id(result + 1);    // it shouldn't hang
        unit::assert(tag, v736 == result);
    }
}

inline void test_main()
{
    unit::test<734>(unit::fixture<Test734>());
    unit::test<735>(TestCase735);
    unit::test<736>(unit::fixture<Test736>());
}
