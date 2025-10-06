// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.array
import test.unit as unit
import test.runner

// Common subexpression elimination

inline void Gather(unit::tag_t tag)
{
    pipelined_for(8, [tag](index_t<8> i)
    {
        bool[3] b = cast<bool[3]>(i);
        bool[3] c = cast<bool[3]>(i);

        // should not be replaced with b or c
        index_t<8> j = static_cast(i + 1);
        bool[3] d = cast<bool[3]>(j);

        unit::assert(tag, equal(b, c));
        unit::assert(tag, !equal(b, d));
    });
}

inline void Lut(unit::tag_t tag)
{
    pipelined_for(4, [tag](uint32 i)
    {
        uint2 a = static_cast(i);
        uint2 b = static_cast(i + 1);
        uint2 c = static_cast(i - 1);

        uint2 d = (a | b) & c;

        inline uint1 ExtractBit(uint2 src, uint1 whichBit)
        {
            return (src >> whichBit) & 1;
        }

        // bits of e and d should be the same
        // but e is computed per-bit
        uint1[2] eBits;
        static for (const auto idx : 2)
        {
            eBits[idx] = (ExtractBit(a, idx) | ExtractBit(b, idx)) & ExtractBit(c, idx);
        }

        uint2 e = cast<uint2>(eBits);

        unit::assert_equal(tag, d, e);
    });
}

inline void test_main()
{
    unit::test<1>(Gather);
    unit::test<2>(Lut);
}
