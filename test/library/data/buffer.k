// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.buffer
import sync.atomic
import test.unit as unit
import test.runner

template <auto N, auto M, bool Init>
inline void SerialInParallelOutTest(unit::tag_t tag)
{
    using elem_t = index_t<N>;

    pipelined_for(N, [tag](elem_t i)
    {
        const auto x = serial_in_parallel_out<M, elem_t, Init>(i);

        elem_t[M] a = {};

        static for(const auto j : M)
        {
            a[M - j - 1] = j < i ? i - j : {};
        }

        const auto b = atomically([a](elem_t[M] b){return a;});

        if (static(Init) || i >= M - 1)
        {
            unit::assert_equal(tag, a, x.second);
        }

        if (static(Init) || i >= M)
        {
            unit::assert_equal(tag, b.first, x.first);
        }
    });
}

inline void test_main()
{
    unit::test<1>(SerialInParallelOutTest<16, 5, true>);
    unit::test<2>(SerialInParallelOutTest<16, 5, false>);
}
