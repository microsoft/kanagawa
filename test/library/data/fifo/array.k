// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.fifo.array
import data.random.lfsr
import test.unit as unit
import test.runner

enum TestType : uint2
{
    One,
    Many,
    All
}

template<TestType Type>
class array_test
{
private:
    const auto ArraySize = 4;
    const auto Depth = 32;
    const auto NumIterations = 1024;

    fifo_array<uint32, Depth, ArraySize> _fifos;

public:
    void Run(unit::tag_t tag)
    {
        print("data.fifo.array start Type {Type}\n");

        // Write into the FIFO
        async_exec([]
        {
            pipelined_for(NumIterations, [](index_t<NumIterations> tid)
            {
                static lfsr_random<32> lfsr;
                uint32 random_value = lfsr.next({});

                if (Type == TestType::One)
                {
                    // write into a single FIFO
                    _fifos.enqueue_one<3>(tid);
                }
                else if (Type == TestType::Many)
                {
                    // write into multiple fifos
                    optional<uint32>[ArraySize] values;

                    static for (const auto i : ArraySize)
                    {
                        values[i].is_valid = 1 == ((random_value >> i) & 1);
                        values[i].value = (tid * ArraySize) + i;
                    }

                    _fifos.enqueue_many(values);
                }
                else
                {
                    assert(Type == TestType::All);

                    uint32[ArraySize] values = map([tid](index_t<ArraySize> i) -> uint32 { return (tid * ArraySize) + i; }, indices<ArraySize>());

                    _fifos.enqueue_all(values);
                }
            });
        });

        // Consumer
        pipelined_for(NumIterations, [tag](index_t<NumIterations> tid)
        {
            static lfsr_random<32> lfsr;
            uint32 random_value = lfsr.next({});

            if (Type == TestType::One)
            {
                // read from a single FIFO
                uint32 actual = _fifos.dequeue_one<3>();

                unit::assert_equal(tag, tid, actual);
            }
            else if (Type == TestType::Many)
            {
                // read from multiple fifos
                bool[ArraySize] valids = {};

                static for (const auto i : ArraySize)
                {
                    valids[i] = 1 == ((random_value >> i) & 1);
                }

                optional<uint32>[ArraySize] actual = _fifos.dequeue_many(valids);

                static for (const auto i : ArraySize)
                {
                    unit::assert_equal(tag, valids[i], actual[i].is_valid);

                    if (valids[i])
                    {
                        unit::assert_equal(tag, (tid * ArraySize) + i, actual[i].value);
                    }
                }
            }
            else
            {
                assert(Type == TestType::All);

                uint32[ArraySize] actual = _fifos.dequeue_all();

                 static for (const auto i : ArraySize)
                {
                    unit::assert_equal(tag, (tid * ArraySize) + i, actual[i]);
                }
            }
        });

        print("data.fifo.array end\n");
    }
}

template<TestType Type>
inline void TestArray(unit::tag_t tag)
{
    static array_test<Type> _test;
    _test.Run(tag);
}

inline void test_main()
{
    unit::test<1>(TestArray<TestType::One>);
    unit::test<2>(TestArray<TestType::Many>);
    unit::test<3>(TestArray<TestType::All>);
}
