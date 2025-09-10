// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.memory.unaligned
import data.random.lfsr
import sync.atomic
import test.unit as unit
import test.runner

class test_unaligned_read_aligned_write_memory
{
    const auto WordSize = 2;
    const auto ReadWordSize = 6;
    const auto MemoryDepth = 128;
    const auto TotalSize = WordSize * MemoryDepth;
    using T = uint32;
    unaligned_read_aligned_write_memory<T, WordSize, ReadWordSize, TotalSize> unalignedMemory;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(MemoryDepth,
            [](index_t<MemoryDepth> tid)
            {
                T[WordSize] data;
                static for(const auto i : WordSize)
                {
                    data[i] = tid * WordSize + i;
                }
                unalignedMemory.write_aligned(tid * WordSize, data);
            }
        );

        pipelined_for(TotalSize,
            [tag](index_t<TotalSize> tid)
            {
                auto data = unalignedMemory.read(tid);
                static for(const auto i : ReadWordSize)
                {
                    T expected = cast<index_t<TotalSize>>(tid + i);
                    unit::assert_equal(tag, expected, data[i]);
                }
            }
        );
    }
}

inline void test_unaligned_read_unaligned_write_memory()
{
    class Fixture
    {
        const auto size_bytes = 256;
        const auto word_size = 16;

        using mem_t = unaligned_read_unaligned_write_memory<uint8, word_size, size_bytes>;

        mem_t mem;
        mem_t tmp;

        void clear()
        {
            pipelined_for(size_bytes,
                    [](index_t<size_bytes> i)
                    {
                        tmp.write<1>(i, {}, 1);
                    });
        }

        bool equal()
        {
            return pipelined_last(size_bytes,
                [](index_t<size_bytes> i)
                {
                    auto result = equal(mem.read<1>(i), tmp.read<1>(i));

                    return second(atomically(
                        [result, i](bool previous_result)
                        {
                            return (previous_result || i == 0) && result;
                        }));
                });
        }

        template <auto N, auto M>
        void copy()
        {
            pipelined_for(size_bytes / N,
                [](index_t<size_bytes / N> i)
                {
                    const mem_t::addr_t addr = static_cast(size_bytes - (i - 1) * N);
                    const auto x = mem.read<N>(addr);
                    tmp.write<N>(addr, x, M);
                });
        }

        void init()
        {
            pipelined_for(size_bytes,
                [](index_t<size_bytes> i)
                {
                    auto x = first(atomically<uint8, 1>(lfsr<8>));
                    mem.write<1>(i, reinterpret_cast<mem_t::word_t>(x), 1);
                });
        }

        template <auto N, auto M>
        void test(unit::tag_t tag)
        {
            clear();
            unit::assert(tag, !equal());
            copy<N, M>();
            unit::assert(tag, equal());
        }

    public:
        void tests()
        {
            init();

            static for (const auto i : 4)
                static for (const auto j : 5 - i)
                {
                    unit::test<j + 1, i + 1>(
                        test<(1 << i), (1 << (i + j))>
                    );
                }
        }
    }

    static Fixture fixture;

    fixture.tests();
}

inline void test_main()
{
    unit::test<1>(unit::fixture<test_unaligned_read_aligned_write_memory>());
    test_unaligned_read_unaligned_write_memory();
}
