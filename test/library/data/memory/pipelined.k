// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.memory.pipelined
import data.random.lfsr
import test.unit as unit
import test.runner

const auto Depth = 512;
const auto NumBanks = 4;
const auto ElementsPerBank = Depth / NumBanks;
using T = uint32;
using addr_t = index_t<Depth>;

// Compare pipelined memory against reference implementation
class TestComparison
{
private:
    memory<T, Depth> _reference;
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        const auto compare = [tag]
        {
            pipelined_for(Depth, [tag](addr_t addr)
            {
                const T expected = _reference[addr];
                const T actual = _pipelined.read(addr);

                unit::assert_equal(tag, expected, actual);
            });
        };

        // Initialize
        pipelined_for(Depth, [](addr_t addr)
        {
            const T data = addr * 9;

            _reference[addr] = data;
            _pipelined.write(addr, data);
        });

        compare();

        // Read-modify-write
        pipelined_for(1024, [](index_t<1024> i)
        {
            static lfsr_random<clog2(Depth)> lfsr;

            addr_t addr = lfsr.next({});

            atomic
            {
                _reference[addr] = _reference[addr] + 3;
            }

            _pipelined.atomically(addr, [](T src) -> T
            {
                return src + 3;
            });
        });

        compare();
    }
}

// Test return value from atomically
class TestAtomicReturn
{
private:
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            const T data = addr * 11;
            _pipelined.write(addr, data);
        });

        pipelined_for(Depth, [tag](addr_t addr)
        {
            auto actual =_pipelined.atomically(addr, [](T src) -> T
            {
                return src + 1;
            });

            unit::assert_equal(tag, addr * 11, actual.first);
            unit::assert_equal(tag, (addr * 11) + 1, actual.second);
        });
    }
}

// Test atomicity of updates performed by atomically
class TestAtomicity
{
private:
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            _pipelined.write(addr, addr);
        });

        pipelined_for(128, [tag](index_t<128> i)
        {
            // Back-to-back threads will access the same address
            addr_t addr = i / 8;

            auto actual =_pipelined.atomically(addr, [](T src) -> T
            {
                return src + 1;
            });

            uint32 expected = addr + (i % 8);

            unit::assert_equal(tag, expected, actual.first);
            unit::assert_equal(tag, expected + 1, actual.second);
        });
    }
}

// Test atomicity of updates performed by atomically
class TestFold
{
private:
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            _pipelined.write(addr, addr);
        });

        pipelined_for(ElementsPerBank, [tag](index_t<ElementsPerBank> i)
        {
            uint32 actual = _pipelined.bankwise_fold(
                i,
                0,
                [](pair<T, uint32> p)
                {
                    // Compute sum of all elements with matching bank offsets
                    p.second += p.first;

                    // Invert all values in memory
                    p.first = ~p.first;

                    return p;
                });

            uint32 expected = 0;

            static for (const auto b : NumBanks)
            {
                expected += (i + (b * ElementsPerBank));
            }

            unit::assert_equal(tag, expected, actual);
        });

        pipelined_for(Depth, [tag](addr_t addr)
        {
            const T actual = _pipelined.read(addr);
            const T expected = ~cast<T>(addr);

            unit::assert_equal(tag, expected, actual);
        });
    }
}

class TestFoldUpdateRate
{
private:
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            _pipelined.write(addr, addr);
        });

        pipelined_for(ElementsPerBank, [tag](index_t<ElementsPerBank> i)
        {
            // Increment each element
            // Allow the increment to be spread over multiple pipeline stages
            pipelined_for (NumBanks, [i](index_t<NumBanks> b)
            {
                _pipelined.atomically<NumBanks>(
                    checked_cast(b * ElementsPerBank + i),
                    [](uint32 x) -> uint32 {return x + 1;}
                );
            });
        });

        pipelined_for(ElementsPerBank, [tag](index_t<ElementsPerBank> i)
        {
            // Sum the values in each bank
            uint32 actual = _pipelined.bankwise_fold(
                i,
                0,
                [](pair<T, uint32> p)
                {
                    p.second += p.first;

                    return p;
                });

            uint32 expected = NumBanks;

            static for (const auto b : NumBanks)
            {
                expected += (i + (b * ElementsPerBank));
            }

            unit::assert_equal(tag, expected, actual);
        });
    }
}

template<auto N>
class TestReadVec
{
private:
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            _pipelined.write(addr, addr + 1);
        });

        pipelined_for(1024, [tag](uint32 tid)
        {
            // Generate random addresses
            addr_t[N] addresses = pipelined_map<N>(N, [](index_t<N> tid)
            {
                static lfsr_random<clog2(Depth)> lfsr;

                addr_t addr = lfsr.next({});

                return addr;
            });

            T[N] results = _pipelined.read_vec(addresses);

            static for (const auto i : N)
            {
                unit::assert_equal(tag, addresses[i] + 1, results[i]);
            }
        });
    }
}

template<auto N, auto AddressMask = 0xFFFFFFFF>
class TestWriteVec
{
private:
    memory<T, Depth> _reference;
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            _pipelined.write(addr, addr + 1);
            _reference[addr] = addr + 1;
        });

        pipelined_for(1024 * 8, [tag](uint32 i)
        {
            // Generate random addresses and data
            optional<pair<addr_t, T>>[N] writes = pipelined_map<N, index_t<N>>(N, [](index_t<N> tid)
            {
                static lfsr_random<2> enable_lfsr;
                static lfsr_random<clog2(Depth)> addr_lfsr;
                static lfsr_random<bitsizeof T> data_lfsr;

                // AddressMask is used to increase the chance of duplicate addresses
                return make_optional<pair<addr_t, T>>(enable_lfsr.next({}) != 1, { addr_lfsr.next({}) & AddressMask, data_lfsr.next({})});
            });

            _pipelined.write_vec(writes);

            // Also apply writes to reference
            pipelined_for (N, [writes](index_t<N> i)
            {
                auto write = writes[i];

                if (write.is_valid)
                {
                    _reference[write.value.first] = write.value.second;
                }
            });
        });

        pipelined_for(Depth, [tag](addr_t addr)
        {
            const T expected = _reference[addr];
            const T actual = _pipelined.read(addr);

            unit::assert_equal(tag, expected, actual);
        });
    }
}

template<auto N, auto AddressMask = 0xFFFFFFFF>
class TestAtomicallyVec
{
private:
    memory<T, Depth> _reference;
    pipelined_memory<T, Depth, NumBanks> _pipelined;

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(Depth, [](addr_t addr)
        {
            _pipelined.write(addr, addr + 1);
            _reference[addr] = addr + 1;
        });

        pipelined_for(1024 * 8, [tag](uint32 i)
        {
            // Generate random addresses
            optional<addr_t>[N] addresses = pipelined_map<N, index_t<N>>(N, [](index_t<N> tid)
            {
                static lfsr_random<2> enable_lfsr;
                static lfsr_random<clog2(Depth)> addr_lfsr;

                // AddressMask is used to increase the chance of duplicate addresses
                return make_optional<addr_t>(enable_lfsr.next({}) != 1, addr_lfsr.next({}) & AddressMask);
            });

            _pipelined.atomically_vec(addresses, [](T prev, index_t<N> address_index)
            {
                return cast<T>(prev + address_index + 1);
            });

            // Also apply writes to reference
            pipelined_for (N, [addresses](index_t<N> i)
            {
                auto address = addresses[i];

                atomic
                {
                    if (address.is_valid)
                    {
                        _reference[address.value] += (i + 1);
                    }
                }
            });
        });

        pipelined_for(Depth, [tag](addr_t addr)
        {
            const T expected = _reference[addr];
            const T actual = _pipelined.read(addr);

            unit::assert_equal(tag, expected, actual);
        });
    }
}

// Bank count is not a power of 2
inline void TestNonPow2Banks(unit::tag_t tag)
{
    class Helper
    {
        const auto NonPow2Depth = 384;
        const auto NonPow2NumBanks = 12;
        pipelined_memory<T, NonPow2Depth, NonPow2NumBanks> _pipelined_non_pow2;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for (NonPow2Depth, [](index_t<NonPow2Depth> i)
            {
                _pipelined_non_pow2.write(i, i + 14);
            });

            pipelined_for (NonPow2Depth, [tag](index_t<NonPow2Depth> i)
            {
                T actual = _pipelined_non_pow2.read(i);

                T expected = i + 14;

                unit::assert_equal(tag, expected, actual);
            });
        }
    }

    static Helper _helper;
    _helper.test(tag);
}

// Bank count == 1, and depth is not a power of 2
inline void TestNonPow2ElementsPerBank(unit::tag_t tag)
{
    class Helper
    {
        const auto NonPow2Depth = 384;
        const auto NonPow2NumBanks = 1;
        pipelined_memory<T, NonPow2Depth, NonPow2NumBanks> _pipelined_non_pow2;

    public:
        void test(unit::tag_t tag)
        {
            pipelined_for (NonPow2Depth, [](index_t<NonPow2Depth> i)
            {
                _pipelined_non_pow2.write(i, i + 14);
            });

            pipelined_for (NonPow2Depth, [tag](index_t<NonPow2Depth> i)
            {
                T actual = _pipelined_non_pow2.read(i);

                T expected = i + 14;

                unit::assert_equal(tag, expected, actual);
            });
        }
    }

    static Helper _helper;
    _helper.test(tag);
}

inline void test_main()
{
    print("data.memory.pipelined start\n");

    unit::test<1>(unit::fixture<TestComparison>());
    unit::test<2>(unit::fixture<TestAtomicReturn>());
    unit::test<3>(unit::fixture<TestAtomicity>());
    unit::test<4>(unit::fixture<TestFold>());
    unit::test<5>(unit::fixture<TestFoldUpdateRate>());
    unit::test<6>(unit::fixture<TestReadVec<2>>());
    unit::test<7>(unit::fixture<TestReadVec<4>>());
    unit::test<8>(unit::fixture<TestReadVec<8>>());
    unit::test<9>(unit::fixture<TestWriteVec<2>>());
    unit::test<10>(unit::fixture<TestWriteVec<4>>());
    unit::test<11>(unit::fixture<TestWriteVec<8>>());
    unit::test<11>(unit::fixture<TestWriteVec<8, 0xF>>()); // test duplicate addresses
    unit::test<12>(unit::fixture<TestAtomicallyVec<2>>());
    unit::test<13>(unit::fixture<TestAtomicallyVec<4>>());
    unit::test<14>(unit::fixture<TestAtomicallyVec<8>>());
    unit::test<14>(unit::fixture<TestAtomicallyVec<8,0xF>>()); // test duplicate addresses
    unit::test<15>(TestNonPow2Banks);
    unit::test<16>(TestNonPow2ElementsPerBank);

    print("data.memory.pipelined end\n");
}
