// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import sync.atomic
import test.unit as unit
import test.runner

template<auto TransactionSizeA, auto TransactionSizeB>
inline void TestTransaction(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 Shared([[last]] bool is_last)
        {
            return first(atomically([](uint32 prev)->uint32{return prev + 1;}));
        }

        inline void ValidateResult(unit::tag_t tag, uint32 tid, uint32 transaction_size, uint32 actual)
        {
            bool is_first = 0 == (tid % transaction_size);
            bool is_last = (transaction_size - 1) == (tid % transaction_size);

            uint32 expected = second(atomically([is_first, actual](uint32 prev) -> uint32
            {
                // The result from Shared is snapped on the first call in a transaction
                // and used to compute the expected value for the remaining
                // calls in the transaction
                return is_first ? actual : prev + 1;
            }));

            if (!is_first)
            {
                unit::assert_equal(tag, expected, actual);
            }
        }

    public:
        void Test(unit::tag_t tag)
        {
            pipelined_for(4096, [tag](uint32 tid)
            {
                ValidateResult(tag, tid, TransactionSizeA, [[transaction_size(TransactionSizeA)]] Shared((TransactionSizeA - 1) == (tid % TransactionSizeA)));

                ValidateResult(tag, tid, TransactionSizeB, [[transaction_size(TransactionSizeB)]] Shared((TransactionSizeB - 1) == (tid % TransactionSizeB)));
            });
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

template<auto TransactionSizeA, auto TransactionSizeB>
inline void TestTransactionAsync(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 Shared([[last]] bool is_last)
        {
            return first(atomically([](uint32 prev)->uint32{return prev + 1;}));
        }

        inline void ValidateResult(unit::tag_t tag, uint32 tid, uint32 transaction_size, uint32 actual)
        {
            bool is_first = 0 == (tid % transaction_size);
            bool is_last = (transaction_size - 1) == (tid % transaction_size);

            uint32 expected = second(atomically([is_first, actual](uint32 prev) -> uint32
            {
                // The result from Shared is snapped on the first call in a transaction
                // and used to compute the expected value for the remaining
                // calls in the transaction
                return is_first ? actual : prev + 1;
            }));

            if (!is_first)
            {
                unit::assert_equal(tag, expected, actual);
            }
        }

        bool[2] _done;

        template<auto TransactionSize, auto index>
        [[async, pipelined]] void ValidateResultHelper(uint32 tid, unit::tag_t tag)
        {
            ValidateResult(tag, tid, TransactionSize, [[transaction_size(TransactionSize)]] Shared((TransactionSize - 1) == (tid % TransactionSize)));

            if (tid == 4095)
            {
                _done[index] = true;
            }
        }

    public:
        void Test(unit::tag_t tag)
        {
            _done = {};

            ValidateResultHelper<TransactionSizeA, 0>(4096, tag);
            ValidateResultHelper<TransactionSizeB, 1>(4096, tag);

            atomic do; while(!(_done[0] && _done[1]));
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

template<auto TransactionSizeA, auto TransactionSizeB>
inline void TestTransactionAsync2(unit::tag_t tag)
{
    class Helper
    {
    private:
        bool[2] _done;

        // Transaction call is async
        [[async]] void AsyncTransaction(index_t<2> index, [[last]] bool is_last)
        {
            if (is_last)
            {
                _done[index] = true;
            }
        }

        noinline void NoInlineFunction ()
        {

        }

    public:
        void Test(unit::tag_t tag)
        {
            _done = {};

            static_assert(TransactionSizeA >= TransactionSizeB);
            pipelined_for(TransactionSizeA, [](index_t<TransactionSizeA> tid)
            {
                // Async transaction creates multiple transactional FIFOs from
                // the same basic block
                // First argument is compile-time constant and is not store in
                // FIFO for Verilog backend. This affects the offset for the
                // [[last]] bit.
                [[transaction_size(TransactionSizeA)]] AsyncTransaction(0, (tid % TransactionSizeA) == (TransactionSizeA - 1));
                [[transaction_size(TransactionSizeB)]] AsyncTransaction(1, (tid % TransactionSizeB) == (TransactionSizeB - 1));

                // Call to non-inlined function creates another FIFO from the
                // same basic block. Test that this FIFO does not create a
                // deadlock
                NoInlineFunction();
            });

            atomic do; while(!(_done[0] && _done[1]));
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

inline void TestTransactionSizeOne(unit::tag_t tag)
{
    class Helper
    {
    private:
        const auto N = 4096;

        void Transaction(index_t<2> index, [[last]] bool is_last)
        {
            // Create backpressure to fill FIFOs. Test that max transaction
            // count works correctly
            uint64 start_cycles = cycles();
            atomic do; while(cycles() - start_cycles < 128);
        }

    public:
        void Test(unit::tag_t tag)
        {
            pipelined_for(N, [](index_t<N> tid)
            {
                // Max transaction size > 1 but actual transactions are size 1
                // Test constant [[last]] value
                [[transaction_size(64)]] Transaction(0, true);
                // Transaction size 1 but compiler does not statically determine
                // [[last]] value
                [[transaction_size(32)]] Transaction(1, tid >= 0);
            });
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

template<auto TransactionSizeA, auto TransactionSizeB>
inline void TestPredicatedTransaction(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 Shared([[last]] bool is_last)
        {
            return first(atomically([](uint32 prev)->uint32{return prev + 1;}));
        }

        inline void ValidateResult(unit::tag_t tag, bool is_first, uint32 actual)
        {
            uint32 expected = second(atomically([is_first, actual](uint32 prev) -> uint32
            {
                // The result from Shared is snapped on the first call in a transaction
                // and used to compute the expected value for the remaining
                // calls in the transaction
                return is_first ? actual : prev + 1;
            }));

            if (!is_first)
            {
                unit::assert_equal(tag, expected, actual);
            }
        }

    public:
        void Test(unit::tag_t tag)
        {
            pipelined_for(4096, [tag](uint32 tid)
            {
                // Predicate transaction call creating transactions with size
                // less than max size
                if (tid % 2 == 0 || tid % TransactionSizeA == TransactionSizeA - 1)
                {
                    bool is_last = (TransactionSizeA - 1) == (tid % TransactionSizeA);
                    bool is_first = first(atomically<bool, true>([is_last](bool prev_is_first)
                    {
                        return is_last;
                    }));

                    ValidateResult(tag, is_first, [[transaction_size(TransactionSizeA)]] Shared(is_last));
                }

                // Predicate transactions but maintain transaction size
                if (tid % 2 == 1)
                {
                    bool is_first;
                    bool is_last;

                    atomic
                    {
                        static index_t<TransactionSizeB> _transaction_index = 0;

                        is_first = _transaction_index == 0;
                        is_last = _transaction_index == TransactionSizeB - 1;

                        _transaction_index = is_last ? 0 : _transaction_index + 1;
                    }

                    ValidateResult(tag, is_first, [[transaction_size(TransactionSizeB)]] Shared(is_last));
                }
            });
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

// Function with [[last]] parameter has only 1 call site
// The compiler should not auto-inline, to enable the
// store-and-forward semantics to be maintained
class NoAutoInline
{
private:
    void arbitrated(unit::tag_t tag, uint32 actual, [[last]] bool is_last)
    {
        uint32 expected = first(atomically([is_last](uint32 prev)
        {
            return is_last ? 0 : static_cast<uint32>(prev + 1);
        }));

        unit::assert_equal(tag, expected, actual);
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(8, [tag](index_t<8> i)
        {
            [[transaction_size(8)]] arbitrated(tag, i, (i == 7));
        });
    }
}

template<auto TransactionSizeA, auto TransactionSizeB>
inline void TestIndirectTransaction(unit::tag_t tag)
{
    class Helper
    {
    private:
        uint32 Shared([[last]] bool is_last)
        {
            return first(atomically([](uint32 prev)->uint32{return prev + 1;}));
        }

        // This function has a single call site
        // But calls Shared which has multiple call sites
        uint32 SingleCallSite([[last]] bool is_last)
        {
            return Shared(is_last);
        }

        inline void ValidateResult(unit::tag_t tag, uint32 tid, uint32 transaction_size, uint32 actual)
        {
            bool is_first = 0 == (tid % transaction_size);
            bool is_last = (transaction_size - 1) == (tid % transaction_size);

            uint32 expected = second(atomically([is_first, actual](uint32 prev) -> uint32
            {
                // The result from Shared is snapped on the first call in a transaction
                // and used to compute the expected value for the remaining
                // calls in the transaction
                return is_first ? actual : prev + 1;
            }));

            if (!is_first)
            {
                unit::assert_equal(tag, expected, actual);
            }
        }

    public:
        void Test(unit::tag_t tag)
        {
            pipelined_for(4096, [tag](uint32 tid)
            {
                ValidateResult(tag, tid, TransactionSizeA, [[transaction_size(TransactionSizeA)]] SingleCallSite((TransactionSizeA - 1) == (tid % TransactionSizeA)));

                ValidateResult(tag, tid, TransactionSizeB, [[transaction_size(TransactionSizeB)]] Shared((TransactionSizeB - 1) == (tid % TransactionSizeB)));
            });
        }
    }

    static Helper _helper;
    _helper.Test(tag);
}

inline void test_main()
{
    print("start Transaction tests\n");

    unit::test<1>(TestTransaction<64, 32>);
    unit::test<2>(TestTransactionAsync<64, 32>);
    unit::test<3>(TestTransactionAsync2<128, 64>);
    unit::test<4>(TestTransactionSizeOne);
    unit::test<5>(TestPredicatedTransaction<32, 64>);
    unit::test<6>(unit::fixture<NoAutoInline>());
    unit::test<7>(TestIndirectTransaction<64, 32>);

    print("end Transaction tests\n");
}
