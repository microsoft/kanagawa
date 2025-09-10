// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Pipelined memory class.
*/

module data.memory.pipelined
    { pipelined_memory
    }

import data.array
import data.memory.bank.schedule
import data.optional
import numeric.int.operator
import sync.atomic as sync

//| A memory that is physically accessed over multiple clock cycles.
template
    < typename T //< The type of data stored in the memory.
    , auto Depth //< Total number of elements stored in the memory.
    , auto Banks //< Number physical memories that the logical memory comprises.
    , template <typename, auto> typename Memory = memory //< Type of underlying memory to use.
    >
class pipelined_memory
{
public:
    //| Elements must be evenly distributed among banks.
    static assert(0 == (Depth % Banks));

    using addr_t = index_t<Depth>;

private:
    const auto ElementsPerBank = Depth / Banks;

    using bank_index_t = index_t<Banks>;

    using index_within_bank_t = index_t<ElementsPerBank>;

    // Returns (bank index, element index within bank)
    inline auto decompose_address(addr_t addr)
    {
        // If Banks is > 1, then ElementsPerBank must be a power of 2, and div_mod can be used
        // If Banks = 0, then ElementsPerBank can be any value, and the decomposition is trivial
        static if (Banks > 1)
            return div_mod(addr, ElementsPerBank);
        else
            return make_pair(0, addr);
    }

    inline bank_index_t address_to_bank_index(addr_t addr)
    {
        auto decomposed_addr = decompose_address(addr);

        return cast<bank_index_t>(decomposed_addr.first);
    }

public:
    //| `ElementsPerBank` must be a power of 2 if there is more than 1 bank.
    static assert((0 == (ElementsPerBank & (ElementsPerBank - 1))) || (Banks == 1));

    //| Read one element from the memory.
    inline T read
        ( addr_t addr //< Address of the element to read.
        )
    {
        T result;

        auto decomposed_addr = decompose_address(addr);

        static for (const auto i : Banks)
        {
            if (i == decomposed_addr.first)
            {
                result = _memories[i][decomposed_addr.second];
            }
        }

        return result;
    }

    //| Read `N` elements from the memory.
    template
        < auto N //< The number of elements to read.
        >
    inline T[N] read_vec
        ( addr_t[N] addresses //< The addresses to read from.
        )
    {
        // Banks parameter explicitly specified to support the case where Banks = 1.
        // In this case, the number of banks cannoted be deduced from the return type of address_to_bank_index.
        auto per_bank_request_count = read_requests_per_bank<Banks>(addresses, address_to_bank_index);

        // Determine the maximum number of reads for any single bank
        count_t<N> thread_count = maximum(per_bank_request_count);

        return pipelined_last(thread_count, [addresses](index_t<N> tid) -> T[N]
        {
            auto schedule = schedule_read_requests(addresses, address_to_bank_index, tid);

            T[Banks] per_bank_results;

            static for (const auto i : Banks)
            {
                // Select an address for this bank
                auto address_index = schedule.first[i];

                addr_t addr = addresses[address_index.value];

                auto decomposed_addr = decompose_address(addr);

                assert(!address_index.is_valid || (decomposed_addr.first == i));

                // Read from this bank
                per_bank_results[i] = _memories[i][decomposed_addr.second];

                // Ensure banks are accessed in unique pipeline stages
                barrier;
            }

            // Broadcast per-bank values to requests
            auto this_thread_results = map(
                [per_bank_results](optional<bank_index_t> i)
                {
                    return make_optional(i.is_valid, per_bank_results[i.value]);
                },
                schedule.second);

            // Combine requests across threads
            return second(sync::atomically([this_thread_results](T[N] prev)
            {
                return zip_with(from_optional<T>, prev, this_thread_results);
            }));
        });
    }

    //| Write one value into the memory.
    //  Must not be called concurrently with other calls that may write to the memory.
    inline void write
        ( addr_t addr //< Address of the element to write.
        , T data      //< Data to write.
        )
    {
        auto decomposed_addr = decompose_address(addr);

        static for (const auto i : Banks)
        {
            if (i == decomposed_addr.first)
            {
                _memories[i][decomposed_addr.second] = data;
            }

            // Ensure banks are accessed in unique pipeline stages
            barrier;
        }
    }

    //| Write up to `N` values into the memory.
    //  Must not be called concurrently with other calls that may write to the memory.
    //  Throughput can be improved by de-duplicating writes to the same address before calling this method.
    template
        < auto N //< The maximum number of elements to write.
        >
    inline void write_vec
        ( optional<pair<addr_t, T>>[N] writes //< Addresses and corresponding values.
        )
    {
        // unzip to address and data arrays
        pair<optional<addr_t>[N], T[N]> unzipped = unzip_with(
            [](optional<pair<addr_t, T>> write) -> pair<optional<addr_t>, T>
            {
                return { make_optional(write.is_valid, write.value.first), write.value.second };
            },
            writes
        );

        // Determine the number of request for each bank.
        // Banks parameter explicitly specified to support the case where Banks = 1.
        // In this case, the number of banks cannoted be deduced from the return type of address_to_bank_index.
        auto per_bank_request_count = write_requests_per_bank<Banks>(unzipped.first, address_to_bank_index);

        // Determine the maximum number of reads for any single bank
        count_t<N> thread_count = maximum(per_bank_request_count);

        pipelined_for (thread_count, [unzipped](index_t<N> tid)
        {
            // Determine which data to write to which bank on this iteration.
            auto schedule = schedule_write_requests(
                unzipped.first,
                address_to_bank_index,
                tid);

            static for (const auto i : Banks)
            {
                optional<index_t<N>> schedule_entry = schedule[i];

                if (schedule_entry.is_valid)
                {
                    assert(unzipped.first[schedule_entry.value].is_valid);

                    addr_t addr = unzipped.first[schedule_entry.value].value;

                    T data = unzipped.second[schedule_entry.value];

                    auto decomposed_addr = decompose_address(addr);

                    assert(decomposed_addr.first == i);

                    _memories[i][decomposed_addr.second] = data;
                }

                // Ensure banks are accessed in unique pipeline stages
                barrier;
            }
        });
    }

    //| Atomically read and write the memory.
    //  Returns both the old and new value for the specified element.
    //  Must not be called concurrently with other calls that may write to the memory.
    //  Note that `modify` may be called multiple times concurrently (one call site per bank).
    template
        <
        auto BankUpdateRate = 1 //< If a given thread accesses a given bank,
                                //  then the next `BankUpdateRate-1` threads must not access the same bank.
                                //  This enables the implementation of `modify` to be pipelined.
        >
    inline pair<T, T> atomically
        ( addr_t addr //< Address of the element to read.
        , (T)->T modify  //< Function that accepts the value of the element read from the memory
                         //  and returns a value to write into the memory at the same address.
        )
    {
        auto decomposed_addr = decompose_address(addr);

        pair<T, T> result;

        static for (const auto i : Banks)
        {
            if (i == decomposed_addr.first)
            {
                [[schedule(BankUpdateRate)]]
                {
                    result.first = _memories[i][decomposed_addr.second];

                    result.second = modify(result.first);

                    _memories[i][decomposed_addr.second] = result.second;
                }
            }
        }

        return result;
    }

    //| Atomically read and write the memory up to `N` times.
    //  Returns both the old and new value for the specified elements.
    //  Must not be called concurrently with other calls that may write to the memory.
    //  Throughput can be improved by de-duplicating accesses to the same address before calling this method.
    //  Note that `modify` may be called multiple times concurrently (one call site per bank).
    template
        < auto N
        , auto BankUpdateRate = 1 //< If a given thread accesses a given bank,
                                  //  then the next `BankUpdateRate-1` threads must not access the same bank.
                                  //  This enables the implementation of `modify` to be pipelined.
        >
    inline pair<T, T>[N] atomically_vec
        ( optional<addr_t>[N] addresses    //< Addresses of the element to access.
        , (T, index_t<N>)->T modify        //< Function that accepts the value of an element read from the memory
                                           //  and an index of an element of the addresses array.
                                           //  This function returns a value to write into the memory at the same address.
        )
    {
        // Determine the number of request for each bank.
        // Banks parameter explicitly specified to support the case where Banks = 1.
        // In this case, the number of banks cannoted be deduced from the return type of address_to_bank_index.
        auto per_bank_request_count = write_requests_per_bank<Banks>(addresses, address_to_bank_index);

        // Determine the maximum number of reads for any single bank
        count_t<N> thread_count = maximum(per_bank_request_count);

        return pipelined_last(thread_count, [modify, addresses](index_t<N> tid) -> pair<T, T>[N]
        {
            // Determine addresses and banks to access on this iteration
            auto schedule = schedule_write_requests(
                addresses,
                address_to_bank_index,
                tid);

            optional<pair<T, T>>[N] this_iteration_result;

            static for (const auto i : Banks)
            {
                optional<index_t<N>> schedule_entry = schedule[i];

                if (schedule_entry.is_valid)
                {
                    index_t<N> address_index = schedule_entry.value;

                    assert(addresses[address_index].is_valid);

                    addr_t addr = addresses[address_index].value;

                    auto decomposed_addr = decompose_address(addr);

                    assert(decomposed_addr.first == i);

                    pair<T, T> this_bank_result;

                    [[schedule(BankUpdateRate)]]
                    {
                        this_bank_result.first = _memories[i][decomposed_addr.second];

                        this_bank_result.second = modify(this_bank_result.first, address_index);

                        _memories[i][decomposed_addr.second] = this_bank_result.second;
                    }

                    this_iteration_result[address_index] = make_optional(true, this_bank_result);
                }
            }

            // Combine results across iterations
            return second(sync::atomically([this_iteration_result](pair<T, T>[N] prev)
            {
                return zip_with(from_optional<pair<T, T>>, prev, this_iteration_result);
            }));
        });
    }

    //| Combine and update the set of all elements with a given offset with a bank
    //  according to a user-specified function.
    //  Must not be called concurrently with other calls that may write to the memory.
    template
        < typename Ctx //< Context type passed to and returned from the combine function.
        >
    inline Ctx bankwise_fold
        ( addr_t addr //< Logical address of one element within the memory.
                      // All elements that have matching offsets within banks are combined.
        , Ctx initial //< Context value passed to the first call to `combine`.
        , (pair<T, Ctx>)->pair<T, Ctx> combine //< Combination function.
                                               //  Maps a previous value in the memory and the current context
                                               //  to a new value stored in the memory and a new context.
        )
    {
        // Determine offset within any given bank
        auto decomposed_addr = decompose_address(addr);

        Ctx current_context = initial;

        static for (const auto i : Banks)
        {
            atomic
            {
                T prev = _memories[i][decomposed_addr.second];

                auto combined = combine(make_pair(prev, current_context));

                current_context = combined.second;

                _memories[i][decomposed_addr.second] = combined.first;
            }
        }

        return current_context;
    }

private:
    Memory<T, ElementsPerBank>[Banks] _memories;
}
