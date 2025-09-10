// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.memory.bank.schedule
import data.optional as O
import data.random.lfsr
import test.unit as unit
import test.runner

template <auto Banks, index_t<Banks> Bix, typename T>
inline index_t<Banks> const_bank(T address)
{
    return Bix;
}

inline index_t<2> even_odd_bank(uint2 address)
{
    index_t<2> bank;
    if (address == 0 || address == 2)
        bank = 0;
    else
        bank = 1;
    return bank;
}

template <typename Address, auto Banks, auto Addresses>
inline bool test_case_read_requests_per_bank
    ( Address[Addresses] requests
    , count_t<Addresses>[Banks] expected
    , (Address) -> index_t<Banks> address_to_bank_index
    )
{
    auto actual = read_requests_per_bank<Banks>(requests, address_to_bank_index);
    return equal(actual, expected);
}

inline void test_read_requests_per_bank(unit::tag_t tag)
{

    // two banks, 1 addresses per call, address type uint<2>, bank size 2.
    // requests = {0}
    // expected = {1, 0}
    // No bank conflicts.
    unit::assert
        ( tag
        , test_case_read_requests_per_bank<uint2>
              ( {0}
              , {1, 0}
              , const_bank<2, 0, uint2>
              )
        );

    // three banks, 3 addresses per call, address type uint32, bank size 5.
    // requests = {0, 5, 10}
    // expected = {3, 0, 0}
    // Bank conflict at address 0.
    unit::assert
        ( tag
        , test_case_read_requests_per_bank<uint32>
              ( {0, 5, 10}
              , {3, 0, 0}
              , const_bank<3, 0, uint32>
              )
        );

    // four banks, 2 addresses per call, address type uint32
    // requests = {0, 0}
    // expected = {1, 0, 0, 0}
    // Requests are the same, no bank conflicts.
    unit::assert
        ( tag
        , test_case_read_requests_per_bank<uint32>
              ( {0, 0}
              , {1, 0, 0, 0}
              , const_bank<4, 0, uint32>
              )
        );

    // 2 banks, 4 uint2 addresses per call
    // requests = {0, 1, 2, 1}
    // expected = {2, 1}
    // 2 requests on bank 0, 1 request on bank 1.
    unit::assert
        ( tag
        , test_case_read_requests_per_bank<uint2>
              ( {0, 1, 2, 1}
              , {2, 1}
              , [](uint<2> address){ return address == 2 ? cast<index_t<2>>(1) : 0; }
              )
        );
}

template <auto Banks, typename Address, auto Addresses>
inline bool test_case_write_requests_per_bank
    ( optional<Address>[Addresses] requests
    , count_t<Addresses>[Banks] expected
    , (Address) -> index_t<Banks> address_to_bank_index
    )
{
    auto actual = write_requests_per_bank<Banks>(requests, address_to_bank_index);
    return equal(actual, expected);
}

inline void test_write_requests_per_bank(unit::tag_t tag)
{
    // one bank, 1 address per call, address type uint<1>, bank size 1.
    // requests = {{false, 0}}
    // expected = {0}
    // No bank conflicts
    unit::assert
        ( tag
        , test_case_write_requests_per_bank
              ( {{false, 0}}
              , {0}
              , const_bank<1, 0, uint1>
              )
        );

    // one bank, 1 address per call, address type uint<1>, bank size 1
    // requests = {{true, 0}}
    // expected = {1}
    // No bank conflicts
    unit::assert
        ( tag
        , test_case_write_requests_per_bank
              ( {{true, 0}}
              , {1}
              , const_bank<1, 0, uint1>
              )
        );

    // two banks, 1 addresses per call, address type uint<2>, bank size 2.
    // requests = {{true, 0}}
    // expected = {1, 0}
    // No bank conflicts.
    unit::assert
        ( tag
        , test_case_write_requests_per_bank
              ( {{true, 0}}
              , {1, 0}
              , const_bank<2, 0, uint2>
              )
        );

    // one bank, 2 addresses per call, bank size 1
    // Bank conflict.
    unit::assert
        ( tag
        , test_case_write_requests_per_bank
              ( {{true, 0}, {true, 1}}
              , {2}
              , const_bank<1, 0, uint1>
              )
        );

    // one bank, 2 addresses per call, bank size 1
    // Bank conflict caused by the same address
    unit::assert
        ( tag
        , test_case_write_requests_per_bank
              ( {{true, 0}, {true, 0}}
              , {2}
              , const_bank<1, 0, uint1>
              )
        );
}

template <auto Banks, typename Address, auto Addresses>
inline bool test_case_schedule_read_requests
    ( Address[Addresses] addresses
    , (Address) -> index_t<Banks> address_to_bank_index
    , index_t<Addresses> iteration
    , optional<index_t<Addresses>>[Banks] exp1
    , optional<index_t<Banks>>[Addresses] exp2
    )
{
    auto actual = schedule_read_requests(addresses, address_to_bank_index, iteration);
    println(actual);
    return equal_by(O::equal<index_t<Addresses>>, actual.first, exp1) && equal_by(O::equal<index_t<Banks>>, actual.second, exp2);
}

inline void test_schedule_read_requests(unit::tag_t tag)
{

    // 4 banks
    // const map address to bank 0
    // 0th iteration
    // expected:
    // bank address indices {0, x, x, x}
    // address bank indices {0, 0}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 0}
              , const_bank<4, 0, uint32>
              , 0
              , {{true, 0}, {false, 0}, {false, 0}, {false, 0}}
              , {{true, 0}, {true, 0}}
              )
        );

    // 4 banks
    // const map address to bank 0
    // 1st iteration
    // expected:
    // bank address indices {x, x, x, x}
    // address bank indices {x, x}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 0}
              , const_bank<4, 0, uint32>
              , 1
              , {{false, 0}, {false, 0}, {false, 0}, {false, 0}}
              , {{false, 0}, {false, 0}}
              )
        );

    // 3 banks
    // const map address to bank 0
    // 0th iteration
    // expected:
    // bank address indices {0, x, x}
    // address bank indices {0, x}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 1}
              , const_bank<3, 0, uint1>
              , 0
              , {{true, 0}, {false, 0}, {false, 0}}
              , {{true, 0}, {false, 0}}
              )
        );

    // 3 banks
    // const map address to bank 0
    // 1st iteration
    // expected:
    // bank address indices {1, x, x}
    // address bank indices {x, 0}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 1}
              , const_bank<3, 0, uint1>
              , 1
              , {{true, 1}, {false, 0}, {false, 0}}
              , {{false, 0}, {true, 0}}
              )
        );

    // 2 banks
    // map even addresses to bank 0, odds to bank 1.
    // 0th iteration
    // expected:
    // bank address indices {0, 2}
    // address bank indices {0, 0, 1, x, x}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 0, 1, 2, 3}
              , even_odd_bank
              , 0
              , {{true, 0}, {true, 2}}
              , {{true, 0}, {true, 0}, {true, 1}, {false, 0}, {false, 0}}
              )
        );

    // 2 banks
    // map even addresses to bank 0, odds to bank 1.
    // 1st iteration
    // expected:
    // bank address indices {3, 4}
    // address bank indices {x, x, x, 0, 1}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 0, 1, 2, 3}
              , even_odd_bank
              , 1
              , {{true, 3}, {true, 4}}
              , {{false, 0}, {false, 0}, {false, 0}, {true, 0}, {true, 1}}
              )
        );

    // 2 banks
    // map even addresses to bank 0, odds to bank 1
    // 2nd iteration
    // expected:
    // bank address indices {x, x}
    // address bank indices {x, x, x, x, x}
    unit::assert
        ( tag
        , test_case_schedule_read_requests
              ( {0, 0, 1, 2, 3}
              , even_odd_bank
              , 2
              , {{false, 0}, {false, 0}}
              , {{false, 0}, {false, 0}, {false, 0}, {false, 0}, {false, 0}}
              )
        );
}

template <auto Banks, typename Address, auto Addresses>
inline bool test_case_schedule_write_requests
    ( optional<Address>[Addresses] addresses
    , (Address) -> index_t<Banks> address_to_bank_index
    , index_t<Addresses> iteration
    , optional<index_t<Addresses>>[Banks] expected
    )
{
    auto actual = schedule_write_requests(addresses, address_to_bank_index, iteration);
    println(actual);
    return equal_by(O::equal<index_t<Addresses>>, actual, expected);
}

inline void test_schedule_write_requests(unit::tag_t tag)
{
    // 1 bank
    // const map address to bank 0
    // 0th iteration
    // expected:
    // bank address indices {x}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{false, 0}}
              , const_bank<1, 0, uint1>
              , 0
              , {{false, 0}}
              )
        );

    // 1 bank
    // const map address to bank 0
    // 0th iteration
    // expected:
    // bank address indices {0}
    // address bank indices {0}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}}
              , const_bank<1, 0, uint1>
              , 0
              , {{true, 0}}
              )
        );

    // 1 bank
    // const map address to bank 0
    // 0th iteration
    // expected:
    // bank address indices {0, x}
    // address bank indices {0, x}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}, {true, 1}}
              , const_bank<1, 0, uint1>
              , 0
              , {{true, 0}}
              )
        );

    // 1 bank
    // const map address to bank 0
    // 1st iteration
    // expected:
    // bank address indices {1}
    // address bank indices {x, 0}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}, {true, 1}}
              , const_bank<1, 0, uint1>
              , 1
              , {{true, 1}}
              )
        );

    // 2 banks
    // map even addresses to 0, odd to 1 bank.
    // 0th iteration
    // expected:
    // bank address indices {0, 1}
    // address bank indices {0, 1, x, x}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}, {true, 1}, {true, 2}, {true, 3}}
              , even_odd_bank
              , 0
              , {{true, 0}, {true, 1}}
              )
        );

    // 2 banks
    // map even addresses to 0, odd to 1 bank.
    // 1st iteration
    // expected:
    // bank address indices {2, 3}
    // address bank indices {x, x, 0, 1}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}, {true, 1}, {true, 2}, {true, 3}}
              , even_odd_bank
              , 1
              , {{true, 2}, {true, 3}}
              )
        );

    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{false, 0}, {true, 0}}
              , const_bank<1, 0, uint1>
              , 0
              , {{true, 1}}
              )
        );

    // 2 banks
    // map even addresses to 0, odd to 1 bank.
    // 2 requests for bank 0 (same address)
    // 2 requests for bank 1 (different addresses)
    // 1st iteration
    // expected:
    // bank address indices {0, 2}
    // 2nd iteration
    // expected:
    // bank address indices {1, 3}
    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}, {true, 0}, {true, 1}, {true, 3}}
              , even_odd_bank
              , 0
              , {{true, 0}, {true, 2}}
              )
        );

    unit::assert
        ( tag
        , test_case_schedule_write_requests
              ( {{true, 0}, {true, 0}, {true, 1}, {true, 3}}
              , even_odd_bank
              , 1
              , {{true, 1}, {true, 3}}
              )
        );
}

template <auto Banks>
inline index_t<Banks> mod_mapping(uint8 address)
{
    return address % Banks;
}

// Test that each unique address is accessed exactly once given random input addresses.
// Address type is uint8.
template <auto Banks, auto Addresses>
class RandomReadScheduleTest
{
private:
    lfsr_random<8> random_address_generator;
public:
    bool run()
    {
        print("random_read_schedule_test start\n");

        // generate random addresses
        uint8[Addresses] random_addresses;

        for (const auto i : Addresses)
            random_addresses[i] = random_address_generator.next({});

        print("Addresses:\n");
        println(random_addresses);

        print("Banks:\n");
        print("{map(mod_mapping<Banks>, random_addresses)}\n");

        // set address array to zero counts.
        count_t<Addresses>[Addresses] access_count;
        static for(const auto i : Addresses)
            access_count[i] = 0;

        // Calculate each iteration of the schedule; tally indices.
        for (const auto iteration : Addresses)
        {
            auto output = schedule_read_requests(random_addresses, mod_mapping<Banks>, iteration);
            print("\n");
            print("iteration:  {iteration}\n");
            print("address_indices:\n");
            println(output.first);
            print("bank_indices:\n");
            println(output.second);

            // tally accesses
            for (const auto a : Addresses)
                if (output.second[a].is_valid)
                    access_count[a]++;
        }

        // ensure all addresses are accessed only once.
        print("access_count:\n");
        println(access_count);

        bool success = all([](count_t<Addresses> c){ return c == 1; }, access_count);

        print("random_read_schedule_test complete\n");
        return success;
    }
}

class test_random_read_schedule
{
private:
    RandomReadScheduleTest<4, 10> random_read_schedule_test_4_10;
    RandomReadScheduleTest<8, 20> random_read_schedule_test_8_20;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, random_read_schedule_test_4_10.run());
        unit::assert(tag, random_read_schedule_test_8_20.run());
    }
}

inline void test_main()
{
    print("data.memory.bank.schedule test start\n");

    unit::test<1>(test_read_requests_per_bank);
    unit::test<2>(test_write_requests_per_bank);
    unit::test<3>(test_schedule_read_requests);
    unit::test<4>(test_schedule_write_requests);
    unit::test<5>(unit::fixture<test_random_read_schedule>());

    print("data.memory.bank.schedule test complete\n");
}
