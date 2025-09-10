// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Count and schedule memory bank conflicts.
module data.memory.bank.schedule
    { read_requests_per_bank
    , write_requests_per_bank
    , schedule_read_requests
    , schedule_write_requests
    }

import data.array
import data.optional
import data.tuple
import data.vector as vec
import numeric.int.operator

// Validate all bank indices are less than Banks.
// In the case where Banks=1, index_t<Banks> is uint<1> and could have the value 1.
template <auto Banks, typename Address, auto Addresses>
inline bool validate_bank_indices
    ( optional<Address>[Addresses] addresses
    , (Address) -> index_t<Banks> address_to_bank_index
    )
{
    return all
        ( [address_to_bank_index](optional<Address> address)
          {
              return !address.is_valid || address_to_bank_index(address.value) < Banks;
          }
        , addresses
        );
}


template <auto Banks, auto Addresses>
inline count_t<Addresses>[Banks] reduce_counts
    ( count_t<Addresses>[Banks] a
    , count_t<Addresses>[Banks] b
    )
{
    return zip_with(add<count_t<Addresses>, count_t<Addresses>>, a, b);
}


// Example:
// 3 Banks, 5 addresses per call.
// unique_addresses =  {0, x, 3, 2, 1}
// bank_indices     =  {0, 0, 2, 1, 1}
// -----------------------------------
// mask bank 0      =  {1, 0, 0, 0, 0}
// mask bank 1      =  {0, 0, 0, 1, 1}
// mask bank 2      =  {0, 0, 1, 0, 0}
// -----------------------------------
// requests_per_bank = {1, 2, 1}
template <auto Banks, typename Address, auto Addresses>
inline count_t<Addresses>[Banks] requests_per_bank
    ( optional<Address>[Addresses] addresses
    , (Address) -> index_t<Banks> address_to_bank_index
    )
{
    assert(validate_bank_indices<Banks>(addresses, address_to_bank_index));

    return map_reduce
        ( [address_to_bank_index](optional<Address> address)
          {
              count_t<Addresses>[Banks] c;
              c[address_to_bank_index(address.value)] = reinterpret_cast<count_t<Addresses>>(address.is_valid);
              return c;
          }
        , reduce_counts<Banks, Addresses>
        , addresses
        );
}


//| Count the number of unique read requests for each bank.
//  More than 1 request per bank indicates conflict.
template
    < auto Banks
    , typename Address
    , auto Addresses   //< Number of addresses processed per call.
    >
inline count_t<Addresses>[Banks] read_requests_per_bank
    ( Address[Addresses] addresses                      //< Input addresses, duplicates allowed.
    , (Address) -> index_t<Banks> address_to_bank_index //< Map `Address` to bank index.
    )
{
    return requests_per_bank(unique(addresses), address_to_bank_index);
}

//| Count the number of write requests for each bank.
//  More than 1 request per bank indicates conflict.
template
    < auto Banks
    , typename Address
    , auto Addresses   //< Number of addresses processed per call.
    >
inline count_t<Addresses>[Banks] write_requests_per_bank
    ( optional<Address>[Addresses] addresses            //< Input addresses, valid duplicates __not__ allowed.
    , (Address) -> index_t<Banks> address_to_bank_index //< Map `Address` to bank index.
    )
{
    return requests_per_bank(addresses, address_to_bank_index);
}


template
    < auto Banks
    , typename Address
    , auto Addresses   //< Number of addresses processed per call.
    >
inline optional<index_t<Addresses>>[Banks] schedule_requests
    ( optional<Address>[Addresses] unique_addresses
    , (Address) -> index_t<Banks> address_to_bank_index
    , index_t<Addresses> iteration
    )
{
    assert(validate_bank_indices(unique_addresses, address_to_bank_index));

    // construct conflict table.
    auto table = zip_with
        ( [address_to_bank_index](optional<Address> address, index_t<Addresses> index) -> tuple3<bool, index_t<Banks>, index_t<Addresses>>
          {
              return {address.is_valid, address_to_bank_index(address.value), index};
          }
        , unique_addresses
        , indices<Addresses>()
        );

    // Calculate iteration_address_indices. For each bank b:
    // 1. Identify valid requests which map to bank index b.
    // 2. Gather valid request.
    // 3. Retrieve iteration-ith request.
    return map
        ( [table, iteration](index_t<Banks> b)
          {
              auto index_table = unzip_with
                  ( [b](tuple3<bool, index_t<Banks>, index_t<Addresses>> p)
                    {
                        return make_pair(p.first && p.second == b, p.third);
                    }
                  , table
                  );
              auto gathered_table = vec::gather<index_t<Addresses>, Addresses>(index_table.first, index_table.second);
              return make_optional(iteration < gathered_table.size, gathered_table.data[iteration]);
          }
        , indices<Banks>()
        );
}

//| For each iteration, calculate the pair:
//
//  1. The optional address indices for each bank.
//  2. The optional bank index for each address.
template
    < auto Banks
    , typename Address
    , auto Addresses   //< Number of addresses processed per call.
    >
inline pair<optional<index_t<Addresses>>[Banks], optional<index_t<Banks>>[Addresses]> schedule_read_requests
    ( Address[Addresses] addresses                      //< Input addresses, duplicates allowed.
    , (Address) -> index_t<Banks> address_to_bank_index //< Map `Address` to bank index.
    , index_t<Addresses> iteration                      //< Index of bank requests to retrieve from schedule.
    )
{
    // Example:
    // 3 Banks, 5 addresses per call.
    // unique_addresses =  {0, x, 3, 2, 1}
    // bank_indices     =  {0, 0, 2, 1, 1}
    // -----------------------------------
    // table = { {true,  0, 0}
    //         , {false, 0, 1}
    //         , {true,  2, 2}
    //         , {true,  1, 3}
    //         , {true,  1, 4}
    //         }
    // ----------------------------------
    // masked_table bank 0 = { {true,  0}
    //                       , {false, 1}
    //                       , {false, 2}
    //                       , {false, 3}
    //                       , {false, 4}
    //                       }
    // masked_table bank 1 = { {false, 0}
    //                       , {false, 1}
    //                       , {false, 2}
    //                       , {true,  3}
    //                       , {true,  4}
    //                       }
    // masked_table bank 2 = { {false, 0}
    //                       , {false, 1}
    //                       , {true,  2}
    //                       , {false, 3}
    //                       , {false, 4}
    //                       }
    // ------------------------------------
    // gathered_table bank 0 = { {true,  0}
    //                         , {false, 1}
    //                         , {false, 2}
    //                         , {false, 3}
    //                         , {false, 4}
    //                         }
    // gathered_table bank 1 = { {true,  3}
    //                         , {true,  4}
    //                         , {false, 0}
    //                         , {false, 1}
    //                         , {false, 2}
    //                         }
    // gathered_table bank 2 = { {true,  2}
    //                         , {false, 0}
    //                         , {false, 1}
    //                         , {false, 3}
    //                         , {false, 4}
    //                         }
    // ------------------------------------
    // iteration 0:
    // iteration_address_indices = {0, 3, 2}
    // iteration_bank_indices    = {0, x, 2, 1, x}
    //
    // iteration 1:
    // iteration_address_indices = {x, 4, x}
    // iteration_bank_indices    = {x, x, x, x, 1}

    optional<Address>[Addresses] unique_addresses = unique(addresses);
    auto iai = schedule_requests(unique_addresses, address_to_bank_index, iteration);

    // Calculate iteration_bank_indices.
    // For each address a:
    // Return true if any element a' of iteration_address_indices is valid
    // and addresses[a'.value] == a.
    // Finally, zip the valids together with the bank_indices into an optional.
    bool[Addresses] valid_addresses = map
        ( [iai, addresses](Address address)
          {
              return any
                  ( [addresses, address](optional<index_t<Addresses>> iteration_address_index)
                    {
                        return iteration_address_index.is_valid && addresses[iteration_address_index.value] == address;
                    }
                  , iai
                  );
          }
        , addresses
        );
    auto bank_indices = map(address_to_bank_index, addresses);
    auto iteration_bank_indices = zip_with(make_optional<index_t<Banks>>, valid_addresses, bank_indices);
    return make_pair(iai, iteration_bank_indices);
}


//| Calculate the optional address indices for each bank at specific iteration.
template
    < auto Banks
    , typename Address
    , auto Addresses   //< Number of addresses processed per call.
    >
inline optional<index_t<Addresses>>[Banks] schedule_write_requests
    ( optional<Address>[Addresses] addresses            //< Input addresses, valid duplicates __not__ allowed.
    , (Address) -> index_t<Banks> address_to_bank_index //< Map `Address` to bank index.
    , index_t<Addresses> iteration                      //< Index of bank requests to retrieve from schedule.
    )
{
    return schedule_requests(addresses, address_to_bank_index, iteration);
}