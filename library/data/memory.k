// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Common memory aliases.
module data.memory
    { memory
    , memory_norep
    , memory_init
    , ecc
    , memory_ecc
    , memory_ecc_norep
    , memory_ecc_strict
    , memory_ecc_relaxed
    }

import data.optional

template <typename T, auto N>
using memory = [[memory]] T[N];

template <typename T, auto N>
using memory_norep = [[memory, non_replicated]] T[N];

template <typename T, auto N>
using memory_init = [[memory, initialize]] T[N];

//| Type returned by reads from `memory_ecc` of type `T`
template <typename T>
struct ecc
{
    //| True when an ECC error was detected, regardless of whether it was
    // corrected or not.
    bool error;

    //| Value read from the memory, valid if there was no ECC error, or an
    // error was corrected.
    optional<T> data;
}

template <typename T>
inline ecc<T> make_ecc(bool error, bool valid, T value)
{
    return {error, make_optional(valid, value)};
}

//| Alias for error correction code (ECC) memory of type `T` and depth of `N`.
// Reads from the memory return values of type `ecc<T>`.
// Writes to the memory take values of type `T`.
template <typename T, auto N>
using memory_ecc = [[memory, ecc(make_ecc<T>)]] T[N];

//| Alias for non-replicated, ECC memory of type `T` and depth of `N`.
// Reads from the memory return values of type `ecc<T>`.
// Writes to the memory take values of type `T`.
template <typename T, auto N>
using memory_ecc_norep = [[memory, non_replicated, ecc(make_ecc<T>)]] T[N];

template <typename T>
inline optional<T> make_optional_from_ecc_error(bool error, bool valid, T value)
{
    assert(valid || error);
    return make_optional(!error, value);
}

//| Alias for ECC memory of type `T` and depth of `N`.
// Reads from the memory return values of type `optional<T>` that is valid
// if no ECC error, even a correctable one, was detected.
// Writes to the memory take values of type `T`.
template <typename T, auto N>
using memory_ecc_strict = [[memory, ecc(make_optional_from_ecc_error<T>)]] T[N];

template <typename T>
inline optional<T> make_optional_from_ecc_valid(bool error, bool valid, T value)
{
    return make_optional(valid, value);
}

//| Alias for ECC memory of type `T` and depth of `N`.
// Reads from the memory return values of type `optional<T>` that is valid
// if there was no ECC error, or an error was corrected.
// Writes to the memory take values of type `T`.
template <typename T, auto N>
using memory_ecc_relaxed = [[memory, ecc(make_optional_from_ecc_valid<T>)]] T[N];