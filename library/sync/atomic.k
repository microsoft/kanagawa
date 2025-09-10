// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module sync.atomic
    { atomically
    , atomically_one_of
    , first
    , second
    }

import data.tuple
import type.stdtype

//| Atomically apply user specified function to internal state of type `T`,
// returning both old and new state value. Note that a state is maintained
// per each call site of this inline function.
template
    < typename T            //< Type of the internal state
    , T InitialValue = {}   //< Initial value
    >
inline pair<T, T> atomically((T) -> T modify)
{
    pair<T, T> result;

    atomic
    {
        static T state = InitialValue;

        result = {state, modify(state)};
        state = result.second;
    }

    return result;
}

//| Atomically interleave updates among multiple threads to internal state of
// type `T[N]`. Return both old and new element value. A state is maintained
// per each call site of this inline function.
template
    < auto N              //< Size of state array. Must be between 2 and 8 inclusive.
    , typename T          //< Type of the internal state.
    , T InitialValue = {} //< Initial value.
    >
inline pair<T, T> atomically_one_of(index_t<N> i, (T) -> T modify)
{
    static assert(N >= 2 && N <= 8);

    pair<T, T> result;

    [[schedule(N)]]
    {
        // TODO: fix when generic initializer syntax is available
        static if (N == 2)
        {
            static T[N] state = {InitialValue, InitialValue};
        }
        else static if (N == 3)
        {
            static T[N] state = {InitialValue, InitialValue, InitialValue};
        }
        else static if (N == 4)
        {
            static T[N] state = {InitialValue, InitialValue, InitialValue, InitialValue};
        }
        else static if (N == 5)
        {
            static T[N] state = {InitialValue, InitialValue, InitialValue, InitialValue, InitialValue};
        }
        else static if (N == 6)
        {
            static T[N] state = {InitialValue, InitialValue, InitialValue, InitialValue, InitialValue, InitialValue};
        }
        else static if (N == 7)
        {
            static T[N] state = {InitialValue, InitialValue, InitialValue, InitialValue, InitialValue, InitialValue, InitialValue};
        }
        else static if (N == 8)
        {
            static T[N] state = {InitialValue, InitialValue, InitialValue, InitialValue, InitialValue, InitialValue, InitialValue, InitialValue};
        }

        result = {state[i], modify(state[i])};
        state[i] = result.second;
    }

    return result;
}