// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Finite state machine helpers.

FSMs can be tricky to implement in a manner that results in high clock frequency
and high throughput (input element processed per thread).

Consider a FSM implemented with a function `f` which can compute a
new state given a previous state and a new input value:

    atomic
    {
        new_state = f(prev_state, input);
    }

    // computation of FSM output values can occur
    // after the atomic block.

If `f` is complex, then this atomic block can be the critical path for the whole
design.  Additionally this can process at most one state transition per clock
cycle.  A design that can process two transitions per clock cycle is:

    atomic
    {
        new_state = f(f(prev_state, input1), input2);
    }

however, that design has an even longer critical path.

If the total number of possible states is low (say 8 in this example), then one
can precompute the output of `f` for all possible states.  This
can be done before the atomic block:

    State[8] table;

    static for (const auto i : 8)
    {
        table[i] = f(f(cast<State>(i), input1), input2);
    }

    atomic
    {
        new_state = table[cast<uint3>(prev_state)];
    }

This trades area for throughput and frequency because it invokes `f` 8 times more
than the naive design.  However, the critical path is simply an
8:1 mux, no matter how complex `f` is.  Additionally, the critical path length
does not increase with the number of input elements which are processed per
cycle.

Functions in this module contain abstractions useful for implementing this
design pattern.
*/
module control.fsm
    { apply_update
    , speculate_updates
    }

import data.array
import data.optional

//| Speculatively apply a state update to each possible current state and return all results.
template
    < auto StateCount               //< Total number of possible states.
    , typename SpeculativeUpdate    //< The output type of `fn`.
    >
inline auto speculate_updates
    ( (index_t<StateCount>) -> SpeculativeUpdate fn  //< Function which is called `StateCount` times with values equal to either:
                                                     // `0, 1, 2, ..., StateCount-1`
                                                     // or
                                                     // `override.value, override.value, ..., override.value`
    , optional<index_t<StateCount>> override         //< If valid, then all returned results are equal to `fn(override.value)`.
                                                     // Useful for cases where downstream code should behave as if it ignores the "current" state.
    )
{
    return map_indices([override, fn](index_t<StateCount> i)
        {
            return fn(from_optional(i, override));
        });
}

//| Returns a closure which:
//
// * Aggregates a set of pre-computed state updates into a final state update based on a concrete current state value.
// * Computes a new state value based on the current state and the aggregated state update.
//
// The returned closure can be passed to functions like `atomically`.
template
    < auto StateCount                                   //< The number of pre-computed state updates.
    , typename State                                    //< A type that represent the current state.
    , typename SpeculativeUpdate                        //< A type that represents a pre-computed update to apply to the state.
    , typename AggregatedUpdate = SpeculativeUpdate     //< A type that represents an aggregated update to apply to the state.
    >
inline auto apply_update
    ( (SpeculativeUpdate[StateCount], State) -> AggregatedUpdate aggregate  //< Function which computes a final state update to apply,
                                                                            // based on the current state and an array of pre-computed states updates.
    , (AggregatedUpdate, State) -> State apply                              //< Function which applies a state update.
    , SpeculativeUpdate[StateCount] updates                                 //< The array of speculatively pre-computed updates.
    )
{
    return [updates, apply, aggregate](State prev)
        {
            return apply(aggregate(updates, prev), prev);
        };
}