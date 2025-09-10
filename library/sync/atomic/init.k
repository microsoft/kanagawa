// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module sync.atomic.init
    { init_once
    , init_generational
    }

import data.function
import sync.atomic

//| Holds a boolean state which is initially set to `false` and flipped to `true`
// by the first call to `check` method.
class init_once
{
public:
    //| The first call returns `false` and all subsequent calls return `true`.
    bool check()
    {
        return first(atomically(constant(true)));
    }
}

//| Amortize the cost of initializing a set of variables by assigning a
//  generation ID to each variable.  A variable is considered uninitialized if
//  the associated generation ID does not match the current generation ID.
//  Per-variable generation IDs only need to be initialized when the current
//  generation ID overflows.
//
//  The returned `pair` `first` field is true if the generation ID has
//  overflowed and an explicit initialization to a generation ID of 0 is required.
//
//  The returned `pair` `second` field is the generation ID to compare against.
//
//  `reset` must be true on the first call to this function.
template
    < auto Width //< Number of bits in a generation ID.
                 // Higher values result in fewer initializations.
    >
inline pair<bool, uint<Width>> init_generational(bool reset)
{
    using generation_id_t = uint<Width>;

    pair<bool, uint<Width>> result;

    result.second = second(atomically([reset](generation_id_t prev)
    {
        const generation_id_t highest_id = (1 << Width) - 1;

        // reset must be true on the first call to this function
        // That is the only time when prev will be 0
        assert(reset || (prev != 0));

        generation_id_t new = prev;

        if (reset)
        {
            // Never return 0, skip from highest_id to 1
            new = (prev == highest_id) ? 1 : prev + 1;
        }

        return new;
    }));

    // Initialization to 0 is required when reset is true
    // and the new generation is 1.
    // This occurs when either wrapping from highest_id
    // or when incrementing from the default value of 0
    result.first = reset && (result.second == 1);

    return result;
}