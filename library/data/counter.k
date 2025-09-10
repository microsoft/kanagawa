// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.counter
    { counter
    }

import type.coerce
import type.stdtype

//| A thread-safe counter that can be incremented in one location and decremented in another.
template
    < auto Maximum //< The maximum count value.
    , auto Initial = 0 //< The initial value of the counter.
    >
class counter
{
public:
    using ctr_t = count_t<Maximum>;

private:
    ctr_t _first = Initial;
    ctr_t _second = 0;

public:

    //| Reset the counter to the initial state.
    inline void reset()
    {
        atomic
        {
            _first = Initial;
            _second = 0;
        }
    }

    //| Return the current count.
    inline ctr_t count()
    {
        ctr_t result;

        atomic
        {
            result = static_cast(_first - _second);
        }

        return result;
    }

    //| Returns `count() == 0`.
    // Optimized to reduce the number of resources required.
    inline bool is_zero()
    {
        bool result;

        atomic
        {
            result = _first == _second;
        }

        return result;
    }

    //| Add one to the counter.
    inline void increment()
    {
        add(1);
    }

    //| Add amount to the counter.
    inline void add(ctr_t amount)
    {
        atomic
        {
            _first = static_cast(_first + amount);
        }
    }

    //| Subtract one from the counter.
    inline void decrement()
    {
        subtract(1);
    }

    //| Subtract amount from the counter.
    inline void subtract(ctr_t amount)
    {
        atomic
        {
            _second = static_cast(_second + amount);
        }
    }
}