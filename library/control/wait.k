// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module control.wait
    { wait
    , wait_until
    }

import .options as opt
import type.coerce
import type.stdtype

//| Block the calling thread until a condition defined by the function `fn`
// is satisfied. The function `fn` is checked atomically in each cycle.
//
// When `T` is `bool`, the thread is unblocked when `fn` returns `true`.
// Otherwise, if `T` is an instance of `optional`, the thread is unblocked
// when `fn` returns a valid `optional`. In this case, `wait` returns
// the value inside the `optional`.
//
// The function `fn` may have side effects but it must not access memory.
// The more general `wait_until` function allows memory access within `fn`
// however it is more costly in terms of area and throughput.
//
// #### __Example__
//
//     // Wait for the global variable `g_var` is greater than 16
//     wait([]()
//     {
//         return g_var > 16;
//     });
//
//     // Wait for `g_var` is greater than 16, and retrieve the value of `g_var`
//     uint32 result = wait([]()
//     {
//         return make_optional(g_var > 16, g_var);
//     });
//
template <typename T>
inline auto wait(() -> T fn)
{
    static if (T == bool)
    {
        atomic do {} while (!fn())
    }
    else
    {
        T result;
        atomic do
        {
            result = fn();
        } while (!result.is_valid)
        return result.value;
    }
}

//| Block the calling thread until a condition defined by the function `fn`
// is satisfied. The function `fn` is checked atomically in each cycle.
//
// When `T` is `bool`, the thread is unblocked when `fn` returns `true`.
// Otherwise, if `T` is an instance of `optional`, the thread is unblocked
// when `fn` returns a valid `optional`. In this case, `wait_until` returns
// the value inside the `optional`.
//
// The function `fn` may have side effects and may access memory.
// The function is more costly than `wait` in terms of area and as the
// percentage of calls to `fn` which return false increases, throughput
// of `wait_until` decreases more than `wait`.
//
// #### __Example__
//
//     wait_until([x]()
//     {
//         return mem[2] > x;
//     });
//
template <typename T>
inline auto wait_until(() -> T fn)
{
    // Type to express a unique identifier of a thread within a function
    // This type is sufficient to give each thread within a function instance
    // a unique ID.
    using thread_index_within_function_t = index_t<opt::max_threads_limit>;

    // Assign a unique ID to this thread
    thread_index_within_function_t thread_id;

    atomic
    {
        static thread_index_within_function_t _enter_thread_id = 0;

        thread_id = _enter_thread_id;

        _enter_thread_id = static_cast(_enter_thread_id + 1);
    }

    // Loop until condition satisfied
    bool continuing_looping = true;

    T result;

    // Loop body ensures ordering except in the case where wait_until is predicated
    [[reorder_by_looping]]
    do
    {
        // Hint to the compiler that these variables are not live at the start of the loop
        continuing_looping = true;

        result = {};

        atomic
        {
            // Check to see if this thread is at the head of the line
            static thread_index_within_function_t _expected_thread_id = 0;

            bool head_of_line = (_expected_thread_id == thread_id);

            if (head_of_line)
            {
                result = fn();

                static if (T == bool)
                {
                    continuing_looping = !result;
                }
                else
                {
                    continuing_looping = !result.is_valid;
                }

                // increment _expected_thread_id when the condition is satisfied
                _expected_thread_id = static_cast(_expected_thread_id + cast<uint1>(!continuing_looping));
            }
        }
    } while (continuing_looping);

    static if (T != bool)
    {
        return result.value;
    }
}
