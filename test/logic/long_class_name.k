// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Test to ensure that generated SV does not emit identifiers with very long names
// that EDA tools will not accept.
template<auto N>
class helper
{
public:
    // noinline to test long function & basic block names
    noinline uint32 func()
    {
        uint32 result = _member_variable;

        static if (N > 0)
        {
            result += _inner_with_a_very_long_name_inner_with_a_very_long_name_inner_with_a_very_long_name.func();

            result += _mem[N];
        }

        return result;
    }

    void run(unit::tag_t tag)
    {
        unit::assert(tag, func() >= N);
    }

private:
    // to test names of shared variables
    uint32 _member_variable = N;
    memory<uint32, 512> _mem = {};

    static if (N > 0)
    {
        helper<N - 1> _inner_with_a_very_long_name_inner_with_a_very_long_name_inner_with_a_very_long_name;
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<helper<256>>());
}
