// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.array
import .options as opt
import test.unit as unit
import test.runner

// A async function with 2 call sites that has no parameters nor return values
class TwoCallSiteVoidTestCase
{
private:
    uint32 _twoCallSiteVoidTestCount = 0;

    [[async]] void Helper()
    {
        atomic
        {
            _twoCallSiteVoidTestCount++;
        }
    }
    
public:
    void run(unit::tag_t tag)
    {
        Helper();
    
        Helper();
    
        atomic do; while(_twoCallSiteVoidTestCount != 2);
    }
}

template<auto CallSiteCount>
class FairnessTest
{
private:
    uint32[CallSiteCount] _call_count = {};

    void Inc(index_t<CallSiteCount> call_site_index)
    {
        atomic
        {
            _call_count[call_site_index]++;
        }
    }

public:
    void Test(unit::tag_t tag)
    {
        // Asynchronously call Inc from multiple call sites
        static for (const auto call_site_index : CallSiteCount)
        {
            async_exec([call_site_index]()
            {
                pipelined_for (1024, [call_site_index](uint32 _)
                {
                    Inc(call_site_index);
                });
            });
        }

        // Wait for a while
        uint64 start_time = cycles();
        atomic do; while(cycles() <= (start_time + 100));

        // Read the call site counters
        uint32[CallSiteCount] call_count = _call_count;

        uint32 min_count = minimum(call_count);
        uint32 max_count = maximum(call_count);

        uint32 diff = max_count - min_count;
        print("call_count {call_count} min_count {min_count} max_count {max_count} diff {diff}\n");

        const uint32 max_diff = (opt::optimize > 0) ? 8 : 20;
        unit::assert(tag, diff < max_diff);
    }
}

template<auto CallSiteCount>
inline void TestFairness(unit::tag_t tag)
{
    static FairnessTest<CallSiteCount> _test;
    _test.Test(tag);
}

inline void test_main()
{
    unit::test<1>(unit::fixture<TwoCallSiteVoidTestCase>());

    print("MultiCallSiteFairnessTest\n");
    unit::test<2>(TestFairness<2>);
    unit::test<3>(TestFairness<3>);
    unit::test<4>(TestFairness<4>);
    unit::test<5>(TestFairness<5>);
}
