// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import control.async.rendezvous.strict
import data.array
import sync.atomic
import test.unit as unit
import test.runner

class rendezvous_test
{
private:
    uint32[4] _rendezvous_process_count = {};

    rendezvous<pair<uint32, uint32>, uint32, 4> _rendezvous;
    bool[4] _rendezvous_complete = {};

    template<auto Index, auto Iterations>
    void rendezvous_test_helper(unit::tag_t tag)
    {
        async_exec([tag]
        {
            pipelined_for(Iterations, [tag](index_t<Iterations> tid)
            {
            uint32 result = _rendezvous.join<Index>(make_pair<uint32, uint32>(tid, Index));

            unit::assert_equal(tag, tid * 7, result);
            });

            unit::assert_equal(tag, Iterations, _rendezvous_process_count[Index]);

            _rendezvous_complete[Index] = true;
        });
    }

public:
    void run(unit::tag_t tag)
    {
        _rendezvous.start([tag](pair<uint32, uint32>[4] requests)
        {
            uint32[4] result;

            static for (const auto i : 4)
            {
                // second should be the index of the requesting thread
                unit::assert_equal(tag, i, requests[i].second);

                // first should be the thread id
                uint32 expected_tid = first<uint32, uint32>(atomically([](uint32 prev) -> uint32
                {
                    return prev + 1;
                }));

                unit::assert_equal(tag, expected_tid, requests[i].first);

                result[i] = requests[i].first * 7;

                atomic
                {
                    _rendezvous_process_count[i]++;
                }
            }

            return result;
        });

        rendezvous_test_helper<0, 1000>(tag);
        rendezvous_test_helper<1, 1000>(tag);
        rendezvous_test_helper<2, 1000>(tag);
        rendezvous_test_helper<3, 1000>(tag);

        wait([]()
        {
            return and(_rendezvous_complete);
        });
    }
}

inline void test_main()
{
    print("control.async.rendezvous.strict test start\n");

    unit::test<1>(unit::fixture<rendezvous_test>());

    print("control.async.rendezvous.strict test finish\n");
}
