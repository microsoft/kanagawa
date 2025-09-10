// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.external_class { module helper.external_class }

import test.unit as unit

class Adder
{
    [[latency(4)]] uint16 Add(uint16 a, uint16 b);
}

extern Adder;

class AdderWrapper
{
private:
    Adder _adder;

    void test_case(unit::tag_t tag, index_t<8> i)
    {
        uint16 a = i + 10;
        uint16 b = i + 20;

        uint16 result = _adder.Add(a, b);

        uint16 expected_sum = a + b;
        unit::assert_equal(tag, expected_sum, result);
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for (8, [tag](index_t<8> tid)
        {
            test_case(tag, tid);
        });
    }
}

