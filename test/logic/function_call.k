// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// [[max_threads(1)]] function calls an inline function, which calls an [[unordered]] function
class InlineCallsUnordered
{
public:
    [[unordered]]
    void Go()
    {
    }

    inline void Fn()
    {
        Go();
    }

    [[max_threads(1)]]
    void run(unit::tag_t tag)
    {
        Fn();
    }
}

// [[no_backpressure]] function in pure Kanagawa code
class NoBackpressure
{
private:
    uint32 _value = 0;

    [[async]] [[no_backpressure]] void Foo(uint32 x)
    {
        _value = x;
    }

public:
    void run(unit::tag_t tag)
    {
        pipelined_for(8, [tag](index_t<8> i)
        {
            Foo(i);

            wait([i]()
            {
                return _value >= i;
            });

            unit::assert(tag, _value >= i);
        });
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<InlineCallsUnordered>());
    unit::test<2>(unit::fixture<NoBackpressure>());
}
