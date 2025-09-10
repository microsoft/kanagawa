// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Tests for exported class functionality

// Testing protocol attributes like [[no_backpressure]] functions/callbacks on an exported class
class ProtocolAttributesClass
{
    [[no_backpressure, async]] (uint32 a)->void OneMore;

    [[latency(0)]] (uint32 a, uint5 amt)->uint32 ShiftLeftCallback;

public:
    [[no_backpressure]] uint32 PlusFour(uint32 x)
    {
        OneMore(x + 1);
        return x + 4;
    }

    [[latency(0)]] uint32 ShiftLeftOne(uint32 x)
    {
        return ShiftLeftCallback(x, 1);
    }
}

export ProtocolAttributesClass;
