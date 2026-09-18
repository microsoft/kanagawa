// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Two exported classes whose ports use different sets of named types.
// Each export is compiled into its own .sv file, and the typedefs each one
// needs must survive in its own SystemVerilog package (see issue #138).

enum Op : uint2
{
    Read,
    Write
}

struct Header
{
    Op op;
    uint16 len;
}

struct SmallPayload
{
    uint8 data;
    Header hdr;
}

struct BigPayload
{
    uint64 data;
    Header hdr;
}

class SmallSender
{
public:
    SmallPayload Send(SmallPayload p)
    {
        return p;
    }
}

class BigSender
{
public:
    BigPayload Send(BigPayload p)
    {
        return p;
    }
}

export SmallSender;
export BigSender;
