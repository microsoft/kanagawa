// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Exported classes whose ports use different sets of named types.
// Each export is compiled into its own .sv file, and the typedefs each one
// needs must survive in its own SystemVerilog package (see issue #138).
//
// Both shapes that hit the bug are covered: distinct classes using distinct
// named types, and instantiations of one class template, which is how the
// issue was originally reported. The template case matters because every
// instantiation produces a same-named type of a different width, so a design
// that drops one file's declarations silently references the wrong-width type.

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

// Two instantiations of one template. Each produces its own 'TemplatePayload'
// of a different width, so both must be declared in their own package.
template <auto N>
struct TemplatePayload
{
    uint<N> data;
    Header hdr;
}

template <auto N>
class TemplateSender
{
public:
    TemplatePayload<N> Send(TemplatePayload<N> p)
    {
        return p;
    }
}

using TemplateSender8 = TemplateSender<8>;
using TemplateSender64 = TemplateSender<64>;

export SmallSender;
export BigSender;
export TemplateSender8;
export TemplateSender64;
