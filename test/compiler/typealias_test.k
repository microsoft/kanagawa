// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Test that named struct and enum types are emitted as hw.typealias
// in the CIRCT IR output, both on core module ports and ESI wrapper ports.

struct Point
{
    uint16 x;
    uint16 y;
}

struct Line
{
    Point start;
    Point end;
}

enum Color : uint8
{
    Red,
    Green,
    Blue
}

class TypeAliasTestModule
{
public:
    // Exported function with a named struct parameter and return
    Point TranslatePoint(Point p, uint16 dx, uint16 dy)
    {
        Point result = { cast<uint16>(p.x + dx), cast<uint16>(p.y + dy) };
        return result;
    }

    // Exported function with nested named struct parameter
    Line TranslateLine(Line l, uint16 dx, uint16 dy)
    {
        Point s = { cast<uint16>(l.start.x + dx), cast<uint16>(l.start.y + dy) };
        Point e = { cast<uint16>(l.end.x + dx), cast<uint16>(l.end.y + dy) };
        Line result = { s, e };
        return result;
    }

    // Exported function with enum type
    uint8 GetColorValue(Color c)
    {
        return cast<uint8>(c);
    }
}

export TypeAliasTestModule;
