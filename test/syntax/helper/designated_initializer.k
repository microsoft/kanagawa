// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.designated_initializer
  { Foo
  , Bar
  , Variant
  , Class
  , XY
  }

struct Foo
{
    uint32 field1;
    bool field2;
}

struct Bar
{
    Foo foo;
    Foo[2] foos;
    uint3 x;
}

union Variant
{
    uint32 x;
    bool y;
}

class Class
{
public:
    const uint32 fifo_depth = 32;
    
private:
    uint32 index;
}

struct X
{
    int32 x;
}

struct Y
{
    X x;
}

struct XY
{
    X x;
    Y y;
}
