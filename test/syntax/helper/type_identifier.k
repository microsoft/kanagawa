// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.type_identifier
  { C
  , D
  , Outer
  }

template <typename T>
class C
{
    T x = T::One;
}

template <typename T>
class D
{
    T::X x;
}

class Outer
{
public:
    class Inner
    {
        using alias = uint32;
    }
}
