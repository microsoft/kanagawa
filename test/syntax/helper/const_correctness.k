// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.const_correctness
  { S
   ,C
  }

struct S
{
  uint32 x;
}

class C
{
public:
  inline void Foo(uint32 a)
  {
    x = a;
  }
  uint32 x = 0;
  S s = {};
}
