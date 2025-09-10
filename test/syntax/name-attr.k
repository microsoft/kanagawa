// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  [[name("integer")]]
  export uint32;

expected:0

  template <typename T, typename U>
  struct Foo
  {
      T x;
      U y;
  }

  [[name("MyFoo")]]
  export Foo<uint3, Foo<bool, uint32>>;

expected:0

  template <typename T>
  inline void main()
  {
    [[name("array")]]
    export T[10];
  }

expected:0

  class Foo
  {
      struct Bar
      {
      }

      [[name("Bar")]]
      export Bar;
  }

  [[name("TheClass")]]
  export Foo;

expected:0

  [[name("integer")]]
  using t = uint32;

expected:1

  [[name(10)]]
  using t = uint32;

expected:1

  class Bar
  {
  }

  class Foo
  {
      [[name("TheBar")]]
      Bar bar;

      [[name("ThreeBars")]]
      Bar[3] bars;
  }

expected:0

  [[name("X")]]
  uint32 x;

expected:1

  struct Foo
  {
    [[name("X")]]
    uint32 x;
  }

expected:1

  for(const [[name("X")]] uint32 x : 10){}

expected:1
