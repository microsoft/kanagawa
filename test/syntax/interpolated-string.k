// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  const auto s = "{intConst}";

expected:0

  const auto s = " {intConst} ";

expected:0

  const auto s = "{ intConst }";

expected:0

  const auto s = "{ intConst= }";

expected:0

  const auto s = "{intConst=}";

expected:0

  const auto s = "{ intConst = }";

expected:0

  const auto s = "{intConst=,10}";

expected:0

  const auto s = "{intConst=, 10}";

expected:0

  const auto s = "{intConst=:}";

expected:1

  const auto s = "{intConst=:d}";

expected:0

  const auto s = "{intConst=: d}";

expected:0

  const auto s = "{intConst=:d10}";

expected:0

  const auto s = "{intConst=:d10 }";

expected:0

  const auto s = "{intConst=:d 10}";

expected:1

  const auto s = "foo{intConst + 3}";

expected:0

  const auto s = "foo{ intConst + 3 }";

expected:0

  const auto s = "foo{intConst+3=}";

expected:0

  inline void foo()
  {
    const auto s = "foo{intFun(intConst)+3}";
  }

expected:0

  const auto s = "foo{intConst + 3,2}";

expected:0

  const auto s = "foo{intConst + 3,intConst + 2}";

expected:0

  const auto s = "foo{intConst + 3:d}";

expected:0

  const auto s = "foo{intConst + 3:d4}";

expected:0

  const auto s = "foo{intConst + 3:b}";

expected:0

  const auto s = "foo{intConst + 3:b32}";

expected:0

  const auto s = "foo{intConst + 3:o}";

expected:0

  const auto s = "foo{intConst + 3:o10}";

expected:0

  const auto s = "foo{intConst + 3:x}";

expected:0

  const auto s = "foo{intConst + 3:x8}";

expected:0

  const auto s = "foo{intConst + 3:X}";

expected:0

  const auto s = "foo{intConst + 3:X8}";

expected:0

  const auto s = "{intConst=,intConst}";

expected:0

  const auto s = "{intConst=,intConst / 10}";

expected:0

  const auto s = "{intConst1}";

expected:1

  const auto s = "{intConst,intConst1}";

expected:1

  inline void foo()
  {
    const auto s = "{intConst,intFun(intConst)}";
  }

expected:1

  inline void foo()
  {
    const auto s = "{intFun(intConst),intConst}";
  }

expected:0

  const auto s = "{{}";

expected:0

  const auto s = "{ cast<uint32[2]>({0, 1}) }";

expected:0

  const auto s = "{enumConst:x}";

expected:0

  const auto s = "{floatConst:x}";

expected:0

  const auto s = "{boolConst:x}";

expected:1

  const auto s = "{boolConst:b}";

expected:0

  const auto s = "{boolConst:b3}";

expected:0

  inline void foo()
  {
    const auto s = "{stringFun("{boolConst:b3}")}";
  }

expected:0

  inline void foo()
  {
    const auto s = "{stringFun("{boolConst:b3}"):d}";
  }

expected:1

  inline void foo()
  {
    const auto s = "{stringFun("{boolConst:x}")}";
  }

expected:1

  const auto s = "{ {} }";

expected:1

  const auto s = "{ {1, 0} }";

expected:1

  class C{}

  inline void foo()
  {
    static C c;

    const auto s = "{c}";
  }

expected:1

  class C{}

  inline void foo()
  {
    static C[2] c;
    const auto s = "{c}";
  }

expected:1

  struct C{}

  inline void foo()
  {
    static C[2] c;
    const auto s = "{c}";
  }

expected:0

  union C{uint32 x; uint32 y;}

  inline void foo()
  {
    static C[2] c;
    const auto s = "{c}";
  }

expected:0

  inline void foo()
  {
  }

  const auto s = "{foo()}";

expected:1

  // constant string initialized to
  // a interpolation which references a non-constant variable
  class Test
  {
      uint32 a = 3;
      const string s = "{a}";
  }

  export Test;

expected:7
