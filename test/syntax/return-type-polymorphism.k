// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  inline void main()
  {
      bool x = Fn_T();
  }

expected:0

  inline void main()
  {
      uint8 x = Fn_T();
  }

expected:0

  inline void main()
  {
      int16[4] x;

      x[0] = Fn_T();
  }

expected:0

  inline void main()
  {
      bool x = Fn_T_auto(false);
  }

expected:0

  inline void main()
  {
      bool x = Fn_T_auto(10);
  }

expected:1

  inline void main()
  {
      uint2 x = Fn_T_auto(10);
  }

warning:0

  inline void main()
  {
      uint2 x = Fn_T_T(10);
  }

warning:conversion

  inline void main()
  {
      bool x = Fn_T_U(10);
  }

warning:0

  inline void main()
  {
      uint8[4] x = Fn_T_N();
  }

expected:0

  inline void main()
  {
      uint8[4] x;
      x = Fn_T_N();
  }

expected:0

  inline void main()
  {
      bool[4] x = Fn_T_N();
  }

expected:0

  inline void main()
  {
      bool[4] x;
      x = Fn_T_N();
  }

expected:0

  inline void Foo(bool[4] x, uint7[5] y)
  {
  }

  inline void main()
  {
      Foo(Fn_T_N(), Fn_T_N());
  }

expected:0

  inline void Foo(optional<uint32> x)
  {
  }

  inline void main()
  {
      Foo(Fn_optional_T_U(10));
  }

expected:0

  inline void main()
  {
      optional<uint32>[4][3] x;

      x[0][0] = Fn_optional_T_U(0);
  }

expected:0

  inline void main()
  {
      optional<uint32>[4][3] x;

      x[0] = Fn_optional_T_N_U(0);
  }

expected:0

  template <typename T, typename U>
  inline bool Go((optional<T>) -> bool Do, U x)
  {
      return Do(Fn_optional_T_U(x));
  }

  inline void main()
  {
      Go(is_valid<uint32>, 10);
  }

expected:0

  inline uint8[4] Run()
  {
      return Fn_T_N();
  }

expected:0

  inline bool[4] Run()
  {
      return Fn_T_N();
  }

expected:0

  inline auto Run()
  {
      return Fn_T_N();
  }

expected:1

  inline optional<uint32> Run()
  {
      return Fn_optional_T_U(10);
  }

expected:0

  inline optional<uint32>[4] Run()
  {
      return Fn_optional_T_N_U(0);
  }

expected:0

  inline void Go(() -> uint7 Do)
  {
      Do();
  }

  inline void main()
  {
      Go([]() -> uint7
      {
          return Fn_T();
      });
  }

expected:0

  inline void Go(() -> auto Do)
  {
      Do();
  }

  inline void main()
  {
      Go([]() -> uint7
      {
          return Fn_T();
      });
  }

expected:0

  inline void Go(() -> auto Do)
  {
      Do();
  }

  inline void main()
  {
      Go([]() -> uint7[3]
      {
          return Fn_T_N();
      });
  }

expected:0

  template <auto N>
  inline bool[4] Test()
  {
      static if (N == 0)
          return Fn_T_N();
      else
          return Fn_T();
  }

  inline void main()
  {
      Test<0>();
  }

expected:0

  template <auto N>
  inline bool[4] Test()
  {
      static if (N == 0)
          return Fn_T_N();
      else
          return Fn_T();
  }

  inline void main()
  {
      Test<1>();
  }

expected:1
