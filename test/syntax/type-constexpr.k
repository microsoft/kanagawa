// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  static assert(uint32 == uint32);

expected:0

  static assert(uint32 == uint<32>);

expected:0

  inline void main()
  {
    static uint32 x;
    static assert(x == uint32);
  }

expected:1

  inline void main()
  {
    static uint32 x;
    static assert(decltype(x) == uint32);
  }

expected:0

  static assert(uint32 == uint30);

expected:1

  static assert(uint32 == int32);

expected:1

  static assert(uint32 != uint30);

expected:0

  static assert(uint3[2] != uint3[1]);

expected:0

  static assert(uint32 > uint30);

expected:0

  static assert(uint<32> > uint30);

expected:0

  static assert(uint32 >= uint30);

expected:0

  static assert(uint32 < uint30);

expected:1

  static assert(uint32 <= uint30);

expected:1

  inline void main()
  {
      static same_types<uint3, uint3> test;
  }

expected:0

  inline void main()
  {
      static same_types<uint32[10], uint32[10]> test;
  }

expected:0

  inline void main()
  {
      static same_types<(uint3 x) -> void, (uint3) -> void> test;
  }

expected:0

  inline void main()
  {
      static same_types<[[max_threads(2), thread_rate(1)]]  (uint3 x) -> void, [[max_threads(2), thread_rate(1)]]  (uint3) -> void> test;
  }

expected:0

  inline void main()
  {
      static same_types<[[max_threads(2), thread_rate(1)]]  (uint3 x) -> void, [[thread_rate(1), max_threads(2)]]  (uint3) -> void> test;
  }

expected:0

  inline void main()
  {
      static same_types<[[max_threads(1), thread_rate(1)]]  (uint3 x) -> void, [[max_threads(2), thread_rate(1)]]  (uint3) -> void> test;
  }

expected:1

  inline void main()
  {
      static same_types<Struct, Struct_t> test;
  }

expected:0

  inline void main()
  {
      static different_types<uint32, Struct> test;
  }

expected:0

  inline void main()
  {
      static different_types<(uint2) -> bool, (uint2) -> void> test;
  }

expected:0

  inline void main()
  {
      static subtype_of<uint4, uint5> test;
  }

expected:0

  inline void main()
  {
      static subtype_of<Enum, uint8> test;
  }

expected:0

  inline void main()
  {
      static subtype_of<Enum_t, uint8> test;
  }

expected:0

  inline void main()
  {
      static subtype_of<(uint33) -> uint9, (uint32) -> uint10> test;
  }

expected:0

  inline void main()
  {
      static subtype_of<(uint8) -> Struct_t, (Enum_t) -> Struct> test;
  }

expected:0

  template <typename T>
  inline void Go(() -> T fn)
  {
      static if (T == void)
      {
          fn();
      }
      else
      {
          auto x = fn();
      }
  }

expected:0

  static assert(decltype(Class::Run) == (bool) -> void);

expected:0

  static assert(decltype(Class::Run) == (bool) -> bool);

expected:1

  inline void main()
  {
      static Class obj;

      static assert(decltype(obj.Run) == (bool) -> void);
  }

expected:0

  inline void main()
  {
      static Class obj;

      static assert(decltype(obj.Run({})) == void);
  }

expected:0

  class Main
  {
      [[max_threads(2)]]
      void main()
      {
      }

      static assert(decltype(main) == [[max_threads(2)]] () -> void);
  }

expected:0

  inline void main()
  {
  }

  static assert(decltype(main) == [[max_threads(2)]] () -> void);

expected:1

  class Main
  {
      [[max_threads(2)]]
      void main()
      {
          static assert(decltype(main) == [[max_threads(2)]] () -> void);
      }
  }

expected:0

  inline auto main()
  {
      static assert(decltype(main) == () -> uint4);
      return 10;
  }

expected:0

  inline auto main()
  {
      return true;
  }

  static if (decltype(main) == () -> bool)
  {
      static assert(true);
  }
  else
  {
      static assert(false);
  }

expected:0

  inline auto main()
  {
      static if (decltype(main) == () -> bool)
      {
          return true;
      }
      else
      {
          return 10;
      }
  }

expected:1

  import type.stdtype
  static assert(index_t<10> == uint4);

expected:0

  import type.stdtype
  static assert(index_t<10> <= uint4);

expected:0

  import type.stdtype
  static assert(index_t<10> > uint3);

expected:0

  import type.stdtype
  static assert(index_t<10> != uint3);

expected:0

  import type.stdtype
  static assert(index_t<10> == count_t<10>);

expected:0

  import type.stdtype
  static assert(index_t<8> != count_t<8>);

expected:0

  import type.stdtype
  import data.optional

  static assert(optional<index_t<8>> == optional<uint3>);

expected:0

    static assert(!(decltype({}) < uint1));

expected:1

    static assert(!(decltype({}) < uint1[2]));

expected:1

    static assert(decltype({}) < () -> void);

expected:1

    inline void main()
    {
        static uint32 x;
        static assert(decltype(x()) == decltype(x()));
    }

expected:1

    inline uint32 x()
    {
        return {};
    }

    static assert(!(decltype(x()) == decltype(x())));

expected:1

    static assert(!(decltype({}) < float32));

expected:1

