// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  inline void main()
  {
      static Foo foo = {.field1 = 10, .field2 = false};
  }

expected:0

  inline void main()
  {
      static Foo foo = {.field1 = -10, .field2 = false};
  }

expected:1

  inline void main()
  {
      static Foo foo = {10, false};
  }

expected:0

  inline void main()
  {
      static Foo foo = {.field1 = 10, false};
  }

expected:1

  inline void main()
  {
      static Foo foo = {.field1 = 10};
  }

expected:0

  inline void main()
  {
      static Foo foo = {.field2 = false};
  }

expected:0

  inline void main()
  {
      static Foo foo = {field2 = false};
  }

expected:1

  inline void main()
  {
      static Foo foo = {.field2};
  }

expected:1

  inline void main()
  {
      static Foo foo = {.field1 = 10, field2 = false};
  }

expected:1

  inline void main()
  {
      static Bar bar =
        { .foo = {}
        , .foos =
          { { .field1 = 1
            , .field2 = true
            }
          , { .field1 = 3
            , .field2 = false
            }
          }
        , .x = 7
        };
  }

expected:0

  inline void main()
  {
      static Bar bar =
        { .foo = {}
        , .foos =
          { { 1
            , false
            }
          , { .field1 = 3
            , .field2 = true
            }
          }
        , .x = 7
        };
  }

expected:0

  inline void main()
  {
      static Bar bar =
        { {}
        , { { 1
            , false
            }
          , { .field1 = 3
            , .field2 = true
            }
          }
        , 7
        };
  }

expected:0

  inline void main()
  {
      static Variant v1 = {.x = 10};
      static Variant v2 = {.y = false};
  }

expected:0

  inline void main()
  {
      static Variant v = {.x = false};
  }

expected:1

  inline void main()
  {
      static Variant v = { false};
  }

expected:1

  inline void main()
  {
      static Variant v = {.x = 10, .y = false};
  }

expected:1

  inline void Go(Variant v)
  {
  }

  inline void main()
  {
      Go({.x = 10});
  }

expected:0

  inline void Go(uint32[2] a)
  {
  }

  inline void main()
  {
      Go({10, 11});
  }

expected:0

  inline void Go(uint32[2] a)
  {
  }

  inline void main()
  {
      Go({.x = 10, .x = 11});
  }

expected:1

  inline void Go(Foo foo)
  {
  }

  inline void main()
  {
      Go({.field1 = 10, .field2 = false});
  }

expected:0

  inline void Go(Foo foo)
  {
  }

  inline void main()
  {
      Go({.field1 = false, .field2 = 10});
  }

expected:1

  inline void Go(Foo foo)
  {
  }

  inline void main()
  {
      Go({.field1 = 10, .field3 = false});
  }

expected:1

  inline void main()
  {
      static Class[4] c = { {.index = 0}, {.index = 1}, {.index = 2}, {.index = 3} };
  }

  // TODO, this should fail when classes are fully implemented
expected:0

  inline void main()
  {
      static Class c = { .fifo_depth = 64 };
  }

expected:0

  inline void main()
  {
    static Class c = { .fifo_depth = 64 };
  }

expected:0

  inline void main()
  {
    static default = { .fifo_depth = 64 };
    static Class c;
  }

expected:0

  inline void main()
  {
    static default = { .fifo_depth = 64 };
    static default = { .fifo_depth = 15 };
    static Class c;
  }

  // TODO: should fail

expected:0

  inline void Run(bool cond)
  {
    static default = { .fifo_depth = 64 };

    if (cond)
    {
        static default = { .fifo_depth = 15 };

        static Class c;
    }

    static Class c;
  }

expected:0

  class C
  {
  private:
      const uint32 index;
  }

  inline void main()
  {
      static C[4] c = { {.index = 0}, {.index = 1}, {.index = 2}, {.index = 3} };
  }

expected:54

  // Incorrect struct field name used
  inline void main()
  {
      static Foo foo = {.field3 = 10, .field2 = false};
  }

expected:1

  // Incorrect union field name used
  inline void main()
  {
      static Variant v1 = {.z = 10};
  }

expected:1

  // callback and method with the same names
  class C
  {
  private:
    (uint32)->bool func;

    bool func(uint32 x)
    {
      return x == 4;
    }

  public:
    inline void test()
    {
      println(func(4));
    }
  }

expected:1

  // Multiple default initializers in a class
  inline void foo(){}

  class C
  {
    default = {.cb1 = foo};

    default = {.cb2 = foo};
  }

expected:62

  // Mismatch between callback type and default initializer type
  inline void foo(){}

  class A
  {
  private:
    (uint32)->uint32 cb;
  }

  class C
  {
  private:
    default = {.cb = foo};

    A _a;
  }

  class Main
  {
      C _c;
  }

  export Main;

expected:14

  // Default initializer list contains nesting
  inline void foo(){}

  class C
  {
  private:
    default = { .a = {.cb1 = foo} };
  }

expected:62

  // Default initializer applied to a callback within an exported class is fine
  inline void foo(){}

  class E
  {
  private:
    ()->void cb;

  public:
    void entry()
    {
      cb();
    }
  }

  export E;

  class C
  {
  private:
    default = {.cb = foo};

    E _e;

  public:
    inline void entry()
    {
      _e.entry();
    }
  }

  class Main
  {
      C _c;
  public:

    void entry()
    {
      _c.entry();
    }
  }

expected:0

  // Default initializer applied (defined outside of an export class)
  // is not visible to children of exported classes
  inline void foo(){}

  class A
  {
  private:
    ()->void cb;

  public:
    inline void entry()
    {
      cb();
    }
  }

  class E
  {
  private:
    A _a;

  public:
    void entry()
    {
      _a.entry();
    }
  }

  export E;

  class C
  {
  private:
    default = {.cb = foo};

    E _e;

  public:
    inline void entry()
    {
      _e.entry();
    }
  }

  class Main
  {
      C _c;
  
  public:
      void entry()
      {
        _c.entry();
      }
  }

  export Main;

expected:27

  inline void main()
  {
      static Bar bar =
        { .foo = {}
        , .foos =
          { { 1
            , false
            }
          , { .field1 = 3
            , .field2 = true
            , .field1 = 4
            }
          }
        , .x = 7
        };
  }

warning:initializer-overrides

  inline void main()
  {
      static Bar bar =
        { .foo = {}
        , .foos =
          { { 1
            , false
            }
          , { .field1 = 3
            , .field2 = true
            , .field1 = 4
            , .field2 = true
            }
          }
        , .x = 7
        };
  }

warning:initializer-overrides

  inline void main()
  {
      static Bar bar =
        { .foo = {}
        , .foos =
          { { .field2 = false
            , .field1 = 4
            }
          , { .field1 = 3
            , .field2 = true
            }
          }
        , .x = 7
        };
  }

warning:0

    inline void main()
    {
        static XY xy = {.x = {.x = 10}, .y = {.x = {.x = 10}}};
    }

warning:0

    inline void main()
    {
        static XY xy = {.x = {.x = 10}, .y = {.x = {.x = 10}}, .x = {.x = 1, .x = 2}};
    }

warning:initializer-overrides
