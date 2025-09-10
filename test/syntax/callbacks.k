// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  class Foo
  {
      () -> void callback;
  }

expected:0

  class Foo
  {
      () -> void callback = void_F_void;
  }

expected:0

  class Foo
  {
      () -> void callback = c.void_F_void;

      C c;
  }

expected:0

  class Foo
  {
      () -> void callback = c[1].void_F_void;

      C[2] c;
  }

expected:0

  class Foo
  {
      () -> void callback = d.c[1].void_F_void;

      class D
      {
          public:
              C[2] c;
      }

      D d;
  }

expected:0

  class Foo
  {
      auto callback = d.c[1].u32_F_u32;

      class D
      {
          public:
              C[2] c;
      }

      D d;
  }

expected:0

  class Foo
  {
      const () -> void function;
  }

expected:1

  class Foo
  {
      const () -> void function = void_F_void;

      inline void main()
      {
          function();
      }
  }

expected:0

  class Foo
  {
  private:
      () -> void callback;
  }

expected:0

  class Foo
  {
  private:
      (uint<32>) -> void callback;
  }

expected:0

  class Foo
  {
  private:
      (uint<32>) -> void callback;

      inline void main()
      {
          int32 x;
          callback(x);
      }
  }

expected:1

  class Foo
  {
  private:
      (uint32 x) -> void callback;
  }

expected:0

  class Foo
  {
  private:
      (uint32 x, uint32 y) -> uint32 callback;
  }

expected:0

  class Foo
  {
  private:
      [[async, no_backpressure]]
      (uint32 x, uint32 y) -> uint32 callback;
  }

  export Foo;

expected:0

  class Foo
  {
  private:
      auto callback = non_zero;

      inline bool non_zero(uint32 x)
      {
          return x != 0;
      }
  }

expected:0

  class Foo
  {
  private:
      (int32) -> bool callback = non_zero;

      inline bool non_zero(uint32 x)
      {
          return x != 0;
      }
  }

expected:1

  class Foo
  {
  private:
      callback_uint32_to_uint32_t       callback1;
      callback_uint32_bool_to_uint32_t  callback2;
  }

expected:0

  class Foo
  {
  private:
      callback_uint32_to_uint32_t callback = u32_F_u32;
  }

expected:0

  class Foo
  {
  private:
      D d = {.callback1 = u32_F_u32};
  }

expected:0

  class Foo
  {
  private:
      D d = {.callback1 = u32_F_u32};
  }

  inline void main()
  {
    static Foo foo ={ .d = {.callback2 = Fn_T_S_R<uint32, bool, uint32>} };
  }

expected:0

  class Foo
  {
  private:
      D d = {u32_F_u32};
  }

expected:62

  class Foo
  {
  private:
      D d;
  }

  inline void main()
  {
    static Foo foo ={.d = {.callback1 = u32_F_u32, .callback2 = Fn_T_S_R<uint32, bool, uint32>}};
  }

expected:0

  class Foo
  {
  private:
      D[2] d;
  }

  class Bar
  {
  private:
      Foo[2] foo;
  }

  inline void main()
  {
    static C[2] c;

    static Bar bar =
        { .foo =
            { { .d =
                  { {.callback1 = u32_F_u32, .callback2 = Fn_T_S_R<uint32, bool, uint32>}
                  , {.callback1 = c[0].u32_F_u32, .callback2 = Fn_T_S_R<uint32, bool, uint32>}
                  }
              },
              { .d =
                  { {.callback1 = u32_F_u32, .callback2 = Fn_T_S_R<uint32, bool, uint32>}
                  }
              }
            }
        };
  }

expected:0

  class Foo
  {
  private:
      callback_T_to_R_t<uint32, uint32> callback = u32_F_u32;
  }

expected:0

  class Foo
  {
  private:
      callback_uint32_to_uint32_t callback = c.u32_F_u32;

      C c;
  }

expected:0

  class Foo
  {
  private:
      callback_uint32_to_uint32_t callback = c.u32_F_u32;

      C c;
  }

  inline void main()
  {
    static Foo foo ={.callback = u32_F_u32};
  }

expected:0

  class Foo
  {
  private:
      D d = {.callback1 = c.u32_F_u32};

      C c;
  }

expected:0

  class Foo
  {
  private:
      D d = {.callback1 = c.u32_F_u32};

      C c;
  }

  inline void main()
  {
    static Foo foo ={.d = {.callback1 = u32_F_u32} };
  }

expected:0

  class Foo
  {
  private:
      D d = {.callback2 = c.u32_F_u32};

      C c;
  }

expected:1

  class Foo
  {
  private:
      D[2] d = {
          {.callback1 = c.u32_F_u32},
          {.callback1 = u32_F_u32}
      };

      C c;
  }

expected:0

  class Foo
  {
  private:
      const callback_uint32_to_uint32_t callback = u32_F_u32;
  }

expected:0

  class Foo
  {
  private:
    [[max_threads(1)]]
    uint32 max_threads_1_u32_F_u32(uint32 x)
    {
       return x;
    }

      callback_uint32_to_uint32_t callback = max_threads_1_u32_F_u32;
  }

expected:0

  class Foo
  {
  private:
    [[max_threads(1)]]
    uint32 max_threads_1_u32_F_u32(uint32 x)
    {
       return x;
    }

      callback_max_threads_1_uint32_to_uint32_t callback = max_threads_1_u32_F_u32;
  }

expected:0

  class Foo
  {
  private:
    [[max_threads(1)]]
    uint32 max_threads_1_u32_F_u32(uint32 x)
    {
       return x;
    }

      callback_max_threads_1_uint32_to_uint32_t callback = max_threads_1_u32_F_u32;
  }

expected:0

  class Foo
  {
  private:
      template <auto N>
      [[max_threads(N)]]
      uint32 max_threads_N_u32_F_u32(uint32 x)
      {
         return x;
      }

      callback_max_threads_1_uint32_to_uint32_t callback = max_threads_N_u32_F_u32<2>;
  }

expected:1

  class Foo
  {
  private:
      callback_max_threads_1_uint32_to_uint32_t callback = u32_F_u32;
  }

expected:1

  class Foo
  {
  private:
      callback_max_threads_1_uint32_to_uint32_t callback = c.u32_F_u32;

      C c;
  }

expected:1

  class Foo
  {
  private:
      callback_uint32_bool_to_uint32_t callback = u32_F_u32;
  }

expected:1

  template <typename T>
  class Foo
  {
  private:
      T callback;
  }

  using foo1 = Foo<callback_uint32_to_uint32_t>;
  using foo2 = Foo<callback_T_to_R_t<uint32, uint32>>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
      Callback<uint32, bool> callback;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
      Callback<uint32, uint32> callback = u32_F_u32;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
      Callback<int32, uint32> callback = u32_F_u32;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:1

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
      Callback<uint31, int33> callback = u32_F_u32;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
      Callback<uint31, int33> callback = c.u32_F_u32;

      C c;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:0

  template <template <auto, typename, typename> typename Callback>
  class Foo
  {
  private:
    template <auto N>
    [[max_threads(N)]]
    uint32 max_threads_N_u32_F_u32(uint32 x)
    {
       return x;
    }

      Callback<2, uint31, int33> callback = max_threads_N_u32_F_u32<2>;
  }

  using foo = Foo<callback_max_threads_N_T_to_R_t>;

expected:0

  template <template <auto, typename, typename> typename Callback>
  class Foo
  {
  private:
    template <auto N, typename T, typename R>
    [[max_threads(N)]]
    R max_threads_N_T_to_R(T x)
    {
       return x;
    }

      Callback<2, uint31, int33> callback = max_threads_N_T_to_R<2, uint31, int33>;
  }

  using foo = Foo<callback_max_threads_N_T_to_R_t>;

expected:0

  template <template <auto, typename, typename> typename Callback>
  class Foo
  {
  private:
    template <auto N, typename T, typename R>
    [[max_threads(N)]]
    R max_threads_N_T_to_R(T x)
    {
       return x;
    }

      auto callback1 = max_threads_N_T_to_R<2, uint31, int33>;
      Callback<2, uint31, int33> callback2 = callback1;
  }

  using foo = Foo<callback_max_threads_N_T_to_R_t>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
    template <auto N, typename T, typename R>
    [[max_threads(N)]]
    R max_threads_N_T_to_R(T x)
    {
       return x;
    }

      auto callback1 = max_threads_N_T_to_R<2, uint31, int33>;
      Callback<uint31, int33> callback2 = callback1;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
    template <auto N, typename T, typename R>
    [[max_threads(N)]]
    R max_threads_N_T_to_R(T x)
    {
       return x;
    }

      auto callback1 = c.max_threads_N_T_to_R<2, uint31, int33>;
      Callback<uint31, int33> callback2 = callback1;
      C c;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:0

  template <template <typename, typename> typename Callback>
  class Foo
  {
  private:
    template <auto N, typename T, typename R>
    [[max_threads(N)]]
    R max_threads_N_T_to_R(T x)
    {
       return x;
    }

      auto callback1 = max_threads_N_T_to_R<2, uint31, int30>;
      Callback<uint31, uint32> callback2 = callback1;
  }

  using foo = Foo<callback_T_to_R_t>;

expected:1

  // public callback called from the outside
  inline uint32 Ident(uint32 x) { return x;}

  class Foo
  {
  public:
    (uint32)->uint32 cb = Ident;
  }

  class Main
  {
        Foo c;
  public:
      void main()
      {
        println(c.cb(3));
      }
  }
  export Main;

expected:5

  // calling an uninitialized callback
  class Foo
  {
    (uint32)->bool cb;

    public:
      inline void test()
      {
        println(cb(3));
      }
  }

  class Main
  {
    Foo f;
  public:  
    void main()
    {
      f.test();
    }
  }

  export Main;

expected:27

    class Main
    {
        [[async, max_threads(2)]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

    export Main;

expected:0

    class Main
    {
        [[max_threads(2), async]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:0

    class Main
    {
        [[async]]
        [[max_threads(2)]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:0

    class Main
    {
        [[max_threads(2)]]
        [[async]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:0

    class Main
    {
        [[max_threads(2)]]
        [[async, thread_rate(2)]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:0

    class Main
    {
        [[thread_rate(2)]]
        [[max_threads(2)]]
        [[async]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:0

    class Main
    {
        [[async, max_threads(3)]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:1

    class Main
    {
        [[max_threads(2)]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:1

    class Main
    {
        [[async]]
        void Go()
        {
        }

        D d ={.callback3 = Go};
    }

expected:1

    class Foo
    {
        public:
            inline void Run()
            {
                u32_F_u32_F_u32_u32(callback, 10);
            }

        private:
            callback_uint32_to_uint32_t callback;
    }

    inline void main()
    {
        static Foo foo ={.callback = u32_F_u32};
    }

expected:0

    template <typename T>
    class Foo
    {
        T callback;
    }

    inline uint32 F(uint32 x)
    {
        return x;
    }

    inline bool G(bool x)
    {
        return x;
    }

    inline void main()
    {
        static Foo<(uint32) -> uint32> foo1 = {.callback = F};
        static Foo<(bool) -> bool> foo2 = {.callback = G};
    }

expected:0

    inline void Go(() -> void Do)
    {
    }

    template <typename T>
    class Foo
    {
    private:
        () -> void Do;

    public:
        inline void Run()
        {
            Go(Do);
        }
    }

    inline void Bar()
    {
    }

    inline void main()
    {
        static Foo<bool> foo = {.Do = Bar};
    }

expected:0

    inline void Go((bool) -> void Do)
    {
    }

    template <typename T>
    class Foo
    {
    private:
        using callback_t = (T) -> void;

        callback_t Do;

    public:
        inline void Run()
        {
            Go(Do);
        }
    }

    inline void Bar(bool b)
    {
    }

    inline void main()
    {
        static Foo<bool> foo = {.Do = Bar};
    }

expected:0

    class foo
    {
        using callback_t = (uint32) -> auto;
    }

expected:1

    class Foo
    {
        (uint32) -> auto callback;
    }

expected:1

    class Foo
    {
        using callback_t = (uint32) -> auto;
        callback_t callback;
    }

expected:1

    template <typename T>
    class Foo
    {
        using callback_t = T;
    }

    inline void main()
    {
        static Foo<(uint32) -> auto> foo;
    }

expected:1

    template <typename T>
    class Foo
    {
        using callback_t = T;
        callback_t callback;
    }

    inline void main()
    {
        static Foo<(uint32) -> auto> foo;
    }

expected:1

  class Foo
  {
      ([[last]] bool) -> void callback = void_F_last;
  }

expected:0

  class Foo
  {
      ([[last]] bool) -> void callback = c.void_F_last;

      C c;
  }

expected:0

  class Foo
  {
      ([[last]] bool) -> void callback = c[1].void_F_last;

      C[2] c;
  }

expected:0

  class Foo
  {
      ([[last]] bool) -> void callback = d.c[1].void_F_last;

      class D
      {
          public:
              C[2] c;
      }

      D d;
  }

expected:0

  class Foo
  {
      ([[last]] bool) -> void callback = void_F_bool;
  }

expected:1

  class Foo
  {
      (bool) -> void callback = void_F_last;
  }
  // TODO: middle-end should fail because function has attribute affecting call site that is missing on callback

expected:0

  inline void main()
  {
    static D d ={.callback4 = void_F_last};
  }

expected:0

  inline void main()
  {
    static D d ={.callback4 = void_F_bool};
  }

expected:1

  template <typename Callback>
  class Foo
  {
      private:
          Callback callback;
  }

  inline void main()
    {
        static Foo<([[last]] bool) -> void> foo1 = {.callback = void_F_last};
        static Foo<(bool) -> void> foo2 = {.callback = void_F_bool};
    }

expected:0

  template <typename Callback>
  class Foo
  {
      private:
          Callback callback;
  }

  inline void main()
  {
      static Foo<(bool) -> void> foo = {.callback = void_F_last};
  }

  // TODO: middle-end should fail because function has attribute affecting call site that is missing on callback

expected:0

  template <typename Callback>
  class Foo
  {
      private:
          Callback callback;
  }

  inline void main()
  {
      static Foo<([[last]] bool) -> void> foo = {.callback = void_F_bool};
  }

expected:1

  class Foo
  {
      [[reset]] (bool) -> void callback;
  }

expected:1

  class Foo
  {
      [[always]] (bool) -> void callback;
  }

expected:1

  class Foo
  {
      [[latency]] (bool) -> void callback;
  }

expected:1

  class Main
  {
      [[async, max_threads(2)]]
      void Go()
      {
      }

  private:
      default = { .callback1 = u32_F_u32
                , .callback3 = Go
                };
      A a;
  }

expected:0

  class Main
  {
      [[async, max_threads(2)]]
      void Go()
      {
      }

      default = { .callback1 = u32_F_u32
                       , .callback3 = Go
                       };
      A a;
  }

expected:0

  class Main
  {
      [[async, max_threads(2)]]
      void Go()
      {
      }

  private:
      default = { .d = { .callback1 = u32_F_u32
                       , .callback3 = Go
                       }
                };
      A a;
  }

expected:62

  class Main
  {
      [[async, max_threads(2)]]
      void Go()
      {
      }

  private:
      default = { .b = {.d = { { .callback1 = u32_F_u32
                               , .callback3 = Go
                               }
                             , { .callback1 = u32_F_u32
                               , .callback3 = Go
                               }
                             }
                      }
                };
      A a;
  }

expected:62

  class Main
  {
  private:
      default = { .callback1 = u32_F_u32
                , .callback3 = Go
                };
      A a;
  }

expected:1

  inline void Main()
  {
      static default = { .callback1 = u32_F_u32
                       , .callback3 = Go
                       };
      static A a;
  }

expected:1

  class Main
  {
  private:
      default = { u32_F_u32 };
      A a;
  }

expected:1

  class Main
  {
  private:
      default = 10;
  }

expected:1

  // Class callback with [[latency]] attribute
  // Class is not exported
  class Foo
  {
  private:
    [[latency(0)]] (uint32)->uint32 cb;

  public:
    uint32 bar(uint32 a)
    {
      return cb(a);
    }
  }

expected:75

  // Class callback with [[no_backpressure]] attribute
  // Class is not exported
  class Foo
  {
  private:
    [[no_backpressure]] (uint32)->uint32 cb;

  public:
    uint32 bar(uint32 a)
    {
      return cb(a);
    }
  }

expected:75
