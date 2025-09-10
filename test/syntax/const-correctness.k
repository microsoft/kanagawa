// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    const auto x = 10;

    inline void Do()
    {
        --x;
    }

expected:1

    const auto x = 10;

    inline void Do()
    {
        x += 1;
    }

expected:1

    const auto x = 10;

    inline void Do()
    {
        x++;
    }

expected:1

    const auto x = 10;

    inline void Do()
    {
        x = 10;
    }

expected:1

    const bool x = false;

    inline void Do()
    {
        x = true;
    }

expected:1

    const bool x = false;

    inline void Do()
    {
        x &&= true;
    }

expected:1

    inline void Do()
    {
        const auto x = false;

        x = true;
    }

expected:1

    inline void Do()
    {
        const uint32 x = 0;

        x = 1;
    }

expected:1

    inline void Do()
    {
        const uint32 x = 0;

        x--;
    }

expected:1

    inline void Do()
    {
        const uint32 x = 0;

        ++x;
    }

expected:1

    const S s = {};

    inline void Do(uint32 x)
    {
      s.x = x;
    }

expected:1

    const S s = {};

    inline void Do(uint32 x)
    {
      s.x *= 2;
    }

expected:1

    const S s = {};

    inline void Do(uint32 x)
    {
      s.x++;
    }

expected:1

    inline void Do(uint32 x)
    {
      const S s = {};

      s.x = x;
    }

expected:1

    inline void Do(uint32 x)
    {
      const S[2] s = {};

      s[0].x = x;
    }

expected:1

    inline void Do(uint32 x)
    {
      const S s = {};

      for (const uint32 i : 10)
      {
        s.x = x;
      }
    }

expected:1

    inline void Do(uint32 x)
    {
      const S s = {};

      --s.x;
    }

expected:1

    const C c = {};

    inline void Do(uint32 x)
    {
      c.x = x;
    }

expected:1

    const C c = {};

    inline void Do(uint32 x)
    {
      c.Foo(x);
    }

expected:1

    const C c = {};

    inline void Do(uint32 x)
    {
      c.s.x = x;
    }

expected:1

    class Foo
    {
    public:
        const auto _my_value = 0;
    }

    class Bar
    {
    public:
        inline void set_my_value(uint64 value)
        {
            foo._my_value = value;
        }
    private:
        Foo foo;
    }

    inline void set_value(uint64 value)
    {
        static Bar bar;

        bar.set_my_value(value);
    }

expected:1

    enum E : uint1
    {
        A1
    }

    inline void Go(const E e)
    {
    }

    inline void main()
    {
        Go(E::A1);
    }

expected:0

    // const parameter not known at the call site
    inline void Go(const uint32 x)
    {
    }

    inline void Run(uint32 x)
    {
        Go(x);
    }

expected:0
