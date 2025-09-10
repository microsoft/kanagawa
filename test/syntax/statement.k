// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

  inline void main()
  {
      reorder bool x;
  }

expected:1

  inline void main()
  {
      if (true)
          bool x;
  }

expected:1

  inline void main()
  {
      if (true)
      {
      }
      else
          bool x;
  }

expected:1

  inline void main()
  {
      if (true)
          inline void Foo()
          {
          }
  }

expected:1

  struct Foo{};

  inline void main()
  {
      if (true)
          export Foo;
  }

expected:1

  inline void main()
  {
      if (true)
          using foo = uint32;
  }

expected:1

  inline void main()
  {
      if (true)
          static assert(true);
  }

expected:1

  inline void main()
  {
      if (true)
          template <typename T>
          inline void Foo()
          {
          }
  }

expected:1

  inline void main()
  {
      if (true)
          static uint32 x;
  }

expected:1

  inline void main()
  {
      for (const auto i : 10)
          bool x;
  }

expected:1

  inline void main()
  {
      for (const uint32 i : 1-)
          uint32 x;
  }

expected:1

  inline void main()
  {
      if (true)
          print(0);;
  }

expected:0

  inline void main()
  {
      if (true)
          print(0);;
      else
      {
      }
  }

expected:1

  inline void Run(uint32 x)
  {
      switch(x)
      {
          case 0:
            bool x;
            x = true;
            break;
          default:
            float32 x;
            x = 3.14;
            break;
      }
  }

expected:0
