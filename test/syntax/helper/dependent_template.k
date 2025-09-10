// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.dependent_template
  { B
  , C
  , D
  , E
  , AmbiguosSyntax
  }

class B
{
public:
    template <typename T>
    T Go()
    {
        return {};
    }
}

template <auto Cond>
class C
{
    inline void Run()
    {
        obj.template Go<uint32>();
    }

    static if (Cond)
    {
        B obj;
    }
}

template <typename T>
class D
{
public:
    inline void Go()
    {
        obj.template Go<uint32>();
    }

    T obj;
}

class E
{
public:
    () -> uint32 callback;
}

template <typename T, auto a, auto b>
class AmbiguosSyntax
{
public:
    static if (bitsizeof T > 10)
    {
        template <auto N>
        uint32 Do(uint32 x)
        {
            return x;
        }
    }
    else
    {
        uint32 Do;
    }

    auto Go()
    {
        static if (bitsizeof T > 10)
        {
            return template 
                Do<a && b>(10);
        }
        else
        {
            return
                Do<a && b>(10);
        }
    }
}
