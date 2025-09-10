module sample.template_instance
    { Foo
    , A
    , B
    , Test
    }

template <typename T, typename S>
struct Foo
{
    T x;
    S y;
}

template <typename T>
class A
{
public:
    using A1_t = T;

    template <auto N>
    using A2_t = T[N];
}

template <typename T>
class B
{
public:
    template <typename S>
    class A
    {
    public:
        using A1_t = S;

        template <auto N>
        using A2_t = S[N];

        using A3_t = Foo<T, S>;
    }
}

class Test
{
public:
  A<uint32> test_1;

  const A<uint32>::A1_t test_2 = 10;

  const A<uint32[2]>::A1_t test_3 = {1, 2};

  A<bool>::A2_t<2> test_4 = {false, true};

  B<bool>::A<uint32>::A1_t test_5 = 0;

  B<bool>::A<uint32>::A2_t<2> test_6 = {0, 1};

  B<bool>::A<uint32>::A3_t test_7 = {false, 10};

  B<bool>::A<uint32>::A3_t test_8 = {.x = false, .y = 10};

  A<A<bool>::A1_t>::A1_t test_9 = true;

  A<A<bool>::A2_t<2>>::A1_t test_10 = {false, true};

  B<bool>::A<B<bool>::A<uint32>::A3_t>::A1_t test_11 = {true, 10};

  using Bbool = B<bool>;
  Bbool::A<uint32>::A3_t test_12 = {false, 10};

  Bbool::A<uint32>::A3_t test_13 = {.x = false, .y = 10};
  using BboolAuint32 = B<bool>::A<uint32>;
  BboolAuint32::A3_t test_14 = {false, 10};
}
