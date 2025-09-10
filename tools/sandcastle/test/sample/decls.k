/*| Module description.

@@


\
 \
  \
   '-----.
          )
         /
         \
          `----->

@@
*/
module sample.decls
    { array_alias
    , bool_alias
    , index_t
    , sample_union
    , sample_struct
    , CrazyNumbers
    , foo
    , markup_sample
    , sample_class
    , C
    , D
    , Foo
    , W
    , constant
    , private_public_test
    , mem
    , cast_test
    , svgbob_test
    , svgbob_test2
    , svgbob_test3
    , apply
    , long_template_test
    , template_template_test
    , long_template_template_test
    , many_arg_fun
    , accessor_test
    }

//| pre-alias description.
using bool_alias = bool;
//< post-alias description.

template
    < /*| Pre-param 1.*/ typename T = uint8 //< Post-param 1.
    , /*| Pre-param 2.*/ auto     N = 26    //< Post-param 2.
    >
using array_alias = T[N];

template <auto N>
using index_t = uint<(N > 1) ? clog2(N) : 1>;

//| pre-union description.
union sample_union
{
    //| pre-field 1.
    uint8 larger /*< post-field 1.*/;
    //| pre-field 2.
    uint4 smaller /*< post-field 2.*/;

}
//< post-union description.

//| Pre-struct description.
struct sample_struct
{
    //| Pre-field 1.
    float32 struct_field1 /*< Post-field 1.*/;

    //| Pre-field 2.
    bool[4][5][6] struct_field2 /*< Post-field 2.*/;
} //< Post-struct description.

//| Pre-enum description.
enum CrazyNumbers : uint5
{
    //| Pre-value ONE.
    ONE = 1
    //< Post-value ONE.
,
    //| Pre-value FIVE.
    FIVE = ONE + 2
    //< Post-value FIVE.
,
    //| Pre-value NINE.
    NINE = 0
    //< Post-value NINE.
,
    //| Pre-value FIFTEEN.
    FIFTEEN
    //< Post-value FIFTEEN.
}
//< Post-enum description.

//| Pre-function description.
template
    < /*| Pre param T.*/ typename T       //< Post param T.
    , /*| Pre param p.*/ (T, T) -> bool p //< Post param p.
    >
inline void foo
    ( ( uint32   //< post first param of function function param
      , uint32 b //< post second param of function function param
      ) -> bool fn // < post function param fn
    , /*| pre param x*/ uint32 x //< post param x
    )
{} //< Post-function description.

//|
// This is *emphasis*, __bold__, and `monospace`
//
// ## H2 sub-heading
//
// Bullet list:
//
// * A
//
//     * nest A
//
// * B
//
// Enumerated list:
//
// (1) One
// 2. Two
//
// Definition list:
//
// Term 1
//
// :   Definition 1
//
// Term 2 with *inline markup*
//
// :   Definition 2
//
//         { some code, part of Definition 2 }
//
//     Third paragraph of definition 2.
//
// +----------------------+----------+
// | Header rows optional | Header 2 |
// +======================+==========+
// | body row 1, col 1    | col 2    |
// +----------------------+----------+
// | body row 2, col 1    | col 2    |
// +----------------------+----------+
//
// | Right | Left | Default | Center |
// |------:|:-----|---------|:------:|
// |   12  |  12  |    12   |    12  |
// |  123  |  123 |   123   |   123  |
// |    1  |    1 |     1   |     1  |
//
// Math support:
//
// $$
// f(a) = \frac{1}{2\pi i}\oint_\gamma \frac{f(z)}{z-a}\,\mathrm{d}z
// $$
//
// Inline mathematics:
// Area of a circle is $\pi r^2$ and
// $Nat(h^A,F)\cong FA$ is the yoneda lemma.
//
// Urls and links:
//
// <https://bing.com> [bing](https://bing.com) https://bing.com
//
// Code block:
// ```cpp
// some_code_block<example>([](t d)
// {
//     return "yes";
// });
// ```
//
// Another code block:
//
// ```
// some_code_block<example>([](t d)
// {
//     return "yes";
// });
// ```
//
// Test inline code:
// Some `simple` inline
// Try `array_alias<bool_alias>` brackets.
// Test single quotes: 'single_quote', with brackets 'all<id>'
// Some `array_alias<index_t <8 >>` more inline.
//
// Won't this cause an issue? Isn't this fun!
//
// Escapable characters:
// \\ \/ \' \` \" \@ \< \$ \# \> \* \-
//
// \`not code either\`, but `this is code`.
//
// \<thisisnotalink.com\>
//
// More `test formatting`. `isn't\' won't<\'\`>@
//
// [To custom-anchor](links.md#custom-anchor)
//
inline void markup_sample(){}

//| pre class description.
template
    < /*| pre param N. */ auto N     //< post param N.
    , /*| pre param M. */ uint32 M   //< post param M.
    , /*| pre param T. */ typename T //< post param T
    >
class sample_class
{
private:
    //| pre private uint8.
    uint8 private_uint8 = 0xFF;
public:
    //| pre assert.
    static assert(0 == (N & (N - 1)));
    //< post assert

    //| pre square N.
    const auto Square = N * N;

    //| pre index_square_t.
    using index_square_t = index_t<Square>;

    //| pre public uint8.
    uint8 public_uint8 = 0xFE;
    
    //| pre public string.
    const string public_string = "Hello World";
    //< post public string

    //| pre method bar.
    void bar(/*| pre x. */ uint32 x /*< post x. */, /*| pre b. */ bool b /*< post b. */)
    {}

    //| nested struct
    template <typename S /*< S doc.*/ >
    struct nested_struct
    {
        //| nested field 1.
        uint32 nested_field_1;
        //| nested field 2.
        S nested_field_2;
    }

    //| nested_class
    class nested_class
    {
    public:
        //| nested_nested_enum
        enum nested_nested_enum : uint1
        {
            //| On.
            ON,
            OFF
        }
    }

    bool_alias bool_literal = true;

    template <typename S>
    class Foo
    {
    public:
        template <auto N>
        using t = S[N];
    }

    //| Description
    Foo<uint32>::t<10> x;

}
//< post class description.

using non_exposed_alias = uint32;

class C
{
public:
    class D
    {
    public:
        using E = bool;

        template <typename S>
        struct T
        {
        }
    }
}

class D
{
public:
    C::D::E x;
}

class Foo
{
public:
    class Bar
    {
    public:
        template <typename T>
        class S
        {
        public:
            using S = bool;
        }
    }

    struct S
    {
    }

    Bar::S<S>::S x;
    Bar::S<C::D::T<D>>::S y;
}

class W
{
public:
  class X
  {
  public:
    class Y{}
  }
  class Y
  {
  public:
    class X{}
    X wyx;
    W::X wx;
    W::X::Y wxy;
  }
}

//| Always return constant value `X`.
template <auto X>
inline auto constant(decltype(X) x)
{
    return X;
}

class private_public_test
{
    //| private 1
    bool private_bool1;
public:
    bool public_bool1;
private:
    //| prviate 2
    bool private_bool2;
public:
    bool public_bool2;
private:
public:
public:
private:
private:
}

//| Memory attribute
using mem = [[memory]] uint8[5];

template <auto BitWidth>
class cast_test
{
public:
  const uint<BitWidth> MAX_VALUE_UNSIGNED = cast<uint<BitWidth>>((1 << BitWidth)-1);
  const int<BitWidth> MAX_VALUE_SIGNED = cast<int<BitWidth>>(cast<uint<BitWidth>>(1 << (BitWidth-1))-1);
}

//| Test svgbob display
// @@
// +---+      ,-----\->
// |   |----->|   .-|
// |___|-/  /->---|-|-O
// @@
using svgbob_test = string;

//| Test another svg image
// @@
// .---------------.
// |               |
// |  Hello World  |
// |               |
// '---------------'
// @@
//
//  @@
//  .--.
// (    )
//  `--'
// @@
using svgbob_test2 = string;

class svgbob_test3
{
public:
  //|
  // @@
  //  .--.     .--.
  // (    ) = (    )
  //  `--'     `--'
  // @@
  static assert(0 == 0);
}

inline auto apply((auto) -> auto f, auto x)
{
  return f(x);
}

//| Test long template list formatting
template <typename A, typename B, typename C, typename D, typename E, typename F, typename G>
using long_template_test = bool;

//| Test template template parameters
template <template <typename, auto> typename foo>
class template_template_test{}

//| Test long template template parameters
template <
    template <
        typename,
        auto,
        typename,
        typename,
        typename,
        typename,
        typename,
        typename,
        typename,
        typename,
        typename,
        typename,
        typename>
    typename foo>
class long_template_template_test{}

//| Test many argument function
inline float32 many_arg_fun(
    uint8 a,
    uint32 b,
    uint64 c,
    uint128 d,
    string e,
    float32 f,
    float32 g,
    ( uint8
    , uint8
    , uint8
    , uint8
    , uint8
    , uint8
    , uint8
    , uint8
    , string
    , string
    , index_t<8>
    ) -> int64 fn
    )
{
    return 0;
}

class accessor_test
{
public:
  struct s
  {
    uint8 x;
    uint8 y;
    uint8 z;
  }
  s s1 = {1, 2, 3};
  uint8 w = s1.x;
}
