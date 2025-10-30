Make a TextMate grammar for the Kanagawa language. This grammar will be to create a Visual
Studio Code extension (vsix) that provides syntax highlighting and code folding.

## Overview and Basic Syntax

The language name is "Kanagawa", and for the grammar the scopeName should be "source.kanagawa".

Kanagawa syntax looks superficially similar to C++; Lines are terminated with a semicolon `;`
(with the exception of things like class, struct, union, and module declarations and import statements, etc.). Comments,
function declarations, template declarations, expressions, and many other things are similar.

However, there are some noteable differences, so carefully review the remainder of this document
and the additional references provided at the bottom. Your goal should be to provide a correct grammar
sacrificing feature richness for correctness if needed.

## Language Keywords

The list of keywords for the language is in the following list:

```
    [ "as"
    , "atomic"
    , "auto"
    , "barrier"
    , "bitsizeof"
    , "bitoffsetof"
    , "bool"
    , "break"
    , "bytesizeof"
    , "byteoffsetof"
    , "cast"
    , "case"
    , "class"
    , "clog2"
    , "concat"
    , "const"
    , "decltype"
    , "default"
    , "do"
    , "else"
    , "enum"
    , "export"
    , "extern"
    , "false"
    , "fan_out"
    , "float32"
    , "for"
    , "if"
    , "import"
    , "inline"
    , "int"
    , "lutmul"
    , "module"
    , "mux"
    , "noinline"
    , "private"
    , "public"
    , "reorder"
    , "return"
    , "static"
    , "static_assert"
    , "string"
    , "struct"
    , "switch"
    , "template"
    , "true"
    , "typename"
    , "uint"
    , "union"
    , "unrolled_for"
    , "using"
    , "void"
    , "while"
    ]
```

## Code scopes and folding

Scopes / blocks within the code are delineated with curly brackets, `{` and `}`, like many other languages. This should be used for cold folding regions.

## Comments

The language provides syntax for several varieties of comments. Like C++, it supports both
single-line and multi-line comments:

```
// This is a single-line comment
```

```
/* This is a
   multi-line comment */
```

Comments can appear at the end of module declarations and import statements:

```
module Foo // This is a comment
{
    // ...
}

import processor.risc_v // This is also a comment
```

Kanagawa also supports a syntax for code documentation via comments. For example, you can annotate
(document) the code following the comment by using this syntax:

(using single line comments)
```
//| Adds two values of the input type.
// Returns the result as the output type, which might be larger.
// e.g. 3 fits in a uint2, but 3 + 3 does not.
template <typename R, typename T>
inline R add(T a, T b)
{
    return a + b;
}
```

(using multi-line comments)
```
/*|
Adds two values of the input type.
Returns the result as the output type, which might be larger.
e.g. 3 fits in a uint2, but 3 + 3 does not.
*/
template <typename R, typename T>
inline R add(T a, T b)
{
    return a + b;
}
```

Kanagawa also supports a comment syntax that applies documentation to the declaration
before the comment. For example:

```cpp
template
  < typename T //< Type of number to square
  , typename R //< Return type of squared number
  >
inline R square
    ( T x //< Input number
    )
{
    return x * x;
}
//< Square a number
```

Another example:

```
enum RGBColor : uint2
{
    //| Red
    RED
    //< Red
,
    //| Green
    GREEN
    //< Green
,
    //| Blue
    BLUE
    //< Blue
}
```

There is also a multi-line comment flavor of this:

```
template <typename T>
struct optional
{
    //| Pre: valid flag
    bool is_valid /*< Post: valid flag*/;

    //| Pre: underlying value
    T value /*< Post: underlying value*/;
}
```

## Attributes

Some elements in the language can be annotated with "attributes" that occur before the element
being annotated. This is similar to the attribute feature in C#. In Kanagawa, these attributes
can be applied at the class, class instance, function, and function argument level.

Attributes are indicated with a pair of square brackets. For example:

```
    [[async]] void exec(task_t task, then_t then)
    {}
```

Multiple attribute values can be supplied inside the square brackets. For example:

```
[[memory, initialize]] uint32[512] _mem;
```

Note: This syntax is different than C# which just uses a single square bracket.

Some attributes take arbitrary types, for example:
```
    [[latency(LATENCY)]]
    float32 op(float32 x, float32 y);
```

But many take specific keywords:

```
[[memory, initialize]] uint32[512] _mem;
```

Here is the list of keywords that appear in attributes:

```
call_rate
ecc
fifo_depth
latency
max_threads
name
schedule
thread_rate
transaction_size
async
atomic
last
memory
initialize
no_backpressure
pure
non_replicated
pipelined
quad_port
reorder_by_looping
reset
unordered
```

## Primitive Data Types

Kanagawa supports signed and unsigned integers of arbitrary bit widths. There are two ways
to declare a signed or unsigned variable of some width:

```
uint17 a; // Unsigned integer 17 bits wide
uint<17> b;  // Also an unsigned integer, 17 bits wide

int14 c; // Signed integer, 14 bits wide
int<14> c; // Also a signed integer, 14 bits wide
```

In addition to the signed and unsigned integers, Kanagawa supports the following data types:

- void
- bool
- string
- float
- float32

Note that there is no native `float64` type.

## Literals

Kanagawa supports numeric and string liters. The syntax is similar, but not identical to languages like C# and C++.

### Integer Literals

Integer literals may or may not have a prefix indicating the base:

```
const a = 0x1234; // Hexidecimal value
const b = 0b11010; // Binary value
const c = 0o214; // Octal value
const d = 1122; // Decimal value
```

Integer literals can also have embedded underscore `_` characters to make them easier to read:

```
static assert(0x_1234_abcd == 0x1234abcd);
```

Literals can also have a signed/unsigned and width suffix.

```
const auto x = -10i32; // X will be int32
const auto x = 10u16; // X will be uint16
```

## Float Literals

Float literals follow rules similar to C++. Note that there is no provision
for double-precision, so the syntax is somewhat simpler:

```
float x = 2.5;
float32 y = 0.4;
```

### String Literals

Strings are delimeted with double-quote characters:

```
const string x = "This is a string";
```

### String interpolation

An interpolated string is a string literal that might contain
interpolation expressions.  When an interpolated string is resolved to
a result string, the compiler replaces items containing interpolation
expressions with the string representations of the expression results.

The structure of an item with an interpolation expression is as follows:

`{<interpolationExpression>[=][,<alignment>][:<formatString>]}`

where

- `interpolationExpression` is an expression that produces a result to be
formatted. When followed by the `=` character, the result string is prefixed
with string representation of the expression followed by `=` and then formatted
expression value.
- `alignment` is an optional constant expression whose value defines the minimum
number of characters (padded with space) in the string representation of the
expression result. If positive, the string representation is right-aligned; if
negative, left-aligned.
- `formatString` is an optional format string supported by the type of the
expression result.

A format string consists of a format specifier followed by an optional
precision. The following format specifiers are supported:

`b` binary
`d` decimal
`o` octal
`x` hexadecimal
`X` hexadecimal using uppercase letters

All format specifiers are valid for expressions of integer, float and enum
types. The `b` specifier is also valid for bool expressions. Format specifiers
are not allowed for expressions of other types.

When specified, `precision` defines the minimum number of digits in the result
string. Otherwise number of digits is determined from the expression value for
the decimal format, and from the expression type for the other numeric formats.

To include an opening brace `{` in the text produced by an interpolated
string, use two braces, `{{`.

Examples:

```
    uint16 val = 0xf00;
    "{val}";          // "3840"
    "{val:x}";        // "0f00"
    "{val:x3}";       // "f00"
    "{val + 3,10}";   // "      3843"
    "{val + 3,-10}";  // "3843      "
    "{2+2=}";         // "(2 + 2) = 4"
    "{{val}";         // "{val}"

    struct S { uint32 a; bool b; }
    S s = {45, true};
    "{s}";           // "{a:45, b:true}"

    uint32[4] a = { 4, 2, 9, 10 };
    "{a}";          // "[4, 2, 9, 10]"

    enum Color : uint4 { Red, Green, Blue }
    Color c = Color::Green;
    "{c}";          // "Green"
```

## Arrays

Kanagawa supports arrays:

```
uint32[8] an_array;
```

Note that unlike some languages the array dimension occurs before the variable name;

## Variable Declarations

Variable declarations look similar to C/C++

## Functions

Kanagawa functions look like those in C++, for example:

```
uint15 add(uint32 x, uint7 y)
{
    return x + y;
}
```

Note that functions can have an optional modifier, `inline` or `noinline`. These
modifiers are mutually exclusive.

## Classes

Kanagawa supports classes, and has visibility modifiers similar to C++. Note that Kanagawa
does NOT have the concept of the `this` so there is no `this` keyword, nor are there  `parent` or `superclass` keywords.

## Structs and Unions

Kanagawa supports structs and unions. Note that these can be nested.

```
struct AsBits
{
    uint1 a;
    uint1 b;
    uint1 c;
    uint1 d;
    uint1 e;
    uint1 f;
    uint1 g;
    uint1 h;
}

union MyUnion
{
    AsBits as_bits;
    uint8 as_scalar;
}
```

## Enums

Kanagawa supports Enums:

```
enum Bar : uint1
{
    On, Off
}
```

```
enum Color : uint2
{
    Red = 0,
    Green = 1,
    Blue = 2
}
```


## Operators

### Binary Operators

Kanagawa supports the following binary operators:

```
+
-
*
/
%
&
&&
|
||
^
^^
<<
>>
```

### Unary Operators

Kanagawa supports the following unary operators:

```
~
-
!
bitsizeof
bytesizeof
clog2
```

### Comparison Operators

Kanagawa supports the following binary operators:

```
==
!=
>
>=
<
<=
```

### Assignment Operators

Kanagawa supports the following assignment operators:

```
+=
-=
*=
/=
%=
&=
|=
^=
&&=
||=
^^=
<<=
>>=
```

### Ternary Operator

Kanagawa also supports the ternary operator, just like C/C++:

```
x = a ? b : c;
```

Note that `bitsizeof`, `bytesizeof` and `clog2` when used look like function calls:

```
uint32 x;
auto width = bitsizeof(x); // width will be 32
```

### Cast Operators

Kanagawa supports several cast operators:

```
uint48 x = Foo();
y = cast<uint32>(x); // Explicit cast to uint32
```

Note that you may see source code using these types of casts, but they are all implemented as functions in the library and not as a language operators.

```
reinterpret_cast
static_cast
checked_cast
safe_cast
```

### Miscellaneous Operators

Kanagawa supports the `decltype` operator to return the type of some variable or expression.

## Static keyword

The `static` operator can be used to indicate that the subsequent or contained
expression can be (or should be) evaluated at compile time:

```
static assert(x == y); // x and y must be compile-time constants
                       // or constant template arguments

if (static(i % 2 == 0))
{
    // ...
}
```

There is also a `static for` looping construct (see section on looping below)

## Templates

Kanagawa templates are very similar to C++:

```
template <typename T>
inline void foo(T x)
{
    // ...
}

template<auto Width>
inline uint<Width> crc_reduce(uint<Width> a, uint<Width> b)
{
    return a ^ b;
}

template<typename T>
class Bar
{
    T x;
    // ...
}
```

## Looping

Kanagawa supports these types of loops:


```
atomic do
{
    // ...
} while (x > y);
```

```
static for(const auto i : N)
{
    // ...
}
```

```
do
{

} while (x > y);
```

Note that there are other loop-like constructs that implemented as functions in the library that take lambda expressions (for example: `pipelined_for`).

Kanagawa also supports these jump statements in loops: `break` and `continue`.

## Conditional Statements

Kanagawa supports conditional statements like C++:

```
if (x == y)
{
}
else if (x == z)
{
}
else
{
}
```

```
x = a ? b : c;
```

```
switch(value)
{
    case 1:
        // ...
        break;
    case 2:
        // ...
        break;
    default:
        // ...
}
```

## Modules

Kanagawa supports the concept of modules. A file that exposes a module contains a module declaration listing those symbols it exposes:

module compiler.device.config
{
    module compiler.device.config \ hardware.config
    , module hardware.config
}

Note the syntax with the "\" character, which is used to declare that a
module re-exposes some other module's symbols.

There is also syntax to import a module:

```
// Import the hardware.config module
import hardware.config
```

## Additional References

To better understand the language grammar, you may also review the following documents in the #codebase:

`doc/effective-kanagawa.md`
`doc/sandcastle.md`
`compiler/hs/lib/Language/Kanagawa/Parser/Lexer.hs`

There are a number of example of Kanagawa source code in the Kanagawa standard library, for example:

`library/processor/risc_v.k`
