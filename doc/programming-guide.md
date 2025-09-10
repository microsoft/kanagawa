% Programming Guide

# Program structure

A Kanagawa program, sometimes also referred to as _hardware design_, or
_design_, is constructed from types (e.g. classes, structs, unions, enums) and
functions. There is no concept of global variables, and consequently no global
state. All state is declared either as class data members or local static
variables within functions.

Unlike software programs, hardware designs don't have a concept of an entry
point. One way to think about it, is that hardware design is not something that
you run, like a software program, but rather something that you instantiate,
like an object. A design can be instantiated in various contexts: it could be
used as an IP block in a larger hardware design implemented in Verilog, it
could be wrapped in a board support package and exposed over PCIe as an
FPGA-based accelerator, or it could be realized as an ASIC chip. Regardless of
the embodiment, what is instantiated, is a [type exported](#exported-types)
from a Kanagawa program. Although any type can be exported, Kanagawa programs
usually export at least one [class](#exported-classes).

A design doesn't have to be fully implemented in Kanagawa, it can use
(instantiate) externally defined IP blocks, exposed to the rest of Kanagawa
program as [external class](#external-classes). Such classes declare their
interface in Kanagawa, but the implementation is defined in Verilog.

# Language

## Comments

```
// a single line comment

/*
  multi-line comment
*/

/*
  /*
    nested multi-line comments are supported
  */
*/
```
### Documentation comments

Declaration can be annotated with documentation using special comments. Comments starting with `|` are used to add documentation to the following declaration, and comments starting with `<` to the preciding declaration, e.g.:

<!-- invalid -->
```
    //| Read a 32-bit word from hart's data memory at a 32-bit aligned address.
    // More resource efficient than `dmem_read` when unaligned address support
    // is not required.
    inline int_t dmem_read_aligned
        ( hart_index_t hid  //< Hart index.
        , uint_t addr       //< DMEM byte address to read from.
                            // Must be 32-bit aligned.
        )
    {
        return core.dmem_read_aligned(hid, checked_cast<dmem_addr_t>(addr));
    }
```

For the full syntax of documentation comments refer to the Sandcastle documentation.

## Types

### Integers

Integer types can have any width, there are no 'natural' widths.  Signed
integers are encoded in two's-complement.
```
uint7 x; // unsigned, 7-bit integer
int66 y; // signed, 66-bit integer
```

The width of an integer can also be a compile-time expression:
```
const auto N = 16;
int<N> x;
```

#### Integer literals

Integer literals can be written in decimal, hexadecimal, octal, or binary:
```
uint32 decimal = 77;
uint8 octal = 0o115;
uint8 hex = 0x4D;
uint12 bin = 0b01001101;
```

Digits of an literal may be separated with underscores. Placement of the
underscores doesn't have any meaning, it is used purely to increase readability.

```
auto b = 0b_0000_0000_1010;
auto d = 50_403_934_498_988;
auto h = 0x_1234_abcd;
```

Type of integer literal is inferred from the value unless a type is explicitly
specified with literal type specifier. The type specifier is a suffix consisting
of character `i` or `u`, indicating types `int` and `uint` respectively,
followed by the type width in bits. The type specifier suffix may be separated
from number with an underscore.

```
auto x = 10;     // uint4 inferred
auto y = 0xA_i8; // int8
auto z = 10u8;   // uint8
```

The value of a typed literals must be within the range for the specified type.

<!-- invalid -->
```
auto x = 10_i4;  // error: Literal out of range for `i4`
```

#### Integer operators
Kanagawa supports the following operators on integers:

Expression   | Meaning
:----------  | :------
`a + b`      | addition
`a - b`      | subtraction
`a * b`      | multiplication
`a % b`      | remainder
`a / b`      | division
`a & b`      | bitwise and
`a \| b`     | bitwise or
`a ^ b`      | bitwise exclusive or
`a << b`     | left shift
`a >> b`     | right shift
`-a`         | additive inverse
`~a`         | bitwise not
`a++`        | a = a + 1
`a--`        | a = a - 1

For right shifts, the sign of the left-hand side operand determines if the shift
will be arithmetic or logical.

Division and remainder operators are supported only if the expression can be
computed at compile time, or if the right-hand-side is a power of 2.

#### Integer type system
The Kanagawa type system for integers is different than CPU-based languages. In
particular, the type of an expression is wide enough to represent the result
without loss of data.  For example:

```
uint8 x;
uint8 y;
uint9 z = x + y; // the type of (x + y) is uint9
auto w = x + y; // the type of w is uint9
```

The following table shows how Kanagawa computes the width of of an expression
from the width of the operands used in the expression.  The table assumes all
operands have the same signedness.  If the operand types do not have the same
signedness, then all unsigned operands are first converted to signed (which
widens their width by 1).  Subtraction always converts unsigned operands to
signed.

Expression   | Width
:----------  | :------
`a + b`      | max(a, b) + 1
`a - b`      | max(a, b) + 1
`a * b`      | a + b
`a & b`      | max(a, b)
`a \| b`     | max(a, b)
`a ^ b`      | max(a, b)
`a << b`     | a + (1 << b) - 1
`a >> b`     | a
`-a`         | a + 1

For certain expressions with an operand known at compile-time the compiler can
infer a sufficient type that is narrower than what would be indicated by the
above rules.

Expression        | Type
:-----------------|:-------------------------------
`uint<x> % 1`     | `uint1`
`int<x> % 1`      | `int2`
`uint<x> % N`     | `uint<min(x, clog2(N)>`
`int<x> % N`      | `int<min(x, 1 + clog2(N))>`
`uint<X> & 0`     | `uint1`
`int<X> & 0`      | `int2`
`uint<x> & N`     | `uint<min(x, clog2(N + 1))>`
`uint<x> & (-N)`  | `uint<x>`
`int<x> & N`      | `int<1 + clog2(N + 1)>`
`int<x> & (-N)`   | `int<max(x, 1 + clog2(1 - N))>`
`uint<x> << N`    | `uint<x + N>`
`int<x> << N`     | `int<x + N>`
`uint<x> >> N`    | `uint<x - N>`
`int<x> >> N`     | `int<x - N>`
`uint<x> * 0`     | `uint1`
`int<x> * 0`      | `int2`
`uint<x> * N`     | `uint<x + clog2(N)>`
`uint<x> * (-N)`  | `int<x + 1 + clog2(N)>`
`int<x> * N`      | `int<x + clog2(N)>`
`int<x> * (-N)`   | `int<x + 1 + clog2(N)>`
`uint<x> / N`     | `uint<x - clog2(N)>`
`int<x> / N`      | `int<x - clog2(N)>`

#### Implicit integer conversions
Kanagawa will silently widen/narrow integer types as necessary.
For example, the following code is valid:

```
uint8 x;

// The type of "x + 1" is uint9
// but it is implicitly sliced to uint2 on assignment to z
uint2 z = x + 1;
```

There are circumstances (like [mux](#mux)) where a group of integers must be
implicitly converted to a common type.  The common type is the narrowest integer
type that can represent all values representable by the group of source types.

- If any source type is signed, then all unsigned source types are converted to signed by zero-extending by one bit
- The type of the resulting expression is equal to the widest source type

Warning on narrowing conversion of initialization value can be enabled using
`--Wconversion` or `--Wall` command line switches.

The warning is emitted when initialization value used in a variable declaration
would be converted with potential loss of data, e.g.:

```
inline void Foo(uint32 x, uint32 y)
{
    uint32 z = x + y;
}
```

would result in the following warning:

    test.k:3:24:
      |
    3 |             uint32 z = x + y;
      |                        ^
    warning: Narrowing conversion from 'uint33' to 'uint32' [--Wconversion]

The recommended fix for the issues is to explicitly cast the initialization
value to the target type, and declare the variable as `auto` to avoid redundant
type specification. If the narrowing conversion is expected and desired use
`cast` operator:

```
inline void Foo(uint32 x, uint32 y)
{
    auto z = cast<uint32>(x + y);
}
```

If none of the expected values would result in truncation, use `checked_cast`
helper function instead of `cast`. It will trigger an assert in simulation if
the value is truncated.

When narrowing conversion happens as part of initializer list, the warning
points to specific element of the list that results in narrowing conversion.
For example:

```
struct Foo
{
    optional<uint32>[2] items;
}

Foo foo =
    { { { false, 10}
      , { true, -20}
      }
    };
```

compiles with the following warning:

    test.k:7:17:
      |
    7 |       , { true, -20}
      |                 ^
    warning: Narrowing conversion from 'int6' to 'uint32' [--Wconversion]

Narrowing conversions are prohibited in [designated initializers](#designated-initializers).
For example:

<!-- invalid -->
```
struct Foo
{
    optional<uint32>[2] items;
}

Foo foo =
    { .items =
        { {.is_valid = false, .value = 10}
        , {.is_valid = true, .value = -20}
        }
    };
```

will result in compiler error:

    Error 1: test.k:9:39:
      |
    9 |         , {.is_valid = true, .value = -20}
      |                                       ^
    Invalid conversion from `int6` to `uint32`
    Insert an explicit cast to silence this issue `cast<uint32>()`

The `--check-narrowing-assignment` command line switch adds runtime
assertion checking which will fire if information is lost in a narrowing
conversion when the value of a variable is updated (not during variable
initialization).  For example:

```
inline void Foo()
{
    uint2 result = 3;

    result = result + 1;
}
```

will result in the following runtime error:

    Assertion failed: Source value of narrowing conversion cannot be represented by the result type.

The `numeric.int.operator.modular` module contains functions which can be
used to perform arithmetic when interger overflow/underflow (wrapping) behavior
is expected and should not trigger an assertion.

#### Index/Count type aliases

##### index_t
`index_t<N>` is an unsigned integer type wide enough to represent the values:
`0, 1, 2, ..., N-1`.  This type is intended to be used to define an address or
index for elements of an array or a memory.

```
const auto array_size = 8;

uint32[array_size] a;

// wide enough to represent an index into a
index_t<array_size> idx;

println(a[idx]);
```

##### count_t
`count_t<N>` is an unsigned integer type wide enough to represent the values:
`0, 1, 2, ..., N`.  This type is useful for holding sizes or counts.

```
const auto max_elements = 8;

// wide enough to represent the maximum number of elements
count_t<max_elements> current_element_count = 0;
```

### Booleans
Boolean variables in Kanagawa work similarly to boolean variables in other
programming languages. Variables of type `bool` can only have a value of
`false` or `true`. Conversion between boolean and integer types requires an
explicit cast.

```
bool b = false;
bool c = !b;
uint1 x = 3;
bool d = cast<bool>(x);
```

#### Boolean operators
Kanagawa supports the following operators on boolean values:

Expression   | Meaning
:----------  | :------
`a \|\| b`   | logical or
`a && b`     | logical and
`a ^^ b`     | logical exclusive or
`!a`         | logical not

### Floating-point
Kanagawa support 32-bit floating-point numbers.  Unlike C-based programming
languages, Kanagawa does not support a trailing `f` or `F` character after a
floating-point literal.

```
inline float32 times_pi(float32 x)
{
    return x * 3.14159;
}
```

#### Floating-point operators
Most floating-point operations are implemented as functions rather than
operators.  Kanagawa supports the following operators (which are implemented as
device-specific library functions):

Expression   | Meaning
:----------  | :------
`a + b`      | addition
`a - b`      | subtraction
`a * b`      | multiplication

### Miscellaneous operators

#### Sizeof operators
`bitsizeof` evaluates to the width in bits of an expression or
type.  `bytesizeof x` evaluates to `(bitsizeof x) / 8`.  `bytesizeof` generates
a compile error if the width is not a multiple of 8 bits.  Parenthesis are
optional.

```
uint32 x;
const auto y = bitsizeof(x);        // 32
const auto z = bitsizeof uint32;    // 32
const auto w = bytesizeof x;        // 4
```

#### Struct field offset
`bitoffsetof` evaluates to the offset (measured in bits) from the start of a
structure type to a specified field.  `byteoffsetof` evaluates to the offset
measured in bytes (and generates a compile error if the offset is not a multiple
of 8 bits).

```
struct S
{
    uint64 x;
    uint16 y;
}

const auto t = bitoffsetof(S, x); // 0
const auto u = bitoffsetof(S, y); // 64
const auto v = byteoffsetof(S, y); // 8
```

#### Ternary operator
`a ? b : c`  evaluates to `b` if `a` is true, and evaluates to `c` otherwise.  `a`
must be of `bool` type.  The types of `b` and `c` must be the same type, or both
must be [implicitly convertible](#implicit-integer-conversions) to a common type.

```
bool cond;
uint32 x;
uint32 y;

uint32 z = cond ? x : y;
```

### Enumerations
Enumerations are user-defined types that consist of a set of named integral
constants (enumerators). Enumerations in Kanagawa are similar to C++
enumerations except that Kanagawa enumerations must always specify a base type.
If no initialization is provided for an enumerator its value is set to the value
of the preceding enumerator plus 1, or to 0 if it is the first enumerator. Here,
the constants `RED` , `GREEN` , and `BLUE` are set to 0, 1, and 2, respectively.

```
enum RGBColor : uint2
{
    RED,
    GREEN,
    BLUE
}
```

In the example below, the enumerator `ONE` is assigned the value of 1; `FIVE` is
assigned the value of the expression `ONE + 2` , which is 3; `NINE` is assigned
the value of 0; and, finally, `FIFTEEN`  is assigned the value 1.


```
enum CrazyNumbers : uint5
{
    ONE = 1,
    FIVE = ONE + 2,
    NINE = 0,
    FIFTEEN
}
```
Enumerators can only represent values within the range of their base type.

Enumerators are almost always qualified with the enumeration name:

```
enum RGBColor : uint2
{
    RED,
    GREEN,
    BLUE
}

RGBColor color = RGBColor::GREEN;
```

The one case where a enumerator name does not need to be fully qualified is in
the definition of an enumeration.  In the following example, `RED` is referenced
in the defintion of `GREEN`.

```
enum RGBColor : uint2
{
    RED,
    GREEN = RED + 1,
    BLUE
}
```

### Strings
Strings are represented with the `string` type.  Strings variables can be used
in most contexts that other variables can be used (e.g. assigned inside of
conditionals, passed as arguments to functions).  Strings do not affect synthesized
hardware.  The only useful actions that can be performed on strings are:
* Strings can be printed with `print` and `println`
* Strings are used with `inspectable()` to describe an inspectable variable
  (the value of these strings must be known at compile time)

The following restrictions apply to strings:
* Shared variables (class members and static locals) cannot be initialized to a non-empty string
* Strings cannot be passed across the interfaces of exported classes
* Strings cannot be stored in unions
* Strings cannot be stored in memories

#### Interpolation

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

#### Comparison

String literals and constants of string type that don't contain interpolation
expressions can be compared for equality at compile time, e.g.:

```
    const string foo = "foo";

    static if (foo == "foo")
    {
        static assert(true);
    }
    else
    {
        static assert(false);
    }
```

### Structures
Structures are defined with the `struct` keyword, as shown in the following code
sample.

```
struct ExampleStruct
{
    uint8 x;
    uint4 y;
}

inline void MyStructFunction()
{
    ExampleStruct es;
    es.x = 0x20;
    es.y = 0x02;

    // Could also use an initializer list:
    // ExampleStruct es = { 0x20, 0x02 };
}
```
Structures are stored in little-endian format (the first element occupies the
least-significant bit).

### Unions
Unions allow different data types to be efficiently stored in the same memory
location.

```
union ExampleUnion
{
    uint8 larger;
    uint4 smaller;
}

inline void MyUnionFunction()
{
    ExampleUnion eu;
    eu.larger=0x20;
    eu.smaller=0;
    println(eu.smaller);
}
```

All union fields are packed such that the least significant bit of each field
is the least significant bit of the union.  The width of a union is equal to the
width of the widest field.  A write to a field of width `F` in a union of total
width `N` affects the lower `F` bits of all fields.  The remaining bits (up to
`N-F`) of each field are undefined after the write.

### Arrays
Kanagawa supports arrays:

```
uint32[4] x; // An array of 4 uint32 values
array<uint32, 4> y; // Another syntax

uint8 i = 1;
uint32 z = x[i]; // reading from an array
x[i + 1] = z + 2; // writing to an array
```

#### Out-of-bounds array access
Writes beyond the end of an array are silently dropped.  Array reads behave as
if the array size was rounded up a power of two (padding with zeros) and only
the relevant least-significant bits of the index used to access the padded
array.

```
uint8[6] x;
uint4 i = 9;

// Prints x[1] because only the lower 3 bits of i are used
println(x[i]);
```

### Memories
Kanagawa supports memories in addition to arrays.  Memories have different
semantics and performance characteristics than arrays.

```
class Foo
{
    memory<uint32, 4> x;

    void F()
    {
        uint8 i = 1;
        uint32 z = x[i]; // reading from a memory
        x[i + 1] = z + 2; // writing to a memory
    }
}
```

#### Semantic differences between memories and arrays
- Arrays can be copied and passed by value, memories cannot
- If two threads write to the same memory at the same time (even with different
  addresses), one write will be dropped.  See [Data Races](#data-races).

#### Out-of-bounds memory access
Memory read and write addresses are implicitly converted the narrowest unsigned
integer type that can represent all possible addresses for the memory in
question (`index_t<N>` where `N` is the number of elements in the memory).  If
the converted index is greater than or equal to `N` then the access is
considered out-of-bounds. Out-of-bounds reads produced undefined results.
Out-of-bounds writes have no affect.  Note that if `N` is a power of two, then
out-of-bounds accesses are impossible because the conversion to `index_t<N>`
results in an address that is in the range `[0, N)`.

#### Memory initialization
Memories can be declared with initial values (see [Initializer Lists](#initializer-lists)).
For FPGA targets, initial values are set when the FPGA image is programmed (_not_ during reset).
For ASIC targets, initial values are programmed to the memory after reset.

```
static memory<uint32, 4> mem = { 3, 8, 5, 1 };
```

#### Read-only memories
Memories can be declared read-only with the `const` keyword.  Declaring a memory read-only can result
in less resource usage.

```
const memory<uint32, 4> read_only_mem = { 10, 4, 3, 6 };
```

#### Error Correcting Codes

##### memory_ecc
`memory_ecc` can be used to declare a memory with ECC support.  Reads from a
`memory_ecc` return a value of type: `ecc<T>`.

```
import data.memory

class Foo
{
    memory_ecc<uint32, 512> mem;

    void F(uint9 addr, uint32 data)
    {
        // Store a uint32 into the memory
        mem[addr] = data;

        // Loads return ecc<uint32>
        ecc<uint32> result = mem[addr];

        if (result.error)
        {
            // An error (corrected or not) was encountered
        }

        if (result.data.is_valid)
        {
            // Either there was no ECC error
            // or an error was corrected
            // result.data.value can be used
            assert(result.data.value == data);
        }
    }
}
```

`memory_ecc_norep` behaves like `memory_ecc` but without replication.

##### memory_ecc_strict
Reads from a `memory_ecc_strict` return a value of type: `optional<T>`.
The `is_valid` field is set to true if no ECC errors (corrected or not) were detected.

##### memory_ecc_relaxed
Reads from a `memory_ecc_relaxed` return a value of type: `optional<T>`.
The `is_valid` field is set to true if no ECC uncorrectable errors were detected.

##### ECC restrictions
* Memories with ECC enabled may not have initial values
* Quad port memories do not support ECC

##### ecc attribute

The ECC memory aliases defined above are implemented with the `ecc` attribute
and an associated `inline` function.  The function creates values returned by
reads from the memory. The function takes 3 arguments:

* error - A bool set to `true` when an ECC error was detected,
          regardless of whether it was corrected or not.

* valid - A bool set to `true` if there was no ECC error, or an error
          was corrected.

* value - Data read from memory. The type of the argument is the same
          as the type of declared memory word.

#### Multi-dimensional arrays

`uint32[4][2]` can be thought of as a table containing 4 rows and 2 columns:

For example, the following table:

| Row   | Column 0 | Column 1 |
|-------|----------|----------|
| 0     | 3        | 4        |
| 1     | 9        | 2        |
| 2     | 7        | 8        |
| 3     | 1        | 5        |

can be expressed with the following code:

```
uint32[4][2] table = { {3, 4}, {9, 2}, {7, 8}, {1, 5} };
```

The expression: `table[1]` represents row 1.  It has has type `uint32[2]`, and
value `{9, 2}`.

The expression `table[1][0]` represents the entry at row 1, column 0.  It has
type `uint32` and value `9`.

Arrays of memories are supported:

```
static memory<uint32, 32>[4] _mem; // An array of 4 memories
```

Similarly, memories of arrays are supported:

```
static memory<uint32[4], 32> _mem; // A memory with element type = uint32[4]
```

Memories of memories are not allowed.

### Function types
The syntax for a function type is `(parameter types)->return type`:

```
using callback = (uint32, bool) -> uint8;
```

Function types can optionally include parameter names:

```
using callback = (uint32 x, bool f) -> uint8;
```

### Exported types

A type defined in a design can be exported, making it available in generated
Verilog. Types are exported using following syntax:

    export type_name;

The `type_name` can be either a name of a concrete type, such as a struct or
enum, or an instance of a type template, e.g.:

    template <T>
    struct Foo
    {
        T x;
    }

    enum Bar : uint1
    {
        On, Off
    }

    export Foo<uint32>;
    export Bar;

The `export type` declaration can appear in any context where a type declaration
is valid.

When exported, different Kanagawa types map to different Verilog constructs.
Most designs will export at least one [class](#exported-classes), which maps to
Verilog module.

#### Implicit type export

Any types referenced by exported types are implicitly exported as well. For
example, given the following type declarations:

    template <typename T>
    struct Foo
    {
        T x;
    }

    template <typename T>
    struct Bar
    {
        Foo<T> foo;
    }

    using Bux = Bar<uint32>;

the following type export declaration:

    export Bux;

is equivalent to:

    export Foo<uint32>
    export Bar<uint32>;
    export Bux;

#### Generated names

Names in the generated code (e.g. Verilog or C++) corresponding to symbols in
Kanagawa program are by default constructed by the compiler, which means that
they often can be long and not particularly human friendly. The `[[name]]`
attribute can be used to explicitly specify the external name of a symbol.

    // export the type Foo<uint32> as MyStruct
    [[name("MyStruct")]]
    export Foo<uint32>;

Note that while the compiler created names are guaranteed to be unique, the
programmer has to make sure that names in `[[name()]]` annotations don't collide.

### Type aliases
Type aliasing allows giving new names to existing data types. Type aliases can
be created for both built-in and user-defined types.

```
using UnsignedInteger = uint32;
using DWORD = UnsignedInteger;
using callback_t = (int32, uint5) -> bool;
```

### Subtyping

Kanagawa defines subtyping relationship for integer types and function types. A
value of type `S` can be safely used in context expecting a type `T` if and only
if the type `S` is a subtype of the type `T`.

Using the notation `S <: T` to mean that type `S` is a subtype of type `T`, the
following relations are defined for integer types:

- `int<N>  <: int<M>`  when `N <= M`
- `uint<N> <: uint<M>` when `N <= M`
- `uint<N> <: int<M>`  when `N < M`

For function types the subtyping relationship:

    [[attr1]] ([[paramAttr1]] x1, ...) -> r1 <: [[attr2]] ([[paramAttr2]] x2, ...) -> r2

requires all of the following to be true:

- `r1 <: r2`
- `x2 <: x1`, ...
- `paramAttr2` is subset of `paramAttr1`, ...
- `attr2` is a subset of `attr1`

For example function type `(uint32) -> uint8` is a subtype of `(uint16) -> uint16`.
Intuitively this is because callers expecting a function `(uint16) -> uint16`
can call a function of type `(uint32) -> uint8` without any data loss:
 - argument of type `uint16` can be losslessly upcast to `uint32`
 - result of type `uint8` can be upcast to `uint16`

Subtyping of function types applies in any context where functions are used
as values, such as functions passed as arguments to higher order functions,
arguments passed for function type template parameters, and class callbacks
default and initialization values.

### Classes

A class is a container of state and methods.  Class instances are called
objects.  All objects are known at compile time, there is no way to create an
object at runtime.

```
class ExampleClass
{
private: // Private members are not visible from the outside

    // Member variable with an initial value
    uint32 _y = 5;

public: // Public members are visible from the outside

    // Class method
    uint32 ExampleMethod(uint32 x)
    {
        return x + _y;
    }
}

inline void EntryMethod()
{
    // Instance of a class
    static ExampleClass _myExample;

    uint32 y = _myExample.ExampleMethod(4);
    print("{y=}\n");
}
```

#### Callbacks

While public methods allow calling into an object, callbacks are a mechanism to
allow an object to call back. Callbacks are declared as private members with
a function type. Although function type declaration doesn't have to specify
parameter names, it is recommended to include them when declaring callbacks.

```
class Foo
{
private:
    (uint32 x) -> uint2 callback;
}
```

Declaring callbacks as private members of a class reflects the fact that they
are initialized at object instantiation, are called from within the class
implementation, but it doesn't make sense to access a callback externally on an
object of the class.

A callback declaration can specify a default value. The default is used if the
callback is not initialized during object instantiation. The function specified
as the default value must be [compatible](#callback-function-compatibility) with
the declared callback type.

```
class Foo
{
private:
    uint2 default_callback(uint32 x)
    {
        return 0;
    }

    (uint32 x) -> uint2 callback = default_callback;
}
```

Declaration of a callback with a default value can use the `auto` type
placeholder. The callback type is inferred from the type of the default value.

```
class Foo
{
private:
    uint2 default_callback(uint32 x)
    {
        return 0;
    }

    auto callback = default_callback;
}
```

Callbacks can also be initialized [at object declaration](#object-initialization).

##### Callback function compatibility

Callbacks can be initialized only with functions that are compatible with the
callback type. A function is compatible with a callback if the function type is
a [subtype](#subtyping) of the callback type, and the function has the same set
of attributes affecting call site as the callback declaration. The following
function attributes affect the call site:
 - `[[async]]`
 - `[[pipelined]]`
 - `[[unordered]]`
 - `[[latency]]`
 - `[[no_backpressure]]`

as well as the following parameter attributes:
 - `[[last]]`

#### Exported classes

 Like other types, classes can be exported.

```
class Foo
{
    // ...
}

export Foo;
```

Exported classes are mapped to Verilog modules. The ports of the Verilog module
are defined by public methods of the class, callback and nested public objects.

For a class to be exportable it can only have non-inline public methods, private
callbacks and nested objects of other exportable classes as public members.
Classes with public data members or public inline methods are not exportable.

Instances of class templates can be exported:

```
template <typename T>
class Foo
{
    // ...
}

export Foo<uint32>;
```

A class template can also be exported within the class template definitions,
which is equivalent of exporting every instance of the template:

```
template <typename T>
class Foo
{
    // ...
    export Foo;
}

using Foo32 = Foo<uint32>;
```

Classes with public method templates can be exported, however it is important to
note that templates are not part of exported class interface, only concrete
instances of such method templates. Normally method templates are instantiated
when they are called. For exported classes, where methods might not be called
from Kanagawa code at all, it is also possible to explicitly instantiate method
templates using a declaration of local variable initialized to method template
instance:

```
class Foo
{
public:
    template <typename T>
    T Go(T x)
    {
        return x;
    }

private:
    void instances()
    {
        // not called
        assert(false);

        // Force an instance Go<uint32>;
        const auto go_uint32 = Go<uint32>;
    }
}

export Foo;
```

An exported class is implicitly declared as [external](#external-classes),
meaning that its implementation is provided by the generated Verilog module.
When an object of an exported class is instantiated, the compiler will generate
an instance of the Verilog module. Note that this may restrict cross-object
optimizations that could be performed on instances of a non-exported class.

Explicitly declaring an exported class as external is an error.

The name of the module and/or the name of the module instance can be controlled
with the `[[name()]]` attribute.

     [[name("ModuleName")]]
     export Foo;

#### External classes

A class can be declared as external in which case its implementation is
provided by an external Verilog module. Interface of an external class must
satisfy [exportable class](#exported-classes) restrictions.

```
module external.foo
    {Foo}

class Foo
{
public:
    uint2 go(uint32 x);

private:
    (uint32 x) -> uint2 callback;
}

extern Foo;
```

Classes declared as external can be instantiated just like regular classes
except their callbacks can't be initialized to `inline` functions and the type
of a function must be the same as the type of the callback it is assign to, not
a [subtype](#subtyping) of the callback type like for regular classes.

```
import external.foo

class Bar
{
    uint2 Do(uint32 x)
    {
        return 0;
    }

    Foo foo = {.callback = Do};
public:
    void Run()
    {
        foo.go(10);
    }
}
```

Mapping of an external class interface to Verilog module ports follows the same
rules as mapping of [exported classes](#exported-classes) public interface to
Verilog.

##### Method declaration templates

Methods of external classes may be declared as templates:

```
module external.foo_with_template
    {Foo}

class Foo
{
public:
    template <typename T>
    T go(T x);
}

extern Foo;
```

The interface of an external class will include instances of such method
declaration templates, if any. Template arguments can be specified explicitly
or deduced like for any other function template call.

```
import external.foo_with_template

inline void Run()
{
    static Foo foo;

    foo.go(10);
}
```

The method in the external class `Foo` corresponding to the method instance
used in the above example will be called `go___uint_4__`.

##### External class names

The `[[name]` attribute can be used to specify name of the Verilog module that
implements an external class:

    [[name("MyFoo")]]
    extern Foo;


### Type casting
`cast<T>(e)` can be used to convert an expression `e` to type `T`:

```
bool b = false;
uint1 u = cast<uint1>(b);
```

The input and output types must have the same width or the input type must be
[implicitly convertible](#implicit-integer-conversions) to the output type.
`cast` always reinterprets bits, it never performs a conversion like float to
integer.

`cast` supports:

* `int<N>`/`uint<N>` widening with sign/zero extension, e.g.:

    ```
    int32 foo;
    auto bar = cast<int64>(foo);
    ```

* `int<N>`/`uint<N>` narrowing with truncation, e.g.:

    ```
    uint32 foo;
    auto bar = cast<uint16>(foo);
    ```

  In both cases, truncation results in the most-significant bit(s)
  being dropped (including the sign bit for `int<N>`) and can result
  in a loss of information.

* In all other cases, provided that `bitsizeof Input == bitsizeof Output`,
  behave as `reinterpret_cast`, e.g.:

    ```
    bool[32] foo;
    auto bar = cast<uint1[8][4]>(foo);
    ```

`reinterpret_cast<T>(e)` can be used to explicitly cast between types of different widths.
If the source type is wider than the result type, then the least significant
bits of the source type are returned.  If the source type is narrower than the
result type, then the upper bits of the result are zero.

```
struct S
{
    uint32 x;
    int8 y;
}

S s = {43, -2};

uint4 z = reinterpret_cast<uint4>(s);
```

### Type inference

In Kanagawa `auto` is a type placeholder that tells the compiler to deduce a
type based on available context.

Type inference is useful for avoiding intermediate narrowing in computations
with integers:

```
int7 x;
int9 y;
auto z = x + y; // the type of z is inferred to be int10
auto w = z + 1; // the type of w is inferred to be int11

const auto N = 129; // The type of N is uint8
```

#### Expressions evaluated at compile-time

When an expression can be evaluated at compile-time, and the types of operands
are not explicitly specified, the type of the expression is inferred after
evaluation:

```
const auto x = 2 * 2; // the value of x is evaluated to 4 and the type inferred to uint3
uint3 y = x + 3; // no narrowing conversion warning since expression evaluates to 7
```

Expressions evaluated at compile-time but with at least one operand that has an
explicitly specified type follow the regular type inference rules based on the
operator and operand types:

```
const uint2 two = 2;
const auto z = two * two; // the value of x is evaluated to 4 and the type inferred to uint4
```

## Templates

Kanagawa supports parameterized types via templates.  Template parameters can
be types, values, or functions.  Type template parameters are declared with `typename`.
The type of a value template parameter can be explicitly specified or inferred
with `auto`.

A class template with a type template parameter:
```
template<typename T>
class MyClassType
{
public:
    T Add(T a, T b)
    {
        return a + b;
    }
}

inline void TemplateTestFunction()
{
    static MyClassType<uint32> _obj;

    print("{_obj.Add(0x20, 0x02)=}\n");
}
```

Type template parameters can specify enum types and be used as enum constant
qualifiers:

```
template <typename T>
class C
{
    T x = T::One;
}

enum E : uint32
{
    One = 1
}

class Foo
{
    C<E> c;
}
```

Type template parameters can specify a class and be used as qualifier of a type:

```
template <typename T>
class C
{
    T::X x;
}

class Foo
{
public:
    using X = uint32;
}

class Bar
{
    C<Foo> c;
}
```

A function template with an explicitly-specified type for a value parameter:
```
template<uint32 N>
inline uint32 AddN(uint32 a)
{
    return a + N;
}

inline void Test(uint32 x)
{
    print("{AddN<3>(x)=}\n");
}
```

A function template with an inferred type for a value parameter:
```
template<auto N>
inline uint32 AddN(uint32 a)
{
    return a + N;
}

inline void Test(uint32 x)
{
    print("{AddN<3>(x)=}\n");
}
```

Types contained within class templates can be referenced from the outside:
```
template <typename T>
class Area
{
public:
    using Width = T;
}

class Foo
{
    Area<uint32>::Width x = 10;
}
```

Templates can be nested:
```
template <typename T>
class MyClass
{
public:
    template <auto N>
    using MyType = T[N];
}

class Foo
{
    MyClass<bool>::MyType<2> launchGo = {false, true};
}
```

Structure templates are supported:

```
template<typename T>
struct MyStructType
{
    T aStructMember;
}
```

Union templates are supported:

```
template<typename T>
union U
{
    T t;
    uint16 x;
}
```

Function templates are supported:

```
template <typename T>
inline void MyFunction(T x)
{
    // Code for the function goes here.
}

inline void F()
{
    MyFunction<uint32>(10);
}
```

Note that function template parameters do not always need to be explicitly
specified.  In many cases function template parameters can be deduced:

```
template <typename T>
inline void MyFunction(optional<T> x)
{
    // Code for the function goes here.
}

inline void F()
{
    // Deduced make_optional<uint4> and MyFunction<uint4>
    MyFunction(make_optional(true, 10));
}
```

Template parameters are written in order.  This means that if a particular
template parameter cannot be deduced, all following parameters must be specified.
When writing a function template with multiple template parameters,
order the template parameters from least-likely to be deduced to most-likely.
In general, template parameters related to function parameter types can be deduced,
whereas template parameters related to function return types cannot be.

In the following example, `InputType` and `Count` can be deduced, whereas
`ResultType` cannot.

```
template<typename ResultType, typename InputType, auto Count>
inline ResultType[Count] ConvertArray(InputType[Count] inputData)
{
    ResultType[Count] result;

    static for (const auto i : Count)
    {
        result[i] = cast<ResultType>(inputData[i]);
    }

    return result;
}

inline void F()
{
    uint32[4] x = {};

    uint64[4] y = ConvertArray<uint64>(x);
}
```

Alias templates are supported:

```
template<uint32 Foo>
using u = uint<Foo>;
```

Template parameters can be functions:
```
template <(uint32, uint32) -> uint32 Callback>
class AClass
{
public:
    uint32 DoProcessing(uint32 a, uint32 b)
    {
        // Code to do some processing goes here.
        // Now finish up by invoking the callback function.
        return Callback(a,b) + 1;
    }
}

inline uint32 CallbackFunction(uint32 a, uint32 b)
{
    return a + b;
}

inline void TemplateSample()
{
    static AClass<CallbackFunction> _object;

    uint32 result = _object.DoProcessing(123,456);
}
```

### Dependent names

The meaning of some constructs may be ambiguous when they refer to names that
can't be resolved during parsing. For example, names that depend on template
parameters can't be resolved until template is instantiated. Names that are
conditionally declared within `static if` are undefined until the condition is
evaluated. Such names are called dependent names. When using a dependent name in
a construct that may be ambiguous, meaning of the name must be disambiguated.

##### The `typename` disambiguator

Constructs such as `bitsizeof` and `bytesizeof` operators and template instance
declarations may have ambiguous meaning when used with dependent names. For
example the expression:

    bitsizeof Foo[3]

may mean either "size of array with 3 elements of type Foo", when `Foo` is
a type, or "size of an element of an array", when `Foo` is a variable.

A dependent type name can be disambiguated using the `typename` disambiguator.

```
template <typename T>
class Foo
{
    uint<bitsizeof typename T::Nested[3]> x;
}

class Bar
{
public:
    using Nested = uint32;
}

class Bux
{
    Foo<Bar> foo;
}
```

Another common example when dependent type names must be disambiguated is
template instance arguments:

```
template <typename T>
class Foo
{
    optional<typename T::Nested> x;
}

class Bar
{
public:
    using Nested = uint32;
}

class Bux
{
    Foo<Bar> foo;
}
```

##### The `template` disambiguator

A dependent template is a template whose definition depends on a value or
parameter. To instantiate a dependent template prefix an identifier with the
`template` disambiguator to indicate that it refers to a template.

For example, a template that is a member of a type specified by a template
parameter is a dependent template. In the code below, the member function
template `Do` of the object `obj` is a dependent template because it depends on
the type T, which is a template parameter.

```
template <typename T>
class Foo
{
public:
    void Go()
    {
        obj.template Do<uint32>();
    }

    T obj;
}
```

Another common example of dependent templates are those defined within
a `static if` statement. Their definition is dependent on the condition
specified in the `static if`.

```
uint32 x;

static if(bitsizeof(x) > 10)
{
    template <typename T>
    inline void Go()
    {
    }
}

inline void Run()
{
    template Go<bool>();
}
```

Similarly, a member template of an object defined conditionally using `static
if` is a dependent template:

```
class Bar
{
public:
    template <typename T>
    void Go()
    {
    }
}

class Foo
{
    void Run()
    {
        bar.template Go<uint32>();
    }

    static if (true)
    {
        Bar bar;
    }
}
```


### Template parameter defaults

Template parameters can have default values that are used when a given
parameter is not specified during template instantiation. Both type and
non-type template parameters can have default values.

```
    template <typename T = uint32, auto N = 10>
    using MyArray = T[N];
```

The default value of a parameter can depend on value(s) of preceding
parameter(s):

```
    template <auto InstrMemSize, auto InstrTCMSize = InstrMemSize>
    class core
    {
    }
```

Template parameter default values have to follow the same restrictions as
template instance arguments: non-type parameter defaults must be known
at compile time, type parameter defaults must be built-in or user defined
types. Note that the default value of a parameter must be resolvable in the
scope of the template definition, not in the scope where the template will be
instantiated. In particular, a template defined in a module can declare
parameters with defaults referring to symbols visible within that module, even
if they are not visible outside, when the template is instantiated:

```
    module test {Foo}

    inline void Bar()
    {}

    template <() -> void Fn = Bar>
    inline void Foo()
    {
    }
```

For function templates, when a parameter can be deduced from function call
arguments, deduction takes precedence over using the default parameter
value(s):

```
    template <typename T = uint32>
    inline void Foo(T x)
    {
    }

    inline void Bar((uint32) -> void fn)
    {
    }

    inline void Run()
    {
        Foo(false); // Foo<bool>

        Bar(Foo);   // Foo<uint32>
    }
```

A function template for which all parameters can be deduced and/or have
a default value, can be instantiated without angle brackets. Other kinds of
template must be instantiated with a list of arguments enclosed in angle
brackets, even when the list is empty:

```
    template <auto N = 10>
    struct Foo
    {
        uint<N>[N] x;
    }

    Foo<> foo;
```

### Template template parameters

Templates can have parameters specifying type templates. The syntax for template
template parameter declaration is:

`template <`_parameters_`> typename`

where _parameters_ is a comma separated list specifying parameter kinds: `typename`,
`auto` or _type specifier_.

In the body of the template declaration, the name of a template template parameter is
a template-name and needs arguments to be instantiated.

```
    template <template <typename, auto, bool> typename TT>
    class Foo
    {
        TT<uint32, 10, false> x;
    }
```

The argument for a template template parameter must be a type template, such as
a struct template, union template, class template or an alias template, with a
parameter list that matches the parameters in the template template parameter
declaration.

```
    template <template <typename> typename TT>
    class Foo
    {
        TT<uint32> x;
    }

    template <typename T>
    class Bar
    {
    }

    class Bux
    {
        Foo<Bar> foo;
    }
```

A template passed as argument for a template template parameter may have more
parameters than the template template parameter signature specifies, as long as
the extra parameters have default values.

```
    template <template <typename> typename TT>
    class Foo
    {
        TT<uint32> x;
    }

    template <typename T, auto N = 0>
    class Bar
    {
    }

    class Bux
    {
        Foo<Bar> foo;
    }
```

Declaration of a template template parameter can refer to preceding parameters:

```
    template <typename T>
    inline T Fn(T x)
    {
        return x;
    }

    template <typename T, template <(T) -> T> typename TT>
    class Foo
    {
        TT<Fn<T>> x;
    }

    template <(uint32) -> uint32 Fn>
    class Bar
    {
    }

    class Bux
    {
        Foo<uint32, Bar> foo;
    }
```

Like any template parameters, template template parameters can have default
values:

```
    template <typename T>
    struct S
    {
    }

    template <template <typename> typename TT = S>
    class Foo
    {
        TT<uint32> x;
    }

    class Bar
    {
        Foo<> foo;
    }    
```

### External class template

An external class template represents a parameterizable Verilog module. Template
parameters map to Verilog module parameters. Only templates with non-type,
scalar template parameters can be declared as external.
Parameters can be used in the implementation of the Verilog module as well as in
the declaration of module ports, within limitations of Verilog. For example
a scalar parameter may be used to define ports width:

    template <auto WIDTH>
    class Foo
    {
    public:
        uint<WIDTH> go(uint<WIDTH> x);

    private:
        (uint<WIDTH> x) -> uint<WIDTH> callback;
    }

    extern Foo;

The above code would be implemented as a Verilog module:

    module Foo
    #(
        parameter WIDTH
    )
    (
        // ...
        input wire go_valid_in,
        input wire [WIDTH-1:0] go_x_in,
        output logic go_rdy_out,
        input wire go_rden_in,
        output logic go_empty_out,
        output logic [WIDTH-1:0] go_result_out,

        input wire callback_rden_in,
        output logic [WIDTH-1:0] callback_x_out,
        output logic callback_empty_out,
        input wire callback_valid_in,
        input wire [WIDTH-1:0] callback_result_in,
        output logic callback_rdy_out
    );

Note that Verilog module parameters may or may not have default values, but
Kanagawa generated instantiation of the module will always specify values for
all Verilog module parameters corresponding to the template parameters.

It is important to distinguish between external class template declaration and
external class template instance declaration, as shown in the following
example:

    template <auto WIDTH = 32>
    class Foo
    {
    public:
        uint<WIDTH> go(uint<WIDTH> x);

    private:
        (uint<WIDTH> x) -> uint<WIDTH> callback;
    }

    extern Foo<32>;

The above declaration only declares an external implementation of a specific
template instance, `Foo<32>`. In this case, the Kanagawa compiler would not
pass any parameters when instantiating the Verilog module, which could be
defined as follows:

    module Foo_32
    (
        // ...
        input wire go_valid_in,
        input wire [31:0] go_x_in,
        output logic go_rdy_out,
        input wire go_rden_in,
        output logic go_empty_out,
        output logic [31:0] go_result_out,

        input wire callback_rden_in,
        output logic [31:0] callback_x_out,
        output logic callback_empty_out,
        input wire callback_valid_in,
        input wire [31:0] callback_result_in,
        output logic callback_rdy_out
    );

## State

Variables represent state and can be declared within a class, or locally within a
function.

```
class C
{
private:
    // Member variable
    uint32 x;
}

inline void F()
{
    // Local variable
    uint32 x;

    // Static local variable
    static uint32 y;
}
```

Variables are categorized by their interaction with threads.  Some variables are
local to each thread, while others are shared among threads.

Declaration     | Sharing
:----------     | :------
Class Member    | Shared
Static Local    | Shared
Local           | Thread local

A declaration can also initialize a variable to a given value:
```
uint32 a = 3;
```

Shared variables are initialized when the hardware is reset.  If no initial
value is provided for a shared variable, then the initial value of that variable
is undefined.  For FPGA targets, memories are initialized only when the FPGA is programmed.

Local variables are initialized before the first use of the
variable by the corresponding thread.  If no initial value is provided for a
local variable, all bits are initialized to zero.

### Scope

#### Shadowing
Variables in an inner scope can have the same name as variables in an outer
scope.  References always refer to the innermost variable.

```
inline void F()
{
    uint32 x = 4;

    {
        uint32 x = 6;

        println(x); // prints 6
    }
}
```

### Initializer lists
Variables can be initialized with initializer lists:

```
uint32 a = {}; // initialized to 0

struct Flag
{
    bool b;
}

struct S
{
    uint8 x;
    int16 y;
    Flag f;
}

S s1 = {3, 4, {true}}; // x = 3, y = 4, t.f.b = true
S s2 = {3};            // x = 3, y = 0, t.f.b = false

inline S Foo()
{
    // returns an S with x = 6, y = 7, t.f.b = true
    return { 6, 7, {true} };
}
```

When an array or memory is initialized with an initializer list, the number of elements
in the initializer list must be less than or equal to the number of elements in the the
array or memory.  If the initializer list contains fewer elements than the array or memory,
then the remaining elements are initialized to 0.

```
// This is the same as:
// static memory<uint32, 8> mem = { 3, 2, 5, 0, 0, 0, 0, 0 };
static memory<uint32, 8> mem = { 3, 2, 5 };
```

Unions can be initialized with an empty initializer list, or an initializer list
with one element.  If the initializer list contains one element, then the first
field of the union is initialized to the specified value.  [Designated
initializers] (#designated-initializers) are recommended when initializing a
union, because the name of the initialized field is explicitly specified in the
source.

```
union U
{
    uint32 a;
    uint16 b;
}

U u = { 0x12345678 }; // sets u.a = 0x12345678
```

### Designated initializers

Designated initializers allow initializing instances of classes, structs, and unions.
Each designator must name a direct data member of the type to be initialized.
Designators do not have to appear in the same order as the data members in the
type declaration.

```
struct Foo
{
    uint4 size;
    bool valid;
}

Foo foo = {.valid = false, .size = 10};
```

Data members that are not named by any of the designators are implicitly
initialized to {}, e.g.:

```
struct Foo
{
    uint4 size;
    bool valid;
}

Foo foo = {.size = 10};
```

is equivalent to:

```
struct Foo
{
    uint4 size;
    bool valid;
}

Foo foo = {.size = 10, .valid = {}};
```

The implicit initialization doesn't apply to members of a function type.

Implicit narrowing conversions of initialization values in a designated
initializer list are prohibited:

<!-- invalid -->
```
struct Foo
{
    uint4 size;
    bool valid;
}

Foo foo = {.size = 100}; // error: Invalid conversion
```

Designated initializer used to initialize a union must specify at most
one designator:

```
union Foo
{
    uint4 integer;
    bool boolean;
}

Foo foo1 = {.integer = 10};
Foo foo2 = {.boolean = true};
```

Specifying more than one designator in union initializer is an error:

<!-- invalid -->
```
union Foo
{
    uint4 integer;
    bool boolean;
}

Foo foo3 = {.integer = 10, .boolean = true}; // error: Excess elements in union initializer
```

Designator can only name a direct data member of a type. Initialization
of subaggregates is achieved using nested initialization lists, e.g.:

```
struct Foo
{
    bool valid;
    uint4 index;
}

struct Bar
{
    Foo[4] foos;
}

Bar bar = { .foos =
    { { .valid = false
      , .index = 0
      }
    , { .valid = true
      , .index = 1
      }
    }
};
```

#### Object initialization

Objects may be initialized at instantiation point using the designated initializers:

    Type name = {.member1 = value,
                 .member2 = value, ...};

where the _members_ are [callbacks](#callbacks) or nested objects. A callback can
be initialized with a free function:

```
inline uint2 go(uint32 x)
{
    return x;
}

class Foo
{
private:
    (uint32 x) -> uint2 callback;
}

inline void Run()
{
    // Initialize foo's callback to free function `go`
    static Foo foo = {
        .callback = go
    };
}
```

or with a method of an object:

```
class Bux
{
public:
    uint2 go(uint32 x)
    {
        return x;
    }
}

class Foo
{
private:
    (uint32)->uint2 callback;
}

class Bar
{
private:
    uint2 go(uint32 x)
    {
        return x;
    }

    // Initialize Foo's callback to a method of the
    // current object
    Foo foo1 = {
        .callback = go
    };

    Bux bux;

    // Initialize Foo's callback to a method of a
    // nested object bux
    Foo foo2 = {
        .callback = bux.go
    };
}
```

The function used to initialize a callback must be
[compatible](#callback-function-compatibility) with the declared callback type.

Object arrays are initialized using initialization list syntax:

```
class Foo
{
private:
    (uint32)->uint32 callback;
}

inline uint32 go1(uint32 x)
{
    return x + 1;
}

inline uint32 go2(uint32 x)
{
    return x + 2;
}

inline void Run()
{
    static Foo[2] foo = {
        { .callback = go1 },
        { .callback = go2 }
    };
}
```

Callbacks in nested objects can be initialized with nested initializers:

```
inline uint32 AddOne(uint32 x)
{
    return x + 1;
}

class Inner
{
private:
    (uint32)->uint32 cb;
}

class Outer
{
private:
    Inner inner;
}

inline void Foo()
{
    static Outer _outer = { .inner = {.cb = AddOne} };
}
```

Callbacks of nested arrays of objects are initialized with an initializer list
containing designated initializers:

```
inline uint32 AddOne(uint32 x)
{
    return x + 1;
}

inline uint32 AddTwo(uint32 x)
{
    return x + 2;
}

class Inner
{
private:
    (uint32)->uint32 cb;
}

class Outer
{
private:
    Inner[2] inners;
}

inline void Foo()
{
    static Outer _outer = { .inners = { {.cb = AddOne}, {.cb = AddTwo} } };
}
```

##### Default initialization declaration

A class can include a default initialization declaration. The declaration uses
the same designated initializer syntax as object initialization, but instead of
applying to a specific object instance it specifies a default initialization
value(s) applicable to all objects within a hierarchy. For example given:

    uint2 go(uint32 x)
    {
        return x;
    }

    class Foo
    {
    private:
        (uint32 x) -> uint2 callback;
    }

    class Bar
    {
    private:
        (uint32 x) -> uint2 callback;

        Foo foo1;
        Foo foo2;
    }

the following:

    class Bux
    {
    private:
        default = {.callback = go};
        Bar bar;
    }

is a shorthand for:

    class Bux
    {
    private:
        Bar bar = { .callback = go
                  , .foo1 = {.callback = go}
                  , .foo2 = {.callback = go}
                  };
    }

The values specified in the default initialization declaration are used only
for callbacks that are not explicitly initialized, e.g.:

    class Bux
    {
    private:
        default = {.callback = go};

        Bar bar = {.foo1 = {.callback = something}};
    }

is equivalent to:

    class Bux
    {
    private:
        Bar bar = { .callback = go
                  , .foo1 = {.callback = something}
                  , .foo2 = {.callback = go}
                  };
    }

Default initialization declarations may be specified at multiple levels of
object hierarchy. Inner declarations override outer declarations on
a initializer-by-initializer basis, e.g.:

    class Foo
    {
    private:
        (uint32 x) -> uint2 callback1;
        (bool x) -> bool callback2;
    }

    class Bar
    {
    private:
        // overrides default initialization for `callback1` for children (foo1, foo2)
        default = { .callback1 = something };

        Foo foo1;
        Foo foo2;

        // initialized using default initialization for `callback1` from parent
        (uint32 x) -> uint2 callback1;
    }

    class Bux
    {
        default = { .callback1 = go1
                  , .callback2 = go2
                  };

        Bar bar;
    }

It is important to note that default initialization declarations, just like
object initializations apply to the object hierarchy, not the class hierarchy.

    class Foo
    {
    private:
        (uint32 x) -> uint2 callback;
    }

    class Bar
    {
    private:
        Foo foo1;
        Foo foo2;
    }

    class Bux
    {
    private:
        default = { .callback = go };
        Bar bar;
    }

    class Buz
    {
    private:
        default = { .callback = something };
        Bar bar;
    }

    Run()
    {
        // instance of `Foo` within `Bar` instantiated by `Bux` will use
        // `go` as the value of `callback`
        static Bux bux;

        // instance of `Foo` within `Bar` instantiated by `Buz` will use
        // `something` as the value of `callback`
        static Buz buz;
    }

### Constants
Constants are defined using the `const` keyword. Constants can be defined in the
global scope, as class members and locally within functions.

```
const auto x = 4;
const uint32 y = 5;
```

A compile time error occurs if a statement attempts to modify a constant.

Objects cannot be declared `const`.

### Static local variable instances

A given static local variable may have many instances in the generated hardware.
The following table describes how many instances are created for a static
local variable.

Containing Function        | Number of Instances
:----------                | :------
inline                     | 1 per call site
non-inline method          | 1 per object

## Control flow

### Conditional
```
uint32 c;

if (c > 75)
{
    c += 75;
}
else
{
    c = 0;
}
```

if/else ladders are supported:

```
uint32 c;

if (c > 75)
{
    c += 75;
}
else if (c < 32)
{
    c = 32;
}
else
{
    c--;
}
```

### Switch statement

```
uint8 x;
uint64 result;

switch (x)
{
case 0:
    result = 3;
    break;

case 1:
    result = 5;
    break;

default:
    result = 44;
    break;
}
```

All case statements must have a corresponding break statement, fall-through is
not supported.

### Loops

#### do/while
do/while loops are similar to other programming languages:

```
uint32 i = 6;
do
{
    i--;
} while (i != 3)
```

#### Range for
Ranged based for loops iterate from 0 to N-1.  Note that the type of the
induction variable can be inferred.  It is the narrowest unsigned integer type
that can represent N-1.

```
for (const auto i : 8)
{
    // 8 loop iterations
    // i is: 0, 1, 2, 3, 4, 5, 6, 7
}
```

#### Static for

Static for loops iterate from 0 to N-1.  The body of the loop is replicated N
times, which can affect the performance and correctness of Kanagawa code.  See
[Spatial Computing](#spatial-computing) for more details.

This snippet:
```
uint8[4] a;

static for (const auto i : 4)
{
    println(a[i]);
}
```

is equivalent to:
```
uint8[4] a;
{
    {
        const uint2 i = 0;
        {
            println(a[i]);
        }
    }

    {
        const uint2 i = 1;
        {
            println(a[i]);
        }
    }

    {
        const uint2 i = 2;
        {
            println(a[i]);
        }
    }

    {
        const uint2 i = 3;
        {
            println(a[i]);
        }
    }
}
```

The induction variable of `static for` is known at compile time for each
iteration of the loop. As such it can be used as an argument to templates,
in conditions of `static if`, or in type declarations, e.g.:

```
static for (const auto i : 2)
{
    index_t<i> x;
}
```

When a [lambda](#lambdas) is declared within `static for` body then each
iteration of the loop instantiates its own lambda, allowing different iterations
to operate on different types, e.g.:

```
import data.array

inline void Run()
{
    static for(const auto i : 2)
    {
        index_t<i>[5+i] a;

        sum<uint<5+i>>(a);
    }
}
```

#### Looping functions
There are a number of commonly used functions in the standard library that
implement looping behavior (see [concurrency library functions](#concurrency-library-functions)).

## Functions

Logic of a design is expressed with functions. This section explains how to
declare and call functions.

### Function declaration

A function declaration consists of its name, parameters, return type, and the
function body, which contains zero or more statements. Here's the basic syntax
for declaring a function:

```
inline uint32 add(uint32 x, uint32 y)
{
    return x + y;
}
```

Parameters in a function declaration must have value types, and the return type
can be a value type or `void`, indicating that the function does not return any
result. If the return type is anything other than `void`, the function must
have exactly one `return` statement. If the return type is specified as `auto`,
the compiler will attempt to infer the type from the value the function
returns:

```
// Return type inferred by the compiler as `uint33`
inline auto add(uint32 x, uint32 y)
{
    return x + y;
}
```

#### Global functions

Functions declared in global scope of a module are visible to other code in that
module. Global functions can also be exposed by a module and then they are
visible to code in other modules that import it. Global functions must be declared
inline.

```
module foo.bar
    { DoFoo
    }

inline void DoFoo()
{
}
```

#### Local functions

A function can be declared locally within the body of another function. These
local functions have access to any local types defined in their parent scope,
but they do not have access to local variables or parameters of the enclosing
function. Here's an example of a local function:

```
inline auto add(uint32 x, uint32 y)
{
    struct Record
    {
        uint32 x;
        uint32 y;
    };

    inline auto helper(Record record)
    {
        // ...
    }

    return helper({x, y});
}
```

Local functions defined within global functions must be declared inline. Local
functions defined within methods can be either inline or non-inline.

#### Methods

Functions declared within a class declaration are called methods. Methods are
visible to code in other methods of the class, and when they are declared in the
`public` section of the class, they are also accessible from outside the class
using an object qualifier syntax.

```
class Foo
{
public:
    void Run()
    {
    }
}

inline void Run()
{
    static Foo foo;

    foo.Run();
}
```

### Function call

When a function is called, its body defines a piece of hardware that is used to
execute the function's logic. Multiple calls to a function may either share the
same hardware or be inlined as completely independent pieces of hardware. The
choice between sharing hardware and inlining has implications for performance
and correctness.

#### Function inlining

By default, functions with multiple call sites are not inlined, meaning that
callers share the same piece of hardware for the function implementation,
potentially leading to queueing and contention. On the other hand, functions
with only one call site are by default inlined. This means that the hardware is
instantiated at the call site, and there is no queue to access it. The default
behavior can be modified using function modifiers `inline` and `noninline`.

### Function attributes
The following attributes are supported in function declarations and function
types:

* `[[async]]` functions execute asynchronous to the calling thread (which
  continues executing statements after the function call without waiting for the
  function to complete)
* `[[latency(N)]]` specifies that the function completes in exactly `N` clock
  cycles
* `[[max_threads(N)]]` specifies that no more than `N` threads may be executing
  in a function concurrently
* `[[no_backpressure]]` specifies that there are no backpressure control signals
  from the function
* `[[pure]]` functions do not have side-effects.  The compiler may choose to
  call `[[pure]]` functions more often than is required by control flow.
  `[[pure]]` cannot be specified on functions implemented in Kanagawa.
* `[[unordered]]` specifies that threads may return from the function in a
  different order than the order they entered the function
* `[[reset]]` functions are automatically called after reset (see [reset
  functions](#reset-functions))

### Generic functions

In Kanagawa, functions can be defined in a generic way, without assuming
specific parameter types. This allows writing more versatile functions that
can operate on different data types as long as the required operations are
defined for those types. Here's an example of a generic function:

```
inline auto add(auto x, auto y)
{
    return x + y;
}
```

The function `add` can be used for any types of arguments, as long as the `+`
operator is defined for those types. When a generic function is called, the
compiler creates an instance of the function for each set of parameter types
used. For example, calling `add(10, 17)` would create the following instance:


    inline uint6 ?add$uint@4$uint@5(uint4 x, uint5 y)
    {
        return x + y;
    }

#### Function templates

Function templates provide a more general way to declare generic functions.
Templates define a list of parameters, which can be both types and values. When
a template function is called, template arguments can be specified explicitly,
or they may be deduced from the arguments passed to the function:

```
template <typename T, auto N>
inline void Foo(T[N] a)
{
}

inline void Run()
{
    int6[8] a;

    // Deduced Foo<int6, 8>
    Foo(a);
}
```

#### Abbreviated function templates

Generic functions can be declared using abbreviated syntax that doesn't
explicitly specify template parameters:

```
inline auto Do(auto x, auto y)
{
    return x + y;
}

inline void Run()
{
    Do(10, -10);
}
```

#### Return type polymorphism

Templates can be used to define functions that are polymorphic on the return
type:

```
template <typename T>
inline T Foo(auto x)
{
    return reinterpret_cast<T>(x);
}
```

The template parameter(s) specifying the function return type can be deduced by
the compiler from the context in which the function is called:

```
template <typename T>
inline T Foo(auto x)
{
    return reinterpret_cast<T>(x);
}

inline void Bar(uint23 x)
{
}

inline void Run()
{
    Bar(Foo(true));
}
```

The compiler can deduce expected return type of a function template when the
result of the function call is used in one of the following contexts:

- When result is passed as an argument to a function, the expected return type
is deduced from the type of corresponding function parameter:

    ```
    inline void Foo(uint8[4] x)
    {
    }

    template <typename T, auto N>
    inline T[N] Bar()
    {
        return {};
    }

    inline void Run()
    {
        Foo(Bar());
    }
    ```

- When result is assigned to a variable the return type is deduced from the
type of the variable:

    ```
    template <typename T>
    inline T Foo()
    {
        static if (T == bool)
            return true;
        else
            return 42;
    }

    inline void Run()
    {
        uint32[10] x;

        x[2] = Foo();
    }
    ```

- When result is used as an initialization value in variable declaration, the
return type is deduced from the declared type of the variable:

    ```
    inline void Run()
    {
        uint32[16] x = pipelined_map(10, [](index_t<10> x)
            {
                return x;
            });
    }
    ```

- When result is used as return value of a function

    ```
    template <typename T, auto N>
    inline T[N] Fn_T_N()
    {
        return {};
    }

    inline uint8[4] Run()
    {
        return Fn_T_N();
    }
    ```

### Lambdas

Lambdas are unnamed, inline, local functions that allow defining pieces of
logic in-line. They are particularly useful for one-off functions that don't
need to be named or reused. Here's the basic syntax for defining a lambda:

```
const auto fn = [](uint32 x, uint32 y) -> uint32
{
    return x + y;
};
```

The return type of a lambda can be explicitly specified or omitted. If omitted,
it is inferred by the compiler based on the return statement in the lambda's
body.

```
const auto fn = [](uint32 x, uint32 y)
{
    return x + y;
};
```

In a definition of a lambda without parameters the empty parameter list may be
omitted:

```
const auto fn = []{};
```

is the same as:

```
const auto fn = [](){};
```

Lambdas can also capture values from their surrounding scope. Captures
are copies of captured values.

#### Closures

Closures are special values that encapsulate a function with a set of captured
values from their defining scope. They can be called like functions, but they
can also be stored and passed around like regular values. Closures are
particularly useful in situations where a context needs to be carried with
a function. Here's an example of a closure:

```
inline void Run(uint32 a)
{
    auto fn = [a](uint32 x, uint32 y) -> uint32
    {
        return a - (x + y);
    };

    auto fn2 = [fn]()
    {
        fn(10, 20);
    };

    fn2();
}
```

In the above example, the closure `fn` captures the value of `a`, allowing it
to use that value in its implementation. The closure `fn2` in turn captures the
closure `fn` and calls it within its implementation.

Closures can be very useful when additional data or context needs to be passed to
a function without modifying its parameters.

#### Class member access within lambda

Lambdas declared within class methods can access members of the class object in the
context in which the closure was created. This makes it convenient to use
closures within class methods while still having access to the class's internal
data and functions.

```
class Foo
{
    void Run()
    {
        auto fn = []()
        {
            // call method `Do` of the same object for which `Run` was called.
            Do();

            // modify field `x` of the same object
            x += 1;
        };
    }

    void Do()
    {
        // ...
    }

    uint32 x;
}
```

Lambdas may also capture member variables by value (copy a value):

```
class Foo
{
    void Run()
    {
        auto fn = [x]()
        {
            // call method `Do` of the same object for which `Run` was called.
            Do();

            // modify captured copy of field `x`, the field `x` is not changed
            x += 1;
        };
    }

    void Do()
    {
        // ...
    }

    uint32 x;
}
```

#### Generic lambdas

Just like regular functions, lambdas can be defined in a generic way, without
assuming particular parameter types. This allows writing more versatile closures
that can work with different data types, as long as the necessary operations are
defined for those types. Here's an example of a generic lambda:

```
[](auto x, auto y)
{
    return x + y;
}
```

#### Closures in the global scope

Closures are usually defined in function scope however it is also possible
define closures in the global scope. This is particularly useful for defining
global closure constants.

```
import data.function
import numeric.int.operator

const auto xor_and_increment = compose2(increment, xor);

inline void Run()
{
    auto x = xor_and_increment(10, 13);
}
```

Closures defined in the global scope can be thought of as being "created" at
compile time, and as such they can't capture data values. For example the
following is illegal because the lambda creating the closure attempts to capture
value of `x` which is a meaningless thing to do at compile-time:

<!-- invalid -->
```
uint32 x;

const auto illegal = [x](uint32 y)
{
    return x + y;
};
```

A little bit more subtly, the following is also illegal:

<!-- invalid -->
```
import data.function
import numeric.int.operator

const auto cipher = bind2nd(xor, 0xC0DE);
```

The function `bind2nd` creates a closure that captures the value of its second
argument, which is not valid when declaring a closure in the global scope.

Note that closures declared in the global scope can capture other closures, as
long as captured closures don't themselves capture data values.

### Higher order functions

Higher-order functions are functions that take one or more functions as
arguments and/or return functions as their result. This powerful concept enables
building more flexible and composable code by treating functions as values.

#### Functions as function parameters

One common use of higher-order functions is to implement some general-purpose
structure where specifics are filled in by functions passed as arguments. For
example, consider the `async_then` function from the `control.async` module in
the standard library:

```
template <typename T>
inline void async_then(() -> T task, (T) -> void then)
{
    // ... implementation omitted for clarity
}
```

The `async_then` function allows to execute a task asynchronously and pass
the result to another function upon completion. The function is completely
generic with regard to the logic of task and completion functions, which are
both passed as arguments.

#### Calling higher order functions

When calling higher-order functions, closures are passed as arguments for the
function type parameters. Lambdas are commonly used for this purpose because
they are convenient and can capture local values, making them flexible. Here's
an example:

```
import control.async

inline void Run(uint32 x)
{
    async_then(
        [x]() -> uint32
        {
            uint32 result;
            // some lengthy calculation
            return result;
        },
        [](uint32 result)
        {
            // use the result
        }
    );
}
```

Named functions as well as object methods and callbacks can also be passed as
arguments to a higher order function:

```
import control.async

class Foo
{
public:
    void Run()
    {
        async_then(compute, complete);
    }

private:
    uint32 compute()
    {
        return 10;
    }

    (uint32) -> void complete;
}
```

The ability to treat functions (closures) as values enables creating very
composable and flexible code.

#### Higher order function templates

Templates allow parametrizing higher order function on types of parameter(s)
the function(s) passed as argument take(s). The compiler can deduce the template
arguments from the type of function (or closure) passed when higher order
function templates is called:

```
template <typename T>
inline void Go((T) -> uint32 Do)
{
    Do(10);
}

inline void Run()
{
    // Deduce to be Go<uint32>
    Go([](uint32 x)
    {
        return x;
    });
}
```

Template argument deduction can deduce _number_ of parameters a function passed
as an argument to a higher order function accepts. This allows writing higher
order functions which can take as arguments, and call, functions with different
number of parameters:

```
template <typename T1, typename T2>
inline void Go((T1, T2) -> uint32 Do)
{
    static if (T1 == void)
    {
        Do();
    }
    else static if (T2 == void)
    {
        Do(10);
    }
    else
    {
        Do(10, 11);
    }
}

inline void Run()
{
    Go([](uint32 x)
    {
        return x;
    });

    Go([]
    {
        return 0;
    });
}
```

#### Functions as function results

Higher-order functions can also return functions. This allows defining
functions that generate new functions. Here's a simple example:

```
inline auto constant(auto x)
{
    return [x](auto y)
    {
        return x;
    };
}
```

The `constant` function returns a one parameter, generic closure that always
returns the value `x`, regardless of the argument passed to it. While it may
seem simple, such higher-order functions are powerful building blocks for
creating more complex functions and structures.

For example, consider how it can be used with the `map` function from the
`data.array` module:

```
import data.array
import data.function

inline void Run(uint32[10] a)
{
    map(constant(false), a);
}
```

The `map` function creates a new array by applying a given function to every
element of the argument array. Using `constant(false)` as the function results
in an array with every element set to `false`.

Functions creating functions often also take functions as arguments. A common
use case is transforming a function into a slightly different one. Let's say we
want to add elements of two lists. The standard library defines `zip_with`
function that applies a given function to elements of two arrays, and returns
array of results. It could be used like this:

```
import data.array

inline auto add(auto x, auto y)
{
    return x + y;
}

inline void Run(int32[10] x, int32[10] y)
{
    auto result = zip_with(add, x, y);
}
```

This returns an array `int33[10]`, which is sufficient to store the result of
the addition. However, what if the result array needs to be of the same type as
the input arrays, `int32[10]`? That requires a different `add` function, one
that casts the result of addition to `int32`. It is easy to define such
a function but the problem can be solved in a more general way by using
a higher order function to transform our existing `add` into a function that
returns a result of desired type:

```
import data.array
import type.coerce

inline auto add(auto x, auto y)
{
    return x + y;
}

inline auto compose2((auto) -> auto f, (auto, auto) -> auto g)
{
    return [f, g](auto x, auto y)
        {
            return f(g(x, y));
        };
}

inline void Run(int32[10] x, int32[10] y)
{
    auto result = zip_with(compose2(static_cast<int32>, add), x, y);
}
```

The function `compose2` defined above is one of the functions available in the
`data.function` module of the standard library. It takes as an argument two
functions, `f` and `g`, and returns a function which composes them: first the
function `g` is applied to the arguments and then the function `f` is applied
to the result of `g`. In our specific example the `static_cast<int32>` is
applied to result of addition.

Higher-order functions can greatly enhance the expressiveness and composability
of the code, allowing creation of reusable, generic, and powerful algorithms
and structures.

#### Function currying

Function currying is a technique for converting a function that takes multiple
arguments into a function taking one argument and returning a function. Normally
multi-parameter functions in Kanagawa have an uncurried form, however it is
sometimes convenient to work with curried functions so the standard library
module `data.function` provides utilities for currying and uncurrying functions.
Curried form is especially useful for applying functions one argument at a time,
or for constructing functions one parameter at at time.

##### Partial application

Since [curried functions](#function-currying) take one argument and return
another function, they can naturally be partially applied. Here's a simple
example that illustrates the concept:

```
import data.function
import numeric.int.operator

inline void Foo()
{
    auto fn1 = curry(add<uint8, uint8>);
    auto fn2 = fn1(20);
    auto x = fn2(56);

    assert(x == add<uint8>(20, 56));
}
```

The above example is contrived, there is no reason to apply `add` one argument
at a time in this case. However the technique is useful in the real world,
allowing composition of existing, general functions instead of writing
special-case, one-off code. To illustrate let's consider adding a value to every
element of an input array. It would be trivial to write a loop to do it:

```
inline void Foo()
{
    uint32[10] a;
    uint32[10] b;

    static for (const auto i : 10)
    {
        b[i] = a[i] + 7;
    }
}
```

Applying a function to every element of an array is such
a common thing to do that the standard library provides the `map` function specially
for this purpose:

```
import data.array

inline void Foo()
{
    uint32[10] a;

    auto b = map([](uint32 x)
            {
                return x + 7;
            },
            a);
}
```

In terms of code reuse this is a clear improvement. The special purpose, one-off
loop has been eliminated and now there is just a lambda implementing the special
purpose "increment by 7" operation. However, this still is not fully satisfying.
The only thing that is special about the lambda is the constant `7`, other than
that it is just a regular addition. Using utilities from `data.function` the
one-off code can be further reduced to that essence:

```
import data.array
import data.function
import numeric.int.operator

inline void Foo()
{
    uint32[10] a;

    auto b = map(apply(curry(add<uint32, uint32>), 7), a);
}
```

##### Constructing functions

Curried functions can also be constructed one parameter at a time. A simple
example is function `constant` which returns a function that always returns
the value passed to `constant` as argument:

```
import data.function

inline void Foo()
{
    auto fn = constant(false);

    // `fn` is a function that ignores its argument and always returns `false`
    assert(false == fn(10));
    assert(false == fn(true));
}
```

Applying `constant` to the result of `constant` produces a function which
returns a function which returns a specified value. In other words it is
a curried form of binary function which ignores both its arguments and always
returns the specified value:

```
import data.function

inline void Foo()
{
    auto fn = uncurry(constant(constant(false)));

    // `fn` is a binary function that ignores its arguments and always returns `false`
    assert(false == fn(10, 34));
    assert(false == fn(true, 1));
}
```


### Reset functions
Functions annotated with `[[reset]]` attribute are called automatically after
the design is reset (typically used to initialize state).  Reset functions must
have void return type and an empty parameter list.

```
class Foo
{
private:
    [[reset]] void init()
    {
        pipelined_for(32, [](index_t<32> i)
        {
            mem[i] = 0;
        });
    }

    memory<uint32, 32> mem;
}
```

The order in which reset functions are called after reset is undefined.  Reset
functions will execute concurrently with other reset functions.

Reset functions can be called directly just like any other functions.

### Function Replication

A single function in the source code may correspond to many hardware replicas
(i.e. instances).  Function replication can be used to improve performance (at
the cost of area) by increasing hardware parallelism.  Function replication can
affect program behavior.  For instance, two threads can execute the statements
inside of an `atomic` block in parallel if the threads are executing separate
replicas of the same function.  Similarly, each replica of a function contains a
separate replica of each static local variable defined within the function.

Each call site of an inline function (including lambdas) replicates the body of
the inline function at the call site.

This snippet:
```
class C
{
    inline uint32 Foo(uint32 x)
    {
        return x + 1;
    }

    void Bar()
    {
        println(Foo(1));
        println(Foo(2));
    }
}
```

is equivalent to:
```
class C
{
    inline uint32 Foo(uint32 x)
    {
        return x + 1;
    }

    void Bar()
    {
        println(1 + 1);
        println(2 + 1);
    }
}
```

The inlining rule also applies to callbacks which are connected to inline functions.

This snippet:
```
class C
{
    (uint32)->uint32 cb;

public:
    void Bar()
    {
        println(cb(1));
        println(cb(2));
    }
}

class D
{
    inline uint32 Foo(uint32 x)
    {
        return x + 1;
    }

    C c = {.cb = Foo};
}
```

is equivalent to:
```
class C
{
    (uint32)->uint32 cb;

public:
    void Bar()
    {
        println(1 + 1);
        println(2 + 1);
    }
}

class D
{
    inline uint32 Foo(uint32 x)
    {
        return x + 1;
    }

    C c = {.cb = Foo};
}
```

Calls to closures are always inlined.

This snippet:
```
class C
{
    inline void Go((uint32) -> uint32 Do)
    {
        println(Do(1));
        println(Do(2));
    }

    void Bar()
    {
        const auto fn = [](uint32 x) -> uint32
        {
            return x + 1;
        };

        Go(fn);
    }
}
```

is equivalent to:
```
class C
{
    inline void Go()
    {
        println(1 + 1);
        println(2 + 1);
    }

    void Bar()
    {
        Go();
    }
}
```

Non-inline methods of a class are replicated once per class instance (i.e. object).

This snippet:
```
class C
{
public:
    uint32 Foo(uint32 x)
    {
        return x + 1;
    }
}

class D
{
    C c1;
    C c2;

    void Bar()
    {
        c1.Foo(1);
        c2.Foo(2);
    }
}
```

is equivalent to:
```
class C_1
{
public:
    uint32 Foo(uint32 x)
    {
        return x + 1;
    }
}

class C_2
{
public:
    uint32 Foo(uint32 x)
    {
        return x + 1;
    }
}

class D
{
    C_1 c1;
    C_2 c2;

    void Bar()
    {
        c1.Foo(1);
        c2.Foo(2);
    }
}
```

[Template](#templates) functions are a separate mechanism which can achieve
similar results to function replication.  Rather than creating copies of the
same function, each template instance is a separate function (which then can be
replicated using the orthogonal mechanisms described above).

In the following example, there are three instances of the template function
`Foo`.

This snippet:
```
class C
{
    template<typename T, auto N>
    T Foo(T x)
    {
        return x + N;
    }

    void Bar()
    {
        Foo<uint32, 4>(1);
        Foo<uint32, 4>(2);
        Foo<uint32, 8>(3);
        Foo<uint16, 4>(4);
    }
}
```

is desugared to:
```
class C
{
    uint32 Foo_uint32_4(uint32 x)
    {
        return x + 4;
    }

    uint32 Foo_uint32_8(uint32 x)
    {
        return x + 8;
    }

    uint32 Foo_uint16_4(uint16 x)
    {
        return x + 4;
    }

    void Bar()
    {
        Foo_uint32_4(1);
        Foo_uint32_4(2);
        Foo_uint32_8(3);
        Foo_uint16_4(4);
    }
}
```

The techniques described above can be organized into a hierarchy:

* Class template instantiation generates a unique class per template instance
* Function template instantiation generates a unique function per template instance
* Class instantiation generates a method replica per object
* Inlining replicates the body of a function at each call site

# Meta-programming

## Conditional code

The `static if` syntax allows including code conditionally based on a boolean
expression that can be evaluated at compilation time. A canonical use case is to
provide alternative implementations based on template argument(s):

```
template <typename T, auto N>
inline auto Foo(T[N] a)
{
    static if (N > 16)
    {
        // implementation for large arrays
    }
    else
    {
        // implementation for small arrays
    }
}
```

The `static if` syntax looks similar to the standard `if` conditional statement
but semantically it differs from it in significant ways. The branches of `static if`
do not form nested [scopes](#scope). A symbol declared within a `static if` can
be accessed outside of it:

```
static if (true)
{
    uint32 x;
}

x++;
```

A corollary is that declaration within `static if` are in the same scope as the
`static if`, and can conflict with declaration outside of it. For example, the
following code is invalid because the variable `x` is declared multiple times:

<!-- invalid -->
```
bool x;

static if (true)
{
    uint32 x;
}
```

A `static if` branch can contain a `return` statement, and if the function is
declared with `auto` return type, different branches can return values of
different types:

```
template <auto N>
inline auto Foo()
{
    static if (N == 1)
    {
        return;
    }
    else
    {
        return N;
    }
}
```

The `static if` syntax can be used outside of function body to make
conditional declarations. The canonical example is declaring class
members conditionally based on values of template parameters:

```
template <typename T, auto N>
class Storage
{
public:
    static if (N > 10 && ((N & (N - 1)) == 0))
    {
        memory<T, N> storage;
    }
    else
    {
        array<T, N> storage;
    }
}

class Foo
{
    Storage<uint32, 8> s8;
    Storage<bool, 16> s16;
}
```

Any part of class body can be made conditional, including access
specifier(s):

```
template <bool Public>
class Foo
{
    static if (Public)
public:
    else
private:

    void Do()
    {
    }
}

class Bar
{
    Foo<true> publicDo;
    Foo<false>privateDo;
}
```

While code within `static if` branches must be syntactically correct, code
within a branch that is not included based on the `static if` condition may include
statements or declarations that would otherwise be erroneous:

```
const auto N = 0;

static if (N > 1)
    uint<clog2(N)> x;

```

In the example above the `uint` type would be invalid for `N` that is not
greater than `1`. The snippet above is however valid because the declaration is
included in the program only when the `static if` condition `N > 1` is `true`.

It is important to note that `static if` is not analogous to `#ifdef` preprocessor
directive used in languages like C and C++. Since `static if` is part of the
language syntax, the condition can evaluate values at the language level:

```
template <typename T>
inline auto Foo()
{
    static if (bitsizeof(T) > 32)
    {
    }
}
```

On the other hand, unlike preprocessor directives that operate at the level of
text, the `static if` syntax can't conditionally include/exclude any random
fragment of the source file, only whole statements or declarations.

### Static recursion

While Kanagawa doesn't support runtime recursion, recursive algorithms can be
expressed statically if the recursion "size" is known at compile time. The trick
is to use `static if` syntax to terminate the recursive calls of a template
function:

```
template <typename T, auto N>
inline auto reduce(T[N] a, (T, T) -> T fn)
{
    static assert((N & (N-1)) == 0);

    static if (N == 1)
    {
        return a[0];
    }
    else
    {
        const auto b = split(a);
        return fn(reduce(b.first, fn), reduce(b.second, fn));
    }
}

template <typename T, auto N>
inline auto maximum(T[N] a)
{
    return reduce(a, [](uint32 x, uint32 y)
    {
        return x > y ? x : y;
    });
}
```

Another form of static recursion is recursive instantiation of object
hierarchies.

```
template <auto N, template <auto> typename Node>
class List
{
    public:
        void Do()
        {
            node.Do();
            static if (N > 1)
            {
               next.Do();
            }
        }
    private:
        Node<N-1> node;

        static if (N > 1)
        {
            List<N - 1, Node> next;
        }
}

template <auto N>
class Worker
{
    public:
       void Do()
       {
       }
}

class Foo
{
    List<7, Worker> workers;
}
```
### Type comparison

Types can be compared for equality and subtype relationship. When applied to
types the comparison operators have the following semantics:

- `==` true if operands are equal types
- `<=` true if left operand is subtype of the right operand
- `<` true if left operand is subtype of the right operand but is not the same type

Note that the comparison is about types, not symbols specifying types. For
example `uint8`, `uint<8>`, `index<200>` and `uint8_t` all specify the same type
(the last two are type aliases defined in the standard library).

Type comparisons are evaluated at compile time and can be used in contexts like
`static if` or `static assert`. A common usage scenario is specialization of
templates based on type parameters, e.g.:

```
template <typename T>
inline void Go(() -> T fn)
{
    static if (T == void)
    {
        // ...
    }
    else
    {
        // ...
    }
}
```

Any kind of type specifier can be used in type comparison expression, e.g.:

- built-in types

      uint32 == uint<32>

- names of user defined types

      struct Foo{}
      struct Bar()

      Foo != Bar

- type aliases

      uint32_t == uint32

- instances of type templates

      intex_t<10> == uint4

- template type parameters

      template <typename T>
      void Go(uint32 x)
      {
          static if (T == void)
          {}
      }

- arrays

      bool[10] != bool[11]

- function types

      (uint33) -> uint9 < (uint32) -> uint10

- decltype declarations

      decltype(Foo::Run) == (uint32) -> bool

# Modules

Kanagawa uses modules to package reusable code. A module provides
functionality through the types, functions, and constants that make up its
public interface. Collections of modules can be organized into libraries.

## Using modules

The first way you are likely to encounter modules is by importing a module from
the standard library. Modules are imported using an import statement.

```
import data.array
```

Modules provide functionality via a set of exposed symbols that can be used
after a module has been imported. For example, the `data.array` module exposes
a function `sum` to compute the sum of an array of integers.

```
import data.array

inline uint32 func(uint32[4] ary)
{
    return sum<uint32>(ary);
}
```

The name of a module must correspond to the directory and filename in which the
module is defined. For example, a module `foo.bar` must be defined in a file
foo/bar.k relative to a directory specified as a source for imported modules
(specified with the `--import-dir` compiler switch).  The `--import-dir` switch
can be used multiple times to specify multiple search locations for modules.

Each module is associated with a unique namespace whose name is derived from
the module name.  This namespace is never directly referenced in Kanagawa
source code (but it is present in the interface to generated hardware). All
symbols declared within a module are placed into the associated namespace.

Importing a module makes all symbols exposed by the module visible within the
importing file. This may lead to name ambiguity if multiple imported modules
expose symbols with identical names, or importing file declares a symbol whose
name conflicts with a symbol from an imported module. Name ambiguities can be
resolved using qualified imports:

```
import numeric.float32 as F32
import numeric.float64 as F64

inline void func(uint32 a)
{
    auto x = F32::from_uint<32>(a);

    auto y = F64::from_uint<32>(a);
}
```

In the example above, the `numeric.float32` and `numeric.float64` modules
expose many symbols that have the same names; to resolve these conflicts they
are imported with the `F32` and `F64` qualifiers which can then be used to
disambiguate the conflicting names.

It is important to note that mere importing of modules that expose symbols with
identical names doesn't create conflicts. A name ambiguity occurs only when
_referencing_ a name that is defined in multiple modules. If only some names in
an imported module cause ambiguity then it is often convenient to import the
module both with and without qualification so that its symbols are accessible
both with and without the qualifier.

```
import numeric.float32 as F32
import numeric.float64
import numeric.float64 as F64

inline void func(uint32 a)
{
    float32 x = F32::from_uint<32>(a);

    float64 y = F64::from_uint<32>(a);
}
```

### Device specific modules

Device specific modules allow providing different implementation for parts of
a design or library depending on the target device. The compiler looks for
device specific modules in `device\<target-device>` subdirectories of the
import directories provided via `--import-dir` switch. For example, to provide
device-specific implementations of a module `bus.pci` for `Agilex`, `Arria10`
and `Stratix10` devices, the following directory structure would be created:

    +---device
       +---Agilex
       |   +---bus
       |       +---pci.k
       +---Arria10
       |   +---bus
       |       +---pci.k
       +---Stratix10
           +---bus
               +---pci.k

Each of the `pci.k` files would have the following structure:

```
module bus.pci
    { // exposed symbols
    }

// implementation
```

A design using the `bus.pci` module would import it like any other module:

```
import bus.pci
```

and the `--import-dir` switch would be used to specify the location of the
directory structure shown above.

When the compiler is unable to locate a module in a directory specific to
a device, it will then search for it in the designated import directories. This
enables the provision of a default implementation for the module.

## Defining modules
To define a module, simply include a module declaration at the top of a
Kanagawa source code file.

```
module some.other
{
    fn,     // exposes symbol 'fn', a locally-defined function
    stu     // exposes symbol 'stu', a locally-defined structure
}

inline void fn()
{
}

struct stu
{
    uint32 foo;
}
```

The module declaration specifies a name for the module and a list of symbols
it makes available when its imported; those symbols make up the public interface
of the module.  A module can expose functions, types, and constants
only.

Because a module can be imported with and without an alias it is good practice
to keep symbol names short.  There is no need to add a prefix to symbol names to
indicate which module they are included in.

```
module binary_search_tree
{
    // data types:
    binary_search_tree,

    // functions:
    // Avoid this style
    // (pre-pending 'binary_search_tree_' to function names)
    binary_search_tree_insert,
    binary_search_tree_find
}

struct binary_search_tree {}
inline void binary_search_tree_insert() {}
inline void binary_search_tree_find() {}
```

```
module binary_search_tree
{
    // data types:
    binary_search_tree,

    // functions:
    // Prefer this style (short function names)
    insert,
    find
}

struct binary_search_tree {}
inline void insert() {}
inline void find() {}
```

## Re-exposing symbols
When a module `A` imports a module `B`, the symbols from `B` are not exposed
from `A` by default.  Sometimes it is useful for module `A` to expose symbols
that it imports from module `B`.  This is needed if some parts of `B` are
required as a part of the public interface of `A`.

```
module matrix
{
    // 'fixed' from numeric.fixed is re-exposed by the 'matrix' module
    fixed
}

import numeric.fixed
```

A module can expose all symbols from an imported module by explicitly listing
the module to be re-exposed:

```
// All symbols exposed by numeric.fixed will be exposed by matrix
module matrix { module numeric.fixed }

import numeric.fixed
```

A module can expose itself, making all symbols defined within it public, without
the need to list each one individually:

```
module foo.bar { module foo.bar }

// All exposable symbols (X and Z) will be exposed by foo.bar
const auto X = 3;

struct Z
{
    uint32 y;
}
```

Note that when exposing s module's symbols this way, only functions, types and
constants are exposed.

### Module difference

Limit exposed symbols to a difference between two modules using the backslash
symbol:

```
module params
    { module .cmdargs
    , module params \ .cmdargs
    }

import .cmdargs

const auto foo = 0;
const auto bar = 0;

class DefaultApp
{}

using app_t = DefaultApp;
```

The module `params` exposes symbols imported from the module `.cmdargs` as well
as any symbols defined in the module `params` that are not defined in module
`.cmdargs` (i.e. difference between modules `params` and `.cmdargs`).

The module `.cmdargs` is a [special module](#special-modules) that contains
symbols defined via compiler command line options `--define` and `--using`.
This technique allows specification of design parameters with default values
that can be overwritten via compiler command line options.

## The base module

By default, all files import the module named `base`.  This can be
disabled with the `--no-implicit-base` compiler argument.  The
Kanagawa standard library includes an implementation of the `base` module
that provides commonly used functions and symbols.

The `base` re-exposes symbols from device-specific modules named `hardware.*`.
Each device has separate implementations of these modules.  The
`--target-device` compiler argument determines which directory the compiler
loads `hardware.*` modules from.

If the `--no-implicit-base` compiler argument is specified during compilation,
then compilation errors will occur if the source code uses floating-point
operators or device-specific functions.

## Special modules

Special modules are not defined in a source file but instead are automatically
generated by the compiler. Other than the source of their definition these
modules can be used just like regular modules.

### The .cmdargs module

The special module `.cmdargs` exposes symbols defined with `--define` and
`--using` command line arguments. For example when compiling a program with the
following options:

    --define Foo=10 --using Bar=uint32

the special module `.cmdargs` would be defined as following:

    module .cmdargs {module .cmdargs}

    const auto Foo = 10;
    using Bar = uint32;

The module `.cmdargs` is always imported implicitly, however like any other
module it can also be imported explicitly. This is particularly useful when it
is desired to qualify symbols imported from `.cmdargs`, either in order to
avoid conflicts, or to distinguish externally defined symbols for code clarity:

```
import .cmdargs as params

class Foo
{
    memory<uint32, params::MemoryLength> mem;
}
```

When using compiler command line options to specify design parameters, it is
recommended to define a module with default values for each parameter, and
expose a [difference](#module-difference) between that module and `.cmdargs`.

### The .options module

The special module `.options` exposes constants corresponding to selected
compiler options:

 Compiler option            | Constant name             | Constant type
:-------------              | :-----------              | :-----------
--stall                     | stall                     | integer
--backend                   | backend                   | enum `Backend` from `compiler.options`
--target-device             | target_device             | enum `Device` from `compiler.options`
--target-device             | target_device_name        | string
--optimize                  | optimize                  | integer
--max-threads-limit         | max_threads_limit         | integer
--max-threads-default       | max_threads_default       | integer

The module `.options` is not imported implicitly. Since future versions of the
compiler may define constants for other options, it is recommended to always
import the module qualified in order to avoid potential name conflicts:

```
import .options as opt

inline void Run()
{
    static if (opt::backend == opt::Backend::Sv)
    {
        // ...
    }
}
```

An example use case of `.options::max_threads_limit` is sizing counters which count
the number of concurrent threads in a function.  An example use case of
`.options::max_threads_default` is setting the number of threads used by
`pipelined_do`.

# Concurrency and parallelism

Concurrency and parallelism are two distinct concepts.  Concurrency refers to
multiple tasks in progress at any one time whereas parallelism refers to
multiple tasks executing at the same time.

For example, a process running on a single core CPU could be decomposed into
multiple concurrent threads.  At any one moment in time, one thread is executing
on the CPU core, while the remaining are sitting in an OS scheduler queue
waiting for a turn to execute.  This situation has high concurrency, but no
parallelism.

If that same program was run on a 4 core CPU, there would be same amount of
concurrency (dictated by the number of threads).  However there would be more
parallelism, because up to 4 threads could be executing in parallel at any
moment in time.

In traditional software languages, concurrency is expressed in the source code
(with concepts like threads).  Parallelism however is not specified in the
source code; instead the OS and hardware cooperate to perform the computation in
parallel.

In Kanagawa both concurrency and parallelism are explicitly expressed in the
source code.  This makes it easier to reason about performance but requires
programmers to design for parallelism.  Too little parallelism will result in
poor performance, too much parallelism will result in circuits that consume too
many resources.

## Threads
Concurrency is expressed in Kanagawa via threads.  The context of a thread
consists of:

- The next statement to be executed
- A value for each local variable (see [Variables](#state)).

Threads can be created in a few ways:

- When a public method of exported class is called from outside of Kanagawa code
- Through library functions see [concurrency library functions](#concurrency-library-functions).

Kanagawa threads are lightweight compared to software threads.  The following
table illustrates some relative costs:

Attribute            | Software     | Kanagawa
:--------            | :------      | :------
Creation Cost        | Microseconds | Free
Synchronization Cost | Nanoseconds  | Free
Context Size         | KB or MB     | Bytes

Because threads are lightweight, they can be used in ways that software threads
cannot efficiently be used in.  It is reasonable to create hundreds or thousands
of threads to perform a short-duration task.  Idiomatic Kanagawa expresses
loops with calls to functions like `pipelined_for` to create one thread per loop
iteration.  For example, it is normal practice to implement a memory copy
operation by creating 1 thread per byte to be copied.

## Concurrency library functions
This section describes commonly used library functions for creating threads.

### pipelined_for
`pipelined_for` creates N threads, each of which executes the provided function
one time.  A thread identifier is passed to each one.

```
auto numIter = 4;

// prints: 0, 1, 2, 3
pipelined_for(numIter, [](uint2 thread_id)
{
    println(thread_id);
});
```

### pipelined_do
`pipelined_do` creates 2^N threads, each of which execute the provided function
as long as the provided function returns true.

```
// 8 threads will execute the lambda body forever
pipelined_do([](uint3 thread_id)
{
    return true;
});
```

### pipelined_map
`pipelined_map` creates N threads, each of which executes the provided function
one time.  The return values from all threads are combined into an array that is
returned from `pipelined_map`.  The number of elements in the combined array is
fixed at compile time.  The thread count is determined at run time.  If
the thread count is smaller than the array size, then the upper elements of the
returned array are undefined.  The thread count must not be larger than the array
size.

```
auto numIter = 4;

uint32[4] result = pipelined_map<4>(
    numIter,
    [](uint2 thread_id) -> uint32
{
    return thread_id + 3;
});

// result contains { 3, 4, 5, 6 }
```

### pipelined_last
`pipelined_last` creates N threads, each of which executes the provided function
one time.  The return value from the last thread is returned from
`pipelined_last`.

```
auto numIter = 4;

uint32 result = pipelined_last(numIter, [](uint2 thread_id)
{
    return thread_id + 3;
});

// result is 6
```

### async_exec
`async_exec` creates a thread which starts execution in the specified function.

```
async_exec([]()
{
    print("executing the lambda asynchronously\n");
});
```

Note that if that are no ordering guarantees between multiple asynchronous call sites to the same function:

```
inline void common_function(uint5 tid, uint32 call_site)
{
    print("{tid=} {call_site=}\n");
}

inline void F()
{
    pipelined_for(2, [](uint5 tid)
    {
        async_exec([tid]() { common_function(tid, 1); });

        async_exec([tid]() { common_function(tid, 2); });
    });
}

// Valid output sequences:
//
// 0, 1
// 0, 2
// 1, 1
// 1, 2
//
// and:
//
// 0, 2
// 0, 1
// 1, 2
// 1, 1
//
```

## Spatial computing

The Kanagawa compiler translates imperative source code into spatial hardware.
The structure of the generated hardware determines how much parallelism can be
achieved.  The circuits generated by Kanagawa comprise:

- A set of hardware operators
- Connectivity between the operators
- Grouping of operators into stages
- A schedule that determines the time at which stages perform their
  computation (relative to other stages)

Each hardware operator is specialized: it can only perform one simple task
(like integer addition, bitwise XOR, or reading from an array).  The Kanagawa
compiler performs non-trivial decompositions to optimize the generated hardware.
For example, a wide integer addition in the source code may be translated into
multiple narrow hardware adders to keep each hardware operator small and fast.

Hardware operators are grouped into stages.  Multiple
operators can be assigned to the same stage, even if there is a
dependency between them.  For example, consider the following Kanagawa
source:

<!-- invalid -->
```
e = ((a + b) & c) - d;
```

This could translate into 3 hardware operators:

<!-- invalid -->
```
op1: t1 = a + b
op2: t2 = t1 & c
op3: e = t2 - d
```

Which could be grouped into two stages:

<!-- invalid -->
```
stage0: op1 op2
stage1: op3
```

There is a relationship between Kanagawa threads and Kanagawa-generated
circuits.  A Kanagawa thread corresponds to data flowing through the
stages in a circuit.  A thread physically moves through the circuit like how
a thread logically moves through the statements in the imperative source code.
The local variables associated with a thread are the data flowing between
operators in the generated hardware.  The movement of threads through a circuit occurs in discrete units of time (clock cycles).

When executing straight-line code, a thread moves through a linear list of
stages in order.  A list of stages that are always executed in the same order is
called a pipeline.  Kanagawa control flow constructs cause threads to
conditionally move between stages.

For example, the following code:

```
uint32 sum = 0;

for (const auto i : 4)
{
    uint32 x = i + 4;
    uint32 y = x * 3;
    sum += y;
}

println(sum);
```

could be decomposed into the following stages:

<!-- invalid -->
```
stage0: sum = 0; <jump to stage 1>

stage1: x = i + 4;
stage2: y = x * 3;
stage3: sum += y; <jump to either stage 1 or stage 4>

stage4: println(sum)
```

The following properties hold at all possible moments in time:

- A particular thread will never be executing in more than one stage
- A particular stage will never be executing for more than one thread

A few interesting consequences can be derived from these properties.  First,
when multiple threads converge from distinct paths onto a shared path through
the circuit then some threads must wait for their turn to execute the shared
stage. Imagine two threads are executing the example above, and at a specific
moment in time one thread is at `stage0`, while another is at `stage3` (ready
to loop back).  Both threads need to execute `stage1` next, but `stage1` can only
service one thread at a time. The generated hardware will contain a queue, and
the context of the thread at `stage0` will be placed into that queue until it has a
chance to execute `stage1`.

Secondly, there is an ordering among Kanagawa threads. If a group of threads
are in a pipeline in a specific order, that order will be maintained.  There is
no way for one thread to 'jump ahead' of another.  Thread ordering is well
defined by all Kanagawa control flow constructs.  See [Thread
Ordering](#thread-ordering).

The amount of concurrency at any given moment in time is determined by the
number of threads that are active.  Maximum parallelism is determined by the
number of operators in the generated hardware. Maximum parallelism is achieved
when enough threads exist that all stages are executing in parallel.  It is the
programmer's responsibility to express enough concurrency to ensure that most of
the stages can do useful work most of the time - thus enabling enough parallelism in
the circuit to achieve the desired throughput.  A rule of thumb is use 100s or
1000s of threads.

In the following examples, assume that the only relevant threads are threads created in the
snippet and one thread that exists a priori and calls the function `F`.

The following code snippet has low concurrency and low parallelism.  No threads are created
in the snippet, and thus only one thread executes all of the loop iterations serially.

```
class Foo
{
    uint32 sum = 0;

    void F()
    {
        for (const auto i : 256)
        {
            sum += i;
        }
    }
}
```

This next code snippet has high concurrency and low parallelism. The call to `pipelined_for` creates
many threads but the total number of hardware operators is low (just add operator in the generated circuit).
The `atomic` block is needed to synchronize access to the shared `sum` variable see [Scheduling Constraints](#scheduling-constraints).

```
class Foo
{
    uint32 sum = 0;

    void F()
    {
        pipelined_for(256, [](uint8 i)
        {
            atomic
            {
                sum += i;
            }
        });
    }
}
```

The next code snippet has high parallelism (the generated circuit has 16 add operators) but that parallelism will not be fully utilized because of low concurrency (only one thread).

```
class Foo
{
    uint32 sum = 0;

    void F()
    {
        for (const auto i : 16)
        {
            static for (const auto j : 16)
            {
                sum += ((i * 16) + j);
            }
        }
    }
}
```

The next code snippet has high parallelism (16 threads) and high concurrency (16 add operators).

```
class Foo
{
    uint32 sum = 0;

    void F()
    {
        pipelined_for(16, [](uint4 i)
        {
            uint32 local_sum = 0;

            static for (const auto j : 16)
            {
                local_sum += ((i * 16) + j);
            }

            atomic
            {
                sum += local_sum;
            }
        });
    }
}
```

## Predicting throughput
Explicit parallelism makes it possible to accurately predict the throughput of
Kanagawa code and enables programmers to trade resource usage for throughput.
The trick is to remember that each stage can service at most one thread at a
time.  If there are enough threads executing a given block of code, then maximum
throughput for that block will be reached.

The following examples illustrate how to predict throughput.  Assume that
`numIter` is large (hundreds or thousands).  Each example calls `pipelined_for`
to create `numIter` threads, each of which executes the specified lambda.  In
the descriptions that follow, 'approximately' is used to describe a value that
performance asymptotically approaches as the number of threads increases.

The following code snippet will execute in approximately `numIter` clock
cycles.  The reasoning behind this is that the addition and multiplication are
decomposed into operators in stages, and every thread will execute each of those
stages exactly once.  The only way for this code to complete in less than
`numIter` clock cycles would be if one stage could be doing computations for two
threads in parallel (which is impossible).

```
uint32 numIter;

pipelined_for(numIter, [](uint32 tid)
{
    uint32 a = tid + 1;

    uint32 b = a * 3;
});
```

The following code snippet will execute in approximately `numIter` clock
cycles.  The operators corresponding to the `x = x - tid` statement can service
at most one thread per cycle.  Therefore, there is no end-to-end time saved by
skipping the body of the `if` statement for half of the threads.  The generated
circuit may choose to skip the body of the `if` statement to save power, but
this will not affect throughput.

```
uint32 numIter;

pipelined_for(numIter, [](uint32 tid)
{
    uint32 x = tid;

    if ((tid & 1) == 1)
    {
        x++;

        x = x * 5;

        // many more operators on x
    }

    x = x - tid;

    println(x);
});
```

The following code snippet will execute in approximately `numIter * 4` clock
cycles.  The key insight is that any particular thread must execute the
operators corresponding to `x += tid` four times.

```
uint32 numIter;

pipelined_for(numIter, [](uint32 tid)
{
    uint32 x = tid & 5;

    for (const auto i : 4)
    {
        x += tid;
    }

    println(x);
});
```

The following code snippet has nested concurrency.  It will execute in
approximately `numIter * 4` clock cycles.  Any particular outer thread will
create 4 inner threads, each of which executes the operators corresponding to
`inner_tid + 4` one time.

```
uint32 numIter;

pipelined_for(numIter, [](uint32 outer_tid)
{
    pipelined_for(4, [](uint32 inner_tid)
    {
        uint32 j = inner_tid + 4;

        println(j);
    });
});
```

The previous two examples execute in the same amount of time.  The first example
introduces new threads with the outer `pipelined_for` call, and uses those
threads to loop 4 times.  The second example introduces new threads in two
different places: the outer and inner `pipelined_for` calls.  The second example
is usually the better choice, as it exposes more concurrency (more threads) to
the compiler.  The complexity of the body of the inner loop determines how many
threads are required to achieve peak performance.  The second example can
achieve peak performance with more complex code because there are more threads
that execute the body of the loop. The first example should be used in cases
where there are complex dependencies between iterations of the inner loop that
are hard to express as communication between threads.

The next example will execute in approximately `numIter * 2` clock cycles.
Half of the outer threads will create four inner threads, the other half will
create zero threads.  Thus the average outer thread will create two inner
threads.

```
uint32 numIter;

pipelined_for(numIter, [](uint32 outer_tid)
{
    if (1 == (outer_tid & 1))
    {
        pipelined_for(4, [](uint32 inner_tid)
        {
            uint32 j = inner_tid + 4;

            println(j);
        });
    }
});
```

The next example will execute in approximately `numIter * 2` clock cycles.
Note how `Helper` is not marked `inline`.  `Helper` corresponds to one piece of
hardware that is shared by both call sites.  Each thread executes through this
hardware twice.

```
class Foo
{
    uint32 Helper(uint32 x)
    {
        return x + 1;
    }

    void Execute(uint32 numIter)
    {
        pipelined_for(numIter, [](uint32 tid)
        {
           uint32 x = Helper(tid);

           uint32 y = Helper(tid);

           println(x + y);
        });
    }
}
```

If `Helper` is marked inline, then the code snippet will execute in
`numIter` clock cycles.  This occurs because each call site to `Helper` will
have unique hardware corresponding to the expression: `x + 1`.

```
class Foo
{
    inline uint32 Helper(uint32 x)
    {
        return x + 1;
    }

    void Execute(uint32 numIter)
    {
        pipelined_for(numIter, [](uint32 tid)
        {
           uint32 x = Helper(tid);

           uint32 y = Helper(tid);

           println(x + y);
        });
    }
}
```

## Increasing parallelism
This section describes the constructs in Kanagawa that enable peak parallelism
to be increased.  These techniques are useful when the parallelism of a given
Kanagawa program is insufficient to meet performance goals.  Increasing peak
parallelism increases the amount of resources consumed by the generated
hardware.

`static for` replicates the body of the loop, causing operators to be
replicated.

```
uint32 numIter;

// Throughput: one addition per clock cycle
// Execution Time: numIter*4 clock cycles
// Resources: One set of operators that implement the loop body
pipelined_for(numIter, [](uint32 tid)
{
    uint32[4] src;
    uint32[4] dst;

    for (const auto i : 4)
    {
        dst[i] = src[i] + 1;
    }
});

// Throughput: four additions per clock cycle
// Execution Time: numIter clock cycles
// Resources: Four sets of operators that implement the loop body
pipelined_for(numIter, [](uint32 tid)
{
    uint32[4] src;
    uint32[4] dst;

    static for (const auto i : 4)
    {
        dst[i] = src[i] + 1;
    }
});

// Throughput: two additions per clock cycle
// Execution Time: numIter * 2 clock cycles
// Resources: Two sets of operators that implement the loop body
pipelined_for(numIter, [](uint32 tid)
{
    uint32[4] src;
    uint32[4] dst;

    for (const auto i : 2)
    {
        static for (const auto j : 2)
        {
            uint2 idx = (i * 2) + j;

            dst[idx] = src[idx] + 1;
        }
    }
});
```

```
class Foo
{
    uint32 numIter;

    uint32 AddOne(uint32 x)
    {
        return x + 1;
    }

public:    
    void Run()
    {
        // Throughput: one addition per clock cycle
        // Execution Time: numIter*4 clock cycles
        pipelined_for(numIter, [](uint32 tid)
        {
            uint32[4] src;
            uint32[4] dst;

            static for (const auto i : 4)
            {
                dst[i] = AddOne(src[i]);
            }
        });
    }
}
```

`inline` functions replicate the body of a function once per call site, causing
operators to be replicated once per call site.

```
class Foo
{
    uint32 numIter;

    inline uint32 AddOneInline(uint32 x)
    {
        return x + 1;
    }

public:    
    void Run()
    {
        // Throughput: four additions per clock cycle
        // Execution Time: numIter clock cycles
        pipelined_for(numIter, [](uint32 tid)
        {
            uint32[4] src;
            uint32[4] dst;

            static for (const auto i : 4)
            {
                dst[i] = AddOneInline(src[i]);
            }
        });
    }
}
```

Objects are another way to replicate operators.  Each non-inline method is
replicated once per object.  In other words, each object has a separate copy of
each method.


```
class multiply_by_three
{
public:
    uint32 mul(uint32 x)
    {
        return x * 3;
    }
}

// Throughput: one multiplication per clock cycle
// Execution Time: numIter*4 clock cycles
inline void ExecuteOneReplica(uint32 numIter)
{
    pipelined_for(numIter, [](uint32 tid)
    {
        static multiply_by_three _multiplier;

        uint32[4] src;
        uint32[4] dst;

        static for (const auto i : 4)
        {
            dst[i] = _multiplier.mul(src[i]);
        }
    });
}

// Throughput: Four multiplication per clock cycle
// Execution Time: numIter clock cycles
inline void ExecuteFourReplicas(uint32 numIter)
{
    pipelined_for(numIter, [](uint32 tid)
    {
        static multiply_by_three[4] _multipliers;

        uint32[4] src;
        uint32[4] dst;

        static for (const auto i : 4)
        {
            dst[i] = _multipliers[i].mul(src[i]);
        }
    });
}
```

## Thread ordering

Kanagawa constructs such as functions, branching statements, and looping
statements, have the property that if two threads enter a construct in a given
order, then the threads will leave that construct in the same order.

Thread ordering is a useful synchronization construct. A typical example
is to identify the first and/or last thread within a group:


```
const auto numIter = 256;

pipelined_for(numIter, [numIter](uint32 thread_id)
{
    bool is_first = (thread_id == 0);
    bool is_last = (thread_id == (numIter - 1));

    // a bunch of code

    // is_first and is_last are still valid

    if (is_last)
    {
        print("last thread done\n");
    }
});
```

The only language construct that enables two threads to change order is a loop
with the `[[unordered]]` attribute.  If two threads take different number of
trips through the loop, then they can leave the loop in a different order than
they entered.  If the `[[unordered]]` attribute is **not** present, then threads
that finish all loop iterations will be held in a buffer and not allowed to move
past the loop until all preceding threads have.

If threads can exit a function in a different order than they entered, then the
function must have the `[[unordered]]` attribute.  This requirement is enforced
at compile time.

Unordered loops and functions are useful in cases where a set of threads each
execute a loop and there are significant differences in the trip counts among
the threads.  If the application demands a low latency for threads with low trip
counts, then an unordered loop can ensure that these threads are not held up by
the threads with high trip counts.

The `reorder` block can be used to put threads back into their original order.


```
const auto numIter = 256;

pipelined_for(numIter, [](uint32 thread_id)
{
    uint2 trip_count = thread_id + 4;

    reorder
    {
        [[unordered]]
        for (const auto i : trip_count)
        {
        }

        // This will _not_ print thread IDs in order
        print("at end of loop {thread_id}\n");
    }

    // This _will_ print thread IDs in order
    print("after reorder {thread_id}\n");
});
```

## Scheduling constraints
The `[[schedule(N)]]` attribute can be used to constrain operator scheduling.
`[[schedule(N)]]` forces the compiler to ensure that no more than `N` threads
can be present in a block of code at any moment in time.  This is accomplished
by ensuring that the generated hardware corresponding to the block of code
is scheduled into `N` pipeline stages.

`[[schedule(N)]]` is frequently used as a synchronization construct.  In the
example below, `[[schedule(1)]]` is used to ensure that a shared variable is
updated atomically.


```
// This variable is shared among threads
static uint32 res = 0;

// Only 1 thread allowed in this block at a time
[[schedule(1)]]
{
    // Read-modify-write the shared variable
    uint32 sum = res + 2;
    res = sum * 6;
}
```

Without the `[[schedule(1)]]`, there could be a bug where the addition and
multiplication occur on different clock cycles.  The following tables
illustrates how such a bug could occur.

**With `[[schedule(1)]]`:**

Clock Cycle | res       | Thread A Operations     | Thread B Operations
:--------   | :------   | :------                 | :------
0           | 0         | sum = 2, res = 12       |
1           | 12        |                         | sum = 14, res = 84
2           | 84        |                         |

**Without `[[schedule(1)]]`:**

Clock Cycle | res       | Thread A Operations |Thread B Operations
:--------   | :------   | :------             | :------
0           | 0         | sum = 2             |
1           | 0         | res = 12            | sum = 2
2           | 12        |                     | res = 12
3           | 12        |                     |

The `atomic` keyword is a synonym for `[[schedule(1)]]`:


```
// This variable is shared among threads
static uint32 _counter = 0;

// Only 1 thread allowed in this block at a time
atomic
{
    // Read-modify-write the shared variable
    _counter = _counter + 2;
}
```

It is common practice in Kanagawa to read-modify-write the contents of memories
in a `[[schedule(N)]]` block:


```
class Foo
{
    memory<uint32, 512> g_histogram;

public:
    void update_histogram(uint32 src)
    {
        atomic
        {
            // atomically read-increment-write
            g_histogram[src]++;
        }
    }
}
```

There is no need to wrap a read or write of a single variable in
`[[schedule(N)]]/atomic`.  Accesses to a single variable (including a memory)
are always atomic.  For example, there is no need to use `atomic` in the
following code when writing to `_shared_var`.


```
auto numIter = 256;

pipelined_for(numIter, [](uint32 thread_id)
{
    static uint32 _shared_var;

    _shared_var = thread_id;
});
```

### Nesting scheduling constraints

Schedule blocks can always be nested if the nested, inner schedule block is equally
or less restrictive than the outer schedule block. A `[[schedule(N)]]` block
is less restrictive than `[[schedule(M)]]` block if `N > M`. In these cases,
the nested schedule block is ignored because the outer, more restrictive
schedule takes precedence. For example:

```
[[schedule(2)]]
{
    // No more than 2 threads can be executing this block

    [[schedule(3)]]
    {
        // The schedule(3) indicates that no more than 3 threads can be
        // executing this block. However, the outer schedule(2) indicates
        // that no more than 2 threads can be executing this block and so
        // the inner schedule(3) is ignored.
    }

    [[schedule(2)]]
    {
        // Similarly, nesting a schedule(2) is redundant and is also ignored.
    }
}
```

A more restrictive schedule block can be nested within another schedule block
under the following restrictions:
- Memory accesses cannot occur in the outer schedule block
For example:

```
class Foo
{
    // Memory
    memory<uint32, 32> g_mem;

    void Foo()
    {
        [[schedule(3)]]
        {
            // Memory accesses are not allowed here because of the atomic block below
            // g_mem[0]++;

            atomic
            {
                // Memory accesses are allowed here
                g_mem[0]++;
            }

            [[schedule(2)]]
            {
                atomic
                {
                    // Multiple nestings are also allowed
                    g_mem[0]++;
                }
            }
        }

        // Memory accesses are always allowed outside of any schedule blocks
        g_mem[0]++;
    }
}
```

### Shared state accesses in schedule blocks

Shared variables in a `schedule` block will only read from shared state once
per thread, typically at the start of the `schedule` block. Subsequent reads
will use a local copy. Similarly, shared variables in a `schedule` block will
only write to the shared state once per thread, typically at the end of the
`schedule` block. Earlier writes will write to a local copy before the final
write to shared state. For example:

```
inline void Foo()
{
    static uint32 _var;

    [[schedule(4)]]
    {
        // First read will access shared state and store it into a thread-local
        // register.
        uint32 x = _var;

        // Second read does not access shared state. Instead, it uses the
        // thread-local copy from above.
        uint32 y = _var;

        // This is not the last write to _var and does not write to shared
        // state. I.e., other threads will not see _var = 1. This instead
        // writes to the thread-local copy.
        _var = 1;

        // This read does not access shared state. It will read from the
        // thread-local copy which has a value of 1.
        uint32 z = _var;

        // This is the final write to _var and will be written to shared state.
        // Other threads will only see _var = 2.
        _var = 2;
    }
}
```

For nested schedule blocks, this behavior is also applied to each section of
outer schedule blocks that does not contain an inner schedule block. For
example:

```
inline void Foo()
{
    [[schedule(8)]]
    {
        // Read from shared state

        // Compute on thread-local copy

        // Write to shared state

        [[schedule(4)]]
        {
            // Read from shared state

            // Compute on thread-local copy

            // Write to shared state
        }
        // Read from shared state

        // Compute on thread-local copy

        // Write to shared state
    }
}
```

### atomic do
An `atomic do` loop causes the calling thread to spin until the loop condition
evaluates to false.

```
static uint32 x; // shared variable
uint32 y; // local variable

// Loop until some other thread to set x to a value larger than y
atomic do
{
} while (x <= y)
```

`atomic do` has 'head-of-line blocking', which means that while one thread is
waiting for the loop condition to return true, no other thread has a chance to
evaluate the loop.

A common pattern is to use `atomic do` with an inline function.  This is a safe
way to update shared state within a `atomic do`:

```
class Foo
{
    // A shared variable
    bool _locked = false;

    inline bool test_and_set()
    {
        // The read-modify-write of _locked is atomic
        // because test_and_set is called within atomic do
        bool result = false;

        if (!_locked)
        {
            _locked = true;
            result = true;
        }

        return result;
    }

    void F()
    {
        atomic do {} while (!test_and_set());
    }
}
```

The `atomic do` loop body and condition are executed atomically and are
subject to the same restrictions as an `atomic` block plus an extra restriction
that memories cannot be accessed. Since the loop body and condition are executed
atomically, the following is equivalent to the above example:

```
class Foo
{
    // A shared variable
    bool _locked = false;

    inline bool test_and_set()
    {
        // The read-modify-write of _locked is atomic
        // because test_and_set is called within atomic do
        bool result = false;

        if (!_locked)
        {
            _locked = true;
            result = true;
        }

        return result;
    }

    void F()
    {
        bool done;
        atomic do
        {
            done = test_and_set();
        } while (!done);
    }
}
```

## Data races
In Kanagawa, a data race occurs in the following situations

- More than one thread writes to the same shared variable at the same time
- More than one thread reads from the same non-replicated memory (`memory_norep`)
  at the same time

When a data race occurs, one thread is arbitrarily chosen as the winner.  Writes
from other threads are dropped, reads from a non-replicated memory will return
undefined results.

When writing to an array, a data race only occurs if the parallel writes use
the same array index.  When writing to a structure, a data race only occurs if
the parallel writes write to the same member.

When writing to a memory, a data race occurs **even if** the threads involved
use distinct addresses.  When reading from a non-replicated memory, a data race
occurs **even if** the threads involved use distinct addresses.

### Read replication and memory_norep
In order to support parallel reads of a single memory object, the Kanagawa
compiler transforms programs such that each memory is only read from one
location.  This transformation instantiates a memory replica corresponding to
each memory read expression in the program.  The following two code snippets
illustrate this transformation.

Original:

```
class Foo
{
    memory<uint32, 4> mem;

    uint32 F(uint2 idx)
    {
        return mem[idx] + 1;
    }

    uint32 G(uint2 idx)
    {
        return mem[idx] / 2;
    }

    void Write(uint2 idx, uint32 val)
    {
        mem[idx] = val;
    }
}
```

Transformed:
```
class Foo
{
    memory<uint32, 4> mem_replica1;
    memory<uint32, 4> mem_replica2;

    uint32 F(uint2 idx)
    {
        return mem_replica1[idx] + 1;
    }

    uint32 G(uint2 idx)
    {
        return mem_replica2[idx] / 2;
    }

    void Write(uint2 idx, uint32 val)
    {
        mem_replica1[idx] = val;
        mem_replica2[idx] = val;
    }
}
```

For cases where the programmer knows that two memory reads will not occur
at the same time, `memory_norep` can be used.  Using `memory_norep` ensures that
resources will not be spent on memory replicas, however it is up to the
programmer to ensure there will not be parallel reads.  If there are
parallel reads to the same memory (even with different addresses), then read
results are undefined.

```
class Foo
{
    memory_norep<uint32, 4> mem;

    // Programmer ensures that F and G cannot run at the same time
    uint32 F(uint2 idx)
    {
        return mem[idx] + 1;
    }

    uint32 G(uint2 idx)
    {
        return mem[idx] / 2;
    }

    void Write(uint2 idx, uint32 val)
    {
        mem[idx] = val;
    }
}
```

### Incoherent memory read-write
An incoherent memory read-write describes the following situation:
* Thread A writes to memory M at address E
* Thread B reads from memory M at address E
* The read and write occur at the same time
* The read and write are not contained under a mutual `[[schedule(N)]]/atomic` block

In this case, thread B will read either the previous memory content or the data
written by thread A.  This behavior is hardware dependent.  In general, this
situation should be avoided in Kanagawa code. 

## Function arbitration
If multiple threads can call a non-inline function from multiple call sites in
parallel then the generated hardware will contain an arbiter that
determines the order in which the calls enter the shared function.  By default
the arbiter fairly chooses among call sites to ensure no starvation.  This
section describes language constructs to control arbitration policy.

### [[last]] attribute
The `last` attribute can be applied to `bool` function parameters.
`last` enables a series of threads to call a function from a common
call site with assurance that those threads will enter the function without
interleaving with calls from another call site.

In the following example the `accumulate` function computes the sum of a series
of integers and returns the current sum.  Each call to `accumulate` adds one
more integer to the running total.  `accumulate` uses an static local variable
to hold the current sum. This variable is reset when the `is_last` parameter is
`true`.  If `accumulate` is called from 2 call sites in parallel, then it will
not operate as desired.  The problem is that calls from the 2 call sites will
interleave and thus the running total will not have the expected value.

```
class Foo
{
    uint32 accumulate(uint32 value, bool is_last)
    {
        uint32 result;

        atomic
        {
            static uint32 _sum = 0;

            result = _sum + value;

            _sum = is_last ? 0 : result;
        }

        return result;
    }

    // assume function1 and function2 run in parallel
    void function1()
    {
        pipelined_for(1000, [](uint32 i)
        {
            bool is_last = (i == 999);

            uint32 result = accumulate(i, is_last);

            if (is_last)
            {
                print("result from function1: {result}\n");
            }
        });
    }

    void function2()
    {
        pipelined_for(500, [](uint32 i)
        {
            bool is_last = (i == 499);

            uint32 result = accumulate(i, is_last);

            if (is_last)
            {
                print("result from function2: {result}\n");
            }
        });
    }
}
```

This problem can be fixed by adding the `last` attribute to the
`is_last` parameter:

```
inline uint32 accumulate(uint32 value, [[last]] bool is_last)
{
    uint32 result;

    atomic
    {
        static uint32 _sum = 0;

        result = _sum + value;

        _sum = is_last ? 0 : result;
    }

    return result;
}
```

`last` causes the arbiter to pick one call site and stick with it
(not allowing calls any other call sites through) until a call is made with the
`last` parameter set to true.

The `transaction_size` attribute can be used at the call site to specify the
maximum number of calls before `is_last` is `true` (note that this count
includes the call with `is_last` set to `true`). This attribute will cause
calls to be buffered until an entire transaction is available before issuing
any calls. The specified size is used to properly size the buffering FIFO to
help prevent deadlock. The transaction size can be different for different call
sites. If `transaction_size` is not specified for a call site, then calls will
not be buffered until a full transaction and the default FIFO size will be used.

```
class Foo
{
    uint32 accumulate(uint32 value, [[last]] bool is_last)
    {
        uint32 result;

        atomic
        {
            static uint32 _sum = 0;

            result = _sum + value;

            _sum = is_last ? 0 : result;
        }

        return result;
    }

    // assume function1 and function2 run in parallel
    void function1()
    {
        pipelined_for(1000, [](uint32 i)
        {
            uint32 result = [[transaction_size(1000)]] accumulate(i, i == 999);
        });
    }

    void function2()
    {
        pipelined_for(500, [](uint32 i)
        {
            uint32 result = [[transaction_size(500)]] accumulate(i, i == 499);
        });
    }
}
```

The `--Wtransaction-size` or `--Wall` command line switches cause the compiler
to emit a warning if a call with the `transaction_size` attribute is not
specified at a call to a function with a parameter annotated with the `last`
attribute.

## Memory consistency model
The Kanagawa compiler can reorder reads of shared variables (including memories) past
other reads.  Writes are never reordered past other writes or reads.  To read
multiple variables atomically, ensure all of the read operations are contained
within an `atomic` block.

# Intrinsics

## Debugging

### Simulation log output

Intrinsic function `__print` writes string passsed as argument to the simulation log.
The standard library provides `println` and `print` function templates that are simple
wrappers of `__print` and take one argument which can be an expression of
any value type, including composite types like structs, or arrays.
Multiple values can be printed by using interpolated string sytax.

```
uint32 x = 4;
uint32 y = 5;
print("The value of {x=} and {y=}\n");
```

Strings passed as arguments to `print` or `println` can contain special
characters expressed using following escape sequences:

    newline: \n
    tab: \t
    quote: \"
    backslash: \\

```
print("\"quoted string\"\n");
```

Multiple print calls can be put in an `atomic` block to ensure output is not
interleaved with output from other print calls.

```
uint32 x;
uint32 y;
atomic
{
    print(x);
    if (x > y)
    {
        print(" ");
        print(y);
    }
    print("\n");
}
```

### Assertions
`assert` halts simulation if the specified expression evaluates to false.

```
uint32 x;
uint32 y;

assert(x < y);
```

`static assert` causes compilation to fail if the supplied expression evaluates
to false at compile time.  Compilation also fails if the value of the expression
is not known at compile time.

```
const uint32 x = 4;
const uint32 y = 4;
static assert(x == y);
```

## Logic

### concat
`concat` returns an unsigned integer that is the concatenation of multiple
expressions.  Note that the arguments are specified in big-endian order (the
most significant bit of first argument is placed in the most significant bit of
the result).

```
uint8 x = 0x45;
uint16 y = 0x1267;
uint48 z = concat(y, x); // 0x126745
```

When using literals as arguments to `concat` it is a best practice to
explicitly specify intended [literal width](#integer-literals).

```
uint32 x = concat(32u16, 5433u16);
```

### mux
`mux` uses the first argument (an unsigned integer) to select one of the
remaining input arguments.  The number of arguments required by `mux` is
determined by the width of the first argument.  If the first argument is `N`
bits wide, then there must be `2^N` additional arguments.

The remaining arguments must all have the same type must all be [implicitly
convertible](#implicit-integer-conversions) to a common type.

The arguments are passed from least significant to most significant (if the
index argument is 0, then argument 1 is selected).

This snippet:
```
uint2 idx = 2;
uint32 x = 54;
uint32 y = 98;

uint32 result = mux(idx, x, 3, 4, y);
// result = 4
```

Is equivalent to:
```
uint2 idx = 2;
uint32 x = 54;
uint32 y = 98;
uint32[4] choices = { x, 3, 4, y };

uint32 result = choices[idx];
// result = 4
```

### clog2
`clog2` evaluates to the ceiling of the base 2 logarithm of the supplied
unsigned integer.  The operand must be known at compile time.  `clog2` is
useful for determining how wide to size an integer to hold a given maximum
value.  In general use [index_t](#index_t) or [count_t](#count_t) rather than `clog2`
directly.

```
const auto x = 7;
const auto y = clog2(x); // 3
```

## Time

### cycles
`cycles` evaluates to a `uint64` value that represents the number of clock cycles
that have elapsed since the hardware circuit was last reset.

```
uint64 timestamp = cycles();
print("Event occurred at time: {timestamp}\n");
```

## Scheduling
The following keywords are rarely used.  They are escape hatches to influence
the operation scheduling algorithm.

### barrier
`barrier` causes all statements before the barrier to be executed before all
statements after the barrier (for a specific thread).

The library function template `stages<N>()` is a simple wrapper that injects 
specified number of additional stages into the generated hardware pipeline.

```
inline void F()
{
    // some code

    stages<2>();

    // more code
}
```

### fan_out
`fan_out` returns `N` copies of an input expression.  The copy steps takes a
pipeline stage in the generated circuit, and compiler optimizations will not
eliminate the replicas.

```
uint32 x;
uint32[4] y = fan_out<4>(x);
// all elements of y are equal to x
```
