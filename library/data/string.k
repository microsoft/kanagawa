// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.string
    { eol
    , empty
    , show
    , append
    , replicate
    , concatenate
    , fold_map
    , unlines
    , unwords
    , punctuate
    , comma_sep
    , parens
    , braces
    , brackets
    , angles
    , quotes
    , squotes
    , backquotes
    }

import data.array as A

//| End-of-line string
const auto eol = "\n";

//| Empty string
const auto empty = "";

//| String representation of the argument
//
// #### __Example__
//
//      >>> show(bitsizeof uint32);
//      "32"
//
inline string show(auto x)
{
    return "{x}";
}

//| Append two strings
//
// #### __Example__
//
//      >>> append("foo", "bar")
//      "foobar"
//
inline string append(string x, string y)
{
    return "{x}{y}";
}

//| Return a string that contains `x` replicated `N` times
template <auto N>
inline string replicate(string x)
{
    string result;

    static for (const auto i : N)
    {
        result = append(result, x);
    }

    return result;
}

//| Concatenate an array of strings
//
// #### __Example__
//
//      >>> concatenate({"f", "o", "o", eol});
//      "foo\n"
//
template <auto N>
inline string concatenate(string[N] x)
{
    return A::foldr(append, x);
}

//| Map elements of an array to strings and concatenate results
//
// #### __Example__
//
//      >>> fold_map(show, {0, 1, 2, 3, 4});
//      "01234"
//
template <typename T, auto N>
inline string fold_map((T) -> string fn, T[N] x)
{
    return concatenate(A::map(fn, x));
}

//| Append end-of-line to each input string and concatenate
//
// #### __Example__
//
//      >>> unlines({0, 1, 2, 3, 4});
//      "0\n1\n2\n3\n4\n"
//
template <auto N>
inline string unlines(string[N] x)
{
    return concatenate(A::zip_with(append, x, A::repeat(eol)));
}

//| Join string representations of input values with a separator
//
// #### __Example__
//
//      >>> punctuate(", ", {0, 1, 2, 3, 4});
//      "0, 1, 2, 3, 4"
//
template <typename T, auto N>
inline string punctuate(string sep, T[N] x)
{
    return concatenate(A::intersperse(sep, A::map(show, x)));
}

//| Join string representations of input values with separating spaces
//
// #### __Example__
//
//      >>> unwords({0, 1, 2, 3, 4});
//      "0 1 2 3 4"
//
template <typename T, auto N>
inline string unwords(T[N] x)
{
    return punctuate(" ", x);
}

//| Comma separate values
//
// #### __Example__
//
//      >>> comma_sep({0, 1, 2, 3, 4});
//      "0, 1, 2, 3, 4"
//
template <typename T, auto N>
inline string comma_sep(T[N] x)
{
    return punctuate(", ", x);
}

//| Wrap a value in parentheses
inline string parens(auto x)
{
    return "({x})";
}

//| Wrap a value in braces
inline string braces(auto x)
{
    return "{{{x}}";
}

//| Wrap a value in square brackets
inline string brackets(auto x)
{
    return "[{x}]";
}

//| Wrap a value in angle brackets
inline string angles(auto x)
{
    return "<{x}>";
}

//| Wrap a value in quotes
inline string quotes(auto x)
{
    return "\"{x}\"";
}

//| Wrap a value in single quotes
inline string squotes(auto x)
{
    return "'{x}'";
}

//| Wrap a value in backquotes
inline string backquotes(auto x)
{
    return "`{x}`";
}
