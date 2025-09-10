// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module debug.print
    { module debug.print
    }

//| Format the argument as a string and write to simulation log.
// Use interpolated string syntax to output multiple values.
//
// #### __Example__
//
//      >>> const uint32 foo = 10; const bool bar = true; print("{foo=}\n{bar=}");
//      foo = 10
//      bar = true
inline void print(auto x)
{
    static if (decltype(x) == string)
    {
        // Optimize printing of strings
        __print(x);
    }
    else
    {
        __print("{x}");
    }
}

//| Format the argument as a string and write as a line to simulation log.
// Multiple values can be printed using interpolated string syntax, however
// it is more efficient to use the `print` function and explicitly append
// end of line `\n` to the string.
//
// #### __Example__
//
//      >>> const uint32 foo = 10; println(foo);
//      10
//
inline void println(auto x)
{
    __print("{x}\n");
}
