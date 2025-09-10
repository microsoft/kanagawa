// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.int.limits
    { limits
    }

//| Numeric limit constants for integer types
template<typename T>
class limits
{
public:
    //| Indicates whether the type is signed
    const bool is_signed = cast<T>(-1) < 0;

    //| Smallest value of the type
    const T min = is_signed ? cast<T>(1 << (bitsizeof T - 1)) : 0;

    //| Largest value of the type
    const T max = cast<T>(min - 1);
}