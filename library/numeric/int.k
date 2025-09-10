// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//| Copyright: (c) Microsoft Corporation. All rights reserved.
//
// Integer functions.
module numeric.int
    { even
    , odd
    }

//| Return true if the input is even.
//
// #### __Examples__
//
//     >>> even(0)
//     true
//
//     >>> even(1)
//     false
//
//     >>> even(2)
//     true
inline bool even(auto a)
{
    return a % 2 == 0;
}

//| Return true if the input is odd.
//
// #### __Examples__
//
//     >>> odd(0)
//     false
//
//     >>> odd(1)
//     true
//
//     >>> odd(2)
//     false
inline bool odd(auto a)
{
    return !even(a);
}