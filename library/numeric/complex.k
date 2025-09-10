// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module numeric.complex
    { complex
    }

//| Complex number
template<typename T> 
struct complex
{
	T real_part;
	T imag_part;
}