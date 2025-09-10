// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.random.lfsr
    { lfsr
    , lfsr_random
    }

import data.optional

//| A Linear Feedback Shift Register, which is a simple type of
// pseudo-random number generator. `lfsr_random` uses this function to generate successive
// pseudo-random numbers: pass any non-zero seed value into the initial
// call to the `lfsr` method, and then pass the returned value back in
// as the new seed on successive calls.
//
// The polynomial used for this LFSR will visit every value
// except zero. Never use zero as a seed, the LFSR will
// always return zero for a zero input. If you are looking
// for a traditional random number generator, this may not be a good
// choice because the LFSR will not revisit a value until
// it has visited all other $2^N-2$ values.
template <auto Width /*< The bit-width of the LFSR. Widths from 2 to 32 inclusive are supported.*/>
inline uint<Width> lfsr(uint<Width> seed)
{
    static assert(Width >= 2 && Width <= 32);
    assert(seed != 0);

    uint1[Width] bits = cast<uint1[Width]>(seed);
    uint<Width-1> right = cast<uint<Width-1>>(seed);

    uint<Width> result;

    if (Width ==  2)
    {
        result = concat(right, bits[ 1] ^ bits[ 0]);
    }
    else if (Width ==  3)
    {
        result = concat(right, bits[ 2] ^ bits[ 1]);
    }
    else if (Width ==  4)
    {
        result = concat(right, bits[ 3] ^ bits[ 2]);
    }
    else if (Width ==  5)
    {
        result = concat(right, bits[ 4] ^ bits[ 2]);
    }
    else if (Width ==  6)
    {
        result = concat(right, bits[ 5] ^ bits[ 4]);
    }
    else if (Width ==  7)
    {
        result = concat(right, bits[ 6] ^ bits[ 5]);
    }
    else if (Width ==  8)
    {
        result = concat(right, bits[ 7] ^ bits[ 5] ^ bits[ 4] ^ bits[ 3]);
    }
    else if (Width ==  9)
    {
        result = concat(right, bits[ 8] ^ bits[ 4]);
    }
    else if (Width == 10)
    {
        result = concat(right, bits[ 9] ^ bits[ 6]);
    }
    else if (Width == 11)
    {
        result = concat(right, bits[10] ^ bits[ 8]);
    }
    else if (Width == 12)
    {
        result = concat(right, bits[11] ^ bits[ 5] ^ bits[ 3] ^ bits[ 0]);
    }
    else if (Width == 13)
    {
        result = concat(right, bits[12] ^ bits[ 3] ^ bits[ 2] ^ bits[ 0]);
    }
    else if (Width == 14)
    {
        result = concat(right, bits[13] ^ bits[ 4] ^ bits[ 2] ^ bits[ 0]);
    }
    else if (Width == 15)
    {
        result = concat(right, bits[14] ^ bits[13]);
    }
    else if (Width == 16)
    {
        result = concat(right, bits[15] ^ bits[14] ^ bits[12] ^ bits[ 3]);
    }
    else if (Width == 17)
    {
        result = concat(right, bits[16] ^ bits[13]);
    }
    else if (Width == 18)
    {
        result = concat(right, bits[17] ^ bits[10]);
    }
    else if (Width == 19)
    {
        result = concat(right, bits[18] ^ bits[ 5] ^ bits[ 1] ^ bits[ 0]);
    }
    else if (Width == 20)
    {
        result = concat(right, bits[19] ^ bits[16]);
    }
    else if (Width == 21)
    {
        result = concat(right, bits[20] ^ bits[18]);
    }
    else if (Width == 22)
    {
        result = concat(right, bits[21] ^ bits[20]);
    }
    else if (Width == 23)
    {
        result = concat(right, bits[22] ^ bits[17]);
    }
    else if (Width == 24)
    {
        result = concat(right, bits[23] ^ bits[22] ^ bits[21] ^ bits[16]);
    }
    else if (Width == 25)
    {
        result = concat(right, bits[24] ^ bits[21]);
    }
    else if (Width == 26)
    {
        result = concat(right, bits[25] ^ bits[ 5] ^ bits[ 1] ^ bits[ 0]);
    }
    else if (Width == 27)
    {
        result = concat(right, bits[26] ^ bits[ 4] ^ bits[ 1] ^ bits[ 0]);
    }
    else if (Width == 28)
    {
        result = concat(right, bits[27] ^ bits[24]);
    }
    else if (Width == 29)
    {
        result = concat(right, bits[28] ^ bits[26]);
    }
    else if (Width == 30)
    {
        result = concat(right, bits[29] ^ bits[ 5] ^ bits[ 3] ^ bits[ 0]);
    }
    else if (Width == 31)
    {
        result = concat(right, bits[30] ^ bits[27]);
    }
    else if (Width == 32)
    {
        result = concat(right, bits[31] ^ bits[21] ^ bits[ 1] ^ bits[ 0]);
    }
    else
    {
        assert(false);
    }

    assert(result != 0);

    return result;
}


//| A stateful pseudo-random number generator using a linear
// feedback shift register (LFSR).
//
// The polynomial used for this LFSR will visit every value
// except zero. Never use zero as a seed, the LFSR will
// always return zero for a zero input. If you are looking
// for a traditional random number generator, this may not be a good
// choice because the LFSR will not revisit a value until
// it has visited all other $2^N-2$ values.
template <auto SeedWidth /*< The bit-width of the LFSR. Widths from 2 to 32 inclusive are supported.*/>
class lfsr_random
{
public:
    using value_t = uint<SeedWidth>;

private:
    value_t _value = 1;

public:
    //| Get the next random number in the sequence, optionally seeding
    // the random number generator first. Do not use zero for the
    // seed value.
    value_t next(optional<uint<SeedWidth>> seed /*< If `is_valid` then seed the generator, otherwise ignore.*/)
    {
        assert(!(seed.is_valid && (seed.value == 0)));

        value_t new_value;

        atomic
        {
            new_value = lfsr(from_optional(_value, seed));
            _value = new_value;
        }

        return new_value;
    }
}