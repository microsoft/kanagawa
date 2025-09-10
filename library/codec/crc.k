// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module codec.crc
    { crc_calculator
    }

import data.array
import data.bits

template<auto Width>
inline uint<Width> crc_reduce(uint<Width> a, uint<Width> b)
{
    return a ^ b;
}

//| Cyclic Redundancy Check (CRC) computation
//
// Example CRC-32 with 64-bytes/cycle input width and 4 simultaneous input streams:
//
//     crc_calculator<32, 0xEDB88320, 0xFFFFFFFF, 0xFFFFFFFF, true, true, 64, 4> crc_calc;
template
    < auto Width            //< Width (in bits) of the computed CRC.
    , uint<Width> P         //< The reversed polynomial.
    , uint<Width> Init      //< The initial value of CRC.
    , uint<Width> XorOut    //< The value to XOR with the final result.
    , bool ReflectInput     //< Whether to reverse the bits of each input byte.
    , bool ReflectOutput    //< Whether to reverse the bits of the CRC output result.
    , auto MaxBytes         //< The maximum number of bytes that can be processed per call.
    , auto ThreadCount      //< Number of CRC values that can be computed concurrently.
    >
class crc_calculator
{
private:
    const auto max_bits = 8 * MaxBytes;

public:
    //| Process up to `MaxBytes` of new input data.
    inline uint<Width> compute
        ( count_t<ThreadCount> identifier   //< Which of the `ThreadCount` CRCs to update.
        , uint8[MaxBytes] input             //< Input data.
        , count_t<MaxBytes> input_bytes     //< Number of valid bytes in the input data.
        , bool reset                        //< Indicates if CRC value should be reset before processing `input`.  Should be set to `true` on the first call associated with a particular input stream.
        )
    {
        const auto table = generate();

        // Because the CRC calculator operates on a reversed polynomial
        // input bits are reversed in order to produce the same output
        // as a normal (not reversed) polynomial.
        // If ReflectInput is true, then input bits are not reversed
        // which produces the same output as a CRC operating on a normal polynomial
        // and reversed inputs.
        if(!ReflectInput)
        {
            static for(const auto i : MaxBytes)
            {
                input[i] = reverse<uint8>(input[i]);
            }
        }

        // The initial value is given in big-endian.
        const auto initial = reverse<uint<Width>>(Init);
        const auto input_bits = input_bytes * 8;
        const auto off_bits = max_bits - input_bits;

        // crc(r1, d) = crc(r1, 0) + crc(0, d). crc(0, d) depends on the width of input
        const auto off_input = cast<uint<max_bits>>(cast<uint<max_bits>>(input) << off_bits);
        const uint<Width> reduced_crc = crc_lookup(table, off_input);

        uint<Width> crc_local;
        atomic
        {
            // crc(r1, 0) depends on the width of remainer
            uint<Width> crc_keep = reset ? initial : _crc_results[identifier];
            uint<Width> updated_crc = crc_map(table, crc_keep, checked_cast<count_t<data_segments>>(off_bits / 4));

            // combine crc(0,d) and crc(r,0)
            crc_local = updated_crc ^ reduced_crc ^ (crc_keep >> input_bits);
            _crc_results[identifier] = crc_local;
        }

        // Similar to the handling of ReflectInput, only reverse output bits
        // when ReflectOutput is false (because the CRC operates on a reversed polynomial)
        return (ReflectOutput ? crc_local : reverse<uint<Width>>(crc_local)) ^ XorOut;
    }

private:
    const auto data_segments = max_bits / 4;
    const auto crc_segments = Width / 4;
    uint<Width>[ThreadCount] _crc_results;

    inline uint<Width>[data_segments][16] generate()
    {
        uint<Width>[data_segments][16] table;
        uint<Width>[16] remainder;

        static for(const auto i : 16)
        {
            remainder[i] = i;
        }

        static for(const auto j : data_segments)
        {
            static for(const auto i : 16)
            {
                static for(const auto bit : 4)
                {
                    uint1 accumulated = cast<uint1>(remainder[i]);
                    remainder[i] = (remainder[i] >> 1) ^ (accumulated ? P : 0);
                }
                table[(data_segments - 1) - j][i] = remainder[i];
            }
        }

        return table;
    }

    inline uint<Width> crc_lookup(uint<Width>[data_segments][16] table, uint<max_bits> input)
    {
        const auto lut_addresses = cast<uint4[data_segments]>(input);

        uint<Width>[data_segments] tab;
        static for(const auto i : data_segments)
        {
            tab[i] = table[i][lut_addresses[i]];
        }

        uint<Width>[data_segments] reduce_stage;
        reduce_stage = cast<uint<Width>[data_segments]>(tab);
        return reduce(crc_reduce<Width>, reduce_stage);
    }

    inline uint<Width> crc_map(uint<Width>[data_segments][16] table, uint<Width> input, count_t<data_segments> table_offset)
    {
        const auto lut_addresses = cast<uint4[crc_segments]>(input);

        uint<Width>[crc_segments] tab;
        static for(const auto i: crc_segments)
        {
            if(i + table_offset < data_segments)
            {
                tab[i] = table[i + table_offset][lut_addresses[i]];
            }
        }

        uint<Width>[crc_segments] reduce_stage;
        reduce_stage = cast<uint<Width>[crc_segments]>(tab);
        return reduce(crc_reduce<Width>, reduce_stage);
    }
}