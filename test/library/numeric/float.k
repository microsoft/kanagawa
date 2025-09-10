// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import numeric.float32 as F32
import numeric.float64 as F64
import test.runner
import test.unit as unit

template <typename T>
inline bool CheckIntToFloatConversion32(T int_val, float32 expected)
{
    float32 float_val = F32::from_int(int_val);
    bool equal = (cast<uint32>(float_val) == cast<uint32>(expected));
    print("{int_val} --> {float_val} correct: {equal}\n");
    if (!equal)
        print("Expected {expected}\n");

    return equal;
}

template <typename T>
inline bool CheckIntToFloatConversionU32(T uint_val, float32 expected)
{
    float32 float_val = F32::from_uint<bitsizeof(T)>(uint_val);
    bool equal = (cast<uint32>(float_val) == cast<uint32>(expected));
    print("{uint_val} --> {float_val} correct: {equal}\n");
    if (!equal)
        print("Expected {expected}\n");

    return equal;
}

template <typename T>
inline bool CheckIntToFloatConversion64(T int_val, uint64 expected)
{
    uint64 float_val = F64::from_int(int_val);
    bool equal = (float_val == expected);
    print("{int_val} --> {float_val} correct: {equal}\n");
    if (!equal)
        print("Expected {expected}\n");

    return equal;
}

template <typename T>
inline bool CheckIntToFloatConversionU64(T uint_val, uint64 expected)
{
    uint64 float_val = F64::from_uint<bitsizeof(T)>(uint_val);
    bool equal = (float_val == expected);
    print("{uint_val} --> {float_val} correct: {equal}\n");
    if (!equal)
        print("Expected {expected}\n");

    return equal;
}


template <typename T, typename F>
struct IntFloat
{
    T int_val;
    F float_val;
}

class IntToFloat32Helper
{
public:
    template <typename I, auto N>
    [[pipelined]] bool CheckIntToFloat32(uint32 tid, IntFloat<I, float>[N] fi)
    {
        return CheckIntToFloatConversion32<I>(fi[tid].int_val, fi[tid].float_val);
    }

    template <typename I, auto N>
    [[pipelined]] bool CheckUintToFloat32(uint32 tid, IntFloat<I, float>[N] fi)
    {
        return CheckIntToFloatConversionU32<I>(fi[tid].int_val, fi[tid].float_val);
    }
}

class IntToFloat64Helper
{
public:
    template <typename I, auto N>
    [[pipelined]] bool CheckIntToFloat64(uint32 tid, IntFloat<I, uint64>[N] fi)
    {
        return CheckIntToFloatConversion64<I>(fi[tid].int_val, fi[tid].float_val);
    }

    template <typename I, auto N>
    [[pipelined]] bool CheckUintToFloat64(uint32 tid, IntFloat<I, uint64>[N] fi)
    {
        return CheckIntToFloatConversionU64<I>(fi[tid].int_val, fi[tid].float_val);
    }
}


template <typename T>
struct IntFloat32
{
    T int_val;
    float32 float_val;
}

inline bool TestIntToFloat32()
{
    static IntToFloat32Helper intToFloat32Helper;

    print("\n");
    print("Testing IntToFloat32\n");

    bool result = true;

    if (result)
    {
        print("testing int<3>\n");
        IntFloat<int3, float32>[8] test_values = {
            {-4, -4.0},
            {-3, -3.0},
            {-2, -2.0},
            {-1, -1.0},
            {0, 0.0},
            {1, 1.0},
            {2, 2.0},
            {3, 3.0}
        };
        bool[8] results = intToFloat32Helper.CheckIntToFloat32<int3, 8>(8, test_values);
        static for(const auto i : 8)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<3>\n");
        IntFloat<uint3, float32>[8] test_values = {
            {0, 0.0},
            {1, 1.0},
            {2, 2.0},
            {3, 3.0},
            {4, 4.0},
            {5, 5.0},
            {6, 6.0},
            {7, 7.0}
        };
        bool[8] results = intToFloat32Helper.CheckUintToFloat32<uint3, 8>(8, test_values);
        static for(const auto i : 8)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<16>\n");
        IntFloat<int16, float32>[5] test_values = {
            {-32768, -32768.0},
            {32767, 32767.0},
            {-1, -1.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[5] results = intToFloat32Helper.CheckIntToFloat32<int16, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<16>\n");
        IntFloat<uint16, float32>[3] test_values = {
            {65535, 65535.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[3] results = intToFloat32Helper.CheckUintToFloat32<uint16, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<32>\n");
        IntFloat<int32, float32>[5] test_values = {
            {-2147483648, -2147483648.0},
            {2147483647, 2147483647.0},
            {-1, -1.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[5] results = intToFloat32Helper.CheckIntToFloat32<int32, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<32>\n");
        IntFloat<uint32, float32>[3] test_values = {
            {4294967295, 4294967295.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[3] results = intToFloat32Helper.CheckUintToFloat32<uint32, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<64>\n");
        IntFloat<int64, float32>[5] test_values = {
            {-9223372036854775808, -9223372036854775808.0},
            {9223372036854775807, 9223372036854775807.0},
            {-1, -1.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[5] results = intToFloat32Helper.CheckIntToFloat32<int64, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<64>\n");
        IntFloat<uint64, float32>[3] test_values = {
            {18446744073709551615, 18446744073709551615.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[3] results = intToFloat32Helper.CheckUintToFloat32<uint64, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<96>\n");
        IntFloat<int96, float32>[5] test_values = {
            {-39614081257132168796771975168, -39614081257132168796771975168.0},
            {39614081257132168796771975167, 39614081257132168796771975167.0},
            {-1, -1.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[5] results = intToFloat32Helper.CheckIntToFloat32<int96, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<96>\n");
        IntFloat<uint96, float32>[3] test_values = {
            {79228162514264337593543950335, 79228162514264337593543950335.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[3] results = intToFloat32Helper.CheckUintToFloat32<uint96, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<112>\n");
        IntFloat<int112, float32>[5] test_values = {
            {-2596148429267413814265248164610048, -2596148429267413814265248164610048.0},
            {2596148429267413814265248164610047, 2596148429267413814265248164610047.0},
            {-1, -1.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[5] results = intToFloat32Helper.CheckIntToFloat32<int112, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<112>\n");
        IntFloat<uint112, float32>[3] test_values = {
            {5192296858534827628530496329220095, 5192296858534827628530496329220095.0},
            {0, 0.0},
            {1, 1.0}
        };
        bool[3] results = intToFloat32Helper.CheckUintToFloat32<uint112, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    return result;
}


template <typename T>
struct IntFloat64
{
    T int_val;
    uint64 float_val;
}

inline bool TestIntToFloat64()
{
    static IntToFloat64Helper intToFloat64Helper;

    print("\n");
    print("Testing IntToFloat64\n");

    bool result = true;

    if (result)
    {
        print("testing int<3>\n");
        IntFloat<int3, uint64>[8] test_values = {
            {-4, 0xc010000000000000},
            {-3, 0xc008000000000000},
            {-2, 0xc000000000000000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000},
            {2, 0x4000000000000000},
            {3, 0x4008000000000000}
        };
        bool[8] results = intToFloat64Helper.CheckIntToFloat64<int3, 8>(8, test_values);
        static for(const auto i : 8)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<3>\n");
        IntFloat<uint3, uint64>[8] test_values = {
            {0, 0x0},
            {1, 0x3ff0000000000000},
            {2, 0x4000000000000000},
            {3, 0x4008000000000000},
            {4, 0x4010000000000000},
            {5, 0x4014000000000000},
            {6, 0x4018000000000000},
            {7, 0x401c000000000000}
        };
        bool[8] results = intToFloat64Helper.CheckUintToFloat64<uint3, 8>(8, test_values);
        static for(const auto i : 8)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<16>\n");
        IntFloat<int16, uint64>[5] test_values = {
            {-32768, 0xc0e0000000000000},
            {32767, 0x40dfffc000000000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[5] results = intToFloat64Helper.CheckIntToFloat64<int16, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<16>\n");
        IntFloat<uint16, uint64>[3] test_values = {
            {65535, 0x40efffe000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint16, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<32>\n");
        IntFloat<int32, uint64>[5] test_values = {
            {-2147483648, 0xc1e0000000000000},
            {2147483647, 0x41dfffffffc00000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[5] results = intToFloat64Helper.CheckIntToFloat64<int32, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<32>\n");
        IntFloat<uint32, uint64>[3] test_values = {
            {4294967295, 0x41efffffffe00000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint32, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<64>\n");
        IntFloat<int64, uint64>[5] test_values = {
            {-9223372036854775808, 0xc3e0000000000000},
            {9223372036854775807, 0x43e0000000000000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[5] results = intToFloat64Helper.CheckIntToFloat64<int64, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<64>\n");
        IntFloat<uint64, uint64>[3] test_values = {
            {18446744073709551615, 0x43f0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint64, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<96>\n");
        IntFloat<int96, uint64>[5] test_values = {
            {-39614081257132168796771975168, 0xc5e0000000000000},
            {39614081257132168796771975167, 0x45e0000000000000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[5] results = intToFloat64Helper.CheckIntToFloat64<int96, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<96>\n");
        IntFloat<uint96, uint64>[3] test_values = {
            {79228162514264337593543950335, 0x45f0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint96, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<112>\n");
        IntFloat<int112, uint64>[5] test_values = {
            {-2596148429267413814265248164610048, 0xc6e0000000000000},
            {2596148429267413814265248164610047, 0x46e0000000000000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[5] results = intToFloat64Helper.CheckIntToFloat64<int112, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<112>\n");
        IntFloat<uint112, uint64>[3] test_values = {
            {5192296858534827628530496329220095, 0x46f0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint112, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<128>\n");
        IntFloat<int128, uint64>[5] test_values = {
            {-170141183460469231731687303715884105728, 0xc7e0000000000000},
            {170141183460469231731687303715884105727, 0x47e0000000000000},
            {-1, 0xbff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[5] results = intToFloat64Helper.CheckIntToFloat64<int128, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<128>\n");
        IntFloat<uint128, uint64>[3] test_values = {
            {340282366920938463463374607431768211455, 0x47f0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint128, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<192> (positive values only)\n");
        IntFloat<int192, uint64>[3] test_values = {
            {3138550867693340381917894711603833208051177722232017256447, 0x4be0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckIntToFloat64<int192, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<192>\n");
        IntFloat<uint192, uint64>[3] test_values = {
            {6277101735386680763835789423207666416102355444464034512895, 0x4bf0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint192, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<256> (positive values only)\n");
        IntFloat<int256, uint64>[3] test_values = {
            {57896044618658097711785492504343953926634992332820282019728792003956564819967, 0x4fe0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckIntToFloat64<int256, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<256>\n");
        IntFloat<uint256, uint64>[3] test_values = {
            {115792089237316195423570985008687907853269984665640564039457584007913129639935, 0x4ff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint256, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        print("testing int<512> (positive values only)\n");
        IntFloat<int512, uint64>[3] test_values = {
            {6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042047, 0x5fe0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckIntToFloat64<int512, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }
    if (result)
    {
        print("testing uint<512>\n");
        IntFloat<uint512, uint64>[3] test_values = {
            {13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084095, 0x5ff0000000000000},
            {0, 0x0},
            {1, 0x3ff0000000000000}
        };
        bool[3] results = intToFloat64Helper.CheckUintToFloat64<uint512, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    return result;
}



template <typename From, typename To>
inline bool CheckFloatToIntConversionHelper(From from, To expected, bool valid_expected, optional<To> int_val)
{
    bool ret_val = true;

    if (int_val.is_valid != valid_expected)
    {
        if (valid_expected)
            print("Expected valid result, got invalid\n");
        else
            print("Expected invalid result, got valid\n");

        ret_val = false;
    }
    else if (int_val.is_valid && int_val.value != expected)
    {
        print("Expected {expected}\n");

        ret_val = false;
    }

    print("{from} --> {int_val.value} valid: {int_val.is_valid} correct: {ret_val}\n");

    return ret_val;
}

template <typename T>
inline bool CheckFloatToIntConversion32(float32 from, T expected, bool valid_expected)
{
    optional<T> int_val = F32::to_int<bitsizeof(T)>(from);

    return CheckFloatToIntConversionHelper<float32, T>(from, expected, valid_expected, int_val);
}

template <typename T>
inline bool CheckFloatToUintConversion32(float32 from, T expected, bool valid_expected)
{
    optional<T> int_val = F32::to_uint<bitsizeof(T)>(from);

    return CheckFloatToIntConversionHelper<float32, T>(from, expected, valid_expected, int_val);
}

template <typename T>
inline bool CheckFloatToIntConversion64(uint64 from, T expected, bool valid_expected)
{
    optional<T> int_val = F64::to_int<bitsizeof(T)>(from);

    return CheckFloatToIntConversionHelper<uint64, T>(from, expected, valid_expected, int_val);
}

template <typename T>
inline bool CheckFloatToUintConversion64(uint64 from, T expected, bool valid_expected)
{
    optional<T> int_val = F64::to_uint<bitsizeof(T)>(from);

    return CheckFloatToIntConversionHelper<uint64, T>(from, expected, valid_expected, int_val);
}


template <typename T, typename I>
struct FloatInt
{
    T float_val;
    I int_val;
    bool valid;
}

class Float32ToIntHelper
{
public:
    template <typename I, auto N>
    [[pipelined]] bool CheckFloat32ToInt(uint32 tid, FloatInt<float, I>[N] fi)
    {
        return CheckFloatToIntConversion32<I>(fi[tid].float_val, fi[tid].int_val, fi[tid].valid);
    }

    template <typename I, auto N>
    [[pipelined]] bool CheckFloat32ToUint(uint32 tid, FloatInt<float, I>[N] fi)
    {
        return CheckFloatToUintConversion32<I>(fi[tid].float_val, fi[tid].int_val, fi[tid].valid);
    }
}

class Float64ToIntHelper
{
public:
    template <typename I, auto N>
    [[pipelined]] bool CheckFloat64ToInt(uint32 tid, FloatInt<uint64, I>[N] fi)
    {
        return CheckFloatToIntConversion64<I>(fi[tid].float_val, fi[tid].int_val, fi[tid].valid);
    }

    template <typename I, auto N>
    [[pipelined]] bool CheckFloat64ToUint(uint32 tid, FloatInt<uint64, I>[N] fi)
    {
        return CheckFloatToUintConversion64<I>(fi[tid].float_val, fi[tid].int_val, fi[tid].valid);
    }
}


inline bool TestFloat32ToInt()
{
    static Float32ToIntHelper float32ToIntHelper;
    print("\n");
    print("Testing float32 to int\n");

    struct FloatStruct
    {
        uint32 u;
    }

    const FloatStruct inf32 = {0x7F800000};
    const float32 infinity = cast<float32>(inf32);
    const FloatStruct nan32 = {0x7F800001};
    const float32 nan = cast<float32>(nan32);

    bool result = true;

    if (result)
    {
        FloatInt<float32, int8>[8] test_values = {
            {0.0, 0, true},
            {-0.86323547363, 0, true},
            {42.0, 42, true},
            {42.6, 42, true},
            {-128.0, -128, true},
            {-128.6, -128, true},
            {403.0, 0, false},
            {-403.0, 0, false}
        };

        bool[8] results = float32ToIntHelper.CheckFloat32ToInt<int8, 8>(8, test_values);
        static for(const auto i : 8)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<float32, int16>[5] test_values = {
            {0.0, 0, true},
            {32767.0, 32767, true},
            {-32768.0, -32768, true},
            {32767.7, 32767, true},
            {-32768.4, -32768, true}
        };

        bool[5] results = float32ToIntHelper.CheckFloat32ToInt<int16, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<float32, int32>[12] test_values = {
            {0.0, 0, true},
            {-42.0, -42, true},
            {2147483647.0, 0, false},
            {2147483648.0, 0, false},
            {2147483649.0, 0, false},
            {2147483520.0, 2147483520, true},
            {2147483524.0, 2147483520, true},
            {2147483518.0, 2147483520, true},
            {4147483647.0, 0, false},
            {-1.0, -1, true},
            {infinity, 0, false},
            {nan, 0, false}
        };

        bool[12] results = float32ToIntHelper.CheckFloat32ToInt<int32, 12>(12, test_values);
        static for(const auto i : 12)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<float32, int64>[5] test_values = {
            {0.0, 0, true},
            {2147483647.0, 2147483648, true},
            {2147483648.0, 2147483648, true},
            {2147483649.0, 2147483648, true},
            {4147483647.0, 4147483648, true}
        };

        bool[5] results = float32ToIntHelper.CheckFloat32ToInt<int64, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<float32, uint8>[3] test_values = {
            {0.0, 0, true},
            {256.0, 0, false},
            {255.6, 255, true}
        };

        bool[3] results = float32ToIntHelper.CheckFloat32ToUint<uint8, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<float32, uint16>[3] test_values = {
            {0.0, 0, true},
            {65535.4, 65535, true},
            {65536.4, 0, false}
        };

        bool[3] results = float32ToIntHelper.CheckFloat32ToUint<uint16, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    return result;
}

inline bool TestFloat64ToInt()
{
    static Float64ToIntHelper float64ToIntHelper;

    print("\n");
    print("Testing float64 to int\n");

    const uint64 infinity = 0x7FF0000000000000;
    const uint64 nan = 0x7FF0000000000001;

    bool result = true;

    if (result)
    {
        FloatInt<uint64, int8>[3] test_values = {
            {0x0, 0, true},
            {0x4045000000000000, 42, true},     // 42.0
            {0x40454ccccccccccd, 42, true}      // 42.6
        };

        bool[3] results = float64ToIntHelper.CheckFloat64ToInt<int8, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<uint64, int16>[3] test_values = {
            {0x0, 0, true},
            {0x40dfffc000000000, 32767, true},  // 32767.0
            {0xc0e0000000000000, -32768, true}  // -32768.0
        };

        bool[3] results = float64ToIntHelper.CheckFloat64ToInt<int16, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<uint64, int32>[5] test_values = {
            {0x0, 0, true},
            {0xc045400000000000, -42, true},        // -42.5
            {0x41dfffffffc00000, 2147483647, true}, // 2147483647.0
            {infinity, 0, false},
            {nan, 0, false}
        };

        bool[5] results = float64ToIntHelper.CheckFloat64ToInt<int32, 5>(5, test_values);
        static for(const auto i : 5)
        {
            if (results[i] == false)
                result = false;
        }
    }


    if (result)
    {
        FloatInt<uint64, uint8>[2] test_values = {
            {0x0, 0, true},
            {0x4079300000000000, 0, false}    // 403, too big for 8 bits
        };

        bool[2] results = float64ToIntHelper.CheckFloat64ToUint<uint8, 2>(2, test_values);
        static for(const auto i : 2)
        {
            if (results[i] == false)
                result = false;
        }
    }

    if (result)
    {
        FloatInt<uint64, uint64>[3] test_values = {
            {0x0, 0, true},
            {0x41effffffff80000, 4294967295, true},     // 4294967295.75
            {0x41efffffffd80000, 4294967294, true}      // 4294967294.75
        };

        bool[3] results = float64ToIntHelper.CheckFloat64ToUint<uint64, 3>(3, test_values);
        static for(const auto i : 3)
        {
            if (results[i] == false)
                result = false;
        }
    }

    return result;
}

inline bool TestFloat32ToFloat64()
{
    print("\n");
    print("Testing Float32ToFloat64\n");

    bool result = true;

    template<typename From, typename To>
    struct FromTo
    {
        From from;
        To to;
    }

    FromTo<float32, F64::float64>[11] test_values = {
        {0.0, 0x0},
        {403.0, 0x4079300000000000},
        {42.0, 0x4045000000000000},
        {-42.0, 0xc045000000000000},
        {42.6, 0x40454cccc0000000},
        {-42.5, 0xc045400000000000},
        {2147483647.0, 0x41e0000000000000},
        {32767.0, 0x40dfffc000000000},
        {4294967295.75, 0x41f0000000000000},
        {1.0, 0x3ff0000000000000},
        {2.80259692864963414184745916658E-45, 0x36B0000000000000}   // denormalized float; print will show this as 0
    };
    static for (const auto i : bitsizeof(test_values)/ bitsizeof(test_values[0]))
    {
        F64::float64 converted_value = F64::from_float32(test_values[i].from);
        bool equal = converted_value == test_values[i].to;
        print("{test_values[i].from} --> {converted_value} correct: {equal}\n");
        if (!equal)
        {
            print("Expected {test_values[i].to}\n");
            result = false;
        }
    }

    return result;
}

inline void test_main()
{
    unit::check<1>(TestIntToFloat32);
    unit::check<2>(TestIntToFloat64);
    unit::check<3>(TestFloat32ToInt);
    unit::check<4>(TestFloat64ToInt);
    unit::check<5>(TestFloat32ToFloat64);
}
