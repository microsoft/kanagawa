// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.buffer.gearbox
import test.runner
import test.unit as unit

inline bool TestGearboxLargeToSmallLevelTwo()
{
    print("Testing level two larger to smaller\n");
    bool rv = true;
    uint32 input_value = 0x12345678;

    gearbox(input_value, true, true, [](uint8 value, bool last, count_t<bytesizeof(uint8)> num_bytes)
    {
        print("level_two_callback. value: {value} last: {last} num_bytes: {num_bytes}\n");
    });

    return rv;
}

inline bool TestGearboxSmallToLargeLevelTwo()
{
    print("Testing level two smaller to larger\n");
    bool rv = true;
    uint8[4] input_values = {0x78, 0x56, 0x34, 0x12};

    pipelined_for(4, [input_values](uint2 i)
    {
        // Passing false as the last parameter tests the function's ability to
        // detect whether the input buffer is full.
        gearbox(input_values[i], i == 0, false, [](uint32 value, bool last, count_t<bytesizeof(uint32)> num_bytes)
        {
            print("level_two_callback. value: {value} last: {last} num_bytes: {num_bytes}\n");
        });
    });

    return rv;
}

inline bool TestGearboxPartialInputLevelTwo()
{
    print("Testing level two smaller to larger partial input\n");
    bool rv = true;
    uint8[4] input_values = {0x78, 0x56, 0x34, 0x12};

    pipelined_for(3, [input_values](uint2 i)
    {
        gearbox(input_values[i], i == 0, i == 2, [](uint32 value, bool last, count_t<bytesizeof(uint32)> num_bytes)
        {
            print("level_two_callback. value: {value} last: {last} num_bytes: {num_bytes}\n");
        });
    });

    return rv;
}

inline bool TestGearboxSameSizeLevelTwo()
{
    print("Testing level two same size\n");
    bool rv = true;
    uint32 input_value = 0x12345678;

    gearbox(input_value, true, true, [](uint32 value, bool last, count_t<bytesizeof(uint32)> num_bytes)
    {
        print("level_two_callback. value: {value} last: {last} num_bytes: {num_bytes}\n");
    });
    return rv;
}


class TestGearboxExtraLargeToSmallLevelTwo
{
private:
    uint8[64] callback_result_512_to_8;
    uint8 cb_result_ndx = 0;

public:
    void run(unit::tag_t tag)
    {
        print("Testing level two very large to small\n");
        bool rv = true;

        uint512 input_value = 0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF;
        uint8[64] expected = {
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
            0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01
        };

        gearbox(input_value, true, true, [](uint8 value, bool last, count_t<bytesizeof(uint8)> num_bytes)
        {
            print("level_two_callback_512_to_8. value: {value} last: {last} num_bytes: {num_bytes}\n");
            atomic
            {
                callback_result_512_to_8[cb_result_ndx] = value;
                ++cb_result_ndx;
            }
        });

        static for(const auto i : 64)
        {
            if (callback_result_512_to_8[i] != expected[i])
            {
                print("***Expected {expected[i]} got {callback_result_512_to_8[i]}\n");
                rv = false;
            }
        }

        unit::assert_equal(tag, true, rv);
    }
}

class TestMultiThreadLevelTwo
{
private:
    uint512 callback_result_8_to_512;
    uint8[64] inputs_8_to_512 = {
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01,
        0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01
    };

    [[pipelined]] void send_inputs(count_t<bytesizeof(uint512)> tid)
    {
        print("send_inputs. tid: {tid}\n");
        gearbox(inputs_8_to_512[tid], tid == 0, tid == 63, [](uint512 value, bool last, count_t<bytesizeof(uint512)> num_bytes)
        {
            print("level_two_callback_8_to_512. value: {value} last: {last} num_bytes: {num_bytes}\n");
            callback_result_8_to_512 = value;
        });
    }

public:
    void run(unit::tag_t tag)
    {
        print("Testing multi-threaded via pipelined function.\n");

        bool rv = true;
        uint512 expected = 0x0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF;

        send_inputs(64);
        if (callback_result_8_to_512 != expected)
        {
            print("***Expected {expected} got {callback_result_8_to_512}\n");
            rv = false;
        }

        unit::assert_equal(tag, true, rv);
    }
}

inline void test_main()
{
    unit::check<1>(TestGearboxLargeToSmallLevelTwo);
    unit::check<2>(TestGearboxSmallToLargeLevelTwo);
    unit::check<3>(TestGearboxSameSizeLevelTwo);
    unit::check<4>(TestGearboxPartialInputLevelTwo);
    unit::test<5>(unit::fixture<TestGearboxExtraLargeToSmallLevelTwo>());
    unit::test<6>(unit::fixture<TestMultiThreadLevelTwo>());
}
