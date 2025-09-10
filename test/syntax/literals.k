// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    static assert(0b_0000_0000 == 0x00);

expected:0

    static assert(0b_0000_0000 == 0x_0_0);

expected:0

    static assert(0b_0000_0111 == 0x07);

expected:0

    static assert(0b_0000_0111 == 0x_07);

expected:0

    static assert(0b_0000_0001 == 0x01);

expected:0

    static assert(0b_1000_0001 == 0x81);

expected:0

    static assert(0b_1111_1111 == 0xff);

expected:0

    static assert(0b_0111_0111 == 0x77);

expected:0

    static assert(0b_1111_0011 == 0xf3);

expected:0

    static assert(0b_1111_0001 == 0xf1);

expected:0

    static assert(0b_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1000 == 0xf00000000000008);

expected:0

    static assert(0b111100000000000000000000000000000000000000000000000000001000 == 0xf00000000000008);

expected:0

    const auto b = 0b_0101_0101;

expected:0

    const auto b = 0b0;

expected:0

    const auto b = 0b_0;

expected:0

    const auto b = 0b0_;

expected:0

    const auto b = 0b;

expected:1

    const auto b = 0b_;

expected:1

    static assert(0x_1234_abcd == 0x1234abcd);

expected:0

    static assert(0x_1234_abcd == 305_441_741);

expected:0

    static assert(0x_1234_abcd == 0o_2_215_125_715);

expected:0
    
    const auto x = 100_000;

expected:0
    
    const auto x = _100_000;

expected:1
    
    const auto x = 100_000_;

expected:0
    
    const auto x = 0o_100_000;

expected:0
    
    const auto x = 0o_100_000_;

expected:0
    
    const auto x = 0x_100_000;

expected:0
    
    const auto x = 0x_100_000_;

expected:0

    const uint32 x = 10i0;

expected:1

    const uint32 x = 10u0;

expected:1

    const auto x = -10u32;

expected:0

    const auto x = -10i32;
    static assert(bitsizeof x == 32);

expected:0

    static assert(-8i32 == -8);

expected:0

    static assert(-8u32 == -8);

expected:0

    static assert(-10i32 == -10_i32);
    static assert(bitsizeof(-10_i32) == 32);

expected:0
    
    enum E : uint8
    {
        X = 10u4,
        Y
    }

    static assert(E::Y == 11);

expected:0

    const auto x = 0x100_u8;
    const auto y = 0x100u8;

expected:1

    const auto x = 0xFF_u8;
    const auto y = 0xFFu8;

expected:0

    const auto x = 0x80_i8;
    const auto y = 0x80i8;

expected:1

    const auto x = 0x7F_i8;
    const auto y = 0x7Fi8;

expected:0
