// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import data.order
import test.runner
import test.unit as unit

// Unsigned min
inline bool TestMinUintAltB()
{
    bool result = true;

    if (min(254, 255) != 254)
    {
        print("Error: unsigned min(254,255) did not return expected value (254)\n");
        result = false;
    }

    return result;
}

inline bool TestMinUintAgtB()
{
    bool result = true;

    if (min(255, 254) != 254)
    {
        print("Error: unsigned min(255,254) did not return expected value (254)\n");
        result = false;
    }

    return result;
}

inline bool TestMinUintAeqB()
{
    bool result = true;

    if (min(254, 254) != 254)
    {
        print("Error: unsigned min(254,254) did not return expected value (254)\n");
        result = false;
    }

    return result;
}

// Unsigned max
inline bool TestMaxUintAltB()
{
    bool result = true;

    if (max(254, 255) != 255)
    {
        print("Error: unsigned max(254,255) did not return expected value (255)\n");
        result = false;
    }

    return result;
}

inline bool TestMaxUintAgtB()
{
    bool result = true;

    if (max(255, 254) != 255)
    {
        print("Error: unsigned max(255,254) did not return expected value (255)\n");
        result = false;
    }

    return result;
}

inline bool TestMaxUintAeqB()
{
    bool result = true;

    if (max(254, 254) != 254)
    {
        print("Error: unsigned max(254,254) did not return expected value (254)\n");
        result = false;
    }

    return result;
}

// Signed min
inline bool TestMinIntAltB()
{
    bool result = true;

    if (min(-128, 127) != -128)
    {
        print("Error: signed min(-128, 127) did not return expected value (-128)\n");
        result = false;
    }

    return result;
}

inline bool TestMinIntAgtB()
{
    bool result = true;

    if (min(127, -128) != -128)
    {
        print("Error: signed min(127, -128) did not return expected value (-128)\n");
        result = false;
    }

    return result;
}

inline bool TestMinIntAeqB()
{
    bool result = true;

    if (min(-128, -128) != -128)
    {
        print("Error: signed min(-128, -128) did not return expected value (-128)\n");
        result = false;
    }

    return result;
}

// Signed max
inline bool TestMaxIntAltB()
{
    bool result = true;

    if (max(-128, 127) != 127)
    {
        print("Error: signed max(-128, 127) did not return expected value (127)\n");
        result = false;
    }

    return result;
}

inline bool TestMaxIntAgtB()
{
    bool result = true;

    if (max(127, -128) != 127)
    {
        print("Error: signed max(127, -128) did not return expected value (127)\n");
        result = false;
    }

    return result;
}

inline bool TestMaxIntAeqB()
{
    bool result = true;

    if (max(127, 127) != 127)
    {
        print("Error: signed max(127, 127) did not return expected value (127)\n");
        result = false;
    }

    return result;
}

inline void test_main()
{
    unit::check<1>(TestMinUintAltB);
    unit::check<2>(TestMinUintAgtB);
    unit::check<3>(TestMinUintAeqB);
    unit::check<4>(TestMaxUintAltB);
    unit::check<5>(TestMaxUintAgtB);
    unit::check<6>(TestMaxUintAeqB);
    unit::check<7>(TestMinIntAltB);
    unit::check<8>(TestMinIntAgtB);
    unit::check<9>(TestMinIntAeqB);
    unit::check<10>(TestMaxIntAltB);
    unit::check<11>(TestMaxIntAgtB);
    unit::check<12>(TestMaxIntAeqB);
}

