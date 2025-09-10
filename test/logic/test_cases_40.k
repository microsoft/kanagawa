// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

inline void TestCase694(unit::tag_t tag)
{
    int32 expected = -20;
    int32 actual = cast<int32>(0 - 20);

    print("{actual} {expected}\n");

    unit::assert(tag, actual == expected);
}

// Deferred type resolution within parameterized int and array types
struct Test695Struct
{
    uint1 x;
}

inline void TestCase695(unit::tag_t tag)
{
    uint<bitsizeof(Test695Struct)> x;
    uint32[bitsizeof(Test695Struct)] y;

    unit::assert(tag, bitsizeof(x) == bitsizeof(Test695Struct));
    unit::assert(tag, bitsizeof(y) == bitsizeof(Test695Struct) * bitsizeof(uint32));
}

template <uint32 N>
class GetN
{
public:
    uint32 getN()
    {
        return N;
    }
}

class Test696
{
private:
    GetN<clog2(1)> test696_1;
    GetN<clog2(2)> test696_2;
    GetN<clog2(3)> test696_3;
    GetN<clog2(4)> test696_4;
    GetN<clog2(5)> test696_5;
    GetN<clog2(6)> test696_6;
    GetN<clog2(7)> test696_7;
    GetN<clog2(8)> test696_8;
    GetN<clog2(9)> test696_9;
    GetN<clog2(1 << 70)> test696_a;
    GetN<clog2((1 << 70) - 1)> test696_b;
    GetN<clog2((1 << 70) + 1)> test696_c;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, test696_1.getN() == clog2(1));
        unit::assert(tag, test696_2.getN() == clog2(2));
        unit::assert(tag, test696_3.getN() == clog2(3));
        unit::assert(tag, test696_4.getN() == clog2(4));
        unit::assert(tag, test696_5.getN() == clog2(5));
        unit::assert(tag, test696_6.getN() == clog2(6));
        unit::assert(tag, test696_7.getN() == clog2(7));
        unit::assert(tag, test696_8.getN() == clog2(8));
        unit::assert(tag, test696_9.getN() == clog2(9));
        unit::assert(tag, test696_a.getN() == 70);
        unit::assert(tag, test696_b.getN() == 70);
        unit::assert(tag, test696_c.getN() == 71);
    }
}

class Test698
{
private:
    GetN<bitsizeof(0)> test698_0;
    GetN<bitsizeof(1)> test698_1;
    GetN<bitsizeof(2)> test698_2;
    GetN<bitsizeof(3)> test698_3;
    GetN<bitsizeof(4)> test698_4;
    GetN<bitsizeof(5)> test698_5;
    GetN<bitsizeof(6)> test698_6;
    GetN<bitsizeof(7)> test698_7;
    GetN<bitsizeof(8)> test698_8;
    GetN<bitsizeof(9)> test698_9;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, test698_0.getN() == bitsizeof(0));
        unit::assert(tag, test698_1.getN() == bitsizeof(1));
        unit::assert(tag, test698_2.getN() == bitsizeof(2));
        unit::assert(tag, test698_3.getN() == bitsizeof(3));
        unit::assert(tag, test698_4.getN() == bitsizeof(4));
        unit::assert(tag, test698_5.getN() == bitsizeof(5));
        unit::assert(tag, test698_6.getN() == bitsizeof(6));
        unit::assert(tag, test698_7.getN() == bitsizeof(7));
        unit::assert(tag, test698_8.getN() == bitsizeof(8));
        unit::assert(tag, test698_9.getN() == bitsizeof(9));
    }
}

template <uint32 N>
class Test697
{
public:
    const auto x = N;
}

struct nested697
{
    float f;
    bool b;
    uint32[1][3][54] a;
}

struct struct697
{
    nested697 n;
    int3      m;
    uint<54>  o;
}

class TestCase697
{
private:
    Test697<bitsizeof(float)>     test697_float;
    Test697<bitsizeof(bool)>      test697_bool;
    Test697<bitsizeof(struct697)> test697_struct;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, test697_float.x == bitsizeof(float));
        unit::assert(tag, test697_bool.x == bitsizeof(bool));
        unit::assert(tag, test697_struct.x == bitsizeof(struct697));
    }
}

inline void test_main()
{
    unit::test<694>(TestCase694);
    unit::test<695>(TestCase695);
    unit::test<696>(unit::fixture<Test696>());
    unit::test<697>(unit::fixture<TestCase697>());
    unit::test<698>(unit::fixture<Test698>());
}
