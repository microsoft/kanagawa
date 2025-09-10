// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Tests for export classes which instantiate other export classes
import test.unit as unit
import test.runner

template<auto N>
inline uint32 AddN(uint32 x)
{
    return x + N;
}

// Export class with a private method connected as a callback to a member object
// IR should not be generated for the private method
// when compiling uses of the exported class
class ExportCallsCallbackInner
{
private:
    (uint32)->uint32 _cb;

public:
    uint32 Bar(uint32 x)
    {
        return _cb(x);
    }
}

class ExportCallsCallbackOuter
{
private:
    ExportCallsCallbackInner _inner = {._cb = Foo};

    (uint32)->uint32 _cb;

    uint32 Foo(uint32 x)
    {
        return _cb(x);
    }

public:
    void Test(unit::tag_t tag, uint32 src, uint32 expected)
    {
        unit::assert_equal(tag, expected, _inner.Bar(src));
    }
}

export ExportCallsCallbackInner;
export ExportCallsCallbackOuter;

inline void TestExportCallsCallback(unit::tag_t tag)
{
    static ExportCallsCallbackOuter _test = {._cb = AddN<1>};
    _test.Test(tag, 2, 3);
}

inline void test_main()
{
    unit::test<1>(TestExportCallsCallback);
}
