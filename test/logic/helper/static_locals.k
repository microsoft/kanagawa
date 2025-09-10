// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.static_locals
{
    TestStaticLocalWithinModule_Inlined
}

import test.unit as unit

class StaticLocalWithinModule
{
private:
    uint32 _value = 3;

public:
    uint32 GetAndSet(uint32 new)
    {
        uint32 result = _value;

        _value = new;

        return result;
    }
}

inline void TestStaticLocalWithinModule_Inlined(uint32 expected, uint32 newVal, unit::tag_t tag)
{
    static StaticLocalWithinModule s;

    uint32 result = s.GetAndSet(newVal);

    unit::assert_equal(tag, expected, result);
}
