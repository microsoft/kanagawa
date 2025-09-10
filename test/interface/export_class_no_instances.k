// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

class ExportClassNoInstance
{
  private:
    uint32 _a = 0;

  public:
    [[reset]] void Entry()
    {
        auto r = _a + 1;
        print("Entry called, r is {r}\n");
        assert(r == 1);
    }
}

export ExportClassNoInstance;

class ExportClassWithInstances
{
private:
    (uint32)->uint32 _cb;

public:
    uint32 Test(uint32 x)
    {
        return _cb(x) + 2;
    }
}

export ExportClassWithInstances;

class ExportClassWithArrayInstances
{
private:
    (uint32)->uint32 _cb;

public:
    uint32 Test(uint32 x)
    {
        return _cb(x) + 2;
    }
}

export ExportClassWithArrayInstances;

class Container
{
private:
    ExportClassWithInstances _inst =  {._cb = CallbackWrapper};
    ExportClassWithArrayInstances[2] _inst_array =
    {
       {._cb = CallbackWrapper},
       {._cb = CallbackWrapper}
    };

    uint32 CallbackWrapper(uint32 x)
    {
        return x + 1;
    }

public:
    void run(unit::tag_t tag)
    {
        unit::assert_equal(tag, 4, _inst.Test(1));
        unit::assert_equal(tag, 5, _inst_array[0].Test(2));
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<Container>());
}
