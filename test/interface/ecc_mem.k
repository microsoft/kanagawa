// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

struct Result
{
    uint32 _num_invalid;
    uint32 _num_error;
};

template<auto Depth>
class EccMemTest
{
private:
    memory_ecc<uint32, Depth> _mem;

    Result _result;

public:
    Result Test()
    {
        print("enter Test\n");

        _result = {};

        pipelined_for (Depth, [](index_t<Depth> i)
        {
            _mem[i] = i;
        });

        pipelined_for (Depth, [](index_t<Depth> i)
        {
            ecc<uint32> data = _mem[i];

            atomic
            {
                if (!data.data.is_valid)
                {
                    _result._num_invalid++;
                }

                if (data.error)
                {
                    _result._num_error++;
                }
            }

            if (data.data.is_valid)
            {
                assert(data.data.value == i);
            }
        });

        print("exit Test\n");

        return _result;
    }
} 

class EccMemMain
{
    EccMemTest<512> _testBRAM;
    EccMemTest<8192> _testURAM;

public:
    Result main(bool testURAM)
    {
        Result result;

        if (testURAM)
        {
            result = _testURAM.Test();
        }
        else
        {
            result = _testBRAM.Test();
        }

        return result;
    }
}

export EccMemMain;
