// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Tests for exported class functionality

// Callback with [[last]] attribute
// arbiters should be generated with transactions enabled
class IsLastExportedClass
{
private:
    [[async]] (uint32 a, [[last]] bool is_last)->void cb;

public:
    [[async]] void Go()
    {
        static for (const auto i : 2)
        {
            async_exec([]()
            {
                pipelined_for(16, [](index_t<16> i)
                {
                    cb(i, i==15);
                });
            });
        }
    }
}

export IsLastExportedClass;
