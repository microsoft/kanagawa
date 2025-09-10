// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // quad port memory with more than 2 write locations
    class Test
    {
        [[memory, quad_port]] uint32[512] _mem;
    public:
        void F(uint32 idx)
        {
            static for (const auto i : 3)
            {
                _mem[idx + i] = idx - i;
            }
        }
    }

    export Test;

expected:67

    // quad port memory with more than 2 read locations
    class Test
    {
        [[memory, quad_port]] uint32[512] _mem;
    public:
        uint32 F(uint32 idx)
        {
            uint32 result = 0;

            static for (const auto i : 3)
            {
                result += _mem[idx + i];
            }

            return result;
        }
    }

    export Test;

expected:67
