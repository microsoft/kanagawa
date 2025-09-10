// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    import import control.loop

expected:1

    inline void Foo(uint32 x)
    {
    }

    inline void main()
    {
        int32 x;
        Foo(x);
    }

expected:1

    inline void main()
    {
        bool x;
        {
            uint32 x;
        }
    }

expected:1
