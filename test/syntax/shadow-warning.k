// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        bool x;
        {
            uint32 x;
        }
    }

warning:shadow

    inline void main()
    {
        const bool x = false;
        {
            uint32 x;
        }
    }

warning:shadow

    inline void main()
    {
        bool x;
        {
            const uint32 x = 1;
        }
    }

warning:shadow

    inline void main()
    {
        bool x;
        {
            auto x = 0;
        }
    }

warning:shadow

    const bool x = true;

    inline void main()
    {
        {
            uint32 x;
        }
    }

warning:0

    inline void Run(bool x)
    {
        {
            uint32 x;
        }
    }

warning:shadow

    import helper.shadow_warning

warning:shadow

    inline void Run(bool x)
    {
        for (const auto x : 10)
        {
        }
    }

warning:shadow

    inline void main()
    {
        for (const auto x : 10)
        {
        }

        bool x;
    }

warning:0

    inline void main()
    {
        for (const auto x : 10)
        {
            for (const auto x : 6)
            {
            }
        }
    }

warning:shadow

    inline void Run(bool x)
    {
        if (x)
        {
            uint32 y;
        }
        else
        {
            uint32 y;
        }
    }

warning:0

    inline void Run(bool x)
    {
        if (x)
        {
            uint32 y;

            if (x)
            {
                uint32 y;
            }
            else
            {
                uint32 y;
            }
        }
    }

warning:shadow

    inline void main()
    {
        uint32 x;

        inline void Bar()
        {
            uint32 x;

            x = 0;
        }

        x = 0;
    }

warning:0

    class Foo
    {
        uint32 x;

        inline void Bar()
        {
            bool x;
        }
    }

warning:shadow
