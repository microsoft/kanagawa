// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // Empty struct returned from a function (not inlined)
    struct Empty
    {
    }

    class Main
    {
        noinline Empty make_empty()
        {
            return {};
        } 

        inline void Bar(Empty y)
        {
        }

    public:    
        void main()
        {
            Bar(make_empty());
        }
    }

    export Main;

expected:0

    // Empty struct returned from a function (inlined)
    struct Empty
    {
    }

    class Main
    {
        inline Empty make_empty()
        {
            return {};
        } 

        inline void Bar(Empty y)
        {
        }

    public:
        void main()
        {
            Bar(make_empty());
        }
    }

    export Main;

expected:0
