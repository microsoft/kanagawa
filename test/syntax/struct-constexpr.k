// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline void main()
    {
        Go<Foo, {10, false}>();
    }

expected:0
    
    inline void main()
    {
        const Foo x = {10, false};
        
        Go<Foo, x>();
    }

expected:0
    
    inline void main()
    {
        Foo x = {10, false};
        
        Go<Foo, x>();
    }

expected:1

    inline void main()
    {
        Go<Foo, {10}>();
    }

expected:0
    
    inline void main()
    {
        Go<Foo, {10, 10}>();
    }

expected:1
    
    inline void main()
    {
        Go<Foo, {10, false, 10}>();
    }

expected:1
    
    inline void main()
    {
        struct Bar
        {
            uint32 x;
        }

        Go<Bar, {10}>();
    }

expected:0
    
    inline void main()
    {
        Go<Foo, {}>();
    }

expected:0
    
    inline void main()
    {
        using Bar = Foo;

        Go<Bar, {10, false}>();
    }

expected:0

    inline void Run(bool b)
    {
        Go<Foo, {10, b}>();
    }

expected:1

    inline void Run(bool b)
    {
        Foo x = {10, b};
        Go<Foo, x>();
    }

expected:1
    
    inline void main()
    {
        struct Bar
        {
            Foo x;
            Foo y;
        }

        Go<Bar, {{10, false}, {11, true}}>();
    }

expected:0
    
    inline void Run(bool b)
    {
        struct Bar
        {
            Foo x;
            Foo y;
        }

        Go<Bar, {{10, b}, {11, true}}>();
    }

expected:1
