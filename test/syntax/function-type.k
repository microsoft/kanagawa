// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    using fn = ((uint32) -> void) -> void;

expected:1

    class Foo
    {
        ((uint32) -> void) -> void callback;
    }

expected:1

    inline void Go((bool b, (uint32) -> void fn) -> void Do)
    {
    }

expected:1

    inline void Go((bool b) -> void Do)
    {
    }

expected:0

    template <(bool b, (uint32) -> void fn) -> void Do>
    inline void Go()
    {
    }

expected:1

    template <(bool b) -> void Do>
    inline void Go()
    {
    }

expected:0

    class Foo
    {
        (uint32) -> void callback;
    }

expected:0

    class Foo
    {
        (uint32 x, uint32 y) -> void callback;
    }

expected:0

    class Foo
    {
        (uint32 x, uint32 x) -> void callback;
    }

expected:1

    using fn = () -> const uint32;

expected:1

    using fn = () -> uint32;

expected:0

    using fn = (void) -> uint32;

expected:1

    using fn = () -> () -> void;

expected:1

    using fn = () -> string;

expected:0

    using fn = (string) -> void;

expected:0

    using fn = (auto) -> void;

expected:1

    using fn = () -> auto;

expected:1

    using fn = ([[memory]] uint32[4]) -> void;

expected:1

    using fn = () -> [[memory]] uint32[4];

expected:1

    import helper.function_auto_return

    class Foo
    {
        public:
            using fn_t = (uint32 Fn_void) -> void;

            inline void Run()
            {
                Fn_void();
            }
    }

expected:0
