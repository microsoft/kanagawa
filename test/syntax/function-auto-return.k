// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    inline UINT<4> main()
    {
        return cast<UINT<4>>(Fn_uint4());
    }

expected:0

    inline UINT<5> main()
    {
        return cast<UINT<4>>(Fn_uint4());
    }

expected:1

    inline void main()
    {
        Fn_void();
    }

expected:0

    inline uint32 main()
    {
        return Fn_void();
    }

expected:21

    inline void main()
    {
        Fn_return_void();
    }

expected:0

    inline uint32 main()
    {
        return Fn_return_void();
    }

expected:21

    inline bool main()
    {
        return Fn_T(true);
    }

expected:0

    inline UINT<5> Run()
    {
        return cast<UINT<5>>(Fn_Tx2(10));
    }

expected:0

    inline UINT<6> Run()
    {
        return cast<UINT<6>>(Fn_Tx2(1));
    }

expected:14

    inline UINT<5> Run()
    {
        return cast<UINT<5>>(Fn_Tx2(Fn_T(10)));
    }

expected:0

    inline UINT<4> Run()
    {
        return cast<UINT<4>>(Fn_Tx2(Fn_T(Fn_Tx2(2))));
    }

expected:0

    inline auto Run()
    {
        return Fn_T(10) + 30;
    }

expected:0

    inline auto Run()
    {
        return Fn_T(10) > 4 ? true : false;
    }

expected:0

    inline auto Run()
    {
        return {0};
    }

expected:1

    inline void main()
    {
        Go(Fn_uint4);
    }

expected:0

    inline void main()
    {
        Go([]() -> auto {return 10;});
    }

expected:0

    inline void main()
    {
        Go([](){return 10;});
    }

expected:0

    inline auto Foo()
    {
    }

    inline auto main()
    {
        return Foo();
    }

expected:0

    inline void Foo()
    {
    }

    inline auto main()
    {
        return Foo();
    }

expected:0

    inline void Foo()
    {
    }

    inline void main()
    {
        return Foo();
    }

expected:0
