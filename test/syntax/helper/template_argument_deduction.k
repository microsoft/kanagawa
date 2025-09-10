// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.template_argument_deduction
    { Fn_T
    , Fn_T_U
    , Fn_T_10
    , Fn_T_N
    , Fn_T_N_M
    , Fn_T_T
    , Fn_T_F
    , Fn_R_T
    , Fn_T_T_N
    , Fn_T_N_T_N
    , Fn_uint_N
    , Fn_int_N
    , Fn_uint_N_M
    , Fn_Fn
    , Fn_Fn1
    , Fn_Fn3
    , Fn_FnA1
    , Fn_Fn2A
    , Fn_T_Fn1
    , Fn_U_Fn1
    , Fn_Fn1_Fn1
    , Fn_Fn1_Fn2
    , Fn_Fn_uint32
    , Fn_Fn_uint32_Fn_bool
    , Enum
    , Struct
    , Union
    , Class
    , C_Fn_Go
    , Template_T
    , Template_N
    , check_type
    }

template <typename T, typename U>
inline void check_type(U x)
{
    T[1] a1;
    U[1] a2;

    a1 = a2;
}

template <typename T, typename U>
inline auto Fn_T_U(T x, U y)
{
    return x + y;
}

template <typename T>
inline T Fn_T(T x)
{
    return x;
}

template <typename T, typename F>
inline T Fn_T_F(F fn, T x)
{
    return fn(x);
}

template <typename T>
inline T Fn_T_10(T[10] x)
{
    return x[0];
}

template <typename T, auto N>
inline T[N+1] Fn_T_N(T[N] x)
{
    T[N+1] y;
    y[0] = x[0];
    return y;
}

template <typename T, auto N, auto M>
inline auto Fn_T_N_M(T[N][M] x)
{
    return x;
}

template <typename T, auto N>
inline T[N+1] Fn_T_N_T_N(T[N] x, T[N] y)
{
    T[N+1] z;
    z[0] = x[0];
    return z;
}

template <typename T>
inline T Fn_T_T(T x, T y)
{
    return x;
}

template <typename R, typename T>
inline R Fn_R_T(T y)
{
    R x;
    return x;
}

template <typename T, auto N>
inline T[N+1] Fn_T_T_N(T x, T[N] y)
{
    T[N+1] z;
    z[0] = x;
    z[1] = y[0];
    return z;
}

template <auto N>
inline uint<N+1> Fn_uint_N(uint<N> x)
{
    return x;
}

template <auto N>
inline int<N+1> Fn_int_N(int<N> x)
{
    return x;
}

template <auto N, auto M>
inline uint<N+1>[M+1] Fn_uint_N_M(uint<N>[M] x)
{
    uint<N+1>[M+1] y;
    y[0] = x[0];
    return y;
}

template <typename T, typename R>
inline R Fn_Fn1((T) -> R fn)
{
    return fn(0);
}

template <typename R>
inline R Fn_Fn(() -> R fn)
{
    return fn();
}

template <typename T, typename R>
inline R Fn_T_Fn1(T x, (T) -> R fn)
{
    return fn(x);
}

template <typename U, typename T, typename R>
inline R Fn_U_Fn1(U x, (T) -> R fn)
{
    return fn(x);
}

template <auto N, typename T, typename R>
inline R Fn_FnA1((T[N]) -> R fn)
{
    return fn({});
}

template <auto N, typename T, typename R>
inline R[N] Fn_Fn2A((T) -> R fn)
{
    R[N] r;
    static for(const auto i : N)
    {
        r[i] = fn(0);
    }
    return r;
}

template <typename T1, typename T2, typename T3, typename R>
inline R Fn_Fn3((T1, T2, T3) -> R fn)
{
    return fn(0, 0, 0);
}

template <typename T, typename R>
inline R Fn_Fn1_Fn1((T) -> R fn1, (T) -> R fn2)
{
    return fn1(0);
}

template <typename T, typename U, typename R>
inline R Fn_Fn1_Fn2((T) -> R fn1, (U) -> R fn2)
{
    return fn1(0);
}

template <(uint32) -> uint32 Fn>
inline void Fn_Fn_uint32()
{
    Fn(10);
}

template <(uint32) -> uint32 Fn1, (bool) -> bool Fn2>
inline void Fn_Fn_uint32_Fn_bool()
{
    Fn1(10);
    Fn2(true);
}

enum Enum : uint4
{
    Zero
}

struct Struct
{
    uint32 x;
}

union Union
{
    uint32 x;
}

template <() -> void Fn>
class C_Fn_Go
{
    public:
        inline void Go(auto x)
        {
        }    
}

class Class
{
public:    
    template <typename T>
    T Fn_T(T x)
    {
        return x;
    }

    template <typename T, typename F>
    T Fn_T_F(F fn, T x)
    {
        return fn(x);
    }

    template <typename T>
    T Fn_T_10(T[10] x)
    {
        return x[0];
    }

    template <typename T, auto N>
    T[N+1] Fn_T_N(T[N] x)
    {
        T[N+1] y;
        y[0] = x[0];
        return y;
    }

    template <typename T, auto N>
    T[N+1] Fn_T_N_T_N(T[N] x, T[N] y)
    {
        T[N+1] z;
        z[0] = x[0];
        return z;
    }

    template <typename T>
    T Fn_T_T(T x, T y)
    {
        return x;
    }

    template <typename R, typename T>
    R Fn_R_T(T y)
    {
        R x;
        return x;
    }

    template <typename T, auto N>
    T[N+1] Fn_T_T_N(T x, T[N] y)
    {
        T[N+1] z;
        z[0] = x;
        z[1] = y[0];
        return z;
    }

    template <auto N>
    uint<N+1> Fn_uint_N(uint<N> x)
    {
        return x;
    }

    template <auto N>
    int<N+1> Fn_int_N(int<N> x)
    {
        return x;
    }

    template <auto N, auto M>
    uint<N+1>[M+1] Fn_uint_N_M(uint<N>[M] x)
    {
        uint<N+1>[M+1] y;
        y[0] = x[0];
        return y;
    }

    template <typename T, typename U, typename R>
    inline R Fn_Fn1_Fn2((T) -> R fn1, (U) -> R fn2)
    {
        return fn1(0);
    }
}

template <typename T>
class Template_T
{
public:
    template <auto N>
    T[N+1] Fn_T_N(T[N] x)
    {
        T[N+1] y;
        y[0] = x[0];
        return y;
    }

    template <auto N>
    uint<N+1> Fn_uint_N(uint<N> x)
    {
        return x;
    }

    template <auto N>
    int<N+1> Fn_int_N(int<N> x)
    {
        return x;
    }
}


template <auto N>
class Template_N
{
public:
    template <typename T>
    T[N+1] Fn_T_N(T[N] x)
    {
        T[N+1] y;
        y[0] = x[0];
        return y;
    }

    template <auto M>
    uint<N+1>[M+1] Fn_uint_N_M(uint<N>[M] x)
    {
        uint<N+1>[M+1] y;
        y[0] = x[0];
        return y;
    }

    template <auto M>
    uint<M+1>[N+1] Fn_uint_M_N(uint<M>[N] x)
    {
        uint<M+1>[N+1] y;
        y[0] = x[0];
        return y;
    }
}
