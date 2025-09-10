// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.callbacks
  { void_F_void
  , void_F_bool
  , void_F_last
  , u32_F_u32
  , u32_F_u32_F_u32_u32
  , Fn_T_S_R
  , A
  , B
  , C
  , D
  , callback_uint32_to_uint32_t
  , callback_uint32_bool_to_uint32_t
  , callback_max_threads_1_uint32_to_uint32_t
  , callback_T_to_R_t
  , callback_max_threads_N_T_to_R_t
  }

template <typename T, typename S, typename R>
inline R Fn_T_S_R(T x, S y)
{
    return {};
}

inline void void_F_void()
{
}

inline void void_F_bool(bool is_last)
{
}

inline void void_F_last([[last]] bool is_last)
{
}

inline uint32 u32_F_u32(uint32 x)
{
    return x;
}

inline uint32 u32_F_u32_F_u32_u32((uint32) -> uint32 fn, uint32 x)
{
    return fn(x);
}

class C
{
public:
    inline void void_F_void()
    {
    }

    inline void void_F_last([[last]] bool is_last)
    {
    }

    inline uint32 u32_F_u32(uint32 x)
    {
        return x;
    }

    [[max_threads(1)]]
    uint32 max_threads_1_u32_F_u32(uint32 x)
    {
       return x;
    }

    template <auto N>
    [[max_threads(N)]]
    uint32 max_threads_N_u32_F_u32(uint32 x)
    {
       return x;
    }

    template <auto N, typename T, typename R>
    [[max_threads(N)]]
    R max_threads_N_T_to_R(T x)
    {
       return x;
    }
}

using callback_uint32_to_uint32_t = (uint32 a) -> uint32;
using callback_max_threads_1_uint32_to_uint32_t = [[max_threads(1)]] (uint32 a) -> uint32;
using callback_uint32_bool_to_uint32_t = (uint32, bool b) -> uint32;

template <typename T, typename R>
using callback_T_to_R_t = (T x) -> R;

template <auto N, typename T, typename R>
using callback_max_threads_N_T_to_R_t = [[max_threads(N)]] (T x) -> R;

class D
{
private:
  callback_uint32_to_uint32_t          callback1;
  callback_uint32_bool_to_uint32_t     callback2;
  [[async, max_threads(2)]] () -> void callback3;
  ([[last]] bool) -> void              callback4;
}

class B
{
private:
    D d;
}

class A
{
private:
    B[2] b;

    default = { .callback2 = Fn_T_S_R<uint32, bool, uint32>
              , .callback4 = void_F_last
              };
}
