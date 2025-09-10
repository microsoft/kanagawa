// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.return_type_polymorphism
  { Fn_T
  , Fn_T_auto
  , Fn_T_T
  , Fn_T_U
  , Fn_T_N
  , Fn_optional_T_U
  , Fn_optional_T_N_U
  , optional
  , is_valid
  }

import type.coerce
import data.optional

template <typename T>
inline T Fn_T()
{
    static if (T == bool)
        return true;
    else
        return 42;
}

template <typename T>
inline T Fn_T_auto(auto x)
{
    return x;
}

template <typename T>
inline T Fn_T_T(T x)
{
    return x;
}

template <typename T, typename U>
inline T Fn_T_U(U x)
{
    return reinterpret_cast<T>(x);
}

template <typename T, auto N>
inline T[N] Fn_T_N()
{
    T[N] result;

    static for (const auto i : N)
    {
        result[i] = Fn_T();
    }

    return result;
}

template <typename T, typename U>
inline optional<T> Fn_optional_T_U(U x)
{
    return {true, x};
}

template <typename T, auto N, typename U>
inline optional<T>[N] Fn_optional_T_N_U(U x)
{
    optional<T>[N] result;

    static for (const auto i : N)
    {
        result[i] = {true, x};
    }

    return result;
}
