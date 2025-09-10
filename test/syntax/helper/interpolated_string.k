// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.interpolated_string
  { intConst
  , boolConst
  , enumConst
  , floatConst
  , intFun
  , boolFun
  , stringFun
  , id
  }

enum E : uint8
{
    One
}

const auto enumConst = E::One;
const auto floatConst = 3.14;
const auto intConst = 0xF00;
const auto boolConst = true;

inline auto intFun(uint32 x)
{
    return x;
}

inline auto boolFun(bool x)
{
    return x;
}

inline auto stringFun(string x)
{
    return x;
}

template <typename T>
inline auto id(T x)
{
    return x;
}

