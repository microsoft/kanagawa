// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module type.newtype
    { newtype
    , wrap_newtype
    , unwrap_newtype
    }

//| Defines a struct with one field of type `T`.
// At runtime a value of newtype<T> is equivalent to a value of type T.
// Since newtype<T> is a unique type that is not implicitly convertible to/from T,
// it can be used to improve type safety.
template <typename T>
struct newtype
{
    T unwrap;
}

//| Wrap a value into a newtype
template <typename T>
inline newtype<T> wrap_newtype(T x)
{
    return {x};
}

//| Unwrap a value of a newtype
template <typename T>
inline auto unwrap_newtype(newtype<T> x)
{
    return x.unwrap;
}
