// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.type_constexpr
  { same_types
  , different_types
  , subtype_of
  , Struct
  , Struct_t
  , Enum
  , Enum_t
  , Class
  }

template <typename S, typename T>
class same_types
{
    static assert(S == T);
    static assert(S >= T);
    static assert(S <= T);
    static assert(!(S != T));
    static assert(!(S < T));
    static assert(!(S > T));
}

template <typename S, typename T>
class different_types
{
    static assert(!(S == T));
    static assert(!(S >= T));
    static assert(!(S <= T));
    static assert(S != T);
    static assert(!(S < T));
    static assert(!(S > T));
}

template <typename S, typename T>
class subtype_of
{
    static assert(S != T);
    static assert(!(S >= T));
    static assert(S <= T);
    static assert(!(S == T));
    static assert(S < T);
    static assert(!(S > T));
}

struct Struct
{
}

enum Enum : uint7
{
    One
}

using Struct_t = Struct;

using Enum_t = Enum;

class Class
{
    public:
        inline void Run(bool x)
        {
        }
}
