// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.function
    { apply
    , bind
    , bind1st
    , bind2nd
    , compose
    , compose2
    , constant
    , curry
    , curry3
    , flip
    , id
    , not
    , not2
    , on1st
    , on2nd
    , selecting
    , uncurry1
    , uncurry
    , uncurry3
    , compose_endo
    }

//| Identity function.
//
//     prop> id(x) == x
inline auto id(auto x)
{
    return x;
}

//| Return a generic closure which ignores its argument and always returns `x`
//
// #### __Example__
//
//     >>> apply(constant(false), 10)
//     false
inline auto constant(auto x)
{
    using return_t = decltype(x);

    return [x](auto y) -> return_t
           {
               return x;
           };
}

//| Function composition
inline auto compose((auto) -> auto f, (auto) -> auto g)
{
    return [f, g](auto x)
        {
            return f(g(x));
        };
}

//| Composition with a binary function
//
// #### __Example__
//
//     >>> zip_with(compose2(reinterpret_cast<uint32>, f32::mul), {1.2, 3.0}, {9.0, 4.3})
//     {0xCD, 0x67}
inline auto compose2((auto) -> auto f, (auto, auto) -> auto g)
{
    return [f, g](auto x, auto y)
        {
            return f(g(x, y));
        };
}

//| Apply one argument to a curried function
inline auto apply((auto) -> auto f, auto x)
{
    return f(x);
}

//| Curry a binary function
//
// A curried function is a function that takes one argument at a time.
//
// For example, a curried binary functions is a unary function that returns a unary
// function.  Curried functions can be applied using `apply` or converted into
// uncurried functions using `uncurry`.
//
//     prop> apply(apply(curry(add), 1), 2) == add(1, 2)
inline auto curry((auto, auto) -> auto f)
{
    return [f](auto x)
        {
            return [f, x](auto y)
                {
                    return f(x, y);
                };
        };
}

inline auto curry3((auto, auto, auto) -> auto f)
{
    return [f](auto x)
        {
            return [f, x](auto y)
                {
                    return [f, x, y](auto z)
                        {
                            return f(x, y, z);
                        };
                };
        };
}

//| Convert a curried function into uncurried unary function
inline auto uncurry1((auto) -> auto f)
{
    return [f](auto x)
        {
            auto g = f(x);
            return g();
        };
}

//| Convert a curried function into uncurried binary function
inline auto uncurry((auto) -> auto f)
{
    return [f](auto x, auto y)
        {
            auto g = f(x);
            return g(y);
        };
}

//| Convert a curried function into uncurried three parameter function
inline auto uncurry3((auto) -> auto f)
{
    return [f](auto x, auto y, auto z)
        {
            auto g = f(x);
            auto h = g(y);
            return h(z);
        };
}

//| Bind a value to the function argument
inline auto bind((auto) -> auto f, auto x)
{
    return [f, x]
        {
            return f(x);
        };
}

//| Bind a value to the first argument of a function
//
// #### __Example__
//
//     >>> apply(bind1st(sub, 6), 3)
//     3
inline auto bind1st((auto, auto) -> auto f, auto x)
{
    return apply(curry(f), x);
}

//| Apply a function to the first argument
inline auto on1st((auto) -> auto f)
{
    return [f](auto x, auto y)
        {
            return f(x);
        };
}


//| Bind a value to the second argument of a function
//
// #### __Example__
//
//     >>> apply(bind2nd(sub, 2), 6)
//     4
inline auto bind2nd((auto, auto) -> auto f, auto y)
{
    return apply(curry(flip(f)), y);
}

//| Apply a function to the second argument
inline auto on2nd((auto) -> auto f)
{
    return [f](auto x, auto y)
        {
            return f(y);
        };
}

inline bool operator_not(bool x)
{
    return !x;
}

//| Return true if the unary predicate is false
//
// #### __Example__
//
//     >>> apply(not(is_valid), make_optional(true, 10))
//     false
inline auto not((auto) -> bool f)
{
    return compose(operator_not, f);
}

//| Return true if the binary predicate is false
//
// #### __Example__
//
//     >>> zip_with(not2(less_than), {1, 2}, {3, 1});
//     {false, true}
inline auto not2((auto, auto) -> bool f)
{
    return compose2(operator_not, f);
}

//| Select one of two arguments using the binary predicate
//
// #### __Example__
//
//     >>> reduce(selecting(less_then), {1, 4, 0});
//     0
inline auto selecting((auto, auto) -> bool f)
{
    return [f](auto x, auto y)
        {
            return f(x, y) ? x : y;
        };
}

//| Flip function parameters
//
// #### __Example__
//
//     >>> zip_with(flip(less_than), {1, 2}, {3, 1});
//     {false, true}
inline auto flip((auto, auto) -> auto f)
{
    return [f](auto x, auto y)
        {
            return f(y, x);
        };
}

//| Compose the function `N` times.
//
// #### __Examples__
//
//     >>> apply(compose_endo<3>(increment), 1)
//     4
//
//     >>> apply(compose_endo<3>(compose_endo<2>(increment)), 1)
//     7
template <auto N>
inline auto compose_endo((auto) -> auto f)
{
    static assert(N > 0);
    static if (N == 1)
        return f;
    else
        return compose(f, compose_endo<N - 1>(f));
}