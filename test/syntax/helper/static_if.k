// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.static_if
    { auto_Fn
    , factorial
    , reduce
    , unroll_for
    , True
    , False
    , Ten
    , Testable
    , Tree
    , Method
    , Storage
    , Go
    , Exec
    }

import data.tuple
import data.memory
import data.array as A

const bool True = true;
const bool False = false;
const auto Ten = 10;

inline void Exec(() -> void Do)
{
    Do();
}

template <typename T>
inline T Go((T) -> T Do, T x)
{
    return Do(Do(x));
}

template <auto Cond>
inline auto auto_Fn()
{
    static if (Cond)
        return true;
    else
        return 10;
}

template <auto N>
inline auto factorial()
{
    static assert (N > 0);

    static if (N == 1)
        return 1;
    else
        return N * factorial<N-1>();
}

template <typename T, auto N>
inline auto split(T[N] a)
{
    pair<T[N / 2], T[N / 2]> result;

    static for (const auto i : N / 2)
    {
        result.first[i] = a[2 * i];
        result.second[i] = a[2 * i + 1];
    }

    return result;
}

template <typename T, auto N>
inline auto reduce(T[N] a, (T, T) -> T fn)
{
    static assert((N & (N-1)) == 0);

    static if (N == 1)
    {
        return a[0];
    }
    else
    {
        const auto b = split(a);
        return fn(reduce(b.first, fn), reduce(b.second, fn));
    }
}

template <auto N, typename T>
inline void unroll_for((T) -> void body)
{
    static assert (N > 0);

    body(N - 1);

    static if (N > 1)
    {
        unroll_for<N - 1>(body);
    }
}

template <bool Test>
class Testable
{
    static if (Test) public:

    bool foo;
}

template <auto N, template <auto> typename Node, auto Level = 0>
class Tree
{
    public:
        inline void Do()
        {
            node.Do();

            static if (N > 1)
            {
                left.Do();
            }

            static if (N > 2)
            {
                left.Do();
            }
        }

    private:
        Node<Level> node;

        static if (N > 1)
        {
            Tree<N / 2, Node, Level + 1> left;
        }

        static if (N > 2)
        {
            Tree<(N - 1) / 2, Node, Level + 1> right;
        }
}

template <bool B>
class Method
{
    public:
        static if (B)
        {
            inline void Do()
            {
            }
        }
}

template <typename T, auto N>
class Storage
{
public:
    static if (N > 10 && ((N & (N - 1)) == 0))
    {
        memory<T, N> storage;
    }
    else
    {
        A::array<T, N> storage;
    }
}

