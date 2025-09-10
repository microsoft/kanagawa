// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

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

inline void FactorialTest(unit::tag_t tag)
{
    unit::assert(tag, 120 == factorial<5>());
}

inline void ReduceTest(unit::tag_t tag)
{
    uint32[8] a = {8, 43, 8, 2, 0, 1, 123, 8};

    const auto x = reduce(a, [](uint32 x, uint32 y)
            {
                return x > y ? x : y;
            });

    unit::assert(tag, x == 123);
}

class UnrollForTest
{
private:
    uint32[8] a;

public:
    void run(unit::tag_t tag)
    {
        unroll_for<8>([](uint3 i)
        {
            a[i] = i;
        });

        unroll_for<8>([tag](uint3 i)
        {
            unit::assert(tag, a[i] == i);
        });

        static for(const auto i : 8)
        {
            unit::assert(tag, a[i] == i);
        }
    }
}

template <typename T>
class Limits
{
public:
    static if (cast<T>(-1) > 0)
    {
        const T MAX = cast<T>((1 << (bitsizeof T)) - 1);
        const T MIN = 0;
    }
    else
    {
        const T MAX = cast<T>(cast<uint<bitsizeof T>>(1 << ((bitsizeof T) - 1)) - 1);
        const T MIN = cast<T>(1 << ((bitsizeof T) - 1));
    }
}


inline void LimitsTest(unit::tag_t tag)
{
    static Limits<uint5> l1;
    static Limits<int5> l2;

    unit::assert(tag, l1.MAX == 31);
    unit::assert(tag, l1.MIN == 0);

    unit::assert(tag, l2.MAX == 15);
    unit::assert(tag, l2.MIN == -16);
}


template <auto N, template <auto> typename Node>
class List
{
    public:
        inline void Do((uint32, uint3)->void write_arr)
        {
            node.Do(write_arr);
            static if (N > 1)
            {
               next.Do(write_arr);
            }
        }
    private:
        Node<N-1> node;

        static if (N > 1)
        {
            List<N - 1, Node> next;
        }
}

template <auto N>
class Worker
{
    public:
       inline void Do((uint32, uint3)->void write_arr)
       {
           write_arr(N+1, N);
       }
}

class ListTest
{
private:
    uint32[7] a1;

    List<7, Worker> workers;

    inline void WriteArr(uint32 data, uint3 idx)
    {
        a1[idx] = data;
    }

public:
    void run(unit::tag_t tag)
    {
        workers.Do(WriteArr);

        static for(const auto i : 7)
        {
            unit::assert(tag, a1[i] == i + 1);
        }
    }
}

inline void test_main()
{
    print("factorial\n");
    unit::test<1>(FactorialTest);

    print("reduce\n");
    unit::test<2>(ReduceTest);

    print("unroll_for\n");
    unit::test<3>(unit::fixture<UnrollForTest>());

    print("limits\n");
    unit::test<4>(LimitsTest);

    print("object list\n");
    unit::test<5>(unit::fixture<ListTest>());
}

