// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Operations on arrays.
*/
module data.array
    { array
    , //| # Constructing arrays
      indices
    , copy_array
    , repeat
    , singleton
    , iterate
    , //| # Element access
      at
    , front
    , back
    , //| # Transforming arrays
      map
    , map_indices
    , shift_array_right
    , shift_array_left
    , drop
    , tail
    , take
    , init
    , intersperse
    , rotate_array_right
    , rotate_array_left
    , partial_sort
    , bitonic_sort
    , transpose
    , unique_by
    , unique
    , //| # Reducing arrays
      foldr
    , foldl
    , reduce
    , map_reduce
    , or
    , and
    , any
    , all
    , minimum
    , maximum
    , sum
    , last_valid
    , first_valid
    , //| # Zipping and unzipping arrays
      zip
    , zip_with
    , zip_with_indices
    , unzip_with
    , unzip
    , //| # Scanning arrays
      inclusive_scan
    , prefix_sum
    , //| # Comparing arrays
      equal_by
    , equal
    }

import data.function
import data.optional as Opt
import data.order    as Ord
import data.tuple    as P
import numeric.int.operator
import type.coerce
import type.stdtype

template <typename T, auto N>
using array = T[N];

//| Perform an inclusive scan on the given array.
//
// @@
// x[0]  x[1]  x[2]
//
//  |     |     |
//  |   +-+-+   |
//  |   |   |   |
//  +---+ f +   |
//  |   |   |   |
//  |   +-+-+ +-+-+
//  |     |   |   |
//  |     +---+ f +
//  |     |   |   |
//  |     |   +-+-+
//  |     |     |
//  |     |     +--- . . .
//  |     |     |
//
// r[0]  r[1]  r[2]
// @@
//
// #### __Example__
//
//     >>> inclusive_scan<uint8>({1, 2, 3}, add);
//     {1, 3, 6}
template <typename R, typename T, auto N>
inline R[N] inclusive_scan(T[N] input, (R, R) -> R AssociativeFn)
{
    // Two sets of results are kept - one set from the previous iteration and the other
    // set is the values to use on the next iteration.  Each pass through the outer for
    // loop below switches back and forth between which set is for the next versus
    // previous iteration.  This avoids copying between two arrays after each iteration.
    R[2][N] results;

    static for (const auto i : N)
    {
        results[0][i] = cast<R>(input[i]);
    }

    static for (const auto j : clog2(N))
    {
        const uint1 prev_index = j % 2;
        const uint1 next_index = (j + 1) % 2;

        static for (const auto i : N)
        {
            const auto shifted_by_j = 1 << j;
            if (i < shifted_by_j)
            {
                results[next_index][i] = results[prev_index][i];
            }
            else
            {
                const auto idx = i - shifted_by_j;
                results[next_index][i] = AssociativeFn(results[prev_index][i], results[prev_index][idx]);
            }
        }
    }

    return results[clog2(N) % 2];
}


//| Compute the prefix sum of the given array.
//
// #### __Examples__
//
//     >>> prefix_sum<uint1>({1});
//     {1}
//
//     >>> prefix_sum<uint3>({0, 1, 2, 3});
//     {0, 1, 3, 6}
template <typename R, typename T, auto N>
inline R[N] prefix_sum(T[N] input)
{
    return inclusive_scan(input, add<R, R>);
}


//| An inline function that returns the smallest N elements seen in a sequence
// of elements so far, provided one at a time over multiple calls.
// Internally, this is achieved by using a pipeline of N atomic stages each of
// which accepts an element, compares it with its current smallest, and swaps
// the two if the provided lambda returns true. The remaining element is then
// passed onto the next stage, or discarded if no stage follows.
// This iterative nature is suitable for use inside a `[[pipelined]]` function
// which would supply one piece of data per cycle (for example, extracted from
// a memory).
//
// One use case for this component would be as part of a multi-pass sorter; the
// first pass over the dataset would extract the smallest N elements, and by
// preventing those returned elements from being considered again each
// following pass would extract the next smallest N elements until the dataset
// is fully sorted.
//
// #### __Example__
//
//     const auto N = 6;
//     uint8[N] input = {5, 1, 3, 2, 7, 3};
//     optional<uint8>[6] sorted = pipelined_last(N, [input, N](index_t<N> tid)
//     {
//         return partial_sort<6>(input[tid], less_than<uint8>, tid == N - 1);
//     });
//     print(sorted);
//     // {{true, 1}, {true, 2}, {true, 3}, {true, 3}, {true, 5}, {true, 7}}
template
    < auto N     //< Maximum number of elements to be extracted.
    , typename T //< Type of each element.
    >
inline optional<T>[N] partial_sort
    ( T x
    , (T, T) -> bool cmp //< Binary function that returns true when the lhs
                         //  should be swapped and stored.
    , bool is_last       //< Indicates a call with the last element in the
                         //  sequence so that state can be reset for the next
                         //  call.
    )
{
    optional<T>[N] snappedResult;

    optional<T> lhs = {true, x};
    static for(const auto i : N)
    {
        atomic
        {
            static optional<T> result = {};

            auto rhs = result;
            if (lhs.is_valid && (!rhs.is_valid || cmp(lhs.value, rhs.value)))
            {
                result = lhs;
                lhs = rhs;
            }

            snappedResult[i] = result;

            if (is_last)
                result.is_valid = false;
        }
    }

    return snappedResult;
}


// Bitonic comparator network. Swaps pairs of values such that
// cmp(x[i], x[i + N/2]) is true.
template <auto N, typename T>
inline T[N] bitonic_comparator(T[N] x, (T, T)->bool cmp)
{
    assert(N % 2 == 0);

    const auto half_N = N / 2;

    T[N] y;
    static for (const auto i : half_N)
    {
        bool cmp_flag = cmp(x[i], x[i + half_N]);

        y[i] = cmp_flag ? x[i] : x[i + half_N];
        y[i + half_N] = cmp_flag ? x[i + half_N] : x[i];
    }

    return y;
}


template <auto N, typename T>
inline T[N] bitonic_merge(T[N] x, (T, T)->bool cmp)
{
    // Must be power of 2
    assert((N & (N - 1)) == 0);

    x = bitonic_comparator(x, cmp);

    // Recursively apply smaller bitonic comparator networks
    static if (N > 2)
    {
        const auto half_N = N / 2;

        T[2][half_N] x_split;
        static for (const auto i : half_N)
        {
            x_split[0][i] = x[i];
            x_split[1][i] = x[i + half_N];
        }

        x_split[0] = bitonic_merge(x_split[0], cmp);
        x_split[1] = bitonic_merge(x_split[1], cmp);

        static for (const auto i : half_N)
        {
            x[i] = x_split[0][i];
            x[i + half_N] = x_split[1][i];
        }
    }

    return x;
}


//| Bitonic sorting network. Sorts the passed array such that
// cmp(x[i], x[i + 1]) is true for all items.
//
// #### __Example__
//
//     >>> bitonic_sort({4, 2, 3, 1}, less_than<uint32>);
//     {1, 2, 3, 4}
template <auto N, typename T>
inline T[N] bitonic_sort(T[N] x, (T, T)->bool cmp)
{
    static assert((N & (N - 1)) == 0);

    // Recursively apply bitonic sort
    static if (N > 2)
    {
        const auto half_N = N / 2;

        const auto not_cmp = [cmp](T x, T y)
        {
            return !cmp(x, y);
        };

        T[2][half_N] x_split;
        static for (const auto i : half_N)
        {
            x_split[0][i] = x[i];
            x_split[1][i] = x[i + half_N];
        }
        x_split[0] = bitonic_sort(x_split[0], cmp);
        x_split[1] = bitonic_sort(x_split[1], not_cmp);
        static for (const auto i : half_N)
        {
            x[i] = x_split[0][i];
            x[i + half_N] = x_split[1][i];
        }
    }

    x = bitonic_merge(x, cmp);

    return x;
}


//| Given an array of `optional<T>`, return the last (highest array index)
// item with `is_valid` true. If there are no items with `is_valid` true, then
// the returned `optional<T>` has `is_valid` set to false.
//
// #### __Examples__
//
//     >>> last_valid<uint8>({{false, 0xAB}, {false, 0x22}});
//     {false, 0xAB}
//
//     >>> last_valid<uint8>({{true, 0xCD}, {true, 0xFF}});
//     {true, 0xFF}
template <typename T, auto N>
inline optional<T> last_valid(optional<T>[N] x)
{
    return reduce(
            [](optional<T> a, optional<T> b)
            {
                return b.is_valid ? b : a;
            },
            x);
}

//| Transpose rows and columns in a 2-dimensional array.
template <typename T, auto I, auto J>
inline T[J][I] transpose(T[I][J] src)
{
    T[J][I] result;

    static for (const auto i : I)
    {
        static for (const auto j : J)
        {
            result[j][i] = src[i][j];
        }
    }

    return result;
}

//| Shift the elements of an arbitrary-typed array right by the given number
// of positions, with the possibility of returning a different number of
// elements as the input array.
// Once the input array has been exhausted, default (zero)-initialized
// elements will be shifted in from the end of the array.
//
// #### __Examples__
//
// Shift 5 right by 0:
//
//     >>> shift_array_right<5>({0, 1, 2, 3, 4}, 0);
//     {0, 1, 2, 3, 4}
//
// Shift 5 right by 2 and return 4:
//
//     >>> shift_array_right<4>({0, 1, 2, 3, 4}, 2);
//     {2, 3, 4, 0}
//
// Shift 5 right by 10 and return 4:
//
//     >>> shift_array_right<4>({0, 1, 2, 3, 4}, 10);
//     {0, 0, 0, 0}
template
    < auto M     //< Number of output elements.
    , typename T //< Data type of input array.
    , auto N     //< Number of input elements.
    , typename U //< Data type for shift amount. Must be unsigned. Care should
                 //  be taken to minimize the bitwidth of U where posible
                 //  since it affects the size of the multiplexers and hence
                 //  area.
    >
inline T[M] shift_array_right(T[N] data, U amount)
{
    // Require U be unsigned
    static assert(cast<U>(-1) > 0);

    T[M] out;
    const auto Size = 1 << bitsizeof(U);
    static for(const auto i : M)
    {
        T[Size] data_i;
        static for(const auto j : Size)
            if (i + j < N)
                data_i[j] = data[i + j];
        out[i] = data_i[amount];
    }
    return out;
}

//| Drop first M elements of an array
//
// #### __Examples__
//
//     >>> drop<2>({0, 1, 2, 3, 4});
//     {2, 3, 4}
template <auto M, typename T, auto N>
inline T[N-M] drop(T[N] x)
{
    static assert(M < N);

    T[N-M] result;

    static for (const auto i : N - M)
        result[i] = x[i + M];

    return result;
}

//| Return an array containing all but the first element
//
// #### __Examples__
//
//     >>> tail({0, 1, 2, 3, 4});
//     {1, 2, 3, 4}
template <typename T, auto N>
inline T[N-1] tail(T[N] x)
{
    return drop<1>(x);
}

//| Take first M elements of an array
//
// #### __Examples__
//
//     >>> take<2>({0, 1, 2, 3, 4});
//     {0, 1}
template <auto M, typename T, auto N>
inline T[M] take(T[N] x)
{
    static assert(M <= N);
    static assert(M > 0);

    T[M] result;

    static for (const auto i : M)
        result[i] = x[i];

    return result;
}

//| Return the array without the last element
//
// #### __Examples__
//
//     >>> init({0, 1, 2, 3, 4});
//     {0, 1, 2, 3}
template <typename T, auto N>
inline T[N-1] init(T[N] x)
{
    return take(x);
}

//| Intersperse a value between elements of an array
//
// #### __Examples__
//
//     >>> intersperse(-1, {0, 1, 2, 3, 4});
//     {0, -1, 1, -1, 2, -1, 3, -1, 4}
template <typename T, auto N>
inline T[2*N-1] intersperse(T a, T[N] x)
{
    T[2*N-1] result;

    static for (const auto i : 2*N-1)
        if (static(i % 2 == 0))
            result[i] = x[i/2];
        else
            result[i] = a;

    return result;
}

//| Shift the elements of an arbitrary-typed array left by the given number
// of positions, with the possibility of returning a different number of
// elements as the input array.
// Once the input array has been exhausted, default (zero) -initialized
// elements will be shifted in from the beginning of the array.
//
// #### __Examples__
//
// Shift 5 left by 0:
//
//     >>> uint32[5] a = shift_array_left({0, 1, 2, 3, 4}, 0);
//     {0, 1, 2, 3, 4}
//
// Shift 5 left by 2 and return 4:
//
//     >>> uint32[4] a = shift_array_left({0, 1, 2, 3, 4}, 2);
//     {0, 0, 0, 1}
//
// Shift 5 left by 10 and return 4:
//
//     >>> uint32[4] a = shift_array_left({0, 1, 2, 3, 4}, 10);
//     {0, 0, 0, 0}
template
    < auto M     //< Number of output elements.
    , typename U //< Data type for shift amount. Must be unsigned. Care should
                 //  be taken to minimize the bitwidth of U where possible
                 //  since it affects the size of the multiplexers and hence
                 //  area.
    , typename T //< Data type of input array.
    , auto N     //< Number of input elements.
    >
inline T[M] shift_array_left(T[N] data, U amount)
{
    // Require U be unsigned
    static assert(cast<U>(-1) > 0);

    T[M] out;
    const auto Size = 1 << bitsizeof(U);
    static for(const auto i : M)
    {
        T[Size] data_i;
        static for(const auto j : Size)
        {
            const auto i_minus_j = i - j;
            if (i_minus_j >= 0 && i_minus_j < N)
                data_i[j] = data[i_minus_j];
        }
        out[i] = data_i[amount];
    }
    return out;
}


// Function: rotate_array
//
// Common code for rotate_array_right and rotate_array_left.
//
// RotateRight - rotate right if true. rotate left if false.
template <auto M, typename T, auto N, typename U, bool RotateRight>
inline T[M] rotate_array(T[N] data, U amount)
{
    static if (N > 1)
    {
        static assert(bitsizeof(U) <= clog2(N));
        // Require U be unsigned
        static assert(cast<U>(-1) > 0);
        // Workaround bug 13575
        const auto N_min_2 = (N >= 2) ? N : 2;

        // Convert U to an array of uint2
        const auto U_Width_2 = (bitsizeof(U) + 1) / 2;
        uint2[U_Width_2] U_uint2 = reinterpret_cast<uint2[U_Width_2]>(concat(0_u1, amount));
        static assert(bitsizeof(U_uint2) == bitsizeof(amount) || bitsizeof(U_uint2) == bitsizeof(amount) + 1);

        // Each iteration, use 2 bits of U to shift
        static for (const auto i : U_Width_2)
        {
            // Constant shifts of last round by 0, 1 << i, 2 << i, and 3 << i.
            T[4][N] rotated_data;
            static for (const auto j : 4)
            {
                const auto Shift = j << (2 * i);
                if (RotateRight)
                {
                    static for (const auto k : N)
                    {
                        rotated_data[j][k] = data[static((k + Shift) % N_min_2)];
                    }
                }
                else
                {
                    // Bias is Shift round up to the nearest multiple N - used to ensure that array index cannot be negative
                    const auto Bias = ((Shift + N) / N) * N;
                    const auto Offset = cast<index_t<N>>((Bias - Shift) % N_min_2);
                    static for (const auto k : N)
                    {
                        rotated_data[j][k] = data[static((k + Offset) % N_min_2)];
                    }
                }
            }

            // Select between possible rotations depending on actual value of U
            static for (const auto k : N)
            {
                data[k] = mux(U_uint2[i], rotated_data[0][k], rotated_data[1][k], rotated_data[2][k], rotated_data[3][k]);
            }
        }
    }

    // Construct output
    T[M] out;
    static for (const auto i : M)
    {
        out[i] = data[i];
    }
    return out;
}


//| Rotate the elements of an arbitrary-typed array right by the given number
// of positions, with the possibility of returning a different number of
// elements as the input array.
// Elements that are rotated past the beginning of the array will wrap-around
// and re-appear at the end of the array. If this wrap-around behaviour is
// not strictly necessary, consider if `shift_array_right` may be more
// efficient.
//
// #### __Examples__
//
// Rotate 5 right by 0:
//
//     >>> uint32[5] a = rotate_array_right({0, 1, 2, 3, 4}, 0);
//     {0, 1, 2, 3, 4}
//
// Rotate 5 right by 2 returning 4:
//
//     >>> uint32[4] a = rotate_array_right({0, 1, 2, 3, 4}, 2);
//     { 2, 3, 4, 0}
template
    < auto M     //< Number of output elements.
    , typename T //< Data type of input array
    , auto N     //< Number of input elements.
    , typename U //< Data type for rotate amount. Must be unsigned. Bitwidth
                 // for this type must be no more than `clog2(N)`. Care should
                 // be taken to minimize this bitwidth where possible since it
                 // affects the size of the multiplexers and hence area.
    >
inline T[M] rotate_array_right(T[N] data, U amount)
{
    // Workaround bug 30568
    // assert(amount < N);

    return rotate_array<M, T, N, U, true>(data, amount);
}


//| Rotate the elements of an arbitrary-typed array left by the given number
// of positions, with the possibility of returning a different number of
// elements as the input array.
// Elements that are rotated past the end of the array will wrap-around
// and re-appear at the beginning of the array. If this wrap-around behaviour
// is not strictly necessary, consider if `shift_array_left` may be more
// efficient.
//
// #### __Examples__
//
// Rotate 5 left by 0:
//
//     >>> uint32[5] a = rotate_array_left({0, 1, 2, 3, 4}, 0);
//     {0, 1, 2, 3, 4}
//
// Rotate 5 left by 2 returning 4:
//
//     >>> uint32[4] a = rotate_array_left({0, 1, 2, 3, 4}, 2);
//     {3, 4, 0, 1}
template
    < auto M     //< Number of output elements.
    , typename T //< Data type of input array.
    , auto N     //< Number of input elements.
    , typename U //< Type for rotate amount. Must be unsigned. Bitwidth of
                 //  this type must be no more than `clog2(N)`. Care should be
                 //  taken to minimize this bitwidth where possible since it
                 //  affects the size of the multiplexers and hence area.
    >
inline T[M] rotate_array_left(T[N] data, U amount)
{
    assert(amount < N);

    return rotate_array<M, T, N, U, false>(data, amount);
}


//| Map array of input values to result values.
//
// @@
//  x[0]  x[1]
//
//   |     |
// +-+-+ +-+-+
// | f | | f | . . .
// +-+-+ +-+-+
//   |     |
//
//  r[0]  r[1]
// @@
//
// #### __Examples__
//
//     >>> map([](bool b){ return !b; }, {true, false, false});
//     {false, true, true}
//
//     >>> map([](uint32 a){ return a + 1; }, {1, 2, 3});
//     {2, 3, 4}
template <typename T, auto N>
inline auto map((T) -> auto f, T[N] x)
{
    using result_t = decltype(f(x[0]));

    result_t[N] r;

    static for(const auto i : N)
    {
        r[i] = f(x[i]);
    }

    return r;
}

//| Map array of indices to result values.
//
// @@
//   0     1
//
//   |     |
// +-+-+ +-+-+
// | f | | f | . . .
// +-+-+ +-+-+
//   |     |
//
//  r[0]  r[1]
// @@
//
// #### __Examples__
//
//     >>> map_indices<3>(id)
//     {0, 1, 2}
template <auto N>
inline auto map_indices((index_t<N>) -> auto f)
{
    return map(f, template indices<N>());
}

//| Right-associative fold of an array.
//
// When `f` is associative `reduce` is more efficient.
//
// #### __Examples__
//
//     >>> foldr(f, {0, 1, 2, 3})
//     f(0, f(1, f(2, 3)));
//
//     >>> foldr(f, {1})
//     1
//
template <typename T, auto N>
inline auto foldr((T, T) -> T f, T[N] x)
{
    static if (N == 1)
    {
        return x[0];
    }
    else
    {
        return f(x[0], foldr(f, tail(x)));
    }
}

//| Left-associative fold of an array.
//
// When `f` is associative `reduce` is more efficient.
//
// #### __Examples__
//
//     >>> foldl(f, {0, 1, 2, 3})
//     f(3, f(2, f(1, 0)));
//
//     >>> foldl(f, {1})
//     1
//
template <typename T, auto N>
inline auto foldl((T, T) -> T f, T[N] x)
{
    static if (N == 1)
    {
        return x[0];
    }
    else
    {
        return f(foldl(f, init(x)), x[N-1]);
    }
}

//| Implement a binary reducer tree using a function to reduce a pair of
// inputs.
//
// @@
//  x[0]  x[1]  x[2]  x[3]
//
//   |     |     |     |           |     |     |     |
// +-+-----+-+ +-+-----+-+       +-+-----+-+ +-+-----+-+
// |    f    | |    f    | . . . |    f    | |    f    |
// +----+----+ +----+----+       +----+----+ +----+----+
//      |           |                 |           |
//      `--.     .--'                 `--.     .--'
//         |     |                       |     |
//       +-+-----+-+                   +-+-----+-+
//       |    f    |                   |    f    |
//       +----+----+                   +----+----+
//            |                             |
//            `---...-----.     .-----...---'
//                        |     |
//                      +-+-----+-+
//                      |    f    |
//                      +----+----+
//                           |
//
//                           r
// @@
//
// #### __Examples__
//
//     >>> reduce(add, {0x9, 0x2, 0x5});
//     0x10
//
//     >>> reduce<bool>([](bool x, bool y){ return x || y; }, {false, true, false});
//     true
//
template <typename T, auto N>
inline auto reduce((T, T) -> T f, T[N] x)
{
    static if (N == 1)
    {
        return x[0];
    }
    else
    {
        // Combine adjacent elements in x
        const auto NewN = (N + 1) / 2;
        T[NewN] new_array;

        static for(const auto i : N/2)
        {
            new_array[i] = f(x[2*i], x[2*i + 1]);
        }

        // carry odd case
        static if ((N % 2) == 1)
        {
            new_array[NewN - 1] = x[N - 1];
        }

        return reduce(f, new_array);
    }
}


//| Implements map-reduce. Inputs are first mapped into the appropriate result
// type, and then reduced to a single output using a binary reduction tree.
//
// #### __Example__
//
//     map_reduce
//         ( [](uint8 a){ return a % 2 == 0; }
//         , [](bool x, bool y){ return x && y; }
//         , {2, 4, 6}
//         );
//     // true; all values are even.
template <typename T, auto N, typename R>
inline R map_reduce((T) -> R map_fn, (R, R) -> R reduce_fn, T[N] x)
{
    return reduce(reduce_fn, map(map_fn, x));
}


//| Returns true if any of the elements are true.
//
// #### __Examples__
//
//     >>> or({true})
//     true
//
//     >>> or({false})
//     false
//
//     >>> or({true,true,false})
//     true
template <auto N>
inline bool or(bool[N] x)
{
    return reduce(
            [](bool x, bool y)
            {
                return x || y;
            },
            x);
}


//| Returns true if all of the elements are true.
//
// #### __Examples__
//
//     >>> and({true})
//     true
//
//     >>> and({false})
//     false
//
//     >>> and({true,true,false})
//     false
template <auto N>
inline bool and(bool[N] x)
{
    return reduce(
            [](bool x, bool y)
            {
                return x && y;
            },
            x);
}


//| Returns true if any of the elements of array satisfies the predicate.
//
// #### __Examples__
//
//     >>> any([](uint8 x){ return x > 3; }, {1, 2, 3});
//     false
//
//     >>> any([](uint8 x){ return x > 3; }, {1, 2, 3, 4});
//     true
template <typename T, auto N>
inline bool any((T) -> bool predicate, T[N] x)
{
    return or(map(predicate, x));
}


//| Returns true if all elements of the array satisfy the predicate.
//
// #### __Examples__
//
//     >>> all([](uint8 x){ return x < 3; }, {1, 2});
//     true
//
//     >>> all([](uint8 x){return x < 3; }, {1, 2, 3});
//     false
template <typename T, auto N>
inline bool all((T) -> bool predicate, T[N] x)
{
    return and(map(predicate, x));
}


//| Combines elements of two arrays using specified function.
//
// @@
//  x[0]  y[0]  x[1]  y[1]
//
//   |     |     |     |
// +-+-----+-+ +-+-----+-+
// |    f    | |    f    | . . .
// +----+----+ +----+----+
//      |           |
//
//     r[0]        r[1]
// @@
//
// #### __Examples__
//
//     >>> zip_with(add, {1, 2, 3}, {4, 5, 6});
//     {5, 7, 9}
//
//     >>> zip_with(make_optional<uint8>, {true, true, false}, {1, 2, 3});
//     {{true, 1}, {true, 2}, {false, 3}}
template <typename S, typename T, auto N>
inline auto zip_with((S, T) -> auto f, S[N] x, T[N] y)
{
    using result_t = decltype(f(x[0], y[0]));

    result_t[N] r;

    static for(const auto i : N)
    {
        r[i] = f(x[i], y[i]);
    }

    return r;
}

//| Map array of input values and their indices to result values.
//
// @@
//   0  x[0]   1  x[1]
//
//   |   |     |   |
// +-+---+-+ +-+---+-+
// |   f   | |   f   | . . .
// +---+---+ +---+---+
//     |         |
//
//    r[0]      r[1]
// @@
//
// #### __Example__
//
//     >>> zip_with_indices( [](index_t<4> i, uint8 byte)
//                           {
//                               return even(i) ? 0xFF : byte;
//                           }
//                         , {0x00, 0xAB, 0xBC, 0xDE}
//                         )
//     {0xFF, 0xAB, 0xFF, 0xDE}
template <typename T, auto N>
inline auto zip_with_indices((index_t<N>, T) -> auto f, T[N] x)
{
    return zip_with(f, template indices<N>(), x);
}

//| Combines two arrays into an array of pairs.
//
// #### __Example__
//
//     >>> zip({true, false, false}, {1, 2, 3});
//     {{true, 1}, {false, 2}, {false, 3}}
template <typename T1, typename T2, auto N>
inline P::pair<T1, T2>[N] zip(T1[N] x, T2[N] y)
{
    return zip_with(P::make_pair<T1, T2>, x, y);
}


//| Returns array with each element value equal to its index.
//
// #### __Example__
//
//     >>> indices<4>();
//     {0, 1, 2, 3}
//
//     >>> uint32[7] a = indices();
//     {0, 1, 2, 3, 4, 5, 6}
template <auto N /*< Array length*/, typename T = index_t<N> /*< Element type*/>
inline T[N] indices()
{
    T[N] x;

    static for(const auto i : N)
    {
        x[i] = i;
    }

    return x;
}

//| Returns minimum element from an array of integers.
//
// #### __Example__
//
//     >>> minimum({5, 3, 87, 22});
//     3
template <typename T, auto N>
inline auto minimum(T[N] x)
{
    return reduce(
            [](T a, T b)
            {
                return a < b ? a : b;
            },
            x);
}


//| Returns maximum value in the array of integers.
//
// #### __Example__
//
//     >>> maximum({5, 3, 87, 22});
//     87
template <typename T, auto N>
inline auto maximum(T[N] x)
{
    return reduce(
            [](T a, T b)
            {
                return a > b ? a : b;
            },
            x);
}


//| Sum elements in an array.
//
// #### __Example__
//
//     >>> sum<uint9>({0xFF, 0x2F, 0x5E});
//     0x18C
template <typename R, typename T, auto N>
inline R sum(T[N] x)
{
    return map_reduce(static_cast<R>, add<R, R>, x);
}


//| Given an array of `optional<T>`, return the first (lowest array index) item
// with `is_valid` true. If there are no items with `is_valid` true, then
// the returned `optional<T>` has 'is_valid' set to false.
//
// #### __Examples__
//
//     >>> first_valid<uint8>({{false, 0xAB}, {false, 0x22}});
//     {false, 0x22}
//
//     >>>  first_valid<uint8>({{true, 0xCD}, {true, 0xFF}});
//     {true, 0xCD}
template <typename T, auto N>
inline optional<T> first_valid(optional<T>[N] x)
{
    return reduce(
            [](optional<T> a, optional<T> b)
            {
                return a.is_valid ? a : b;
            },
            x);
}

//| Copies an array or creates a subset of an array.
//
// #### __Example__
//
//     >>> copy_array({1, 2, 3, 4}, 1, {0xFF, 0xFF, 0xFF}, 0, 2);
//     {2, 3, 0xFF}
template
    < typename T
    , auto N     //< Source array length.
    , auto M     //< Destination array length.
    >
inline T[M] copy_array
    ( T[N] source
    , index_t<N> s_first          //< The index of the first element of the
                                  //  source array to copy to the destination
                                  //  array.
    , T[M] destination
    , index_t<M> d_first          //< The index in the destination array to
                                  //  copy the first element to.
    , count_t<N> elements_to_copy //< The number of array elements to copy
                                  //  from source to destination.
    )
{
    // Array boundary checks:
    assert(elements_to_copy + s_first <= N);
    assert(elements_to_copy + d_first <= M);

    auto src_dest_offset = s_first - d_first;

    static for (const auto i : M)
    {
        if (i >= d_first && i < (d_first + elements_to_copy))
        {
            destination[i] = source[i + src_dest_offset];
        }
    }

    return destination;
}


//| De-duplicate array elements with user-supplied equality predicate.
// For each element e, check the previous elements for duplicates.
// If there are any duplicates, mark element e as invalid.
// Returns an array of optionals where all valid elements are unique.
//
// #### __Example__
//
//     >>> unique_by(order::equal<uint32>, {0, 1, 0});
//     {{true, 0}, {true, 1}, {false, 0}}
template <typename T, auto N>
inline optional<T>[N] unique_by
    ( (T, T) -> bool equality_fn //< Equality predicate.
    , T[N] xs
    )
{
    template <auto I>
    inline optional<T>[N] remove_dups((T, T) -> bool eq_fn, optional<T>[N] ys)
    {
        static assert(I < N);

        static if (I != 0)
        {
            bool[I] dups;
            static for(const auto j : I)
                dups[j] = eq_fn(ys[j].value, ys[I].value);

            ys[I].is_valid = !or(dups);
            return remove_dups<I - 1>(eq_fn, ys);
        }
        else
        {
            return ys;
        }
    }

    return remove_dups<N - 1>(equality_fn, map(Opt::just, xs));
}


//| De-duplicate array elements. Similar to `unique_by` but uses the `==`
// operator. For each element e, check the previous elements for duplicates.
// If there are any duplicates, mark element e as invalid.
// Returns an array of optionals where all valid elements are unique.
//
// #### __Example__
//
//     >>> unique({0, 1, 0});
//     {{true, 0}, {true, 1}, {false, 0}}
template <typename T, auto N>
inline optional<T>[N] unique(T[N] xs)
{
    return unique_by(Ord::equal, xs);
}


//| Element-wise equality comparison of arrays using specified function.
//
// #### __Examples__
//
//     >>> equal_by(optional::equal<uint8>, {{true, 0xFF}}, {{false, 0xFF}});
//     false
//
//     >>> equal_by(pair::equal<uint8, bool>, {{0xAB, true}, {0xCD, false}}, {{0xAB, true}, {0xCD, false}});
//     true
template <typename T, auto N>
inline bool equal_by((T, T) -> bool equality_fn, T[N] xs, T[N] ys)
{
    return and(zip_with(equality_fn, xs, ys));
}


//| Element-wise comparison of arrays for equality using `==`.
//
// #### __Examples__
//
//     >>> equal({0x22, 0x1E, 0xFA}, {0x22, 0x1E, 0xFA});
//     true
//
//     >>> equal({true, false}, {false, false});
//     false
template <typename T, auto N>
inline bool equal(T[N] xs, T[N] ys)
{
    return equal_by(Ord::equal, xs, ys);
}


//| Transform an array into a pair of arrays using a projection.
//
// #### __Examples__
//
//     >>> unzip_with<bool, uint8>(optional_to_pair<uint8>, {{true, 0x33}, {false, 0xFF}});
//     {{true, false}, {0x33, 0xFF}}
template
    < typename L //< Type of first projection of T.
    , typename R //< Type of second projection of T.
    , typename T //< Input array element type.
    , auto N     //< Array length.
    >
inline P::pair<L[N], R[N]> unzip_with
    ( (T) -> P::tuple2<L, R> f  //< Mapping from input array element to pair of
                             //  result elements.
    , T[N] xs
    )
{
    P::pair<L[N], R[N]> result;
    static for(const auto i : N)
    {
        auto p = f(xs[i]);
        result.first[i] = p.first;
        result.second[i] = p.second;
    }
    return result;
}


//| Transform an array of pairs into a pair of arrays.
//
// #### __Example__
//
//     >>> unzip<uint8, bool>({{0xAF, false}, {0xCE, true}, {0x12, false}});
//     {{0xAF, 0xCE, 0x12}, {false, true, false}}
template <typename L, typename R, auto N>
inline P::pair<L[N], R[N]> unzip(P::tuple2<L, R>[N] xs)
{
    return unzip_with<L, R>(id, xs);
}

//| Construct an array with all elements equal to the same value
//
// #### __Example__
//
//     >>> repeat<4>(false);
//     {false, false, false, false}
//
//     >>> uint8[5] a = repeat(0);
//     {0, 0, 0, 0, 0}
template <auto N, typename T>
inline T[N] repeat(T value)
{
    T[N] result;

    static for (const auto i : N)
    {
        result[i] = value;
    }

    return result;
}

//| Construct array of one element with specified value
//
// #### __Example__
//
//     >>> singleton(false);
//     {false}
//
template <typename T>
inline T[1] singleton(T value)
{
    return {value};
}

//| Return an array of repeated applications of `f` to `x`.
//
//     prop> iterate(f, x) == {x, f(x), f(f(x), ...}
//
// #### __Example__
//
//     >>> iterate<4>([](bool x){ return !x; }, true)
//     {true, false, true, false}
//
//     >>> bool[5] a = iterate([](bool x){ return !x; }, true);
//     {true, false, true, false, true}
template <auto N, typename T>
inline T[N] iterate((T) -> T f, T x)
{
    static assert(N > 0);
    T[N] result;
    result[0] = x;
    static for(const auto i : N - 1)
        result[i+1] = f(result[i]);
    return result;
}

//| Access element `i`. Equivalent to `x[i]`. If `i` is greater than or
// equal to `N` then a `assert` is triggered.
//
// #### __Example__
//
//     >>> at({0, 1, 2, 3}, 2)
//     2
template <typename T, auto N>
inline auto at(T[N] x, index_t<N> i)
{
    assert(i < N);
    return x[i];
}

//| Return the first element in the array. Equivalent to `x[0]`.
//
// #### __Example__
//
//     >>> front({0, 1, 2})
//     0
template <typename T, auto N>
inline auto front(T[N] x)
{
    return x[0];
}

//| Return the last element in the vector. Equivalent to `x[N - 1]`.
//
// #### __Example__
//
//     >>> back({0, 1, 2})
//     2
template <typename T, auto N>
inline auto back(T[N] x)
{
    return x[N - 1];
}
