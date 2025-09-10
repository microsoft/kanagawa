// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Operations on variable-length arrays.

This module is intended to be imported qualified, to avoid name clashes with
`data.array` functions. For example:
```
import data.vector as V
```
*/
module data.vector
    { //| # Construction
      vector
    , from_array
    , from_optional
    , make_vector
    , repeat
    , //| # Element access
      at
    , front
    , back
    , //| # Capacity
      size
    , empty
    , capacity
    , reserve
    , //| # Modifiers
      resize
    , clear
    , insert
    , erase
    , push_back
    , pop_back
    , append
    , //| # Algorithm
      map
    , gather
    , scatter
    , sharded_for_each
    , sharded_map
    , unique_by
    , unique
    , //| ## Reduce
      reduce
    , map_reduce
    , or
    , and
    , any
    , all
    , minimum
    , maximum
    , sum
    , first_valid
    , last_valid
    , map_optional
    , cat_optionals
    , //| ## Zip and unzip
      zip_with
    , zip
    , unzip_with
    , unzip
    , //| ## Scan
      inclusive_scan
    , prefix_sum
    , //| ## Compare
      equal_by
    , equal
    }

import .options as opt
import data.array as A
import data.function
import data.optional as Opt
import data.order as Ord
import data.tuple as P
import sync.atomic
import numeric.int.operator
import numeric.int.operator.unsigned as unsigned
import type.stdtype

template <typename T, auto N /*< Maximum size.*/>
struct vector
{
    T[N] data;
    count_t<N> size;
}

//| Create a vector with specified data and size
template <typename T, auto N>
inline vector<T, N> make_vector
    ( T[N] a        // Data to store in the vector.
    , count_t<N> s  // Number of valid elements in the vector.
    )
{
    assert(s <= N);

    return {a, s};
}


//| Construct a vector from an array.
//
// #### __Example__
//
//     >>> from_array<uint8>({1, 2, 3})
//     {{1, 2, 3}, 3}
template <typename T, auto N>
inline vector<T, N> from_array(T[N] a)
{
    return make_vector(a, N);
}

//| Construct a vector with capacity 1 from an optional. If the optional is
// invalid then return an empty vector otherwise return a singleton vector.
//
// #### __Example__
//
//     >>> from_optional({true, 0xFF})
//     {{0xFF}, 1}
template <typename T>
inline vector<T, 1> from_optional(optional<T> o)
{
    return make_vector({o.value}, o.is_valid ? 1 : 0);
}

//| Construct a vector by replicating a value `length` times.
//
// #### __Example__
//
//     >>> repeat<3>(3, 2)
//     {{3, 3, 0}, 2}
//
//     >>> vector<uint8, 2> v = repeat(2, 2)
//     {{2, 2}, 2}
template <auto N, typename T>
inline vector<T, N> repeat(T x, count_t<N> length)
{
    assert(length <= N);
    vector<T, N> result;
    static for(const auto i : N)
        if (i < length)
            result.data[i] = x;
    return resize(result, length);
}

//| Access element `i`. Equivalent to `v.data[i]`. If `i` is greater than or
// equal to `v.size` then a `assert` is triggered.
//
// #### __Example__
//
//     >>> at({{0, 1, 2, 3}, 4}, 2)
//     2
template <typename T, auto N>
inline auto at(vector<T, N> v, index_t<N> i)
{
    assert(i < v.size);
    return v.data[i];
}

//| Return the first element in the vector. Equivalent to `v.data[0]`. If the
// vector is empty then a `assert` is triggered.
//
// #### __Example__
//
//     >>> front({{0, 1, 2}, 2})
//     0
template <typename T, auto N>
inline auto front(vector<T, N> v)
{
    assert(v.size > 0);
    return at(v, 0);
}

//| Return the last element in the vector. Equivalent to `v.data[v.size - 1]`.
// If the vector is empty then a `assert` is triggered.
//
// #### __Example__
//
//     >>> back({{0, 1, 2}, 2})
//     1
template <typename T, auto N>
inline auto back(vector<T, N> v)
{
    return at(v, unsigned::decrement(v.size));
}

//| Return the number of elements. Equivalent to `v.size`.
//
// #### __Example__
//
//     >>> size({{1, 2, 3}, 2})
//     2
template <typename T, auto N>
inline count_t<N> size(vector<T, N> v)
{
    return v.size;
}

//| Check whether the vector is empty. Equivalent to `v.size == 0`.
//
// #### __Examples__
//
//     >>> empty({{0, 1, 2}, 0})
//     true
//
//     >>> empty({{0, 1, 2}, 1})
//     false
template <typename T, auto N>
inline bool empty(vector<T, N> v)
{
    return v.size == 0;
}

//| Return the maximum number of elements.
//
// #### __Example__
//
//     >>> capacity({{0, 1, 2}, 0})
//     3
template <typename T, auto N>
inline count_t<N> capacity(vector<T, N> v)
{
    return N;
}

//| Reserve `M` number of elements. If `M` is less than `v.size` then a
// `assert` is triggered. Reserving the same capacity as the input vector
// returns the orginal vector.
//
// #### __Examples__
//
//     >>> reserve<5>({{0, 1, 2}, 3})
//     {{0, 1, 2, 0, 0}, 3}
//
//     >>> reserve<3>({{0, 1, 2, 3, 4}, 3})
//     {{0, 1, 2}, 3}
template <auto M, typename T, auto N>
inline vector<T, M> reserve(vector<T, N> v)
{
    assert(v.size <= M);
    const auto Bound = M < N ? M : N;
    vector<T, M> new;
    new.size = v.size;
    static for(const auto i : Bound)
        new.data[i] = v.data[i];
    return new;
}

//| Resize the vector to contain `length` elements. Equivalent to `v.size = length;`.
// If `length > v.size` then new elements are undefined. If `length` is greater
// than `N` then a `assert` is triggered.
//
// #### __Example__
//
//     >>> resize({{0, 1, 2, 3}, 4}, 2)
//     {{0, 1, 2, 3}, 2}
template <typename T, auto N>
inline vector<T, N> resize(vector<T, N> v, count_t<N> length)
{
    assert(length <= N);
    v.size = length;
    return v;
}

//| Clear the contents. Equivalent to `v.size = 0;`.
//
// #### __Example__
//
//     >>> clear({{0, 1, 2, 3}, 4})
//     {{0, 1, 2, 3}, 0}
template <typename T, auto N>
inline vector<T, N> clear(vector<T, N> v)
{
    return resize(v, 0);
}

//| Insert `value` before position `pos` in vector `v`. If `pos > v.size` or
// `v.size >= N` then a `assert` is triggered.
//
// #### __Example__
//
//     >>> insert({{0, 0, 0, 0}, 3}, 2, 0x20)
//     {{0, 0, 0x20, 0}, 4}
//
//     >>> insert({{0xFF, 0xFF}, 0}, 0, 0)
//     {{0, 0xFF}, 1}
template <typename T, auto N>
inline vector<T, N> insert(vector<T, N> v, index_t<N> pos, T value)
{
    assert(pos <= v.size && v.size < N);

    // Note that when appending an eleement to the end of the array
    // `copy_array` will copy 0 elements.  `static_cast` is used because
    // the destination offset computation will wrap (but will not used) in that case.
    v.data = A::copy_array(v.data, pos, v.data, static_cast(pos + 1), checked_cast(v.size - pos));
    v.data[pos] = value;
    v.size++;
    return v;
}

//| Erase element at position `pos` from vector `v`. If `pos >= v.size` or
// the vector is empty then a `assert` is triggered.
//
// #### __Example__
//
//     >>> erase(1, {{0, 1, 2}, 3})
//     {{0, 2, 2}, 2}
template <typename T, auto N>
inline vector<T, N> erase(vector<T, N> v, index_t<N> pos)
{
    assert(pos < v.size && v.size > 0);
    v.data = A::copy_array(v.data, checked_cast(pos + 1), v.data, pos, checked_cast(v.size - pos - 1));
    v.size--;
    return v;
}

//| Append element `x` to the end of vector `v`. If `v.size >= N` then a
// `assert` is triggered.
//
// #### __Example__
//
//     >>> push_back(0xFF, {{0, 1, 2}, 2})
//     {{0, 1, 0xFF}, 3}
template <typename T, auto N>
inline vector<T, N> push_back(T x, vector<T, N> v)
{
    assert(v.size < N);
    v.data[v.size] = x;
    v.size++;
    return v;
}

//| Remove the last element of the vector. If the vector is empty then a
// `assert` is triggered.
//
// #### __Example__
//
//     >>> pop_back({{0, 1, 2, 3}, 2})
//     {{0, 1, 2, 3}, 1}
template <typename T, auto N>
inline vector<T, N> pop_back(vector<T, N> v)
{
    assert(v.size > 0);
    v.size--;
    return v;
}

//| Append vectors. If the sum of the vector sizes is greater than `N` then a
// `assert` is triggered.
//
// #### __Examples__
//
//     >>> append({{1, 2, 0, 0}, 2}, {{3, 4, 0, 0}, 2})
//     {{1, 2, 3, 4}, 4}
//
//     >>> append({{1, 2, 3}, 2}, {0xFF, 0xFF, 0xFF}, 0})
//     {{1, 2, 3}, 2}
//
//     >>> append({{1, 2, 3}, 0}, {{0xFF, 0xFF, 0xFF}, 2})
//     {{0xFF, 0xFF, 3}, 2}
template <typename T, auto N>
inline vector<T, N> append(vector<T, N> x, vector<T, N> y)
{
    assert(x.size + y.size <= N);
    x.data = A::copy_array(y.data, 0, x.data, checked_cast(x.size), y.size);
    return resize(x, checked_cast(x.size + y.size));
}

//| Map vector of input values to result values.
//
// @@
//  x[0]  x[1]        x[x.size "-"1]
//
//   |     |           |
// +-+-+ +-+-+       +-+-+
// | f | | f | . . . | f |
// +-+-+ +-+-+       +-+-+
//   |     |           |
//
//  r[0]  r[1]        r[r.size "-"1]
// @@
//
// #### __Example__
//
//     >>> map(increment, {{1, 2, 3, 4}, 3})
//     {{2, 3, 4, 0}, 3}
template <typename T, auto N>
inline auto map((T) -> auto f, vector<T, N> x)
{
    using result_t = decltype(f(x.data[0]));
    result_t[N] r;
    static for(const auto i : N)
        if (i < x.size)
            r[i] = f(x.data[i]);
    return resize(from_array(r), x.size);
}

//| Gather entries in data that are marked as valid to the front of an array,
// maintaining ordering, and return the resulting array.
//
// #### __Example__
//
//     data = {1, 2, 3, 4, 5, 6, 7, 8}
//     valids = {false, true, false, true, true, false, false, true}
//     result = { {2, 4, 5, 8, X, X, X, X}, 4 } where X is undefined
template <typename T, auto N>
inline vector<T, N> gather(bool[N] valids, T[N] data)
{
    // Prefix sum to number each valid entry
    auto valids_prefix_sum = A::prefix_sum<count_t<N>>(cast<uint1[N]>(valids));

    // Gather into results array
    T[N] result;
    static for (const auto i : N)
    {
        // Mark data entries that could be the i-th valid
        optional<T>[N] finds;
        static for (const auto j : N)
        {
            // Only consider entries that are after i since the (i-1)th
            // entry of a prefix_sum of a uint1 array cannot exceed i
            if (j >= i)
            {
                // Examine valids_prefix_sum[j-1] for efficiency, with
                // exception for j == 0
                if (j == 0)
                    finds[j] = Opt::make_optional(0 == i, data[j]);
                else
                    finds[j] = Opt::make_optional(valids_prefix_sum[j-1] == i, data[j]);
            }
        }
        // Select the last one, since this entry (position i-1) is the one
        // that causes the next entry (position i) to increment
        auto find = A::last_valid<T>(finds);
        result[i] = find.value;
    }

    return make_vector(result, valids_prefix_sum[N-1]);
}

//| Scatter the entries in data into the entries marked as true in the valid
// array.
//
// #### __Example__
//
//     data = {2, 4, 5, 8, 0, 0, 0, 0}
//     valids = {false, true, false, true, true, false, false, true}
//     result = { {X, 2, X, 4, 5, X, X, 8 }, 4 } where X is undefined
template <typename T, auto N>
inline vector<T, N> scatter(bool[N] valids, T[N] data)
{
    // Prefix sum to number valid entries in result
    auto valids_prefix_sum = A::prefix_sum<count_t<N>>(cast<uint1[N]>(valids));

    // Scatter into results array
    T[N] result;
    static for (const auto i : N)
    {
        if (valids[i])
        {
            if (i == 0)
                result[i] = data[0];
            else
                result[i] = data[valids_prefix_sum[i-1]];
        }
    }

    return make_vector(result, valids_prefix_sum[N-1]);
}

//| De-duplicate vector elements with user-supplied equality predicate.
// For each element e, check the previous elements for duplicates.
// If there are any duplicates, mark element e as invalid.
// Return a vector of optionals where all valid elements are unique.
//
// #### __Example__
//
//     >>> unique_by(optional::equal, {{{true, 1}, {false, 1}, {true, 1}}, 3})
//     {{{true, {true, 1}}, {true, {false, 1}}, {false, {true, 1}}}, 3}
template <typename T, auto N>
inline vector<optional<T>, N> unique_by((T, T) -> bool equality_fn, vector<T, N> v)
{
    auto es = A::zip_with_indices( [v](index_t<N> i, T x)
                                   {
                                       return make_optional(i < v.size, x);
                                   }
                                 , v.data
                                 );

    auto xs = A::unique_by( [equality_fn](optional<T> x, optional<T> y)
                            {
                                return Opt::equal_by(equality_fn, x, y);
                            }
                          , es
                          );
    return make_vector(A::map(Opt::join<T>, xs), v.size);
}

//| De-duplicate vector elements. Similar to `unique_by` but uses the `==`
// operator. For each element e, check the previous elements for duplicates.
// If there are any duplicates, mark element e as invalid.
// Return a vector of optionals where all valid elements are unique.
//
// #### __Examples__
//
//     >>> unique({{1}, 1})
//     {{{true, 1}}, 1}
//
//     >>> unique({{1}, 0})
//     {{{false, 0}}, 0}
//
//     >>> unique({{1, 2}, 2})
//     {{{true, 1}, {true, 2}}, 2}
//
//     >>> unique({{1, 1}, 2})
//     {{{true, 1}, {false, 1}}, 2}
//
//     >>> unique({{1, 1}, 1})
//     {{{true, 1}, {false, 0}}, 1}
//
//     >>> unique({{1, 2, 3, 2}, 4})
//     {{{true, 1}, {true, 2}, {true, 3}, {false, 2}}, 4}
//
//     >>> unique({{1, 2, 3, 2}, 2})
//     {{{true, 1}, {true, 2}, {false, 0}, {false, 0}}, 2}
template <typename T, auto N>
inline vector<optional<T>, N> unique(vector<T, N> v)
{
    return unique_by(Ord::equal, v);
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
//     >>> reduce(add, {{0x9, 0x2, 0x5}, 3});
//     0x10
//
//     >>> reduce<bool>([](bool x, bool y){ return x || y; }, {{false, true, false}, 2});
//     true
//
//     >>> reduce(add<uint8, uint8>, {{2}, 1})
//     2
//
//     >>> reduce(add<uint8, uint8>, {{1, 2, 3}, 2})
//     3
template <typename T, auto N>
inline auto reduce((T, T) -> T f, vector<T, N> v)
{
    optional<T>[N] values;
    static for(const auto i : N)
        values[i] = make_optional(i < v.size, v.data[i]);

    optional<T> result = A::reduce
                     ( [f](optional<T> l, optional<T> r)
                       {
                           assert(!r.is_valid || l.is_valid);
                           if (l.is_valid && r.is_valid)
                               l = make_optional(true, f(l.value, r.value));
                           return l;
                       }
                     , values
                     );
    T uninit;
    return Opt::from_optional(uninit, result);
}

//| Implements map-reduce. Inputs are first mapped into the appropriate result
// type `R` and then reduced to a single output using a binary reduction tree.
//
// #### __Example__
//
//     >>> map_reduce
//             ( [](uint8 a){ return a % 2 == 0; }
//             , [](bool x, bool y){ return x && y; }
//             , {{2, 4, 7}, 2}
//             );
//     true
template <typename R, typename T, auto N>
inline R map_reduce((T) -> R map_fn, (R, R) -> R reduce_fn, vector<T, N> v)
{
    return reduce(reduce_fn, map(map_fn, v));
}

//| Return true if any of the elements are true. Return false if the vector is empty.
//
// #### __Examples__
//
//     >>> or({{false, false, true}, 3})
//     true
//
//     >>> or({{false, false, true}, 2})
//     false
//
//     >>> or({{false, false, true}, 0})
//     false
template <auto N>
inline bool or(vector<bool, N> v)
{
    return empty(v) ? false : reduce([](bool x, bool y){ return x || y; }, v);
}

//| Return true if all of the elements are true. Return true if the vector is empty.
//
// #### __Examples__
//
//     >>> and({{true, true, false}, 3})
//     false
//
//     >>> and({{true, true, false}, 2})
//     true
//
//     >>> and({{true, true, false}, 0})
//     true
template <auto N>
inline bool and(vector<bool, N> v)
{
    return empty(v) ? true : reduce([](bool x, bool y){ return x && y; }, v);
}

//| Return true if any of the elements satisfy the predicate. Return false if
// the vector is empty.
//
// #### __Examples__
//
//     >>> any(even, {{1, 3, 6}, 3})
//     true
//
//     >>> any(even, {{1, 3, 6}, 2})
//     false
//
//     >>> any(even, {{1, 3, 6}, 0})
//     false
template <typename T, auto N>
inline bool any((T) -> bool predicate, vector<T, N> v)
{
    return or(map(predicate, v));
}

//| Return true if all of the elements satisfy the predicate. Return true if
// the vector is empty.
//
// #### __Examples__
//
//     >>> all(odd, {{1, 3, 6}, 3})
//     false
//
//     >>> all(odd, {{1, 3, 6}, 2})
//     true
//
//     >>> all(odd, {{1, 3, 6}, 0})
//     true
template <typename T, auto N>
inline bool all((T) -> bool predicate, vector<T, N> v)
{
    return and(map(predicate, v));
}

//| Return the minimum element from a vector of integers. Return undefined if
// the vector is empty.
//
// #### __Examples__
//
//     >>> minimum({{4, 64, 2}, 3})
//     2
//
//     >>> minimum({{4, 64, 2}, 2})
//     4
template <typename T, auto N>
inline auto minimum(vector<T, N> v)
{
    return reduce(
            [](T a, T b)
            {
                return a < b ? a : b;
            },
            v);
}

//| Return the maximum element from a vector of integers. Return undefined if
// the vector is empty.
//
// #### __Examples__
//
//     >>> maximum({5, -23, 182}, 3})
//     182
//
//     >>> maximum({5, -23, 182}, 2})
//     5
template <typename T, auto N>
inline auto maximum(vector<T, N> v)
{
    return reduce(
            [](T a, T b)
            {
                return a > b ? a : b;
            },
            v);
}

//| Sum of the vector elements. Return undefined if the vector is empty.
//
// #### __Examples__
//
//     >>> sum({{3, -9, 7}, 3})
//     1
//
//     >>> sum({{3, -9, 7}, 2})
//     -6
template <typename R, typename T, auto N>
inline R sum(vector<T, N> v)
{
    return map_reduce(static_cast<R>, add<R, R>, v);
}

//| Given a vector of `optional<T>`, return the first (lowest vector index)
// valid element. If there are no valid elements or the vector is empty then
// return invalid.
//
// #### __Examples__
//
//     >>> first_valid({{{true, 1}, {false, 2}, {true, 3}}, 3})
//     {true, 1}
//
//     >>> first_valid({{{false, 1}, {false, 2}, {true, 3}}, 2})
//     {false, 2}
//
//     >>> first_valid({{{true, 1}, {false, 2}, {true, 3}}, 0})
//     {false, 0}
template <typename T, auto N>
inline optional<T> first_valid(vector<optional<T>, N> v)
{
    return reduce(
            [](optional<T> a, optional<T> b)
            {
                return a.is_valid ? a : b;
            },
            v);
}

//| Given a vector of `optional<T>`, return the last (highest vector index)
// valid element. If there are no valid elements or the vector is empty then
// return invalid.
//
// #### __Examples__
//
//     >>> last_valid({{{true, 1}, {false, 2}, {true, 3}}, 3})
//     {true, 3}
//
//     >>> last_valid({{{true, 1}, {false, 2}, {true, 3}}, 2})
//     {true, 1}
//
//     >>> last_valid({{{false, 1}, {false, 2}, {true, 3}}, 2})
//     {false, 1}
//
//     >>> last_valid({{{true, 1}, {false, 2}, {true, 3}}, 0})
//     {false, 0}
template <typename T, auto N>
inline optional<T> last_valid(vector<optional<T>, N> v)
{
    return reduce(
            [](optional<T> a, optional<T> b)
            {
                return b.is_valid ? b : a;
            },
            v);
}

//| Return a vector containing only valid mapped elements.
//
// #### __Example__
//
//     >>> map_optional( [](uint8 x)
//                       {
//                           return make_optional(odd(x), x);
//                       }
//                     , {{1, 2, 3}, 3}
//                     )
//     {{1, 3, 0}, 2}
//
//     >>> map_optional( [](uint8 x)
//                       {
//                           return make_optional(even(x), x);
//                       }
//                     , {{1, 2, 3}, 3}
//                     )
//     {{2, 3, 0}, 1}
template <typename R, typename T, auto N>
inline vector<R, N> map_optional((T) -> optional<R> f, vector<T, N> v)
{
    auto opts   = map(f, v);
    auto elems  = A::map([](optional<R> x){ return x.value; }, opts.data);
    auto valids = A::zip_with_indices( [opts](index_t<N> i, optional<R> x)
                                       {
                                           return i < opts.size && x.is_valid;
                                       }
                                     , opts.data
                                     );
    return gather(valids, elems);
}

//| Return a vector containing only valid input elements.
//
// #### __Examples__
//
//     >>> cat_optionals({{{false, 0}, {true, 1}, {true, 2}}, 3)
//     {{1, 2, 0}, 2}
//
//     >>> cat_optionals({{{true, true}, {true, false}, {true, true}}, 2})
//     {{true, false, false}, 2}
template <typename T, auto N>
inline vector<T, N> cat_optionals(vector<optional<T>, N> v)
{
    return map_optional<T>(id, v);
}

//| Combine elements of two vectors using specified function. Any remaining
// elements of the longer vector are dropped. The capacity of the zipped
// vector is the minimum of `N` and `M`.
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
//     >>> zip_with(add, {{1, 2, 3, 4}, 3}, {{1, 2, 3, 4, 5}, 4})
//     {{2, 4, 6, 0}, 3}
//
//     >>> zip_with(add, {{1, 2, 3, 4}, 3}, {{0}, 0})
//     {{0}, 0}
//
//     >>> zip_with(make_optional, {{true, false, true}, 3}, {{-22, 5}, 2})
//     {{{true, -22}, {false, 5}}, 2}
template <typename S, auto N, typename T, auto M>
inline auto zip_with((S, T) -> auto f, vector<S, N> s, vector<T, M> t)
{
    using result_t = decltype(f(s.data[0], t.data[0]));
    const auto Min = N < M ? N : M;
    result_t[Min] rs;
    auto min_size = Ord::min(s.size, t.size);
    static for(const auto i : Min)
        if (i < min_size)
            rs[i] = f(s.data[i], t.data[i]);
    return resize(from_array(rs), checked_cast(min_size));
}

//| Combine two vectors into a vector of pairs. Any remaining
// elements of the longer vector are dropped. The maximum size of the zipped
// vector is the minimum of `N` and `M`.
//
// #### __Examples__
//
//     >>> zip({{1, 2}, 2}, {{true, false}, 2})
//     {{{1, true}, {2, false}}, 2}
//
//     >>> zip({{1, 2}, 2}, {{1, 2, 3}, 3})
//     {{{1, 1}, {2, 2}}, 2}
template <typename S, auto N, typename T, auto M>
inline auto zip(vector<S, N> s, vector<T, M> t)
{
    return zip_with(P::make_pair<S, T>, s, t);
}

//| Transform a vector `v` into a pair of vectors using a projection `f`.
//
// #### __Example__
//
//     >>> unzip_with(optional_to_pair, {{{true, 1}, {false, 2}, {true, 3}}, 2})
//     {{{true, false, false}, 2}, {{1, 2, 0}, 2}}
template <typename L, typename R, typename T, auto N>
inline pair<vector<L, N>, vector<R, N>> unzip_with
    ( (T) -> P::tuple2<L, R> f
    , vector<T, N> v
    )
{
    pair<vector<L, N>, vector<R, N>> result;
    static for(const auto i : N)
        if (i < v.size)
        {
            auto p = f(v.data[i]);
            result.first.data[i]  = p.first;
            result.second.data[i] = p.second;
        }
    result.first  = resize(result.first,  v.size);
    result.second = resize(result.second, v.size);
    return result;
}

//| Transform a vector of pairs into a pair of vectors.
//
// #### __Example__
//
//     >>> unzip({{{0, 0}, {-1, 1}, {-2, 2}, {-3, 3}}, 4})
//     {{{0, -1, -2, -3}, 4}, {{0, 1, 2, 3}, 4}}
//
//     >>> unzip({{{0, 0}, {-1, 1}, {-2, 2}, {-3, 3}}, 3})
//     {{{0, -1, -2, 0}, 3}, {{0, 1, 2, 0}, 3}}
//
//     >>> unzip({{{0, 0}, {-1, 1}, {-2, 2}, {-3, 3}}, 0})
//     {{{0, 0, 0, 0}, 0}, {{0, 0, 0, 0}, 0}}
template <typename L, typename R, auto N>
inline pair<vector<L, N>, vector<R, N>> unzip(vector<pair<L, R>, N> v)
{
    return unzip_with<L, R>(id, v);
}

//| Perform an inclusive scan on the given vector.
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
//     >>> inclusive_scan({{1, 2, 3}, 3}, add)
//     {{1, 3, 6}, 3}
template <typename R, typename T, auto N>
inline vector<R, N> inclusive_scan(vector<T, N> input, (R, R) -> R AssociativeFn)
{
    // Two sets of results are kept - one set from the previous iteration and the other
    // set is the values to use on the next iteration.  Each pass through the outer for
    // loop below switches back and forth between which set is for the next versus
    // previous iteration.  This avoids copying between two arrays after each iteration.
    R[2][N] results;

    static for (const auto i : N)
    {
        results[0][i] = cast<R>(input.data[i]);
    }

    static for (const auto j : clog2(N))
    {
        const uint1 prev_index = j % 2;
        const uint1 next_index = (j + 1) % 2;

        static for (const auto i : N)
            if (i < input.size)
            {
                const auto shifted_by_j = 1 << j;
                if (i < shifted_by_j)
                    results[next_index][i] = results[prev_index][i];
                else
                {
                    const auto idx = i - shifted_by_j;
                    results[next_index][i] = AssociativeFn(results[prev_index][i], results[prev_index][idx]);
                }
            }
    }

    return make_vector(results[clog2(N) % 2], input.size);
}

//| Compute the prefix sum of the given vector.
//
// #### __Examples__
//
//     >>> prefix_sum({{0, 1, 2, 3}, 4})
//     {{0, 1, 3, 6}, 4}
//
//     >>> prefix_sum({{0, 1, 2, 3}, 3})
//     {{0, 1, 3, 3}, 3}
//
//     >>> prefix_sum({{0, 1, 2, 3, 4}, 3})
//     {{0, 1, 3, 0, 0}, 3}
//
//     >>> prefix_sum({{5}, 1})
//     {{5}, 1}
//
//     >>> prefix_sum({{5}, 0})
//     {{5}, 0}
//
//     >>> prefix_sum({{3, 2, 1}, 2})
//     {{3, 5, 1}, 2}
template <typename R, typename T, auto N>
inline vector<R, N> prefix_sum(vector<T, N> v)
{
    return inclusive_scan(v, add<R, R>);
}

//| Element-wise equality comparison of vectors using specified function.
// Return false if the vectors differ in size.
//
// #### __Examples__
//
//     >>> equal_by( optional::equal
//                 , {{{true, 0x23}, {false, 0xFF}, {true, 0x19}, {false, 0x20}}, 4}
//                 , {{{true, 0x23}, {false, 0x00}, {true, 0x19}}, 3}
//                 )
//     false
//
//     >>> equal_by( optional::equal
//                 , {{{true, 0x23}, {false, 0xFF}, {true, 0x19}, {false, 0x20}}, 3}
//                 , {{{true, 0x23}, {false, 0x00}, {true, 0x19}}, 3}
//                 )
//     true
template <typename T, auto N, auto M>
inline bool equal_by((T, T) -> bool equality_fn, vector<T, N> x, vector<T, M> y)
{
    return x.size == y.size && and(zip_with(equality_fn, x, y));
}

//| Element-wise comparison of vectors for equality using `==`. Return false
// if the vectors differ in size.
//
// #### __Examples__
//
//     >>> equal({{1, 2, 3}, 3}, {{1, 2, 3, 4}, 4})
//     false
//
//     >>> equal({{1, 2, 3}, 3}, {{1, 2, 3, 4}, 3})
//     true
template <typename T, auto N, auto M>
inline bool equal(vector<T, N> x, vector<T, M> y)
{
    return equal_by(Ord::equal, x, y);
}

//| Helper function used by sharded_* functions Determines the set of input
// elements which are assigned to `shard_idx`.  Returns a vector of index
// values, where each index is an index into the input vector.
template
    < auto Shards                                       //< Number of replicas to `body` to instantiate.
    , auto N                                            //< Maximum number of input/output elements.
    , typename T                                        //< Type of each input element.
                                                        // Caller must ensure this limit is not exceeded.
    >
inline vector<index_t<N>, N> gather_shard_indices
    ( vector<index_t<Shards>, N> shard_ids              //< Shard IDs associated with each input element.
    , index_t<Shards> shard_idx                         //< Index of shard to match.
    , vector<T, N> x                                    //< Input data.
    )
{
    assert(shard_ids.size == x.size);

    // Determine which inputs are assigned to this shard
    bool[N] this_shard_mask = A::map_indices<N>(
        [shard_ids, shard_idx, x](index_t<N> element_idx) -> bool
        {
            return (element_idx < x.size) && (shard_ids.data[element_idx] == shard_idx);
        }
    );

    // Gather indices of elements assigned to this shard
    return gather(this_shard_mask, A::indices<N>());
}

//| Transform a vector containing up to `N` elements according to a
// caller-specified function (`f`).  Computation is spread across `Shards`
// instances of `f`. `get_shard_idx` is used to determine which instance
// processes each element.  Each instance can transform at most one input
// element per cycle.
// Return type is a vector with the same size as the input vector.
template
    < auto Shards                                       //< Number of replicas to `f` to instantiate.
    , auto N                                            //< Maximum number of input/output elements.
    , typename T                                        //< Type of each input element.
    , auto MaxCallerThreads = opt::max_threads_limit    //< Maximum number of threads concurrently executing inside of `parallel_map`.
                                                        // Caller must ensure this limit is not exceeded.
    >
inline auto sharded_map
    ( (index_t<Shards>, T)->auto f                      //< Function used to transform input elements.
    , (T)->index_t<Shards> get_shard_idx                //< Function used to determine which hardware instance processes a given input element.
    , vector<T, N> x                                    //< Input data.
    )
{
    // Type alias for the return type
    using R = decltype(f(0, {}));

    // Determine which input each shard is assigned to
    vector<index_t<Shards>, N> shard_ids = map(get_shard_idx, x);

    // Broadcast inputs to all replicas
    optional<R>[Shards][N] per_shard_results = parallel_map<Shards, optional<R>[N], MaxCallerThreads>(
        Shards,
        [f, shard_ids, x](index_t<Shards> shard_idx)
        {
            // Determine which inputs are assigned to this shard
            vector<index_t<N>, N> gathered_indices = gather_shard_indices(shard_ids, shard_idx, x);

            // Process each input element assigned to this shard
            optional<R>[N] results = {};

            // Ensure that shard index passed to f is constexpr
            static for (const auto ShardIndex : Shards)
            {
                if (static (ShardIndex == shard_idx))
                {
                    results = pipelined_last(gathered_indices.size, [gathered_indices, x, f](index_t<N> i)
                    {
                        bool is_first = (i == 0);

                        index_t<N> index = gathered_indices.data[i];

                        T input_element = x.data[index];

                        R result = f(static(ShardIndex), input_element);

                        return second(atomically([result, is_first, index](optional<R>[N] prev)
                        {
                            optional<R>[N] curr = prev;

                            if (is_first)
                            {
                                static for (const auto i : N)
                                {
                                    curr[i].is_valid = false;
                                }
                            }

                            curr[index] = Opt::just(result);

                            return curr;
                        }));
                    });
                }
            }

            return results;
        });

    // Combine results from all shards
    vector<R, N> result = {};

    result.size = x.size;

    optional<R>[N][Shards] transposed = A::transpose(per_shard_results);

    result.data = A::map_indices([transposed, result](index_t<N> e)
    {
        optional<R>[Shards] row = transposed[e];

        optional<R> selected = A::first_valid(row);

        // if (i < result.size), then exactly one element in `row` will be valid
        assert(selected.is_valid || (e >= result.size));

        return selected.value;
    });

    return result;
}

//| Execute a caller-specified function (`f`) once for each input element.
// Computation is spread across `Shards` instances of `f`. `get_shard_idx` is
// used to determine which instance processes each element.  Each instance can
// transform at most one input element per cycle.
template
    < auto Shards                                       //< Number of replicas to `f` to instantiate.
    , auto N                                            //< Maximum number of input/output elements.
    , typename T                                        //< Type of each input element.
    , auto MaxCallerThreads = opt::max_threads_limit    //< Maximum number of threads concurrently executing inside of `parallel_map`.
                                                        // Caller must ensure this limit is not exceeded.
    >
inline void sharded_for_each
    ( (index_t<Shards>, T)->void f                      //< Function used to process input elements.
    , (T)->index_t<Shards> get_shard_idx                //< Function used to determine which hardware instance processes a given input element.
    , vector<T, N> x                                    //< Input data.
    )
{
    // Determine which input each shard is assigned to
    vector<index_t<Shards>, N> shard_ids = map(get_shard_idx, x);

    // Broadcast inputs to all replicas
    parallel_for<Shards, MaxCallerThreads>(
        Shards,
        [f, shard_ids, x](index_t<Shards> shard_idx)
        {
            // Determine which inputs are assigned to this shard
            vector<index_t<N>, N> gathered_indices = gather_shard_indices(shard_ids, shard_idx, x);

            // Ensure that shard index passed to f is constexpr
            static for (const auto ShardIndex : Shards)
            {
                if (static (ShardIndex == shard_idx))
                {
                    // Process each input element assigned to this shard
                    pipelined_for(gathered_indices.size, [gathered_indices, x, f](index_t<N> i)
                    {
                        index_t<N> index = gathered_indices.data[i];

                        T input_element = x.data[index];

                        f(static(ShardIndex), input_element);
                    });
                }
            }
        });
}
