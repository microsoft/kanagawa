// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include <mlir/IR/Builders.h>

#include <algorithm>
#include <array>
#include <atomic>
#include <cctype>
#include <chrono>
#include <condition_variable>
#include <fstream>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <mutex>
#include <numeric>
#include <random>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <stdint.h>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <boost/container/list.hpp>
#include <boost/multiprecision/cpp_int.hpp>
#include <boost/optional.hpp>
#include <boost/optional/optional_io.hpp>

// Use this list implementation to ensure
// size() is constant time.  Old C++ libraries (like the one on CentOS 7)
// have an implementation of size() that runs in linear time.
template <typename T> using boost_list = boost::container::list<T>;

std::string GetStackTrace();

// An exception like std::runtime_error
// that includes a stack trace in the error message
class RuntimeErrorWithTrace : public std::runtime_error
{
  public:
    RuntimeErrorWithTrace(const std::string& errorMessage)
        : std::runtime_error(errorMessage + "\nCompiler Internal Call Stack:\n" + GetStackTrace())
    {
    }
};

void OnAssertionFailure(const char* const fileName, const size_t lineNumber);

template <typename T>
inline void KanagawaAssert(const T conditionValue, const char* const fileName, const size_t lineNumber)
{
    if (!conditionValue)
    {
        OnAssertionFailure(fileName, lineNumber);
    }
}

// Save/restore C++ stream attributes (like std::dec/std::hex)
template <class CharT> class SaveIosFmt
{
    std::basic_ios<CharT>& _stream;
    std::ios_base::fmtflags _flags;
    std::streamsize _precision;
    CharT _fill;

  public:
    SaveIosFmt(std::basic_ios<CharT>& stream)
        : _stream(stream), _flags(stream.flags()), _precision(stream.precision()), _fill(stream.fill())
    {
    }

    ~SaveIosFmt()
    {
        _stream.fill(_fill);
        _stream.precision(_precision);
        _stream.flags(_flags);
    }
};

// Redirect assertions to a function that will throw an exception on release builds
#include "ship_assert.h"

typedef boost::multiprecision::cpp_int mp_int;

inline size_t GetMpWidth(const mp_int& input) { return (input == 0) ? 1 : msb(input) + 1; }

inline size_t MpToSizeT(const mp_int& input)
{
    if (input > std::numeric_limits<size_t>::max())
    {
        throw RuntimeErrorWithTrace("Value too large to convert to integer");
    }

    return static_cast<size_t>(input);
}

inline mp_int MpSlice(const mp_int& t, const size_t offset, const size_t width)
{
    const mp_int mask = (mp_int(1) << width) - 1;

    const mp_int shifted = (t >> offset) & mask;

    return shifted;
}

inline bool bit_test(const mp_int& input, const size_t bitIndex)
{
    assert(bitIndex <= std::numeric_limits<uint32_t>::max());

    return boost::multiprecision::bit_test(input, static_cast<uint32_t>(bitIndex));
}

inline void bit_set(mp_int& val, const size_t bitIndex)
{
    assert(bitIndex <= std::numeric_limits<uint32_t>::max());

    boost::multiprecision::bit_set(val, static_cast<uint32_t>(bitIndex));
}

inline void bit_unset(mp_int& val, const size_t bitIndex)
{
    assert(bitIndex <= std::numeric_limits<uint32_t>::max());

    boost::multiprecision::bit_unset(val, static_cast<uint32_t>(bitIndex));
}

inline std::string Filename(const std::string& path) { return path.substr(path.find_last_of("/\\") + 1); }

// Looks up a value in a map, throws an exception if the key is not present
template <typename KeyType, typename ValueType>
inline const ValueType& SafeLookup(const std::map<KeyType, ValueType>& mapIn, const KeyType& key)
{
    const auto it = mapIn.find(key);
    if (it == mapIn.end())
    {
        std::string error = "Failed to lookup value in a map";

        if constexpr (std::is_same_v<std::string, KeyType>)
        {
            error += " [key: " + key + "]";
        }

        throw RuntimeErrorWithTrace(error);
    }

    return it->second;
}

// Looks for a key in a map
// If the key is not found, returns the default value
// Returns a copy, not a reference
template <typename KeyType, typename ValueType>
inline ValueType LookupWithDefault(const std::map<KeyType, ValueType>& mapIn, const KeyType& key,
                                   const ValueType& defaultValue)
{
    const auto it = mapIn.find(key);
    if (it == mapIn.end())
    {
        return defaultValue;
    }
    else
    {
        return it->second;
    }
}

// Inserts an item into a map.  Throws an exception if the map already contains the key
template <typename KeyType, typename ValueType>
inline void SafeInsert(std::map<KeyType, ValueType>& mapIn, const KeyType& key, const ValueType& value)
{
    const auto insertResult = mapIn.insert(std::pair<KeyType, ValueType>(key, value));
    if (!insertResult.second)
    {
        std::string error = "Item already exists in a map";

        if constexpr (std::is_same_v<std::string, KeyType>)
        {
            error += " [key: " + key + "]";
        }

        throw RuntimeErrorWithTrace(error);
    }
}

// Inserts an item into a set.  Throws an exception if the set already contains the key
template <typename T> inline void SafeInsert(std::set<T>& setIn, const T& value)
{
    const auto insertResult = setIn.insert(value);
    if (!insertResult.second)
    {
        throw RuntimeErrorWithTrace("Item already exists in a set");
    }
}

// Inserts an item into a map.  Throws an exception if the map already contains
// the key with a non-matching value
template <typename KeyType, typename ValueType>
inline void SafeInsertIdempotent(std::map<KeyType, ValueType>& mapIn, const KeyType& key, const ValueType& value)
{
    const auto insertResult = mapIn.insert(std::pair<KeyType, ValueType>(key, value));
    if (!insertResult.second)
    {
        // Check to see if the old value in the map matches the new value
        const auto it = insertResult.first;
        const ValueType& previousValue = it->second;

        if (previousValue != value)
        {
            std::string error = "Item already exists in a map";

            if constexpr (std::is_same_v<std::string, KeyType>)
            {
                error += " [key: " + key + "]";
            }

            throw RuntimeErrorWithTrace(error);
        }
    }
}

// Remove element "i" from a vector
template <typename T> void VectorEraseByIndex(std::vector<T>& vec, const size_t index)
{
    assert(index < vec.size());

    auto it = vec.begin();
    std::advance(it, index);

    vec.erase(it);
}

// Insert an element into the map
// Returns true if the same (key, value) pair did _not_ already exist in the map
template <typename KeyType, typename ValueType>
inline bool InsertAndCheckForChange(std::map<KeyType, ValueType>& mapIn, const KeyType& key, const ValueType& value)
{
    bool result = false;

    ValueType& previous = mapIn[key];

    if (previous != value)
    {
        previous = value;

        result = true;
    }

    return result;
}

template <typename T> std::set<T> Intersection(const std::set<T>& lhs, const std::set<T>& rhs)
{
    std::set<T> result;

    for (const auto& val : lhs)
    {
        if (rhs.end() != rhs.find(val))
        {
            result.insert(val);
        }
    }

    return result;
}

template <typename T> void Union(std::set<T>& result, const std::set<T>& src)
{
    for (const T& a : src)
    {
        result.insert(a);
    }
}

template <typename T> bool Contains(const std::set<T>& srcSet, const T& value)
{
    return srcSet.end() != srcSet.find(value);
}

// Input: {a, e, g}
// Output: {a->0, e->1, g->2}
template <typename T> std::map<T, size_t> SetToIndexedMap(const std::set<T>& s)
{
    size_t i = 0;

    std::map<T, size_t> result;

    for (const T& value : s)
    {
        result[value] = i;
        i++;
    }

    return result;
}

// Returns a value >= src that is a multiple of alignment
inline size_t RoundUp(const size_t src, const size_t alignment)
{
    const size_t result = ((src + alignment - 1) / alignment) * alignment;

    assert(result >= src);
    assert(0 == (result % alignment));

    return result;
}

class Type;
mp_int TypeConvert(const mp_int& srcIn, const Type* const srcType, const Type* const dstType);

class FunctionNode;

// This file includes boost which redefines assert() but the file re-includes ShipAssert.h to restore assert()
#include "branch_and_bound.h"

// Kanagawa compiler files - none of these will redefine assert()

#include "strip_leading_path.h"

#include "register_access_type.h"

#include "debug_symbols.h"

#include "deep_ptr.h"

#include "object_path.h"

#include "platform.h"

#include "thread_pool.h"

#include "errors.h"

#include "parse_tree.h"

#include "place.h"

#include "enumerate_function_instances.h"

#include "ir.h"

#include "control_flow_graph.h"

#include "optimize.h"

#include "data_flow_work_list.h"

#include "conditional_local_updates.h"

#include "dgml.h"

#include "compiler.h"

#include "config.h"

#include "device_config_visitor.h"

#include "backend_common.h"

#include "json_value.h"

#include "json_writer.h"

#include "verilog.h"

#include "symbols.h"

#include "path_length_tracker.h"

#include "resource_usage.h"

#include "internal_tests.h"

#include "serialize_ir.h"

#include "power_report.h"

#include "path_report.h"

#include "local_data_propagation.h"

#include "circt_util.h"

inline size_t Align(const size_t input, const size_t alignment)
{
    // assert that the alignment is a power of 2
    assert((alignment & (alignment - 1)) == 0);

    const size_t mask = alignment - 1;

    return (input + mask) & ~mask;
}

inline size_t AlignNonPow2(const size_t input, const size_t alignment)
{
    const size_t d = (input + alignment - 1) / alignment;

    return d * alignment;
}

// Returns "a" if "a" and "b" are both defined and contain the same value
// Returns an undefined optional otherwise
template <typename T>
inline boost::optional<T> OptionalIntersect(const boost::optional<T>& a, const boost::optional<T>& b)
{
    boost::optional<T> result;

    if (a && b && (*a == *b))
    {
        result = a;
    }

    return result;
}

// Returns false if and only if (a && !b)
inline bool Implies(const bool a, const bool b) { return !a || b; }

#ifdef _WIN32
#define ARRAY_SIZE _countof
#else
#include <strings.h> // cygwin only??
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))
#define _stricmp strcasecmp
#endif

#ifdef __GNUC__
inline unsigned char _BitScanReverse64(unsigned long* Index, unsigned long long Mask)
{
    if (Mask)
    {
        *Index = __builtin_clz(Mask);
        return *Index;
    }
    return 0;
}
#else
#include <intrin.h>
#endif // __GNUC__

// Calls a function in the destructor
class ExitScope
{
  public:
    ExitScope(const std::function<void()>& fn) : _fn(fn) {}

    ~ExitScope() { _fn(); }

  private:
    std::function<void()> _fn;
};

// dynamic_cast, with an assertion that the cast succeeded
template <typename To, typename From> inline To safe_cast(From* const from)
{
    To result = dynamic_cast<To>(from);
    assert(result);

    return result;
}
