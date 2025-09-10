// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

enum class RegisterAccessType : uint8_t
{
    Read,
    Write,
    Wire
};

enum class RegisterSourceType : uint8_t
{
    Local,
    Hidden,
    Predicate,
    Intermediate
};

inline std::string ToString(RegisterAccessType accessType)
{
    switch (accessType)
    {
    case RegisterAccessType::Read:
        return "read";
    case RegisterAccessType::Write:
        return "write";
    case RegisterAccessType::Wire:
        return "wire";
    default:
        return "INVALID";
    }
};

struct LocationId
{
    LocationId(size_t encodedId)
    {
        _index = encodedId >> 32;

        _offset = encodedId & 0xFFFFFFFF;
    }

    LocationId(const size_t index, const size_t offset) : _index(index), _offset(offset) {}

    size_t Serialize() const { return _index << 32 | _offset; }

    std::string str() const
    {
        std::string result("BasicBlock: ");
        result += std::to_string(_index);
        result += ", stage: ";
        result += std::to_string(_offset);
        return result;
    }

    friend std::ostream& operator<<(std::ostream& os, const LocationId& id)
    {
        os << id.str();
        return os;
    }

    // File index or BB index
    size_t _index;

    // Line number or atomic sequence number
    size_t _offset;
};
