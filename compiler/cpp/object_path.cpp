// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

ObjectPath AppendToPath(const ObjectPath& path, const std::string& suffix)
{
    ObjectPath result = path;

    result.push_back(suffix);

    return result;
}

std::string SerializePath(const ObjectPath& path, const char delimeter)
{
    std::ostringstream str;

    bool isFirst = true;

    for (const std::string& s : path)
    {
        if (!isFirst)
        {
            str << delimeter;
        }

        str << s;

        isFirst = false;
    }

    return str.str();
}

ObjectPath SubPath(const ObjectPath& src, const size_t offset, const size_t len)
{
    assert((offset + len) <= src.size());

    ObjectPath result(len);

    for (size_t i = 0; i < len; i++)
    {
        result[i] = src[offset + i];
    }

    return result;
}