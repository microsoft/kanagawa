// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include "json_value.h"

class JsonWriter
{
  public:
    JsonWriter(const std::string& fileName)
    {
        OpenOutputFileStream(_outputStream, fileName.c_str(), false);
        _document = JsonValue::CreateObject();
    }

    ~JsonWriter()
    {
        std::string jsonString = _document.ToString(true);
        _outputStream.write(jsonString.c_str(), jsonString.length());
    }

    JsonValue SerializeString(const std::string& s)
    {
        return JsonValue(s);
    }

    JsonValue SerializeSizeT(const size_t s)
    {
        return JsonValue(s);
    }

    JsonValue SerializeMpInt(const mp_int& m)
    {
        std::ostringstream str;
        str << m;
        return SerializeString(str.str());
    }

    JsonValue SerializeBool(const bool b)
    {
        return JsonValue(b);
    }

    // Dummy allocator for API compatibility
    struct DummyAllocator {};
    DummyAllocator& Allocator()
    {
        static DummyAllocator dummy;
        return dummy;
    }

    template <typename NameT, typename ValueT> void AddMember(const NameT& name, ValueT& value)
    {
        _document.AddMember(name, value);
    }

  protected:
    JsonValue _document;

  private:
    std::ofstream _outputStream;
};
