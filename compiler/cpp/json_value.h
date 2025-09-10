// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include <variant>
#include <map>
#include <vector>
#include <string>
#include <sstream>
#include <iomanip>

class JsonValue;

using JsonObject = std::map<std::string, JsonValue>;
using JsonArray = std::vector<JsonValue>;

class JsonValue
{
  public:
    enum class Type { Null, Bool, Number, String, Object, Array };

    // Constructors for all JSON types
    JsonValue() : _value(nullptr) {}
    JsonValue(bool value) : _value(value) {}
    JsonValue(int value) : _value(static_cast<double>(value)) {}
    JsonValue(size_t value) : _value(static_cast<double>(value)) {}
    JsonValue(double value) : _value(value) {}
    JsonValue(const std::string& value) : _value(value) {}
    JsonValue(const char* value) : _value(std::string(value)) {}
    JsonValue(const JsonObject& value) : _value(value) {}
    JsonValue(const JsonArray& value) : _value(value) {}

    // Factory methods
    static JsonValue CreateObject() { return JsonValue(JsonObject{}); }
    static JsonValue CreateArray() { return JsonValue(JsonArray{}); }

    // Type checking
    bool IsNull() const { return std::holds_alternative<std::nullptr_t>(_value); }
    bool IsBool() const { return std::holds_alternative<bool>(_value); }
    bool IsNumber() const { return std::holds_alternative<double>(_value); }
    bool IsString() const { return std::holds_alternative<std::string>(_value); }
    bool IsObject() const { return std::holds_alternative<JsonObject>(_value); }
    bool IsArray() const { return std::holds_alternative<JsonArray>(_value); }

    // Value access
    bool AsBool() const
    {
        assert(IsBool());
        return std::get<bool>(_value);
    }

    double AsNumber() const
    {
        assert(IsNumber());
        return std::get<double>(_value);
    }

    const std::string& AsString() const
    {
        assert(IsString());
        return std::get<std::string>(_value);
    }

    JsonObject& AsObject()
    {
        assert(IsObject());
        return std::get<JsonObject>(_value);
    }

    const JsonObject& AsObject() const
    {
        assert(IsObject());
        return std::get<JsonObject>(_value);
    }

    JsonArray& AsArray()
    {
        assert(IsArray());
        return std::get<JsonArray>(_value);
    }

    const JsonArray& AsArray() const
    {
        assert(IsArray());
        return std::get<JsonArray>(_value);
    }

    // Operations
    void AddMember(const std::string& key, const JsonValue& value)
    {
        assert(IsObject());
        AsObject()[key] = value;
    }

    void PushBack(const JsonValue& value)
    {
        assert(IsArray());
        AsArray().push_back(value);
    }

    // Compatibility method for RapidJSON replacement
    JsonValue Move() { return std::move(*this); }

    // Serialization with 4-space indentation
    std::string ToString(bool pretty = true, int indent = 0) const
    {
        std::ostringstream ss;
        WriteToStream(ss, pretty, indent);
        return ss.str();
    }

    void WriteToStream(std::ostream& os, bool pretty = true, int indent = 0) const
    {
        switch (GetType())
        {
        case Type::Null:
            os << "null";
            break;
        case Type::Bool:
            os << (AsBool() ? "true" : "false");
            break;
        case Type::Number:
            {
                double num = AsNumber();
                // Check if it's an integer value
                if (num == static_cast<long long>(num))
                {
                    os << static_cast<long long>(num);
                }
                else
                {
                    // Use a temporary stringstream to format the number
                    std::ostringstream tempSS;
                    tempSS << std::fixed << std::setprecision(15) << num;
                    std::string numStr = tempSS.str();

                    // Remove trailing zeros
                    size_t decimalPos = numStr.find('.');
                    if (decimalPos != std::string::npos)
                    {
                        numStr.erase(numStr.find_last_not_of('0') + 1, std::string::npos);
                        if (numStr.back() == '.')
                            numStr.pop_back();
                    }
                    os << numStr;
                }
            }
            break;
        case Type::String:
            os << '"' << EscapeString(AsString()) << '"';
            break;
        case Type::Object:
            {
                const JsonObject& obj = AsObject();
                os << '{';
                if (pretty && !obj.empty())
                {
                    os << '\n';
                    bool first = true;
                    for (const auto& pair : obj)
                    {
                        if (!first)
                        {
                            os << ",\n";
                        }
                        first = false;
                        WriteIndent(os, indent + 1);
                        os << '"' << EscapeString(pair.first) << "\": ";
                        pair.second.WriteToStream(os, pretty, indent + 1);
                    }
                    os << '\n';
                    WriteIndent(os, indent);
                }
                os << '}';
            }
            break;
        case Type::Array:
            {
                const JsonArray& arr = AsArray();
                os << '[';
                if (pretty && !arr.empty())
                {
                    os << '\n';
                    for (size_t i = 0; i < arr.size(); ++i)
                    {
                        if (i > 0)
                        {
                            os << ",\n";
                        }
                        WriteIndent(os, indent + 1);
                        arr[i].WriteToStream(os, pretty, indent + 1);
                    }
                    os << '\n';
                    WriteIndent(os, indent);
                }
                else if (!arr.empty())
                {
                    for (size_t i = 0; i < arr.size(); ++i)
                    {
                        if (i > 0)
                        {
                            os << ",";
                        }
                        arr[i].WriteToStream(os, pretty, indent + 1);
                    }
                }
                os << ']';
            }
            break;
        }
    }

    Type GetType() const
    {
        if (std::holds_alternative<std::nullptr_t>(_value)) return Type::Null;
        if (std::holds_alternative<bool>(_value)) return Type::Bool;
        if (std::holds_alternative<double>(_value)) return Type::Number;
        if (std::holds_alternative<std::string>(_value)) return Type::String;
        if (std::holds_alternative<JsonObject>(_value)) return Type::Object;
        if (std::holds_alternative<JsonArray>(_value)) return Type::Array;
        return Type::Null;
    }

  private:
    std::variant<std::nullptr_t, bool, double, std::string, JsonObject, JsonArray> _value;

    void WriteIndent(std::ostream& os, int indent) const
    {
        for (int i = 0; i < indent * 4; ++i)
        {
            os << ' ';
        }
    }

    std::string EscapeString(const std::string& str) const
    {
        std::ostringstream ss;
        for (char c : str)
        {
            switch (c)
            {
            case '\"':
                ss << "\\\"";
                break;
            case '\\':
                ss << "\\\\";
                break;
            case '/':
                ss << "\\/";
                break;
            case '\b':
                ss << "\\b";
                break;
            case '\f':
                ss << "\\f";
                break;
            case '\n':
                ss << "\\n";
                break;
            case '\r':
                ss << "\\r";
                break;
            case '\t':
                ss << "\\t";
                break;
            default:
                if (static_cast<unsigned char>(c) < 0x20)
                {
                    ss << "\\u" << std::hex << std::setw(4) << std::setfill('0') << static_cast<int>(c);
                }
                else
                {
                    ss << c;
                }
                break;
            }
        }
        return ss.str();
    }
};
