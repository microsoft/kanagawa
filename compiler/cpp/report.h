// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Helper functions for producing reports
#pragma once

// There are 2 levels of paths in the reports:
// There are a fixed number (PathElementCount) of heterogenous path elements
// This is usually used for different kinds of elements (object, function, stage)
//
// Within each heterogenous path element, there can be any number of htereogenous elements
// Consider using TokenizeObjectName to generate path entries for objects
//
// For example;
// ReportBuilder::PathElementCount = 3
// ReportBuilder::PathElementNameArray = { "object", "function", "basic block" }
//
// Valid ReportBuilder::Path examples:
// object                                       function        basic block
// {{ "MutexInst0" },                           { "Lock" },     {"BB0"} }
// {{ "OuterObjectInst0", "InnerObjectInst0" }, { "MyMethod" }, {"BB0"} }

template <size_t ColumnCount, size_t PathElementCount> class ReportBuilder : public JsonWriter
{
  public:
    struct ColumnDesc
    {
        std::string _name;
        std::string _units;
    };

    enum class AggregationRule
    {
        Sum,
        Min,
        Max
    };

    struct Projection
    {
        std::string _name;
        std::array<size_t, PathElementCount> _pathOrder;
        std::array<AggregationRule, ColumnCount>
            _aggregationRules; // 1 per column, in the same order that columns are specified
    };

    using ColumnArray = std::array<ColumnDesc, ColumnCount>;
    using PathElementNameArray = std::array<std::string, PathElementCount>;
    using ProjectionVector = std::vector<Projection>;
    using Path = std::array<std::vector<std::string>, PathElementCount>;
    using Tuple = std::array<size_t, ColumnCount>;

    static std::vector<std::string> SimplePathElement(const std::string s)
    {
        std::vector<std::string> result;
        result.push_back(s);
        return result;
    }

    ReportBuilder(const std::string& fileName, const ColumnArray& columns, const PathElementNameArray& pathElementNames,
                  const ProjectionVector& projections)
        : JsonWriter(fileName), _pathElementNames(pathElementNames), _dataArray(JsonValue::CreateArray())
    {
        // Available resources
        {
            JsonValue availableResources = JsonValue::CreateArray();

            for (const ColumnDesc& cd : columns)
            {
                JsonValue column = JsonValue::CreateObject();

                column.AddMember("name", SerializeString(cd._name));
                column.AddMember("unitOfMeasurement", SerializeString(cd._units));

                availableResources.PushBack(column);
            }

            _document.AddMember("availableResources", availableResources);
        }

        // projections
        {
            JsonValue jsonProjections = JsonValue::CreateArray();

            for (const Projection& projection : projections)
            {
                JsonValue jsonProjection = JsonValue::CreateObject();

                {
                    std::string pathRule;

                    std::set<size_t> usedPathElements;

                    for (size_t i = 0; i < PathElementCount; i++)
                    {
                        const size_t pathElementIndex = projection._pathOrder[i];

                        // Ensure each element of the path rule is unique
                        assert(usedPathElements.end() == usedPathElements.find(pathElementIndex));
                        usedPathElements.insert(pathElementIndex);

                        if (i > 0)
                        {
                            pathRule += "/";
                        }

                        pathRule += PathElementName(pathElementIndex);
                    }

                    jsonProjection.AddMember("pathRule", SerializeString(pathRule));
                }

                {
                    JsonValue columnNames = JsonValue::CreateArray();

                    for (const ColumnDesc& cd : columns)
                    {
                        columnNames.PushBack(SerializeString(cd._name));
                    }

                    jsonProjection.AddMember("includeColumns", columnNames);
                }

                jsonProjection.AddMember("name", SerializeString(projection._name));

                {
                    JsonValue aggregationRules = JsonValue::CreateObject();

                    for (size_t i = 0; i < ColumnCount; i++)
                    {
                        aggregationRules.AddMember(columns[i]._name, SerializeAggreationRule(projection._aggregationRules[i]));
                    }

                    jsonProjection.AddMember("aggregationRules", aggregationRules);
                }

                jsonProjections.PushBack(jsonProjection);
            }

            _document.AddMember("projections", jsonProjections);
        }

        // First element in the data array is a scheme for the remaining elements
        {
            JsonValue dataSchema = JsonValue::CreateArray();

            for (size_t i = 0; i < PathElementCount; i++)
            {
                dataSchema.PushBack(SerializeString(PathElementName(i)));
            }

            dataSchema.PushBack(JsonValue("relativePath"));
            dataSchema.PushBack(JsonValue("rangeStartLine"));
            dataSchema.PushBack(JsonValue("rangeStartCharacter"));
            dataSchema.PushBack(JsonValue("rangeEndLine"));
            dataSchema.PushBack(JsonValue("rangeEndCharacter"));

            for (const ColumnDesc& cd : columns)
            {
                dataSchema.PushBack(SerializeString(cd._name));
            }

            _dataArray.PushBack(dataSchema);
        }
    }

    ~ReportBuilder() { _document.AddMember("data", _dataArray); }

    void AppendRow(const Path& path, const Location& location, const Tuple& columnValues)
    {
        JsonValue row = JsonValue::CreateArray();

        for (size_t i = 0; i < PathElementCount; i++)
        {
            JsonValue jsonPathElementArray = JsonValue::CreateArray();

            for (const std::string& s : path[i])
            {
                JsonValue jsonPathElement = JsonValue::CreateObject();
                jsonPathElement.AddMember("name", SerializeString(s));
                jsonPathElement.AddMember("type", SerializeString(_pathElementNames[i]));

                jsonPathElementArray.PushBack(jsonPathElement);
            }

            row.PushBack(jsonPathElementArray);
        }

        const std::string filePath = LocationToString(location, false, "", false);

        row.PushBack(SerializeString(filePath));

        row.PushBack(JsonValue(location._beginLine));
        row.PushBack(JsonValue(1));
        row.PushBack(JsonValue(location._endLine));
        row.PushBack(JsonValue(1));

        for (const size_t value : columnValues)
        {
            row.PushBack(JsonValue(value));
        }

        _dataArray.PushBack(row);
    }

  private:
    JsonValue SerializeAggreationRule(const AggregationRule rule)
    {
        std::string r;

        switch (rule)
        {
        case AggregationRule::Sum:
            r = "sum";
            break;
        case AggregationRule::Min:
            r = "min";
            break;
        case AggregationRule::Max:
            r = "max";
            break;
        default:
            assert(false);
        }

        return SerializeString(r);
    }

    std::string PathElementName(const size_t pathElementIndex)
    {
        assert(pathElementIndex < PathElementCount);

        std::string result("path");
        result += std::to_string(pathElementIndex);

        return result;
    }

    JsonValue _dataArray;
    PathElementNameArray _pathElementNames;
};
