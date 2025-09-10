// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

enum class DeviceMemoryType : uint8_t
{
    LUT = 0,
    Block = 1,
    Deep = 2
};

struct DeviceMemoryConfiguration
{
    DeviceMemoryType _type;
    float _cost;
    size_t _width;
    size_t _depth;
};

struct DeviceMemoryResource
{
    DeviceMemoryType _type;
    size_t _numAvailable;
};

// Forward declaration
class AbstractDeviceConfigPropertyExtractor;

class DeviceConfigVisitor
{
public:
    DeviceConfigVisitor(Compiler& compiler, CodeGenDeviceConfig& deviceConfig);
    ~DeviceConfigVisitor();

    void VisitNode(const ParseTreeNode* const node, VisitContext& context);
    bool Finalize(std::vector<std::string>& missingPropertyNames) const;

private:
    template<typename PropertyType>
    void AddPropertyExtractor(const std::string& propertyName, PropertyType& propertyValue);

    template<typename ExtractorType, typename PropertyType>
    void AddAggregatePropertyExtractor(const std::string& propertyName, PropertyType& propertyValue);

    const Scope _configScope = {"@compiler@config"};

    std::vector<DeviceMemoryResource> _memoryResources;
    std::vector<DeviceMemoryConfiguration> _memoryConfigurations;
    size_t _defaultClockFrequencyMhz;

    CodeGenDeviceConfig& _deviceConfig;
    Compiler& _compiler;

    std::map<std::string, std::unique_ptr<AbstractDeviceConfigPropertyExtractor>> _propertyVisitors;
};