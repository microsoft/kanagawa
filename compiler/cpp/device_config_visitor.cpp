// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

bool TryMpToMemoryType(mp_int value, DeviceMemoryType& memoryType)
{
    size_t intValue = MpToSizeT(value);

    // Check if the value corresponds to a valid DeviceMemoryType
    switch (intValue)
    {
        case 0:
            memoryType = DeviceMemoryType::LUT;
            return true;
        case 1:
            memoryType = DeviceMemoryType::Block;
            return true;
        case 2:
            memoryType = DeviceMemoryType::Deep;
            return true;
        default:
            return false; // Invalid memory type value
    }
}

bool TryMpToMemoryInitFileType(mp_int value, MemoryInitFileType& memoryInitFileType)
{
    size_t intValue = MpToSizeT(value);

    // Check if the value corresponds to a valid MemoryInitFileType
    switch (intValue)
    {
        case 0:
            memoryInitFileType = MemoryInitFileType::Mif;
            return true;
        case 1:
            memoryInitFileType = MemoryInitFileType::Mem;
            return true;
        default:
            return false; // Invalid memory init file type value
    }
}

template<typename NodeType>
KnownValue GetKnownValueHelper(const ParseTreeNode* const node, VisitContext& context)
{
    // Try to get the known value from the RHS of the assignment
    const auto declareNode = dynamic_cast<const DeclareNode*>(node);
    assert(declareNode != nullptr);
    const auto assignNode = declareNode->GetAssignNode();
    assert(assignNode != nullptr);

    const auto declaredType = declareNode->GetDeclaredType();
    assert(declaredType != nullptr);

    const NodeType* const checkNodeType = dynamic_cast<const NodeType*>(declaredType);
    if (checkNodeType == nullptr)
    {
        return KnownValue{};
    }

    const auto rhs = assignNode->GetRhs();
    assert(rhs != nullptr);

    return TryGetVisitKnownValue(context, rhs, declaredType);
}


class AbstractDeviceConfigPropertyExtractor
{
public:
    virtual void Extract(const ParseTreeNode* const node, VisitContext& context) = 0;
    virtual bool Validate(std::vector<std::string>& missingPropertyNames) const = 0;
    virtual ~AbstractDeviceConfigPropertyExtractor() = default;

protected:
    const Scope _schemaScope = {"@compiler@device@schema"};
};

template<typename T>
class DeviceConfigPropertyExtractor : public AbstractDeviceConfigPropertyExtractor
{
public:
    DeviceConfigPropertyExtractor(T& propertyValue, const std::string& propertyName, Compiler& compiler)
        : _propertyValue(propertyValue), _propertyName(propertyName), _compiler(compiler)
    {
    }

    void Extract(const ParseTreeNode* const node, VisitContext& context) override
    {
        throw std::logic_error(std::string("DeviceConfigPropertyExtractor::Extract not implemented for type: ") + typeid(T).name());
    }

    bool Validate(std::vector<std::string>& missingPropertyNames) const override
    {
        if (!_found)
        {
            missingPropertyNames.push_back(_propertyName);
            return false;
        }

        return true;
    }

private:
    T& _propertyValue;
    std::string _propertyName;
    Compiler& _compiler;
    bool _found = false;
};

template<>
void DeviceConfigPropertyExtractor<bool>::Extract(const ParseTreeNode* const node, VisitContext& context)
{
    auto kv = GetKnownValueHelper<BoolType>(node, context);

    if (kv._type != KnownValueType::Int)
    {
        _compiler.ErrorStream(
            node->GetLocation(),
            CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be a constant boolean value";

        throw std::runtime_error("Device config extraction failed");
    }

    _propertyValue = (kv._intVal != 0);
    _found = true;
}

template<>
void DeviceConfigPropertyExtractor<size_t>::Extract(const ParseTreeNode* const node, VisitContext& context)
{
    auto kv = GetKnownValueHelper<LeafType>(node, context);

    if (kv._type != KnownValueType::Int || kv._intVal < 0)
    {
        _compiler.ErrorStream(
            node->GetLocation(),
            CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be a constant unsigned integer value";

        throw std::runtime_error("Device config extraction failed");
    }

    _propertyValue = MpToSizeT(kv._intVal);
    _found = true;
}

template<>
void DeviceConfigPropertyExtractor<float>::Extract(const ParseTreeNode* const node, VisitContext& context)
{
    const auto declareNode = dynamic_cast<const DeclareNode*>(node);
    assert(declareNode != nullptr);
    const auto declaredType = declareNode->GetDeclaredType();

    auto kv = GetKnownValueHelper<FloatType>(node, context);

    if (kv._type != KnownValueType::Int)
    {
        _compiler.ErrorStream(
            node->GetLocation(),
            CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be a constant floating point value";

        throw std::runtime_error("Device config extraction failed");
    }

    // The float value was bit-cast to a uint32_t in the TryGetKnownValue code, so cast it back
    uint32_t asUint32 = static_cast<uint32_t>(kv._intVal);
    _propertyValue =  *reinterpret_cast<float*>(&asUint32);

    _found = true;
}

template<>
void DeviceConfigPropertyExtractor<std::string>::Extract(const ParseTreeNode* const node, VisitContext& context)
{
    auto kv = GetKnownValueHelper<StringType>(node, context);

    if (kv._type != KnownValueType::String)
    {
        _compiler.ErrorStream(
            node->GetLocation(),
            CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be a constant string value";

        throw std::runtime_error("Device config extraction failed");
    }

    _propertyValue =  kv._stringVal;

    _found = true;
}

template<>
void DeviceConfigPropertyExtractor<MemoryInitFileType>::Extract(const ParseTreeNode* const node, VisitContext& context)
{
    auto kv = GetKnownValueHelper<EnumType>(node, context);

    if (kv._type != KnownValueType::Int || !TryMpToMemoryInitFileType(kv._intVal, _propertyValue))
    {
        _compiler.ErrorStream(
            node->GetLocation(),
            CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be of type MemoryInitFileType";

        throw std::runtime_error("Device config extraction failed");
    }

    _found = true;
}

template<>
void DeviceConfigPropertyExtractor<DeviceMemoryType>::Extract(const ParseTreeNode* const node, VisitContext& context)
{
    auto kv = GetKnownValueHelper<EnumType>(node, context);

    if (kv._type != KnownValueType::Int || !TryMpToMemoryType(kv._intVal, _propertyValue))
    {
        _compiler.ErrorStream(
            node->GetLocation(),
            CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be of type MemoryType";

        throw std::runtime_error("Device config extraction failed");
    }

    _found = true;
}

// Helper function to resolve InitializerListNode from either direct assignment or variable reference
const InitializerListNode* GetInitializerListFromNode(const ParseTreeNode* const node, VisitContext& context)
{
    // First try direct cast
    const InitializerListNode* directInitList = dynamic_cast<const InitializerListNode*>(node);
    if (directInitList != nullptr)
    {
        return directInitList;
    }

    // If not direct, check if it's a variable access
    const VariableAccessNode* varAccess = dynamic_cast<const VariableAccessNode*>(node);
    if (varAccess != nullptr)
    {
        // Get the symbol lookup key (scope and name)
        auto [scope, name] = varAccess->GetSymbolLookupKey();

        // Look up the variable in the symbol table
        auto symbolData = context.LookupSymbol(scope, name, node->GetLocation());

        // Get the declaration for this variable
        const DeclareNode* varDeclaration = symbolData._declaration;
        if (varDeclaration != nullptr)
        {
            const AssignNode* varAssignNode = varDeclaration->GetAssignNode();
            if (varAssignNode != nullptr)
            {
                // Recursively try to get the initializer list from the variable's RHS
                return GetInitializerListFromNode(varAssignNode->GetRhs(), context);
            }
        }
    }

    return nullptr;
}

class DeviceConfigMemoryResourceExtractor : public AbstractDeviceConfigPropertyExtractor
{
public:
    DeviceConfigMemoryResourceExtractor(std::vector<DeviceMemoryResource>& propertyValue, const std::string& propertyName, Compiler& compiler)
        : _propertyValue(propertyValue),
        _propertyName(propertyName),
        _compiler(compiler)
    {
        _memoryResourceType = dynamic_cast<const StructUnionType*>(_compiler.GetNamedType({}, _schemaScope, "MemoryResource", Location{}));
        assert(nullptr != _memoryResourceType);

        _memoryTypeType = compiler.GetNamedType({}, _schemaScope, "MemoryType", Location{});
        assert(nullptr != _memoryTypeType);
    }

    void Extract(const ParseTreeNode* const node, VisitContext& context) override
    {
        const auto declareNode = dynamic_cast<const DeclareNode*>(node);
        assert(declareNode != nullptr);
        const auto assignNode = declareNode->GetAssignNode();
        assert(assignNode != nullptr);
        const auto declaredType = declareNode->GetDeclaredType();
        assert(declaredType != nullptr);
        const auto rhs = assignNode->GetRhs();
        assert(rhs != nullptr);

        const ArrayType* const arrayType = dynamic_cast<const ArrayType*>(declaredType);

        if (nullptr == arrayType || arrayType->_elementType != _memoryResourceType)
        {
            _compiler.ErrorStream(
                node->GetLocation(),
                CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be an array of MemoryResource";

            throw std::runtime_error("Device config extraction failed");
        }

        // Try to get the InitializerListNode, either directly or through a variable reference
        const InitializerListNode* initList = GetInitializerListFromNode(rhs, context);
        if (nullptr == initList)
        {
            _compiler.ErrorStream(node->GetLocation(), CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' has an invalid initializer list. Expected an array of MemoryResource";

            throw std::runtime_error("Device config extraction failed");
        }

        size_t elementIndex = 0;
        for (const ParseTreeNode* element : initList->Children())
        {
            DeviceMemoryResource memoryResource;

            // Each element should be a BaseInitializerListNode (either InitializerListNode or DesignatedInitializerListNode)
            const BaseInitializerListNode* structInit = dynamic_cast<const BaseInitializerListNode*>(element);

            if (!structInit)
            {
                _compiler.ErrorStream(
                    element->GetLocation(),
                    CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' has an invalid element at index " << elementIndex;

                throw std::runtime_error("Device config extraction failed");
            }

            // Process each field in the struct
            for (size_t fieldIndex = 0; fieldIndex < _memoryResourceType->_members.size(); fieldIndex++)
            {
                const std::string& fieldName = _memoryResourceType->_members[fieldIndex].second->GetDeclaredName();
                const Type* fieldType = _memoryResourceType->_members[fieldIndex].second->GetDeclaredType();

                // TryGetField works for both positional and designated initialization
                const ParseTreeNode* fieldValue = structInit->TryGetField(fieldIndex, fieldName);

                if (nullptr == fieldValue)
                {
                    _compiler.ErrorStream(
                        element->GetLocation(),
                        CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' is missing a value for field '" << fieldName
                       << "' at index " << elementIndex;

                    throw std::runtime_error("Device config extraction failed");
                }

                if (fieldName == "type_id")
                {
                    KnownValue kv = TryGetVisitKnownValue(context, fieldValue, fieldType);

                    assert(kv._type == KnownValueType::Int);
                    bool isValidValue = TryMpToMemoryType(kv._intVal, memoryResource._type);
                    assert(isValidValue);
                }
                else if (fieldName == "num_available")
                {
                    KnownValue kv = TryGetVisitKnownValue(context, fieldValue, fieldType);

                    assert(kv._type == KnownValueType::Int);

                    memoryResource._numAvailable = MpToSizeT(kv._intVal);
                }
                else
                {
                    assert(false);
                }
            }

            _propertyValue.push_back(memoryResource);
        }

        _found = true;
    }

    bool Validate(std::vector<std::string>& missingPropertyNames) const override
    {
        if (!_found)
        {
            missingPropertyNames.push_back(_propertyName);
            return false;
        }

        return true;
    }

private:
    std::vector<DeviceMemoryResource>& _propertyValue;
    std::string _propertyName;
    Compiler& _compiler;
    const StructUnionType* _memoryResourceType;
    const Type* _memoryTypeType;
    bool _found = false;
};

class DeviceConfigMemoryConfigurationExtractor : public AbstractDeviceConfigPropertyExtractor
{
public:
    DeviceConfigMemoryConfigurationExtractor(std::vector<DeviceMemoryConfiguration>& propertyValue, const std::string& propertyName, Compiler& compiler)
        : _propertyValue(propertyValue),
        _propertyName(propertyName),
        _compiler(compiler)
    {
        _memoryConfigurationType = dynamic_cast<const StructUnionType*>(compiler.GetNamedType({}, _schemaScope, "MemoryConfiguration", Location{}));
        assert(nullptr != _memoryConfigurationType);

        _memoryTypeType = compiler.GetNamedType({}, _schemaScope, "MemoryType", Location{});
        assert(nullptr != _memoryTypeType);
    }

    void Extract(const ParseTreeNode* const node, VisitContext& context) override
    {
        const auto declareNode = dynamic_cast<const DeclareNode*>(node);
        assert(declareNode != nullptr);
        const auto assignNode = declareNode->GetAssignNode();
        assert(assignNode != nullptr);
        const auto declaredType = declareNode->GetDeclaredType();
        assert(declaredType != nullptr);
        const auto rhs = assignNode->GetRhs();
        assert(rhs != nullptr);

        const ArrayType* const arrayType = dynamic_cast<const ArrayType*>(declaredType);

        if (nullptr == arrayType || arrayType->_elementType != _memoryConfigurationType)
        {
            _compiler.ErrorStream(
                node->GetLocation(),
                CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' must be an array of MemoryConfiguration";

            throw std::runtime_error("Device config extraction failed");
        }

        // Try to get the InitializerListNode, either directly or through a variable reference
        const InitializerListNode* initList = GetInitializerListFromNode(rhs, context);
        if (nullptr == initList)
        {
            _compiler.ErrorStream(node->GetLocation(), CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' has an invalid initializer list. Expected an array of MemoryConfiguration";

            throw std::runtime_error("Device config extraction failed");
        }

        size_t elementIndex = 0;
        for (const ParseTreeNode* element : initList->Children())
        {
            DeviceMemoryConfiguration memoryConfiguration;

            // Each element should be a BaseInitializerListNode (either InitializerListNode or DesignatedInitializerListNode)
            const BaseInitializerListNode* structInit = dynamic_cast<const BaseInitializerListNode*>(element);

            if (!structInit)
            {
                _compiler.ErrorStream(
                    element->GetLocation(),
                    CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' has an invalid element at index " << elementIndex;

                throw std::runtime_error("Device config extraction failed");
            }

            // Process each field in the struct
            for (size_t fieldIndex = 0; fieldIndex < _memoryConfigurationType->_members.size(); fieldIndex++)
            {
                const std::string& fieldName = _memoryConfigurationType->_members[fieldIndex].second->GetDeclaredName();
                const Type* fieldType = _memoryConfigurationType->_members[fieldIndex].second->GetDeclaredType();

                // TryGetField works for both positional and designated initialization
                const ParseTreeNode* fieldValue = structInit->TryGetField(fieldIndex, fieldName);

                if (nullptr == fieldValue)
                {
                    _compiler.ErrorStream(
                        element->GetLocation(),
                        CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << _propertyName << "' is missing a value for field '" << fieldName
                       << "' at index " << elementIndex;

                    throw std::runtime_error("Device config extraction failed");
                }

                if (fieldName == "type_id")
                {
                    KnownValue kv = TryGetVisitKnownValue(context, fieldValue, fieldType);

                    assert(kv._type == KnownValueType::Int);
                    bool isValidValue = TryMpToMemoryType(kv._intVal, memoryConfiguration._type);
                    assert(isValidValue);
                }
                else if (fieldName == "cost")
                {
                    const FloatType* const floatType = dynamic_cast<const FloatType*>(fieldType);

                    KnownValue kv = TryGetVisitKnownValue(context, fieldValue, fieldType);

                    assert(floatType != nullptr);
                    assert(kv._type == KnownValueType::Int);

                    uint32_t asUint32 = static_cast<uint32_t>(kv._intVal);
                    memoryConfiguration._cost =  *reinterpret_cast<float*>(&asUint32);
                }
                else if (fieldName == "width")
                {
                    KnownValue kv = TryGetVisitKnownValue(context, fieldValue, fieldType);

                    assert(kv._type == KnownValueType::Int);

                    memoryConfiguration._width = MpToSizeT(kv._intVal);
                }
                else if (fieldName == "depth")
                {
                    KnownValue kv = TryGetVisitKnownValue(context, fieldValue, fieldType);

                    assert(kv._type == KnownValueType::Int);

                    memoryConfiguration._depth = MpToSizeT(kv._intVal);
                }
                else
                {
                    assert(false);
                }
            }

            _propertyValue.push_back(memoryConfiguration);
        }

        _found = true;
    }

    bool Validate(std::vector<std::string>& missingPropertyNames) const override
    {
        if (!_found)
        {
            missingPropertyNames.push_back(_propertyName);
            return false;
        }

        return true;
    }

private:
    // Same helper method as above
    const InitializerListNode* GetInitializerListFromNode(const ParseTreeNode* const node, VisitContext& context) const
    {
        const InitializerListNode* directInitList = dynamic_cast<const InitializerListNode*>(node);
        if (directInitList != nullptr)
        {
            return directInitList;
        }

        const VariableAccessNode* varAccess = dynamic_cast<const VariableAccessNode*>(node);
        if (varAccess != nullptr)
        {
            auto [scope, name] = varAccess->GetSymbolLookupKey();
            auto symbolData = context.LookupSymbol(scope, name, node->GetLocation());
            const DeclareNode* varDeclaration = symbolData._declaration;
            if (varDeclaration != nullptr)
            {
                const AssignNode* varAssignNode = varDeclaration->GetAssignNode();
                if (varAssignNode != nullptr)
                {
                    return GetInitializerListFromNode(varAssignNode->GetRhs(), context);
                }
            }
        }

        return nullptr;
    }

    std::vector<DeviceMemoryConfiguration>& _propertyValue;
    std::string _propertyName;
    Compiler& _compiler;
    const StructUnionType* _memoryConfigurationType;
    const Type* _memoryTypeType;
    bool _found = false;
};

DeviceConfigVisitor::DeviceConfigVisitor(Compiler& compiler, CodeGenDeviceConfig& deviceConfig)
: _compiler(compiler), _deviceConfig(deviceConfig)
{
    // Fail fast: Validate that the schema types are available, and if not give the user
    // a clear error message about missing device configuration schema

    std::vector<std::string> neededSchemaTypes = {
        "MemoryResource",
        "MemoryType",
        "MemoryConfiguration"
    };

    Scope schemaScope = {"@compiler@device@schema"};

    for (const auto& typeName : neededSchemaTypes)
    {
        if (compiler.GetNamedType({}, schemaScope, typeName, Location{}) == nullptr)
        {
            compiler.ErrorStream(Location{}, CompileError::MissingDeviceConfigSchema)
                << "Device definition schema types are missing. Check that the device configuration modules are reachable and that compiler settings have not set a base module that does not include them.";
            throw std::runtime_error("Device config extraction failed");
        }
    }

    // TODO: Set reasonable defaults for legacy device configuration properties that we
    // will remove with OSS
    _deviceConfig = {};

    AddPropertyExtractor("integer_mul_src_width", _deviceConfig._integerMulSrcWidth);
    AddPropertyExtractor("almost_empty_depth", _deviceConfig._almostEmptyDepth);
    AddPropertyExtractor("unsigned_integer_mul_name", _deviceConfig._unsignedIntegerMulName);
    AddPropertyExtractor("signed_integer_mul_name", _deviceConfig._signedIntegerMulName);
    AddPropertyExtractor("verilog_dont_merge_pragma_name", _deviceConfig._verilogDontMergePragmaName);
    AddPropertyExtractor("memory_init_file_type", _deviceConfig._memoryInitFileType);
    AddPropertyExtractor("small_lut_size", _deviceConfig._smallLutSize);
    AddPropertyExtractor("lut_based_shift_register_available", _deviceConfig._isLutBasedShiftRegisterAvailable);
    AddPropertyExtractor("block_ram_supports_hardened_bypass", _deviceConfig._supportsRmwBram);
    AddPropertyExtractor("global_data_propagation_ram_alignment", _deviceConfig._globalDataPropgationRamAlignment);
    AddPropertyExtractor("min_fifo_depth", _deviceConfig._minFifoDepth);
    AddPropertyExtractor("min_dual_clock_fifo_depth", _deviceConfig._minDualClockFifoDepth);
    AddPropertyExtractor("min_almost_full_depth", _deviceConfig._minAlmostFullDepth);
    AddPropertyExtractor("supports_auto_pipelining", _deviceConfig._supportsAutoPipelining);
    AddPropertyExtractor("min_auto_pipeline_depth", _deviceConfig._minAutoPipelineDepth);
    AddPropertyExtractor("supports_luts", _deviceConfig._supportsLuts);
    AddPropertyExtractor("block_ram_supports_true_dual_port", _deviceConfig._supportsTrueDualPortRam);
    AddPropertyExtractor("use_internal_buffer_fifo_optimization", _deviceConfig._useInternalBufferFifoOptimization);
    AddPropertyExtractor("fifo_depth_pow2", _deviceConfig._fifoDepthPow2);
    AddPropertyExtractor("block_ram_supports_ecc", _deviceConfig._eccMemorySupport);
    AddPropertyExtractor("block_ram_supports_quad_port", _deviceConfig._quadPortRamSupport);
    AddPropertyExtractor("registers_require_power_on_initial_value", _deviceConfig._requirePowerOnReset);
    AddPropertyExtractor("use_thread_rate_in_fifo_sizing", _deviceConfig._useThreadRateInFifoSizing);
    AddPropertyExtractor("fifo_width_alignment", _deviceConfig._fifoWidthAlignment);
    AddPropertyExtractor("fifo_depth_alignment", _deviceConfig._fifoDepthAlignment);
    AddPropertyExtractor("fifo_fixed_cost", _deviceConfig._fifoFixedCost);
    AddPropertyExtractor("fifo_bits_per_register", _deviceConfig._fifoBitsPerRegister);
    AddPropertyExtractor("array_mux_cost_factor", _deviceConfig._arrayMuxCostFactor);
    AddPropertyExtractor("array_register_cost_factor", _deviceConfig._arrayRegisterCostFactor);
    AddPropertyExtractor("array_write_port_cost_factor", _deviceConfig._arrayWritePortCostFactor);
    AddPropertyExtractor("default_clock_frequency_mhz", _defaultClockFrequencyMhz);
    AddPropertyExtractor("trade_area_for_speed", _deviceConfig._largeAndFast);
    AddPropertyExtractor("device_name", _deviceConfig._deviceName);
    AddPropertyExtractor("vendor", _deviceConfig._vendor);
    AddPropertyExtractor("device_family", _deviceConfig._deviceFamily);
    AddPropertyExtractor("hal_device_family", _deviceConfig._halDeviceFamily);

    AddAggregatePropertyExtractor<DeviceConfigMemoryResourceExtractor>("memory_resources", _memoryResources);
    AddAggregatePropertyExtractor<DeviceConfigMemoryConfigurationExtractor>("memory_configurations", _memoryConfigurations);
}

DeviceConfigVisitor::~DeviceConfigVisitor() = default;

void DeviceConfigVisitor::VisitNode(const ParseTreeNode* const node, VisitContext& context)
{
    const auto declareNode = dynamic_cast<const DeclareNode*>(node);
    if (nullptr != declareNode)
    {
        auto scope = declareNode->GetScope();
        auto assignNode = declareNode->GetAssignNode();

        if (nullptr != assignNode && ScopesEqual(_configScope, scope))
        {
            // Look up a matching property visitor in _propertyVisitors
            auto it = _propertyVisitors.find(declareNode->GetDeclaredName());

            if (it != _propertyVisitors.end())
            {
                // Validate that it's constant here - saves us from having to check this in every property visitor
                if (!declareNode->_isConst)
                {
                    auto location = declareNode->GetLocation();

                    _compiler.ErrorStream(
                        location,
                        CompileError::InvalidDeviceConfigProperty) << "Device configuration property '" << declareNode->GetDeclaredName() <<"' must be declared as const";

                    throw std::runtime_error("Device config extraction failed");
                }

                it->second->Extract(node, context);
            }
        }
    }
}

bool DeviceConfigVisitor::Finalize(std::vector<std::string>& missingPropertyNames) const
{
    bool validateSuccess = true;

    // Loop through all property visitors and check if they found their properties
    for (const auto& propertyVisitor : _propertyVisitors)
    {
        if (!propertyVisitor.second->Validate(missingPropertyNames))
        {
            validateSuccess = false;
        }
    }

    // Migrate memory configuration over to the legacy device config structure

    std::vector<CodeGenDeviceConfig::MemoryConfig::width_depth_cost_t> lutRamConfigs;
    std::vector<CodeGenDeviceConfig::MemoryConfig::width_depth_cost_t> blockRamConfigs;
    std::vector<CodeGenDeviceConfig::MemoryConfig::width_depth_cost_t> deepRamConfigs;

    for (const auto& memoryConfiguration : _memoryConfigurations)
    {
        if (memoryConfiguration._type == DeviceMemoryType::LUT)
        {
            lutRamConfigs.push_back({memoryConfiguration._width, memoryConfiguration._depth, memoryConfiguration._cost});
        }
        else if (memoryConfiguration._type == DeviceMemoryType::Block)
        {
            blockRamConfigs.push_back({memoryConfiguration._width, memoryConfiguration._depth, memoryConfiguration._cost});
        }
        else if (memoryConfiguration._type == DeviceMemoryType::Deep)
        {
            deepRamConfigs.push_back({memoryConfiguration._width, memoryConfiguration._depth, memoryConfiguration._cost});
        }
    }

    if (lutRamConfigs.size() > _deviceConfig._memory._lutRamConfigs.size()
        || blockRamConfigs.size() > _deviceConfig._memory._blockRamConfigs.size()
        || deepRamConfigs.size() > _deviceConfig._memory._deepRamConfigs.size())
    {
        _compiler.ErrorStream(
            Location{},
            CompileError::InvalidDeviceConfigProperty) << "Device memory configuration must not exceed 10 elements";

        validateSuccess = false;
    }
    else
    {
        // Make a function to use in the sort below
        auto sortFn = [](const CodeGenDeviceConfig::MemoryConfig::width_depth_cost_t& a, const CodeGenDeviceConfig::MemoryConfig::width_depth_cost_t& b)
        {
            return (a.depth > b.depth) || (a.depth == b.depth && a.width > b.width);
        };

        // Sort the configurations from deepest to shallowest, and then by widest to narrowest (based on comment in GetMemoryComposition)
        std::sort(lutRamConfigs.begin(), lutRamConfigs.end(), sortFn);
        std::sort(blockRamConfigs.begin(), blockRamConfigs.end(), sortFn);
        std::sort(deepRamConfigs.begin(), deepRamConfigs.end(), sortFn);

        // Copy them over to the device config
        std::copy(lutRamConfigs.begin(), lutRamConfigs.end(), _deviceConfig._memory._lutRamConfigs.begin());
        std::copy(blockRamConfigs.begin(), blockRamConfigs.end(), _deviceConfig._memory._blockRamConfigs.begin());
        std::copy(deepRamConfigs.begin(), deepRamConfigs.end(), _deviceConfig._memory._deepRamConfigs.begin());
    }

    // Populate the count of the different types of memory resources
    for (const auto& memoryResource : _memoryResources)
    {
        if (memoryResource._type == DeviceMemoryType::LUT)
        {
            _deviceConfig._memory._numLutRam = memoryResource._numAvailable;
        }
        else if (memoryResource._type == DeviceMemoryType::Block)
        {
            _deviceConfig._memory._numBlockRam = memoryResource._numAvailable;
        }
        else if (memoryResource._type == DeviceMemoryType::Deep)
        {
            _deviceConfig._memory._numDeepRam = memoryResource._numAvailable;
        }
    }

    _deviceConfig._isDeepRamAvailable = (_deviceConfig._memory._numDeepRam > 0) && deepRamConfigs.size() > 0;
    std::fill(_deviceConfig._defaultFreqMhz, _deviceConfig._defaultFreqMhz + MaxClockCount, _defaultClockFrequencyMhz);

    // TODO: Remove before OSS
    if (_deviceConfig._largeAndFast)
    {
        AdjustCodeGenConfigForLargeAndFast();
    }

    _deviceConfig._isInitialized = validateSuccess;

    return validateSuccess;
}

template<typename PropertyType>
void DeviceConfigVisitor::AddPropertyExtractor(const std::string& propertyName, PropertyType& propertyValue)
{
    auto visitor = std::make_unique<DeviceConfigPropertyExtractor<PropertyType>>(propertyValue, propertyName, _compiler);
    _propertyVisitors[propertyName] = std::move(visitor);
}

template<typename ExtractorType, typename PropertyType>
void DeviceConfigVisitor::AddAggregatePropertyExtractor(const std::string& propertyName, PropertyType& propertyValue)
{
    _propertyVisitors[propertyName] = std::make_unique<ExtractorType>(propertyValue, propertyName, _compiler);
}