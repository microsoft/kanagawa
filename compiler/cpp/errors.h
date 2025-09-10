// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

enum class CompileError
{
    SyntaxError = 1,
    DuplicateFunction = 2,
    DuplicateType = 3,
    UnknownType = 4,
    UnknownFunction = 5,
    ExportTypeRestriction = 6,
    InvalidGlobalAssignment = 7,
    MultipleMemoryWrites = 8,
    InvalidArrayIndexing = 9,
    PartialElementWrite = 10,
    InvalidLiteral = 11,
    UnorderedOperation = 12,
    InvalidSwitch = 13,
    InvalidConversion = 14,
    InvalidType = 16,
    InvalidArraySize = 17,
    InvalidFunctionModifier = 18,
    InvalidNesting = 19,
    InvalidFunctionReturnType = 20,
    InvalidReturn = 21,
    InvalidIterationCount = 22,
    NonValueType = 24,
    InvalidLoop = 25,
    InvalidMux = 26,
    InvalidCall = 27,
    InvalidContainer = 28,
    ProtectionModifier = 29,
    InvalidMember = 30,
    InvalidExtern = 31,
    UnknownIdentifier = 32,
    DuplicateIdentifier = 33,
    InvalidAtomic = 34,
    CallRateNotPredicated = 35,
    ExpressionNotStatic = 36,
    RecursionNotSupported = 37,
    InvalidWaitForCondition = 38,
    UnmatchedComment = 40,
    InvalidReset = 41,
    StaticAssert = 43,
    TypeToWide = 44,
    TooManyReadPorts = 45,
    InvalidPerThreadUsage = 47,
    ReservedName = 49,
    InvalidInspectionTarget = 50,
    TooManyInspectableVariables = 51,
    InspectableTooDeep = 52,
    InvalidString = 53,
    InvalidConst = 54,
    InvalidEndTransaction = 56,
    InvalidUpdateRate = 57,
    InvalidClockSpecifier = 58,
    InvalidDebugView = 59,
    DivideByZero = 60,
    AssignmentToConst = 61,
    InvalidInitializerList = 62,
    InvalidStaticLocal = 63,
    BasicBlockNotSchedulable = 64,
    InvalidAttribute = 65,
    InvalidExportClass = 66,
    InvalidQuadPortMemory = 67,
    InvalidRAMInitialization = 68,
    InvalidEccFunction = 69,
    EccUnsupported = 70,
    AmbiguousDebugSymbols = 71,
    InvalidMemoryReadDelay = 72,
    MethodReturningReference = 73,
    InvalidExternFunction = 74,
    InvalidCallback = 75,
    DeprecatedFeature = 76,
    InvalidAssignment = 77,
    InvalidIntrinsicCall = 78,
    InvalidTemplateArgument = 79,
    MissingDeviceConfigProperty = 81,
    InvalidDeviceConfigProperty = 82,
    QuadPortUnsupported = 83,
    MissingDeviceConfigSchema = 84,
};

enum class CompileWarning
{
    TooManyRams = 8,
    LastWithoutTransactionSize = 10,
    NonConstGlobal = 12,
    FreeNonInlineFunction = 13
};

// RAII class to call windows API to change console output color
class ChangeConsoleColor
{
  public:
    ChangeConsoleColor(const ConsoleColor color) { SetConsoleColor(color); }

    ~ChangeConsoleColor() { SetConsoleColor(ConsoleColor::Default); }
};

// Wraps a std::ostream, inserts boilerplate strings around the error
struct ErrStream
{
    ErrStream(const std::string& fileName, const size_t lineNumber, const CompileError error,
              const std::string& callTrace)
        : _stream(std::cout), _changeConsoleColor(ConsoleColor::Red), _callTrace(callTrace)
    {
        _stream << "Error " << static_cast<uint32_t>(error) << " at: " << fileName << " (" << lineNumber << "): ";
    }

    ErrStream(const std::string& fileName, const size_t lineNumber, const CompileWarning warning)
        : _stream(std::cout), _changeConsoleColor(ConsoleColor::Yellow), _callTrace("")
    {
        _stream << "Warning " << static_cast<uint32_t>(warning) << " at: " << fileName << " (" << lineNumber << "): ";
    }

    ErrStream(const CompileWarning warning)
        : _stream(std::cout), _changeConsoleColor(ConsoleColor::Yellow), _callTrace("")
    {
        _stream << "Warning " << static_cast<uint32_t>(warning) << ": ";
    }

    ~ErrStream()
    {
        _stream << "\n";

        _stream << _callTrace;

        // Flush the stream so that the console color change takes effect
        _stream.flush();
    }

    template <typename T> ErrStream& operator<<(const T& obj)
    {
        _stream << obj;

        return *this;
    }

  private:
    ErrStream& operator=(const ErrStream& rhs);

    ChangeConsoleColor _changeConsoleColor;

    std::ostream& _stream;

    std::string _callTrace;
};
