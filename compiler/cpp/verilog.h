// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

void CompileVerilog(const char* const svFileName, const char* const svPackageFileName, const char* const cppFileName,
                    const char* const headerFileName, const char* const tclFileName,
                    const char* const hwConfigMkFileName, const char* const memFileBase,
                    const char* const rtlMapFileName, const char* const circtAsmFileName, const Program& program);

struct RaceCounterTreeDesc
{
    size_t _width;
    size_t _depth;
};

RaceCounterTreeDesc GetRaceCounterWidthDepth(const size_t rawSize, const size_t reductionWidth);

size_t GetClampedRegisterWidth(const size_t logicalWidth);

std::string OptionalWidthDeclarationNoSpace(const size_t width);

std::string OptionalWidthDeclaration(const size_t width);

// RAII class to emit //synopsys translate {off,on}, and apply emulation compile directives
class DisableTranslation
{
  public:
    DisableTranslation(SourceWriter& writer) : _writer(writer)
    {
        _writer.Str() << "`ifndef EMULATION_AND_FPGA";
        _writer.Str() << "//synopsys translate_off";
    }

    ~DisableTranslation()
    {
        _writer.Str() << "//synopsys translate_on";
        _writer.Str() << "`endif";
    }

  private:
    SourceWriter& _writer;
};
