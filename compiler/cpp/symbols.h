// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include "debug_symbols.h"
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <vector>

uint64_t CreateDebugSymbols(const std::string& fileName, const Program& program,
                            const BasicBlockDebugMaps& allBlocksData, const uint64_t symbolHash);

class SymbolWriter
{
  public:
    void Begin(const std::string& title) { _str << title; }

    template <typename T> void Append(const std::string& key, const T& value) { _str << "," << key << "," << value; }

    void End() { _str << std::endl; }

    uint64_t Finalize(const std::string& moduleName, std::ostream& outputStream)
    {
        const std::string contents = _str.str();

        // Compute hash of symbol file contents
        uint64_t hash = Fnv1Hash<64>(contents);

        outputStream << "module,name," << moduleName << ",hash," << std::hex << hash << std::endl;
        outputStream << contents;

        return hash;
    }

  private:
    std::ostringstream _str;
};

class SymbolReader
{
  public:
    bool OpenFile(const std::string& fileName) { return OpenInputFileStream(_inputStream, fileName.c_str()); }

    std::vector<std::string> SplitCsvLine(const std::string& str)
    {
        std::vector<std::string> result;

        std::istringstream iss(str);

        std::string token;

        while (getline(iss, token, ','))
        {
            result.push_back(token);
        }

        return result;
    }

    bool SkipToToken(const std::string& token)
    {
        bool result = false;

        std::string str;
        while (GetNextLine(str))
        {
            if (str.find(token) != std::string::npos)
            {
                result = true;
                break;
            }
        }

        return result;
    }

    bool GetNextLine(std::string& str, bool checkForEndOfSection = false)
    {
        if (_inputStream.eof())
        {
            return false;
        }

        bool result = true;

        _pos = _inputStream.tellg();

        getline(_inputStream, str);

        // Check if next section has been reached
        if (checkForEndOfSection && str.find("---") != std::string::npos)
        {
            // Go to the beginning of the line
            _inputStream.seekg(_pos, std::ios_base::beg);

            result = false;
        }

        return result;
    }

    ~SymbolReader() { _inputStream.close(); }

  private:
    std::ifstream _inputStream;

    size_t _pos;
};
