// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include <string>
#include <fstream>
#include <vector>
#include <iostream>
#include <sstream>
#include <filesystem>
#include <boost/algorithm/string.hpp>

// Test to ensure code snippets in document compile successfully

// Converts <!-- abc --> into: abc
std::string StripHtmlComment(const std::string& line)
{
    std::string result;

    if (line.length() >= 7 && line.substr(0, 4) == "<!--")
    {
        result = line.substr(4, line.length() - 7);

        // remove whitespace
        boost::trim_right(result);
        boost::trim_left(result);
    }

    return result;
}

// Given a code snippet
// Generate a temporary file containing the context + the code snippet inserted at the correct location
// Invoke the compiler and fail if the return code is not zero
bool CompileCodeBlock(const std::vector<std::string>& curCodeBlock,
                      const std::string& contextFile,
                      const std::string& compilerPath,
                      std::string cmdLineArgs,
                      const std::vector<std::string>& libPaths,
                      const bool hideOutput)
{
    cmdLineArgs += " --define=MemoryLength=32";

    cmdLineArgs += " --target-device=mock";

    for (const std::string& s : libPaths)
    {
        cmdLineArgs += " --import-dir=";
        cmdLineArgs += s;
    }

    // Parse the context file, building the final file to test
    std::ifstream context(contextFile);

    if (!context)
    {
        throw std::runtime_error(std::string("Failed to load context file: ") + contextFile);
    }

    static size_t fileIndex = 0;

    // Use a unique file name for each temporary file
    // So that file contents can be inspected after a full run
    const std::string tempFileName = "temp.k" + std::to_string(fileIndex++);

    std::ofstream tempFile(tempFileName);

    size_t numInsertLocations = 0;

    while (true)
    {
        std::string line;
        std::getline(context, line);

        if (!context)
        {
            break;
        }

        if (line == "***")
        {
            // Replace *** with the snippet from the markdown
            for (const std::string& s : curCodeBlock)
            {
                tempFile << s << "\n";
            }

            numInsertLocations++;
        }
        else
        {
            tempFile << line << "\n";
        }
    }

    tempFile.close();

    if (numInsertLocations != 1)
    {
        std::cout << "Insert location count: " << numInsertLocations << "\n";
        throw std::runtime_error("Didn't find exactly 1 insert location");
    }

    std::string commandLine = compilerPath + " " + cmdLineArgs + " " + tempFileName;

    if (hideOutput)
    {
        commandLine += " >> nul";
    }

    const int returnCode = system(commandLine.c_str());

    return returnCode == 0;
}


std::vector<std::string> SplitString(const std::string& str, const char delimiter)
{
    std::vector<std::string> tokens;
    std::istringstream stream(str);

    while (stream)
    {
        std::string token;
        std::getline(stream, token, delimiter);

        if (stream && !token.empty())
        {
            tokens.push_back(token);
        }
    }

    return tokens;
}


// Determines if the code block represents a module
// If so, writes the module to a file so that it can be imported by future code blocks
void WriteModuleToTemporaryFile(const std::vector<std::string>& curCodeBlock)
{
    if (!curCodeBlock.empty())
    {
        const std::string& firstLine = curCodeBlock[0];

        const auto tokens = SplitString(firstLine, ' ');

        if ((tokens.size() == 2) && (tokens[0] == "module"))
        {
            const auto segments = SplitString(tokens[1], '.');

            std::filesystem::path path;

            for (const auto& seg : segments)
            {
                path /= seg;
            }

            path.replace_extension(".k");
            const auto dir = std::filesystem::path(path).parent_path();

            if (!dir.empty())
            {
                std::filesystem::create_directories(dir);
            }

            std::ofstream outputStr(path);

            for (const std::string& line : curCodeBlock)
            {
                outputStr << line << "\n";
            }
        }
    }
}

// returns false if an error is encountered
bool TestCodeBlock(const std::string& line_no,
                   const std::vector<std::string>& curCodeBlock,
                   const std::string& prevLine,
                   const std::string& compilerPath,
                   const std::string& contextPath,
                   const std::vector<std::string>& contextFiles,
                   const std::string& stdlibPath,
                   const std::vector<std::string>& libPaths)
{
    const auto comment = StripHtmlComment(prevLine);

    if (comment == "invalid")
        return true;

    if (!comment.empty())
    {
        const std::string contextFile = contextPath + "/" + comment;

        if (CompileCodeBlock(curCodeBlock, contextFile, compilerPath, "", libPaths, false))
            return true;
    }
    else
    {
        if (contextFiles.empty())
        {
            throw std::runtime_error("A context file must be specified either on command line or in the markdown.");
        }

        // In the first pass, redirect Kanagawa compiler output to nul
        // On the second pass (if all compilations failed, then don't redirect output)
        for (size_t pass = 0; pass < 2; pass++)
        {
            const std::string miniBaseLib = "--base=" + stdlibPath + "/mini-base.k ";
            const std::string none = "";

            for (std::string args: {miniBaseLib, none})
            {
                for(auto& p: contextFiles)
                {
                    const std::string contextFile = contextPath + "/" + p;

                    if (CompileCodeBlock(curCodeBlock, contextFile, compilerPath, args, libPaths, pass == 0))
                        return true;
                }
            }
        }
    }

    std::cout << "Failed to compile code block at line: " << line_no << "\n";
    return false;
}

int main(int argc, const char** argv)
{
    int result = 1;

    try
    {
        if (argc < 6)
        {
            throw std::runtime_error("Incorrect argument count.  Expected: compiler-path contexts-path lib-path stdlib-path [context-file1.k context-file2.k] file1.md file2.md");
        }

        const std::string compilerPath = argv[1];
        const std::string contextPath = argv[2];
        const std::string stdlibPath = argv[4];

        std::vector<std::string> libPaths;
        libPaths.push_back(argv[3]);
        libPaths.push_back(argv[4]);

        std::cout << "Compiler path: " << compilerPath << "\n";
        std::cout << "Context path: " << contextPath << "\n";

        std::cout << "Library paths:\n";
        for (const std::string& s : libPaths)
        {
            std::cout << s << "\n";
        }

        std::vector<std::string> contextFiles;

        int i = 5;

        for (; i < argc; i++)
        {
            const std::string arg = argv[i];

            if (arg.substr(arg.size() - 3) != ".k")
                break;

            contextFiles.push_back(arg);
        }

        size_t errorCount = 0;

        for (; i < argc; i++)
        {
            const std::string mdFileName = argv[i];

            size_t codeBlockCount = 0;

            std::cout << "Processing: " << mdFileName << "\n";

            std::ifstream markdownFile(mdFileName);
            if (!markdownFile)
            {
                throw std::runtime_error(std::string("Failed to open file: ") + mdFileName);
            }

            std::string line_no;
            bool inCodeBlock = false;
            std::vector<std::string> curCodeBlock;

            std::string prevLine;

            bool isCplusPlusCodeBlock = false;

            for (unsigned i = 1; true; ++i)
            {
                std::string line;
                std::getline(markdownFile, line);

                if (!markdownFile)
                {
                    break;
                }

                // Check if the line is the start/end of a code block
                const bool isCodeBlockDelim = (line.substr(0, 3) == "```"); // substr used to allow ```C

                if (isCodeBlockDelim)
                {
                    if (!isCplusPlusCodeBlock && !inCodeBlock)
                    {
                        isCplusPlusCodeBlock = (line.substr(0, 6) == "```C++" || line.substr(0, 7) == "```bash" || line.substr(0, 5) == "```md");
                    }

                    if (inCodeBlock)
                    {
                        // At the end of a code block
                        if (curCodeBlock.empty())
                        {
                            throw std::runtime_error("Found an empty code block at " + line_no);
                        }

                        if (!isCplusPlusCodeBlock)
                        {
                            WriteModuleToTemporaryFile(curCodeBlock);

                            if (!TestCodeBlock(
                                line_no,
                                curCodeBlock,
                                prevLine,
                                compilerPath,
                                contextPath,
                                contextFiles,
                                stdlibPath,
                                libPaths))
                            {
                                // Continue testing other blocks
                                // so that all errors can be enumerated in 1 execution
                                errorCount++;
                            }

                            codeBlockCount++;
                        }

                        isCplusPlusCodeBlock = false;

                        curCodeBlock.clear();
                    }
                    else
                    {
                        line_no = std::to_string(i);
                    }

                    inCodeBlock = !inCodeBlock;
                }
                else if (inCodeBlock)
                {
                    curCodeBlock.push_back(line);
                }
                else
                {
                    prevLine = line;
                }
            }

            if (inCodeBlock)
            {
                throw std::runtime_error("Ending parsing in the middle of a code block");
            }

            std::cout << "Tested: " << codeBlockCount << " code blocks\n";
        }

        if (errorCount > 0)
        {
            throw std::runtime_error("Compilation errors detected");
        }

        // All done
        result = 0;
    }
    catch(std::exception& e)
    {
        std::cout << "error: " << e.what() << "\n";
        result = 1;
    }

    return result;
}
