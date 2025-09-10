// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include <string>

// If the input path has directory names in it, remove them
static std::string StripLeadingPath(const std::string& fullFileName)
{
    // Strip the path from the file name
    std::string fileNameWithoutPath;

    // Windows filename?
    size_t lastSeparator = fullFileName.find_last_of('\\');

    // ..or Unix?
    if (std::string::npos == lastSeparator)
    {
        lastSeparator = fullFileName.find_last_of('/');
    }

    if (std::string::npos == lastSeparator)
    {
        fileNameWithoutPath = fullFileName;
    }
    else
    {
        fileNameWithoutPath = fullFileName.substr(lastSeparator + 1, std::string::npos);
    }

    return fileNameWithoutPath;
}

