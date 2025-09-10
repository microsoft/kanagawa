// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include <boost/stacktrace.hpp>
#include <sstream>

std::string GetStackTrace()
{
    std::stringstream str;

    str << boost::stacktrace::stacktrace();

    return str.str();
}
