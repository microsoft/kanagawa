// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

// A path through the tree of objects
using ObjectPath = std::vector<std::string>;

ObjectPath AppendToPath(const ObjectPath& path, const std::string& suffix);

ObjectPath SubPath(const ObjectPath& src, const size_t offset, const size_t len);

std::string SerializePath(const ObjectPath& path, const char delimeter = '/');