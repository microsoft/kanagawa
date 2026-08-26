// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Importing `data.closure` (which itself imports `data.closure.core`) makes
// the golden manifest cover transitively imported modules.
import data.closure

class Main
{
public:
    void main()
    {
    }
}

export Main;
