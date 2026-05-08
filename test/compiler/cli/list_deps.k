// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Minimal program used by the `list-deps` CLI test. The explicit import
// of `data.closure` proves the dependency walker reaches transitively
// imported modules (closure pulls in `data.closure.core`), and the
// implicitly imported mini-base library should also appear in the
// emitted file list.
import data.closure

class Main
{
public:
    void main()
    {
    }
}

export Main;
