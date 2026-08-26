// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Minimal program used by the --verilator-hier-blocks CLI test.
class VerilatorHierBlock
{
public:
    uint32 TimesTwo(uint32 x)
    {
        return x + x;
    }
}

export VerilatorHierBlock;
