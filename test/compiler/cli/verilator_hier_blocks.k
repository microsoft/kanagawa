// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Minimal program used by the Verilator hier_block CLI test.
class VerilatorHierBlock
{
public:
    uint32 TimesTwo(uint32 x)
    {
        return x + x;
    }
}

export VerilatorHierBlock;
