// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Tests for exported class functionality

class SimpleExportedClass
{
private:
    uint32 _z = 5;

public:
    uint32 TimesFive(uint32 x)
    {
        return lutmul(x, _z);
    }
}

// An exported class
export SimpleExportedClass;
