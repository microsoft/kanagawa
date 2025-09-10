// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Tests for exported class functionality

class AnotherExportedClass
{
private:
    // Callback added to test parameter names on callbacks
    // are honored if they are specified
    (uint32 a, uint32 b)->uint32 Add;

public:
    uint32 PlusThree(uint32 x)
    {
        return Add(x, 3);
    }

private:
    // Callback to test async and no_backpressure attributes
    [[async, no_backpressure]]
    (uint32 value)->void AsyncNoBackPressureCallback;

public:
    // Allows triggering the async and no_backpressure attribute callback above
    [[async, no_backpressure]]
    void CallAsyncNoBackPressureCallback(uint32 value)
    {
        AsyncNoBackPressureCallback(value);
    }
}

export AnotherExportedClass;

