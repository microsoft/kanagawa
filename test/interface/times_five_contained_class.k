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

// A class that contains an instance of SimpleExportedClass
class ContainerOne
{
public:
    inline uint32 CallInner(uint32 x)
    {
        return _inner.TimesFive(x);
    }

private:
    SimpleExportedClass _inner;
}

class TimesFiveContainedClass
{
private:
    ContainerOne _containerOne;

public:
    uint32 TimesFiveContained(uint32 a)
    {
        return _containerOne.CallInner(a);
    }
}

export TimesFiveContainedClass;
