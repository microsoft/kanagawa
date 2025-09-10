// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

class ExportedClassMethodTemplate
{
public:
    template <typename T>
    T TimesFive(T x)
    {
        return lutmul(x, _z);
    }

private:
    uint32 _z = 5;

    void InstantiateMethods()
    {
        // Not called
        assert(false);

        // Instantiate public method template
        const auto TimesFive_uint32 = TimesFive<uint32>;
    }
}

export ExportedClassMethodTemplate;
