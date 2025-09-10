// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    class C
    {
    }

    inline void main()
    {
        // Initializing and object to an empty initializer list is fine
        static C c = {};
    }

expected:0

    class C
    {
    }

    inline void main()
    {
        static C c;

        // Assigning an object to an initializer list is an error
        c = {};
    }

expected:77
