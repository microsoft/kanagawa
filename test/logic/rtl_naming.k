// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.runner

// To test the case of duplicated function names after flattening
// The compiler should handle it apppropriately by generating different basic block names.
class Test
{
public:
    class A
    {
    public:
        noinline void B()
        {
            __print("Function B called from class A\n");
        }
    };

    noinline void instA_B()
    {
        __print("Function A_B\n");
    }

    void call_instA()
    {
        instA.B();
    }

    A instA;
}

inline void test_main()
{
    static Test test;

    test.call_instA();
    test.instA_B();
}
