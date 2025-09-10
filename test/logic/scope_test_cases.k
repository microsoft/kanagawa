// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// inline function accessing a global variable with the same name as a local variable in the calling function
class GlobalLocalScopeTest
{
private:
    uint64 GlobalLocalScopeTestVariable;

public:
    inline void GlobalLocalScopeTestHelper(unit::tag_t tag)
    {
        unit::assert(tag, GlobalLocalScopeTestVariable == 123);
    }

    void run(unit::tag_t tag)
    {
        // Write the global variable
        GlobalLocalScopeTestVariable = 123;

        {
            // Declare a local variable with the same name
            uint64 GlobalLocalScopeTestVariable = 0;

            // Call an inline function that should access the global variable - not the local
            GlobalLocalScopeTestHelper(tag);
        }
    }
}

// Nested class references a constant from outer class
class ParentConstantOuter
{
public:
    const auto C = 8;

    class Inner
    {
    public:
        const auto D = C + 2;

        inline uint32 Get()
        {
            return D;
        }
    }

    Inner _inner;

    inline void Test(unit::tag_t tag)
    {
        unit::assert(tag, _inner.Get() == 10);
    }
}

class ParentConstantOuterTest
{
private:
    ParentConstantOuter parentConstantOuter;

public:
    void run(unit::tag_t tag)
    {
        parentConstantOuter.Test(tag);
    }
}

inline void test_main()
{
    print("GlobalLocalScopeTest\n");
    unit::test<1>(unit::fixture<GlobalLocalScopeTest>());

    print("ParentConstantOuterTest\n");
    unit::test<2>(unit::fixture<ParentConstantOuterTest>());
}
