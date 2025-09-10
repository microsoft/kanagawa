// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// pipelined function call returns a non-array type
class Main
{
    [[pipelined]] int32 Helper(uint32 tid)
    {
        return tid;
    }

public:
    void main()
    {
        uint32 result = Helper(4);
    }
}

expected:1

// pipelined function call returns an array of the wrong type
class Main
{
    [[pipelined]] int32 Helper(uint32 tid)
    {
        return tid;
    }

public:
    void main()
    {
        uint32[4] result = Helper(4);
    }
}

expected:1

// Pipelined export functions are not allowed
class Main
{
public:
    [[pipelined]] void Foo(uint32 tid)
    {
    }
}

export Main;

expected:1

class Main
{
    // pipelined unordered functions are not allowed
    [[pipelined, unordered]] void Foo(uint32 idx)
    {
    }
}

export Main;

expected:18

class Main
{
    // pipelined function call with count not known
    [[pipelined]] uint8 Foo(uint32 idx)
    {
        return idx;
    }

public:
    void Test(uint32 count)
    {
        uint8[4] r = Foo(count);
    }
}

export Main;

expected:0

class Main
{
    // pipelined function call with wrong size
    [[pipelined]] uint8 Foo(uint32 idx)
    {
        return idx;
    }

public:
    void main()
    {
        uint32 ThreadCount = 5;

        uint8[4] r = Foo(ThreadCount);
    }
}

export Main;

expected:0

class Main
{
    // Reasonable pipelined async call
    [[pipelined, async]] void Foo(uint32 iter)
    {
    }

public:
    void main()
    {
        Foo(3);
    }
}

export Main;

expected:0

class Main
{
    // pipelined async call without count argument
    [[pipelined, async]] void Foo()
    {
    }

public:
    void main()
    {
        Foo();
    }
}

export Main;

expected:22

class Main
{
    // pipelined async call with signed count
    [[pipelined, async]] void Foo(int32 iter)
    {
    }

public:
    void main()
    {
        Foo(-1);
    }
}

export Main;

expected:22

// pipelined async call with struct count
struct Bar
{
    uint32 x;
}

class Main
{
    [[pipelined, async]] void Foo(Bar iter)
    {
    }

public:
    void main()
    {
        Bar b;
        Foo(b);
    }
}

export Main;

expected:22

class Main
{
    // pipelined async call argument/parameter count mismatch
    [[pipelined, async]] void Foo(uint32 iter, uint32 x)
    {
    }

public:
    void globalFunc()
    {
        Foo(3);
    }
}

export Main;

expected:1

class Main
{
    // pipelined async call argument/parameter count mismatch
    [[pipelined, async]] void Foo(uint32 iter)
    {
    }

public:
    void main()
    {
        Foo(3, 5);
    }
}

export Main;

expected:1

// parameter type checking for pipelined async_call
struct Bar
{
    uint32 x;
}

class Main
{
    [[pipelined, async]] void Foo(uint32 iter, Bar b)
    {
    }

public:
    void main()
    {
        uint32 y = 0;

        Foo(3, y);
    }
}

export Main;

expected:1

class Main
{
    // pipelined async functions must return void
    [[pipelined, async]] uint32 Foo(uint32 iter)
    {
    }

public:
    void main()
    {
        Foo(3);
    }
}

export Main;

expected:20

    class Main
    {
        [[pipelined]] uint32 Helper671Inner(uint16 tid, uint32 x)
        {
            return tid + 3;
        }

    public:
        void CheckResult671(uint16 threadCount, uint32[4] result)
        {
        }

        void Helper671(uint16 threadCount)
        {
            CheckResult671(threadCount, Helper671Inner(threadCount, 10));
        }
    }

    export Main;

expected:0

