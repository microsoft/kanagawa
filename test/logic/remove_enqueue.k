// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Tests for optimizations that remove Enqueue operations
import test.unit as unit
import test.runner

// A function call that can never occur
inline void RemoveCall(unit::tag_t tag)
{
    inline void inner()
    {
        pipelined_for(32, [](uint32 tid)
        {
            print("in helper\n");
        });
    }

    inline void outer(uint32 x)
    {
        if (x == 3)
        {
            inner();
        }
    }

    outer(4);
}

// Loop backward link
inline void RemoveLoopBackwardLink(unit::tag_t tag)
{
    inline void loop(uint32 x)
    {
        do
        {
            print("inside loop\n");
        } while (x == 0);
    }

    loop(1);
}

// Loop forward link
class RemoveLoopForwardLink
{
private:
    uint32 _forwardLinkCount = 0;

public:
    void run(unit::tag_t tag)
    {
        async_exec([]()
        {
            do
            {
                atomic
                {
                    _forwardLinkCount++;
                }
            } while (true);
        });

        // Verify the variable is incrementing
        uint32 original = _forwardLinkCount;

        atomic do; while(_forwardLinkCount <= original);
    }
}

// Loop entrance fifo
inline void RemoveLoopEntrance(unit::tag_t tag)
{
    inline void loop(uint32 x)
    {
        switch (x)
        {
        case 2:
            do
            {
                print("inside loop\n");
                x++;
            } while (x > 3);
            break;
        }
    }

    loop(1);
}

// Function callee
class RemoveFunctionCall
{
private:
    noinline void f(uint32 x)
    {
        println(x);
    }

    void outer(uint32 x)
    {
        if (x == 2)
        {
            f(x);
        }
    }

public:
    void run(unit::tag_t tag)
    {    
        outer(1);
    }
}

inline void test_main()
{
    unit::test<1>(RemoveCall);
    unit::test<2>(RemoveLoopBackwardLink);
    unit::test<3>(unit::fixture<RemoveLoopForwardLink>());
    unit::test<4>(RemoveLoopEntrance);
    unit::test<5>(unit::fixture<RemoveFunctionCall>());
}
