// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Pre-computing global load address with initial value
class Main
{
public:
    uint32 main(uint32 x)
    {
        static [[memory]] uint32[512] _mem;
        static uint32 _idx = 2;

        uint32 result;

        atomic
        {
            result = _mem[_idx + 1];
            _idx = x;
        }

        return result;
    }
}

export Main;
expected:0

// Pre-computing global load address requires with address written in a separate atomic block
class Main
{
public:
    uint32 main(uint32 x)
    {
        static [[memory]] uint32[512] _mem;
        static uint32 _idx;

        atomic
        {
            _idx = x;
        }

        uint32 result;

        atomic
        {
            result = _mem[_idx + 1];
        }

        return result;
    }
}

export Main;
expected:0

// Pre-computing global load address requires with address written in 2 atomic blocks
class Main
{
public:
    uint32 main(uint32 x)
    {
        static [[memory]] uint32[512] _mem;
        static uint32 _idx;

        atomic
        {
            _idx = x;
        }

        uint32 result;

        atomic
        {
            result = _mem[_idx + 1];

            _idx = x;
        }

        return result;
    }
}

export Main;
expected:0

// Impossible-to-schedule block
class Main
{
public:
    uint32 main(uint32 x)
    {
        uint32 result;

        static [[memory]] uint32[512] _mem1;
        static [[memory]] uint32[512] _mem2;

        [[schedule(1)]]
        {
            uint32 addr = _mem1[x];

            result = _mem2[addr];
        }

        return result;
    }
}

export Main;

expected:64

// Memory read in atomic block
// where address is written before and after the memory read
class Main
{
public:
    uint32 main(uint32 x)
    {
        static [[memory]] uint32[512] _mem;
        static uint32 _addr;

        atomic
        {
            if (x == 0)
            {
                _addr = x;
            }

            _addr = _mem[_addr];
        }

        return _addr; // ensure the shared variables are not optimized away
    }
}

export Main;
expected:64

    // Scheduling succeeds with [[pure]]
    class RegisteredIncrement
    {
    public:
        [[pure]] [[latency(1)]] uint32 increment(uint32 x);
    }

    extern RegisteredIncrement;

    class C
    {
    private:
        RegisteredIncrement _obj;

    public:
        uint32 main(uint32 id)
        {
            uint32 result;

            bool predicate;

            atomic
            {
                static uint32 _idx = -1;

                predicate = (0 != (_idx % 4)) || (0 != (id % 2));

                if (predicate)
                {
                    result = _obj.increment(_idx + 1);
                }

                _idx = id;
            }

            return result;
        }
    }

    export C;

expected:0

    // Scheduling fails without [[pure]]
    // Same as above, but [[pure]] has been removed
    class RegisteredIncrement
    {
    public:
        [[latency(1)]] uint32 increment(uint32 x);
    }

    extern RegisteredIncrement;

    class C
    {
    private:
        RegisteredIncrement _obj;

    public:
        uint32 main(uint32 id)
        {
            uint32 result;

            bool predicate;

            atomic
            {
                static uint32 _idx = -1;

                predicate = (0 != (_idx % 4)) || (0 != (id % 2));

                if (predicate)
                {
                    result = _obj.increment(_idx + 1);
                }

                _idx = id;
            }

            return result;
        }
    }

    export C;

expected:64