// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Quad-port memory tests
// Only invoked on devices that support it
module device.quad_port_tests
    { DeviceMemoryTests
    }

class WriteThenReadTest
{
public:
    void Test()
    {
        pipelined_for (256, [](uint32 i)
        {
            _mem[i] = i + 0;
            _mem[i + 256] = i + 1000;
        });

        pipelined_for (256, [](uint32 i)
        {
            assert(_mem[i] == i);

            // use a shared address for the load
            // to force output register to be disabled for 1 port
            uint32 val;

            atomic
            {
                static uint9 _addr = 256;
                val = _mem[_addr];
                _addr = static_cast(_addr + 1); // Wrap OK
            }

            assert(val == i + 1000);
        });
    }

private:
    [[memory, quad_port]] uint32[520] _mem;
}

class ConcurrentRwTest
{
public:
    void Test()
    {
        _done = { };

        // This async thread operates on the lower 256 elements
        async_exec([]()
        {
            for (const auto outer : 10)
            {
                pipelined_for (256, [outer](uint32 i)
                {
                    _mem[i] = i + outer;
                });

                pipelined_for (256, [outer](uint32 i)
                {
                    assert(_mem[i] == (i + outer));
                });
            }

            _done[0] = true;
        });

        // This async thread operates on the upper 256 elements
        // 200 threads rather than 256 are used to cause writes from thread 2 to
        // overlap with reads from thread 1
        async_exec([]()
        {
            for (const auto outer : 10)
            {
                pipelined_for (200, [outer](uint32 i)
                {
                    _mem[i + 256] = i + outer;
                });

                pipelined_for (200, [outer](uint32 i)
                {
                    assert(_mem[i + 256] == (i + outer));
                });
            }

            _done[1] = true;
        });

        atomic do; while(!(_done[0] && _done[1]));
    }

private:
    [[memory, quad_port]] uint32[512] _mem;
    bool[2] _done;
}

// read-modify-write on both ports in parallel
class BypassTest
{
public:
    void Test()
    {
        _done = { };

        // This async thread operates on the lower 256 elements
        async_exec([]()
        {
            pipelined_for (256, [](uint32 i)
            {
                uint9 slotIndex = i & 1;
                bool isFirst = i < 2;

                atomic
                {
                    uint32 val = _mem[slotIndex];
                    val = isFirst ? (i + 1) : (val + 1);
                    _mem[slotIndex] = val;
                    _resultA[slotIndex] = val;
                }
            });

            pipelined_for (2, [](uint32 i)
            {
                uint32 val = _resultA[i];

                uint32 expected = static_cast(i + 128);

                assert(val == expected);
            });

            _done[0] = true;
        });

        // This async thread operates on the upper 256 elements
        async_exec([]()
        {
            pipelined_for (256, [](uint32 i)
            {
                uint9 slotIndex;

                atomic
                {
                    static uint8 _slotIndex = 0;

                    slotIndex = _slotIndex;

                    _slotIndex = (_slotIndex == 2) ? 0 : (_slotIndex + 1);
                }

                bool isFirst = i < 3;

                atomic
                {
                    uint32 val = _mem[slotIndex + 256];
                    val = isFirst ? (i + 1) : (val + 1);
                    _mem[slotIndex + 256] = val;
                    _resultB[slotIndex - 256] = val;
                }
            });

            pipelined_for (3, [](uint32 i)
            {
                uint32 val = _resultB[i];

                uint32 expected = mux(cast<uint2>(i), 86, 86, 87, 0);

                assert(val == expected);
            });

            _done[1] = true;
        });

        atomic do; while(!(_done[0] && _done[1]));
    }

private:
    [[memory, quad_port]] uint32[512] _mem;
    [[memory]] uint32[256] _resultA;
    [[memory]] uint32[256] _resultB;
    bool[2] _done;
}

// read-modify-write on only one port
class OnePortBypass
{
public:
    void Test()
    {
        _done = { };

        // This async thread operates on the lower 256 elements
        async_exec([]()
        {
            pipelined_for (256, [](uint32 i)
            {
                _mem[i] = i + 3;
            });

            pipelined_for (2, [](uint32 i)
            {
                uint32 val = _mem[i];

                uint32 expected = static_cast(i + 3);

                assert(val == expected);
            });

            _done[0] = true;
        });

        // This async thread operates on the upper 256 elements
        async_exec([]()
        {
            pipelined_for (256, [](uint32 i)
            {
                uint9 slotIndex;

                atomic
                {
                    static uint8 _slotIndex = 0;

                    slotIndex = _slotIndex;

                    _slotIndex = (_slotIndex == 2) ? 0 : (_slotIndex + 1);
                }

                bool isFirst = i < 3;

                atomic
                {
                    uint32 val = _mem[slotIndex + 256];
                    val = isFirst ? (i + 1) : (val + 1);
                    _mem[slotIndex + 256] = val;
                    _result[slotIndex - 256] = val;
                }
            });

            pipelined_for (3, [](uint32 i)
            {
                uint32 val = _result[i];

                uint32 expected = mux(cast<uint2>(i), 86, 86, 87, 0);

                assert(val == expected);
            });

            _done[1] = true;
        });

        atomic do; while(!(_done[0] && _done[1]));
    }

private:
    [[memory, quad_port]] uint32[512] _mem;
    [[memory]] uint32[256] _result;
    bool[2] _done;
}

// Test for stale bypass values
// write a value on port a (that value is placed into bypass)
// then write a different value on port b (same address)
// then load that element on port a
// The value written on port b should be returned
// This is looking for a bug where a write stays in bypass variables/registers
// until it is replaced by future writes.
// Bypass data should age out even if there are not more writes
class StaleBypassTest
{
public:
    void Test()
    {
        // _mem[4] == 6 (port a)
        PortA(4, make_optional<uint32>(true, 6));

        // _mem[4] == 9 (port b)
        PortB(4, make_optional<uint32>(true, 9));

        // read _mem[4], this should see 9
        uint32 result = PortA(4, make_optional<uint32>(false, 0));

        assert(result == 9);
    }

private:
    uint32 PortA(uint9 addr, optional<uint32> write_data)
    {
        uint32 result;

        atomic
        {
            result = _mem[addr];

            uint32 new = write_data.is_valid ? write_data.value : result;

            _mem[addr] = new;
        }

        return result;
    }

    uint32 PortB(uint9 addr, optional<uint32> write_data)
    {
        uint32 result;

        atomic
        {
            result = _mem[addr];

            uint32 new = write_data.is_valid ? write_data.value : result;

            _mem[addr] = new;
        }

        return result;
    }

    [[memory, quad_port]] uint32[512] _mem;
}

inline void DeviceMemoryTests()
{
    print("WriteThenReadTest\n");
    static WriteThenReadTest writeThenReadTest;
    writeThenReadTest.Test();

    print("ConcurrentRwTest\n");
    static ConcurrentRwTest concurrentRwTest;
    concurrentRwTest.Test();

    print("BypassTest\n");
    static BypassTest bypassTest;
    bypassTest.Test();

    print("OnePortBypass\n");
    static OnePortBypass onePortBypass;
    onePortBypass.Test();

    print("StaleBypassTest\n");
    static StaleBypassTest staleBypassTest;
    staleBypassTest.Test();
}
