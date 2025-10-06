// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// combine luts - combine 3 1-input operations
inline uint1 Foo469(uint1 x, uint1 y, uint1 z)
{
    return ~((x & y) | z);
}

inline void TestCase469(unit::tag_t tag)
{
    unit::assert(tag, 1 == Foo469(0, 0, 0));
    unit::assert(tag, 0 == Foo469(0, 0, 1));
    unit::assert(tag, 1 == Foo469(0, 1, 0));
    unit::assert(tag, 0 == Foo469(0, 1, 1));
    unit::assert(tag, 1 == Foo469(1, 0, 0));
    unit::assert(tag, 0 == Foo469(1, 0, 1));
    unit::assert(tag, 0 == Foo469(1, 1, 0));
    unit::assert(tag, 0 == Foo469(1, 1, 1));
}

// combine luts - handle the case where an operand is input to both operations to be merged
inline uint1 Foo470(uint1 x, uint1 y)
{
    return (x & y) | x;
}

inline void TestCase470(unit::tag_t tag)
{
    unit::assert(tag, 0 == Foo470(0, 0));
    unit::assert(tag, 0 == Foo470(0, 1));
    unit::assert(tag, 1 == Foo470(1, 0));
    unit::assert(tag, 1 == Foo470(1, 1));
}

// larger combineLut test
inline uint1 Foo471(uint2 x, uint1 y)
{
    uint1 result = 0;

    switch (x)
    {
    case 0:
        result = ~y;
        break;

    case 1:
        result = y & 1;
        break;

    case 2:
        result = y;
        break;

    case 3:
        result = y | 1;
        break;
    }

    return result;
}

inline void TestCase471(unit::tag_t tag)
{
    unit::assert(tag, Foo471(0, 0) == 1);
    unit::assert(tag, Foo471(0, 1) == 0);
    unit::assert(tag, Foo471(1, 0) == 0);
    unit::assert(tag, Foo471(1, 1) == 1);
    unit::assert(tag, Foo471(2, 0) == 0);
    unit::assert(tag, Foo471(2, 1) == 1);
    unit::assert(tag, Foo471(3, 0) == 1);
    unit::assert(tag, Foo471(3, 1) == 1);
}

// Lut packing for if/else
inline uint8 Foo472(bool predicate, uint8 x, uint8 y)
{
    uint8 result = 7;

    if (predicate)
    {
        result = x;
    }
    else
    {
        result = y;
    }

    return result;
}

inline void TestCase472(unit::tag_t tag)
{
    unit::assert(tag, Foo472(false, 10, 15) == 15);
    unit::assert(tag, Foo472(true, 10, 15) == 10);
}

// Repro of a bug in code that used to handle global RAW hazards
class PacketBuffer473
{
private:
    [[memory]] uint256[512] _buffer;

    uint9  _writePointer = 1;
    uint9  _readPointer = 0;
    uint9  _packetThreadCount = 0;
    bool   _shouldDrop = false;

public:
    inline void Write(uint256 data, bool endOfPacket, uint6 byteCount)
    {
        uint9 writeIndex;
        uint9 packetThreadCount;
        bool shouldDrop;

        atomic
        {
            // wrapping is desired here
            uint9 usedSlotCount = static_cast((_writePointer - _readPointer) - 1);

            shouldDrop = _shouldDrop;

            // For start-of-packet flits, determine if there is enough room
            if (0 == _packetThreadCount)
            {
                if (usedSlotCount < 1)
                {
                    // There is enough room, do not drop the packet
                    shouldDrop = false;
                }
                else
                {
                    // Not enough room - drop the packet
                    shouldDrop = true;
                }
            }

            _shouldDrop = shouldDrop;

            if (!shouldDrop)
            {
                // Increment the slot write index - wrapping is desired
                writeIndex = _writePointer;
                _writePointer++;
            }

            // Increment the number of threads in this packet
            // Note that this occurs even in the drop case - so that the dropping decision is not made for future flits in this packet
            packetThreadCount = _packetThreadCount + 1;
            _packetThreadCount = endOfPacket ? 0 : packetThreadCount;
        }
    }
}

// A queue of packets to send, associated with an internal stream and priority
class SendQueueSlot473
{
private:
    PacketBuffer473 _packetBuffer;

public:
    [[async]] void Send(uint256 data, uint6 byteCount, bool startOfPacket, bool endOfPacket)
    {
        // Enqueue into the internal packet buffer
        _packetBuffer.Write(data, endOfPacket, byteCount);
    }
}

class Test473
{
private:
    SendQueueSlot473 g_slot473;

public:
    void run(unit::tag_t tag)
    {
        g_slot473.Send(0, 0, false, false);
    }
}

// Repro of a bug in lut packing when the prev op had globals as input operands
// 112 bits
struct EthernetHeader474
{
    uint48 _dstMac;
    uint48 _srcMac;
    uint16 _protocol;
}

// 160 bits
struct IpHeader474
{
    uint4 _ihl;
    uint4 _version;
    uint8 _tos;
    uint16 _totalLength;
    uint16 _identification;
    uint16 _fragmentOffset;
    uint8 _ttl;
    uint8 _protocol;
    uint16 _checksum;
    uint32 _sourceAddr;
    uint32 _destAddr;
}

// 64 bits
struct UdpHeader474
{
    uint16 _sourcePort;
    uint16 _destPort;
    uint16 _length;
    uint16 _checksum;
}

// 64 bits
struct GtpHeader474
{
    uint8 flags;	// Top 3-bits - version,
    uint8 type;
    uint16 len;
    uint32 teid;
};

// First 512 bits of a UDP ethernet packet
struct FullHeadersUdp474
{
    EthernetHeader474 _ethernet;
    IpHeader474 _ip;
    UdpHeader474 _udp;
    uint176 _payload;
}

// First 512 bits of a GTP ethernet packet
struct FullHeadersGtp474
{
    EthernetHeader474 _ethernet;
    IpHeader474 _ip;
    UdpHeader474 _udp;
    GtpHeader474 _gtp;
    uint112 _payload;
}

struct FullHeadersGtp474NoPayload
{
    EthernetHeader474 _ethernet;
    IpHeader474 _ip;
    UdpHeader474 _udp;
    GtpHeader474 _gtp;
}

// First 768 bits of a GTP ethernet packet
struct FullHeadersGtp4743Encaped
{
    EthernetHeader474 _ethernet;
    IpHeader474 _ip;
    UdpHeader474 _udp;
    GtpHeader474 _gtp;

    IpHeader474 _ip2;
    uint208 _payload;
}

struct FullHeadersGtp4743
{
    EthernetHeader474 _ethernet;
    IpHeader474 _ip;
    UdpHeader474 _udp;
    GtpHeader474 _gtp;

    uint368 _payload;
}

const auto PacketBufferSize474 = 512;

const auto HalfPacketBufferSize474 = PacketBufferSize474 / 2;

struct PacketBufferWriteResult
{
    // The first slot of the packet
    uint9 _startSlot;

    // The number of threads that have accumulated data for the current packet
    uint9 _numThreadsInPacket;

    // If the packet buffer did not have enough space when the first flit arrived
    // then the packet is dropped.  In this case, Read() and ReleaseSlots() should not be called
    bool _dropped;
}

const auto MaxFlitsPerPacket = 48; // 1536 bytes

// Describes a packet that is queued in a packet buffer
struct PacketMetaData
{
    uint9 _startFlit;
    uint9 _flitCount;
    uint6 _lastFlitByteCount;
}

struct PacketBufferDequeueResult
{
    bool _valid;
    PacketMetaData _metaData;
}

// This is the slot count at which the buffer is full
// (PacketBufferSize474 - 1) because only (PacketBufferSize474 - 1) slots can be used with the writePointer/readPointer scheme
// MaxFlitsPerPacket because a singl packet can consume multiple slots
const auto PacketBufferUpperBound = (PacketBufferSize474 - 1) - MaxFlitsPerPacket;

class PacketBuffer474
{
private:
    [[memory]] uint256[PacketBufferSize474] _buffer;

    uint9  _writePointer = 1;
    uint9  _readPointer = 0;
    uint9  _packetThreadCount = 0;
    bool   _shouldDrop = false;

    // _metaData sized to match _buffer size so that it is impossible to overflow _metaData
    [[memory]] PacketMetaData[PacketBufferSize474] _metaData;
    uint9 _metaDataWritePointer = 1;
    uint9 _metaDataReadPointer = 0;

public:
    // Samples the pointers to determine if a packet should be accepted
    // Returns true if the packet should be accepted
    inline bool CheckSpace(unit::tag_t tag)
    {
        // Sample the number of used slots
        uint9 usedSlotCount;

        atomic
        {
            // wrapping is desired here
            usedSlotCount = (_writePointer - _readPointer) - 1;
        }

        // Use cycle counter as a pseudo-random number in [0, UpperBound)
        uint9 randomNumber = static_cast(cycles());

        if (randomNumber >= PacketBufferUpperBound)
        {
            randomNumber -= PacketBufferUpperBound;
        }

        unit::assert(tag, randomNumber < PacketBufferUpperBound);

        bool result = true;

        // Only drop packets if the buffer is more than 1/2 full
        // During normal operation, the buffer operates at a steady-state
        // with a few 10s of packets queued.
        // This is due to latency incurred by the the sending threads
        // when they search the priority queues
        if (usedSlotCount > HalfPacketBufferSize474)
        {
            // If usedSlotCount is 0, then this will return true
            // If usedSlotCount is > PacketBufferUpperBound, then this will return false
            result = (randomNumber >= usedSlotCount);
        }

        return result;
    }

    inline PacketBufferWriteResult Write(uint256 data, bool endOfPacket, uint6 byteCount, unit::tag_t tag)
    {
        // pointers are uint9, and the code relies on wrapping
        static assert(512 == PacketBufferSize474);

        uint9 writeIndex;
        uint9 packetThreadCount;
        bool shouldDrop;

        atomic
        {
            // wrapping is desired here
            uint9 usedSlotCount = static_cast((_writePointer - _readPointer) - 1);

            shouldDrop = _shouldDrop;

            // For start-of-packet flits, determine if there is enough room
            if (0 == _packetThreadCount)
            {
                if (usedSlotCount < PacketBufferUpperBound)
                {
                    // There is enough room, do not drop the packet
                    shouldDrop = false;
                }
                else
                {
                    // Not enough room - drop the packet
                    shouldDrop = true;
                }
            }

            _shouldDrop = shouldDrop;

            if (!shouldDrop)
            {
                // Increment the slot write index - wrapping is desired
                writeIndex = _writePointer;
                _writePointer++;
            }

            // Increment the number of threads in this packet
            // Note that this occurs even in the drop case - so that the dropping decision is not made for future flits in this packet
            packetThreadCount = _packetThreadCount + 1;
            _packetThreadCount = endOfPacket ? 0 : packetThreadCount;
        }

        // 1 cycle to for address and wren to propagate to the memory
        stages<1>();

        PacketBufferWriteResult result;

        result._dropped = shouldDrop;

        if (shouldDrop)
        {
            result._startSlot = 0;
            result._numThreadsInPacket = 0;
        }
        else
        {
            _buffer[writeIndex] = data;

            // Wrapping is desired here
            result._startSlot = cast<uint9>(writeIndex + 1) - packetThreadCount;
            result._numThreadsInPacket = packetThreadCount;

            // If the ethernet MTU is exceeded, then the packet dropping logic will not work
            unit::assert(tag, packetThreadCount <= MaxFlitsPerPacket);
        }

        if (endOfPacket && !shouldDrop)
        {
            // Save metadata
            uint9 metaDataWriteIndex;
            uint9 metaDataReadIndex;

            atomic
            {
                metaDataWriteIndex = _metaDataWritePointer;
                metaDataReadIndex = _metaDataReadPointer;

                _metaDataWritePointer++;
            }

            // Flit buffer should fill up before meta data buffer fills up
            unit::assert(tag, metaDataWriteIndex != metaDataReadIndex);

            PacketMetaData packetMetaData;

            packetMetaData._startFlit = result._startSlot;
            packetMetaData._flitCount = result._numThreadsInPacket;
            packetMetaData._lastFlitByteCount = byteCount;

            _metaData[metaDataWriteIndex] = packetMetaData;
        }

        return result;
    }

    inline uint256 Read(uint9 index)
    {
        return _buffer[index];
    }

    inline PacketBufferDequeueResult TryDequeue()
    {
        PacketBufferDequeueResult result;

        result._valid = false;

        uint9 metaDataReadIndex;

        atomic
        {
            metaDataReadIndex = _metaDataReadPointer;

            // wrapping is desired here
            uint9 numPacketsQueued = static_cast((_metaDataWritePointer - _metaDataReadPointer) - 1);

            result._valid = (numPacketsQueued > 0);

            // wrapping is desired here
            metaDataReadIndex = _metaDataReadPointer + 1;

            if (result._valid)
            {
                _metaDataReadPointer++;
            }
        }

        // Note that the metadata is read here, after _metaDataReadPointer has been increment
        // It is not possible for another thread to write to _metaData[metaDataReadIndex] before this read occurs
        // Because the call to ReleaseSlots() must come after the call to this function
        // The buffer of flits will fill up before _metaData wraps around
        result._metaData = _metaData[metaDataReadIndex];

        return result;
    }

    // Must be called after TryDequeue
    inline void ReleaseSlots(uint9 slotCount)
    {
        // pointers are uint9, and the code relies on wrapping
        static assert(512 == PacketBufferSize474);

        atomic
        {
            // Wrapping is desired here
            _readPointer += slotCount;
        }
    }
}

struct EthernetHeader474PlusPayload
{
    EthernetHeader474 _ethernet;
    uint8[18] _payload;
}

class UplinkHandler474
{
public:
    uint8[28] _buffer;
    PacketBuffer474 _packetBuffer;

       // Called once for each output packet to send
    [[pipelined]] void SendThread(
        uint10 outputThreadIndex,
        uint10 threadCountIn,
        uint10 threadCountOut,
        uint9 startIndex,
        uint6 lastThreadByteCountOut,
        bool shouldSend,
        uint4 priority,
        unit::tag_t tag)
    {
        // This operation does decap, so the output thread count is <= the input thread count
        unit::assert(tag, threadCountOut <= threadCountIn);

        // An encapsulated packet is at least 70 bytes
        // Ethernet, IP, UDP, GTP, IP
        unit::assert(tag, threadCountIn > 1);

        bool startOfPacket = (0 == outputThreadIndex);
        bool endOfPacket = ((outputThreadIndex + 1) == threadCountOut);

        // Read data from the packet buffer (3 slots)
        uint9 packetBufferSlot = static_cast(startIndex + outputThreadIndex);

        uint256[3] inputData;

        static for (const auto i : 3)
        {
            inputData[i] = _packetBuffer.Read(static_cast(packetBufferSlot + i));
        }

        if (endOfPacket)
        {
            // Release all slots in the packet buffer - that was the final read
            // Slots are not released one at a time too keep the code simple
            // Most calls to this function would release 1 input slot, but some would release up to 3
            _packetBuffer.ReleaseSlots(static_cast(threadCountIn));
        }

        uint8[32] bufferToSend;

        FullHeadersGtp4743Encaped headersEncapped = cast<FullHeadersGtp4743Encaped>(inputData);

        uint48 destMac = 0;

        atomic
        {
            uint8[28] newBuffer;

            if (startOfPacket)
            {
                // inputData[0] is: ETH IP
                //                  14  18
                //
                // inputData[1] is: IP UDP GTP EncapsulatedPacket
                //                  2  8   8   14
                //
                // inputData[2] is more encapsulated packet
                //
                // The middle 36 bytes are removed to produce
                //                  ETH EncapsulatedPacket
                //                  14  18
                //
                // And the remaining 28B of EncapsulatedPacket from inputData[2] are saved in _buffer


                // Generate new ethernet header
                // Place new ethernet header and 14B bits of payload into newData (to be sent)
                EthernetHeader474PlusPayload newData;

                FullHeadersGtp4743 headers = cast<FullHeadersGtp4743>(inputData);

                newData._ethernet = headers._ethernet;
                newData._ethernet._dstMac = destMac;
                newData._ethernet._srcMac = 123; // MAC of the gateway

                uint8[46] inputPayload = cast<uint8[46]>(headers._payload);

                static for (const auto i : 18)
                {
                    newData._payload[i] = inputPayload[i];
                }

                static for (const auto i : 28)
                {
                    newBuffer[i] = inputPayload[i + 18];
                }

                bufferToSend = cast<uint8[32]>(newData);
            }
            else
            {
                // combine 28B from _buffer with 4B from inputData[2] to send
                // save 28B from inputData[2] in _buffer
                uint8[32] inputData2 = cast<uint8[32]>(inputData[2]);

                static for (const auto i : 28)
                {
                    bufferToSend[i] = _buffer[i];
                    newBuffer[i] = inputData2[i + 4];
                }

                static for (const auto i : 4)
                {
                    bufferToSend[i + 28] = inputData2[i];
                }
            }

            _buffer = newBuffer;
        }

        if (shouldSend)
        {
            uint6 byteCount = (endOfPacket ? lastThreadByteCountOut : 32);

            uint256 buffer = cast<uint256>(bufferToSend);

            print("Uplink sending flit {buffer} {byteCount} {startOfPacket} {endOfPacket}\n");

            if (endOfPacket)
            {
                unit::assert(tag, cast<uint32>(buffer) == 0x47464544);
            }
        }
    }
}

class Test474
{
private:
    UplinkHandler474 g_uplinkHandler474;

public:
    void run(unit::tag_t tag)
    {
        uint8[96] input;

        static for (const auto i : 96)
        {
            input[i] = i;
        }

        uint256[3] inputArray = cast<uint256[3]>(input);

        g_uplinkHandler474._packetBuffer.Write(inputArray[0], false, 32, tag);
        g_uplinkHandler474._packetBuffer.Write(inputArray[1], false, 32, tag);
        g_uplinkHandler474._packetBuffer.Write(inputArray[2], true, 7, tag);

        g_uplinkHandler474.SendThread(2, 3, 2, 1, 0xB, true, 0, tag);
    }
}

// Testing that a fifo can be written in kanagawa
const auto FifoSize475 = 512;

class Fifo475
{
private:
    // The location that will be read from next
    uint9 _readPointer = 0;

    // The location that will be written to next
    uint9 _writePointer = 0;

    [[memory]] uint32[FifoSize475] _memory;

public:
    inline bool IsFull(uint9 writePointer)
    {
        uint9 slotsUsed = static_cast(writePointer - _readPointer);

        uint9 slotsRemaining = static_cast((FifoSize475 - slotsUsed) - 1);

        return slotsRemaining == 0;
    }

    inline uint9 Enqueue(uint32 data)
    {
        // Snap and increment the write pointer
        uint9 writePointer;

        atomic
        {
            writePointer = _writePointer;
            _writePointer = static_cast(_writePointer + 1); // Wrap OK
        }

        // Block until there is room
        atomic do; while(IsFull(writePointer));

        // Write into memory
        _memory[writePointer] = data;

        // Return the index that can be used to read the data
        return writePointer;
    }

    // Reads 1 element from memory
    inline uint32 Read(uint9  index)
    {
        return _memory[index];
    }

    // Advances the read pointers - after the element has been read from the memory
    inline void ReleaseSlot()
    {
        atomic
        {
            _readPointer = static_cast(_readPointer + 1); // Wrap OK
        }
    }
}

class Test475
{
private:
    Fifo475 g_fifo475;

    uint32 g_lastEnqueued475 = 0;
    uint32 g_lastDequeued475 = 0;

public:
    [[pipelined, async]] void Foo475(uint32 idx)
    {
        g_fifo475.Enqueue(~idx);

        atomic
        {
            g_lastEnqueued475 = idx;
        }
    }

    [[pipelined, async]] void Bar475(uint32 idx, unit::tag_t tag)
    {
        // Wait for the correct enqueue to complete
        atomic do; while(g_lastEnqueued475 < idx);

        // Verify no overflow
        uint32 numEnqueued;

        atomic
        {
            numEnqueued = g_lastEnqueued475 - g_lastDequeued475;
        }

        unit::assert(tag, numEnqueued < FifoSize475);

        uint32 val = g_fifo475.Read(static_cast(idx));

        unit::assert(tag, val == ~idx);

        atomic
        {
            g_lastDequeued475 = idx;
        }

        g_fifo475.ReleaseSlot();
    }

    void run(unit::tag_t tag)
    {
        const auto numToTest = 1024 * 20;

        Foo475(numToTest);

        // Wait for a while - to let the fifo fill up
        uint64 beginCycle = cycles();

        print("Begin waiting for cycle timer\n");

        uint64 endCycle = static_cast(beginCycle + 2000);

        atomic do; while(cycles() < endCycle);

        print("End waiting for cycle timer\n");

        Bar475(numToTest, tag);

        print("Begin waiting for Bar475\n");

        atomic do; while(g_lastDequeued475 != (numToTest - 1));

        print("End waiting for Bar475\n");
    }
}

class TestCase476
{
private:
    // Test for compiler feature that void register changes for locals in single-threaded functions
    [[max_threads(1), pipelined]] uint32 Foo476(uint16 x)
    {
        uint16 invX = ~x;

        // Force compiler to carry invX through computation
        barrier;

        // 10 stages
        uint32 result = x;

        static for (const auto i : 10)
        {
            result += x;
        }

        return result + invX;
    }

public:
    void run(unit::tag_t tag)
    {
        uint32[4] result = Foo476(4);

        println(result);

        barrier;

        unit::assert(tag, result[0] == 0x0000ffff);
        unit::assert(tag, result[1] == 0x00010009);
        unit::assert(tag, result[2] == 0x00010013);
        unit::assert(tag, result[3] == 0x0001001d);
    }
}

// compile-time definitions
class Test478
{
private:
    COMPILE_PARAM_1 g_foo478 = 3;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_foo478 == 3);

        auto x = COMPILE_PARAM_2 + 1;

        unit::assert(tag, x == 10);
    }
}

// simulation of unitialized globals which are used properly
class Test479
{
private:
    uint32 g_foo479;

public:
    void Foo479(bool init)
    {
        atomic
        {
            uint32 newVal;

            if (init)
            {
                newVal = 3;
            }
            else
            {
                newVal = g_foo479 + 1;
            }

            g_foo479 = newVal;
        }

        println(g_foo479);
    }

    void run(unit::tag_t tag)
    {
        Foo479(true);
        unit::assert(tag, g_foo479 == 3);

        Foo479(false);
        unit::assert(tag, g_foo479 == 4);
    }
}

// typedef
using U32=uint32;

inline uint32 Foo481(uint32 x)
{
    return x + 1;
}

inline void TestCase481(unit::tag_t tag)
{
    U32 val = 7;

    unit::assert(tag, 8 == Foo481(7));
}

// typedef struct
struct Foo482
{
    uint32 x;
    int16 y;
}

using MyType482=Foo482;

inline void TestCase482(unit::tag_t tag)
{
    MyType482 m;

    unit::assert(tag, bitsizeof(m) == 48);
    static_assert(bitsizeof(m) == 48);
}

// ordered do/while loop
class Test484
{
private:
    uint32 g_threadIdx484 = 0;

public:
    [[pipelined]] void Foo484(uint32 threadIdx, unit::tag_t tag)
    {
        uint32 i = 0;
        uint8 iterations = static_cast((~threadIdx) ^ cycles());
        uint32 sum = 0;

        do
        {
            sum += i;
            i++;
        } while (i < iterations);

        uint32 expectedSum;

        if (iterations == 0)
        {
            expectedSum = 0;
        }
        else if (0 == (iterations % 2))
        {
            expectedSum = (iterations - 1) * (iterations / 2);
        }
        else
        {
            expectedSum = ((iterations - 1) * (iterations / 2)) + (iterations / 2);
        }

        unit::assert(tag, sum == expectedSum);

        uint32 snappedIdx;

        atomic
        {
            snappedIdx = g_threadIdx484;
            g_threadIdx484++;
        }

        unit::assert(tag, snappedIdx == threadIdx);
    }

    void run(unit::tag_t tag)
    {
        Foo484(1024, tag);
    }
}

// ordered for loop
class Test485
{
private:
    uint32 g_threadIdx485 = 0;

public:
    [[pipelined]] void Foo485(uint32 threadIdx, unit::tag_t tag)
    {
        uint8 iterations = static_cast((~threadIdx) ^ cycles());
        uint32 sum = 0;

        for (const uint32 i : iterations)
        {
            sum += i;
        }

        uint32 expectedSum;

        if (iterations == 0)
        {
            expectedSum = 0;
        }
        else if (0 == (iterations % 2))
        {
            expectedSum = (iterations - 1) * (iterations / 2);
        }
        else
        {
            expectedSum = ((iterations - 1) * (iterations / 2)) + (iterations / 2);
        }

        unit::assert(tag, sum == expectedSum);

        uint32 snappedIdx;

        atomic
        {
            snappedIdx = g_threadIdx485;
            g_threadIdx485++;
        }

        unit::assert(tag, snappedIdx == threadIdx);
    }

    void run(unit::tag_t tag)
    {
        Foo485(1024, tag);
    }
}

inline void test_main()
{
    unit::test<469>(TestCase469);
    unit::test<470>(TestCase470);
    unit::test<471>(TestCase471);
    unit::test<472>(TestCase472);
    unit::test<473>(unit::fixture<Test473>());
    unit::test<474>(unit::fixture<Test474>());
    unit::test<475>(unit::fixture<Test475>());
    unit::test<476>(unit::fixture<TestCase476>());
    unit::test<478>(unit::fixture<Test478>());
    unit::test<479>(unit::fixture<Test479>());
    unit::test<481>(TestCase481);
    unit::test<482>(TestCase482);
    unit::test<484>(unit::fixture<Test484>());
    unit::test<485>(unit::fixture<Test485>());
}
