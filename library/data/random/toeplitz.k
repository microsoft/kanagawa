// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.random.toeplitz
    { toeplitz
    , toeplitz_random
    }

//| Implementation of the Toeplitz hash algorithm as described
// [here](https://en.wikipedia.org/wiki/Toeplitz_Hash_Algorithm).
//
// Input data is passed into `calc_hash` as one parallel data structure.
//
// If you need a key, a secure rng is a reasonable method of producing one.
// Here is a 256 byte key produced with the default Windows RNGCryptoServiceProvider.
// You can truncate it to whatever length you need.
//
// ```
// 6D 5A 1B A6 54 0E 36 AE 73 84 C9 47 79 71 0E 89 BA B5 77 83 62 E9 B3 02 C3 A2 CF 20 2B 56 15 A9
// DD 5E 8E F2 E2 EF 40 44 4F 7C 23 BB B7 6A 50 8B F4 8B E9 00 D8 A3 3D AE 88 29 FB 3C 64 37 71 A6
// C6 CD 1B F7 80 24 89 A2 35 5A B4 C2 43 56 04 2E 5F 67 99 BC BA 61 1C 0B C6 70 9B FA 9D AC 4B 74
// 2A 8C 31 FA 41 56 81 8F 7D 77 B5 8C F7 B8 29 F6 06 8F 09 A3 20 B1 02 74 A0 95 3B 87 35 C7 2E 3A
// 9D 47 2C 2D C7 C9 0B 9C 07 8E 76 7B 2C D6 47 79 28 F5 ED 41 A7 CD 3A 00 1D 1D A6 CC 6E 3D 0D 0C
// CD DB 40 FC 39 68 BA 50 41 F1 18 25 C7 03 4B 0C F4 3E DD 60 32 6F 41 B3 35 5B 8E E4 D7 93 5B 46
// C7 6A F8 E3 D5 37 17 65 9B 19 8A C8 07 CD 79 D8 C2 E3 EF C0 0E 81 70 81 2D E3 77 B9 96 B6 16 D2
// ED BD 4C 99 4E D8 0E 39 A2 49 1E 73 4A C1 51 05 09 52 85 6B 52 47 58 07 74 10 6D 78 E8 16 94 6E
// ```
template
    < auto PipelineCycles //< The hash computation will be pipelined such that it can
                          // calculate a new result every `DataWidth` cycles. Increasing
                          // this lowers the area consumed to implement the hash at the
                          // expense of throughput.
    , auto HashWidth      //< The width of the output hash value.
    , auto DataWidth      //< The width of the input data.
    , auto KeyWidth
    , uint<KeyWidth> Key  //< Key value used for hash computation. This value must be at
                          // least `HashWidth` bits wide.
    >
class toeplitz
{
public:
    using Hash_t=uint<HashWidth>;
    using Data_t=uint<DataWidth>;
    using  Key_t=uint<KeyWidth>;

    static assert(KeyWidth >= HashWidth);
    static assert(PipelineCycles > 0);

private:
    const auto DataChunkWidth = (DataWidth + PipelineCycles-1) / PipelineCycles; // Width of input data handled per "thread" in [[pipelined]] function _CalcHash
    const auto TidWidth = (PipelineCycles < 2) ? 1 : clog2(PipelineCycles);

    using Tid_t=uint<TidWidth>;
    using DataChunk_t=uint<DataChunkWidth>;

    inline Key_t rotate_key_left(Key_t key)
    {
        uint1 msb = key >> (KeyWidth-1);
        uint<KeyWidth-1> lsbs = cast<uint<KeyWidth-1>>(key);

        return concat(lsbs, msb);
    }

    inline Hash_t hash_align_and_truncate_key(Key_t key)
    {
        return cast<Hash_t>(key >> (KeyWidth - HashWidth)); // Leftmost bits
    }

    // Extracts chunk of input data to process for the specified pipeline thread
    // Data is extracted MSB first, per Toeplitz algorithm description.
    //
    // Example calculation:
    //      DataWidth = 25
    //      PipelineCycles = 3
    //      DataChunkWidth = 9
    //
    //             ------- Input Data ------
    //             1111111110000000000000000xx
    //             876543210FEDCBA9876543210xx
    //       tid   ---Aligned Input Data----    <rshift><lshift>
    //        0    xxxxxxxxx                      16        0
    //        1             xxxxxxxxx              7        0
    //        2                      xxxxxxx00     0        2
    //
    //      rshift = DataWidth - (tid+1)*DataChunkWidth
    //      lshift = (tid == PipelineCycles-1) ? (DataChunkWidth - DataWidth/PipelineCycles) : 0;
    inline DataChunk_t extract_data_chunk(Data_t data, Tid_t tid)
    {
        // Toeplitz processes high order bits first, so chunk
        // number 0 is left-most. If DataWidth is not an even multiple of PipelineCycles
        // we left shift the input data to add LSB zero padding.
        const auto PadBits = PipelineCycles * DataChunkWidth - DataWidth;
        const auto rShiftBits = DataWidth + PadBits - (tid+1)*DataChunkWidth;

        const uint<DataWidth+PadBits> paddedData = data << PadBits;
        return cast<DataChunk_t>(paddedData >> rShiftBits);
    }

    // Calculates a key rotated to account for the chunk offset within the input data.
    // Rotation is accomplished by shifting a doubly large key
    // Example calculation:
    //    DataChunkWidth = 9
    //    KeyWidth = 16
    //    HashWidth = 12
    //
    //          ----------Doubled Key-----------
    //          FEDCBA9876543210FEDCBA9876543210
    //
    //    tid   -----Aligned Key for Hash------- <shift>
    //     0                    xxxxxxxxxxxxxxxx    0
    //     1             xxxxxxxxxxxxxxxx           7
    //     2      xxxxxxxxxxxxxxxx                 14
    //     3               xxxxxxxxxxxxxxxx         5
    //
    //    shift = (2*KeyWidth - ((DataChunkWidth*tid)%KeyWidth))%KeyWidth
    inline Key_t offset_key(Tid_t tid)
    {
        const uint<KeyWidth*2> DoubledKey = concat(cast<uint<KeyWidth>>(Key),cast<uint<KeyWidth>>(Key));

        return cast<Key_t>( DoubledKey >> ((2*KeyWidth - ((DataChunkWidth*tid)%KeyWidth))%KeyWidth) );
    }

    inline bool bit_at(DataChunk_t data, index_t<DataChunkWidth> pos)
    {
        return ((data >> pos) & 0x1) != 0;
    }

    [[pipelined]] Hash_t calc_hash_chunk(Tid_t tid, Data_t data)
    {
        // Figure out the starting offset within the key for all threads,
        // and then select the offset for this thread using the thread id
        // as an index. We do it this way so that the key is actually
        // "baked into the LUTs" rather than being implemented as a
        // separate large register and associated logic.
        Key_t[PipelineCycles] offsetKeys;
        static for(const auto i : PipelineCycles)
        {
            offsetKeys[i] = offset_key(i);
        }

        Key_t offsetKey = offsetKeys[tid];

        // Extract the portion of the input data we process in this thread
        // Toeplitz processes high order bits first, so we start with
        // leftmost bits of data word in tid=0, and move rightwards
        DataChunk_t dataChunk = extract_data_chunk(data, tid);

        // Array is rounded up to power of two because of the reduction loop below
        const auto PartialHashesLength = (1 << clog2(DataChunkWidth));
        Hash_t[PartialHashesLength] partialHashes;

        // Use the input data to calculate intermediate values. A one bit in the
        // input data means we XOR the result (which starts out as zero) with the
        // left-most bits of the key, a zero value means we do nothing.
        static for(const auto i : DataChunkWidth)
        {
            if (bit_at(dataChunk, static_cast(DataChunkWidth-i-1)))
            {
                partialHashes[i] = hash_align_and_truncate_key(offsetKey);
            }
            offsetKey = rotate_key_left(offsetKey);
        }

        // Now combine the intermediate hash results by XOR-ing them together
        const auto ReductionTreeDepth = clog2(DataChunkWidth);

        static for(const auto i : ReductionTreeDepth)
        {
            static for(const auto j : PartialHashesLength / 2)
            {
                // Binary reduction tree. The hope is the compiler should
                // convert this to quatenary for us since the FPGA LUTs
                // support that.
                partialHashes[j] = partialHashes[2*j] ^ partialHashes[2*j+1];
            }
        }

        return partialHashes[0];
    }

public:
    //| Calculate the Toeplitz hash of the supplied data.
    Hash_t calc_hash(Data_t data)
    {
        // Call a "pipelined" function to calculate the hash of an portion of the input data.
        // This lowers the issue rate (to 1:PipelineCycles), but should help with the size
        // of the implementation.
        Hash_t[PipelineCycles] hashChunks;
        hashChunks = calc_hash_chunk(PipelineCycles, data);

        const auto PartialHashesLength = 1 << clog2(PipelineCycles);
        Hash_t[PartialHashesLength] partialHashes;

        static for(const auto i : PipelineCycles)
        {
            partialHashes[i] = hashChunks[i];
        }

        const auto ReductionTreeDepth = clog2(PipelineCycles);
        static for(const auto i : ReductionTreeDepth)
        {
            static for(const auto j : PartialHashesLength / 2)
            {
                partialHashes[j] = partialHashes[2*j] ^ partialHashes[2*j+1];
            }
        }

        return partialHashes[0];
    }
}

//| Implement a pseudo-random number generator by hashing a counter with
// a Toeplitz hash implementation. This implementation is more expensive
// than LFSR, but gives a result that is more analogous to the `std::random`
// algorithms in C++.
//
// A reasonable key for 256-bit or smaller widths is:
// `0x6D5A1BA6540E36AE7384C94779710E89BAB5778362E9B302C3A2CF202B5615A9`
template
    < auto Width          //< The width of the output in bits, as well as the width of the internal counter.
    , auto KeyWidth       //< The width of the Toeplitz hash key in bits.
    , uint<KeyWidth> Key  //< Toeplitz hash key. The width of the key must be greater than or equal to the output `Width`.
    , auto PipelineCycles //< Pipeline cycles to use for the hash calculation. A value of 1 gives maximum throughput
                          // but consumes maximum area. Values larger than one reduce throughput and consume less area.
    >
class toeplitz_random
{
private:
    toeplitz<PipelineCycles, Width, Width, KeyWidth, Key> _hash;
    uint<Width> _counter = 1;

public:
    //| Get the next random number in the sequence, optionally seeding
    // the random number generator first.
    uint<Width> next
        ( bool set_seed    //< If true, the random number will be seeded with the specified seed.
        , uint<Width> seed //< The number to seed the random number generator. Ignored if `set_seed` is false.
        )
    {
        uint<Width> counter;

        atomic
        {
            counter = set_seed ? seed : _counter;

            _counter = static_cast(counter + 1);
        }

        return _hash.calc_hash(counter);
    }
}