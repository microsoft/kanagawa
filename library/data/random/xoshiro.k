// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.random.xoshiro
    { xoshiro_random
    }

//| [Xoshiro-128](http://xoshiro.di.unimi.it/xoshiro128starstar.c) random number generator.
//
// The "128" here refers to the internal state; the RNG actually returns
// 32-bit values.
class xoshiro_random
{
private:
    uint32[4] _s = {};


   // This is the jump function for the generator. It is equivalent
   // to 2^64 calls to next(); it can be used to generate 2^64
   // non-overlapping subsequences for parallel computations.
   inline void jump()
   {
        const uint32[4] JUMP = cast<uint32[4]>(cast<uint128>(0x77f2db5b6fa035c3f542d2d38764000b));

        uint32 s0 = 0;
        uint32 s1 = 0;
        uint32 s2 = 0;
        uint32 s3 = 0;

        for (const uint3 i : 4)
        {
            const uint1[32] jump = cast<uint1[32]>(JUMP[i]);

            for (const uint6 b : 32)
            {
                if (jump[b] == 1)
                {
                    s0 ^= _s[0];
                    s1 ^= _s[1];
                    s2 ^= _s[2];
                    s3 ^= _s[3];
                }
                next();
            }
        }

        _s[0] = s0;
        _s[1] = s1;
        _s[2] = s2;
        _s[3] = s3;
    }

public:
    //| Set the RNG seed. Do not call this concurrently with `next`.
    [[max_threads(1)]] void set_seed(uint32 seed)
    {
        _s[0] = 0;
        _s[1] = 0;
        _s[2] = 0;
        _s[3] = seed;
        jump();
    }

    //| Get the next random number in the sequence.
    uint32 next()
    {
        uint32 s0;

        atomic
        {
            s0 = _s[0];
            uint32 t = static_cast(_s[1] << 9);
            uint32 s2 = _s[2];
            uint32 s3 = _s[3];

            s2 ^= _s[0];
            s3 ^= _s[1];

            _s[0] ^= s3;
            _s[1] ^= s2;
            _s[2] = s2 ^ t;
            _s[3] = static_cast((s3 << 11) | (s3 >> 21));
        }

        return ((cast<uint32>(s0 * 5) << 7) | (cast<uint32>(s0 * 5) >> 25)) * 9;
    }
}