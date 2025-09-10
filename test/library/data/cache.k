// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c) Microsoft Corporation. All rights reserved.
//

import data.cache.read_only as RO
import data.cache.write_through as WT
import test.runner
import test.unit as unit

struct TestShapeInfo
{
    uint8 width;
    uint8 height;
}

// Class: Unit tests for data.cache.read_only and data.cache.write_through.
class CacheTests
{
private:
    // The tests use keys in the range 0..255
    const uint16 KeyCount = 256;

    // How many cache misses and writebacks have we had for each key?
    uint16[KeyCount] MissesPerKey = {};
    uint16[KeyCount] WritesPerKey = {};

    // A trivial transform from the key to the data for easy validation.
    const uint8 widthFactor = 3;
    const uint8 heightFactor = 2;
        
    bool backend_write_error = false;
    bool read_error = false;

    const auto CacheAssociativity = 4; // Tiny so it's easy to test cache eviction
    const auto CacheDepth = 8;

    // Note that the set associative cache, _roCache, uses a larger type for storing the
    // least recently used time for each cache entry.  This ensures correct LRU behavior.
    // The direct cache doesn't need LRU behavior, so it saves space by passing uint1.
    // N.B. The cycles() function behaves differently in different simulators, hardware,
    // etc., so be sure to test carefully before changing to a smaller type.
    RO::cache<uint8, TestShapeInfo, uint64, CacheAssociativity, CacheDepth, 1> _roCache = {.load = BackendLookup };
    RO::cache<uint8, TestShapeInfo, uint1, 1, CacheDepth, 8> _directRoCache = {.load = BackendLookup };
    WT::cache<uint8, TestShapeInfo, uint64, CacheAssociativity, CacheDepth> _wtCache = {.load = BackendLookup, .store = BackendWrite};
    WT::cache<uint8, TestShapeInfo, uint1, 1, CacheDepth> _directWtCache = {.load = BackendLookup, .store = BackendWrite};

    TestShapeInfo GenerateShape(uint8 id)
    {
        TestShapeInfo result;
        result.width = id * widthFactor;
        result.height =  id * heightFactor;

        return result;
    }

    bool ValidateResult(TestShapeInfo actual, uint8 key)
    {
        bool result = true;
        auto expectedWidth = key * widthFactor;
        auto expectedHeight = key * heightFactor;

        if (actual.width != expectedWidth || actual.height != expectedHeight)
        {
            print("ValidateResult for key  {key}  failed.  Expected  {expectedWidth} ,  {expectedHeight}  got:  {actual.width} ,  {actual.height}\n");
            result = false;
        }

        return result;
    }

    TestShapeInfo BackendLookup(uint8 id)
    {
        // This is where a real implementation would fetch from a
        // backing store (perhaps DRAM) in the event of a cache miss.

        atomic
        {
            MissesPerKey[id] = MissesPerKey[id] + 1;
        }

        return GenerateShape(id);
    }


    void BackendWrite(uint8 id, TestShapeInfo value)
    {
        // This is where a real implementation would write to a backing
        // store (perhaps DRAM) after a value is written to the cache.
        // This might be invoked immediately after a put() in the
        // writethrough case, or sometime in the future in the case of
        // a writeback cache.

        atomic
        {
            WritesPerKey[id] = WritesPerKey[id] + 1;
        }

        if (!ValidateResult(value, id))
        {
            print("BackendWrite: invalid result for {id}\n");

            // Set this so the test reports a failure.
            backend_write_error = true;
        }
    }

    inline index_t<CacheDepth / CacheAssociativity> MySimpleKeyHash(uint8 key)
    {
        return key % (CacheDepth / CacheAssociativity);
    }

    inline index_t<CacheDepth> MySimpleKeyHashDirect(uint8 key)
    {
        return key % CacheDepth;
    }

    bool ValidateMissCount(uint8 ExpectedMissCount, uint8 key)
    {
        bool result = true;

        if (MissesPerKey[key] != ExpectedMissCount)
        {
            print("Expected  {ExpectedMissCount}  cache misses for key  {key}  but actual miss count is  {MissesPerKey[key]}\n");
            result = false;
        }

        return result;
    }

    bool ValidateWriteCount(uint8 ExpectedWriteCount, uint8 key)
    {
        bool result = true;

        if (WritesPerKey[key] != ExpectedWriteCount)
        {
            print("Expected  {ExpectedWriteCount}  writes for key  {key}  but actual write count is  {WritesPerKey[key]}\n");
            result = false;
        }

        return result;
    }

    bool TestRoCacheEviction()
    {
        bool result = true;

        _roCache.initialize();
        MissesPerKey = {};

        // Get keys 2, 4, 6, 8.  All four should result in cache misses from the
        // freshly initialized cache.
        // Deliberately using all even keys so they all hash to the same cache line.
        TestShapeInfo shape = _roCache.get(2, MySimpleKeyHash(2));
        if (!ValidateResult(shape, 2))
        {
            result = false;
        }

        shape = _roCache.get(4, MySimpleKeyHash(4));
        if (!ValidateResult(shape, 4))
        {
            result = false;
        }

        shape = _roCache.get(6, MySimpleKeyHash(6));
        if (!ValidateResult(shape, 6))
        {
            result = false;
        }

        shape = _roCache.get(8, MySimpleKeyHash(8));
        if (!ValidateResult(shape, 8))
        {
            result = false;
        }

        // Check the cache miss counts.  Expect 1 for the 4 values we've done "get" on,
        // and 0 for all other keys.
        for (const auto i : KeyCount)
        {
            uint8 expectedMissCount;
            switch(i)
            {
                case 2:
                    expectedMissCount = 1;
                    break;

                case 4:
                    expectedMissCount = 1;
                    break;

                case 6:
                    expectedMissCount = 1;
                    break;

                case 8:
                    expectedMissCount = 1;
                    break;

                default:
                    expectedMissCount = 0;
                    break;
            }

            if (!ValidateMissCount(expectedMissCount, i))
            {
                result = false;
            }
        }

        // Fetch 6 again - we expect to get a hit this time.
        shape = _roCache.get(6, MySimpleKeyHash(6));
        if (!ValidateResult(shape, 6))
        {
            result = false;
        }

        // Miss count should still be 1 from the initial get above.
        if (!ValidateMissCount(1, 6))
        {
            result = false;
        }

        // Now fetch 10 - this should force the 2 out, as it's now the LRU.
        shape = _roCache.get(10, MySimpleKeyHash(10));
        if (!ValidateResult(shape, 10))
        {
            result = false;
        }

        // Get 2 again - this should result in a cache miss because loading 10
        // should have evicted it.
        shape = _roCache.get(2, MySimpleKeyHash(2));
        if (!ValidateResult(shape, 2))
        {
            result = false;
        }

        // Check the cache miss counts again.  Expect that 2 has missed twice,
        // since it was evicted once.
        for (const auto i : KeyCount)
        {
            uint8 expectedMissCount;
            switch(i)
            {
                case 2:
                    expectedMissCount = 2;
                    break;

                case 4:
                    expectedMissCount = 1;
                    break;

                case 6:
                    expectedMissCount = 1;
                    break;

                case 8:
                    expectedMissCount = 1;
                    break;

                case 10:
                    expectedMissCount = 1;
                    break;

                default:
                    expectedMissCount = 0;
                    break;
            }
            if (!ValidateMissCount(expectedMissCount, i))
            {
                result = false;
            }
        }

        return result;
    }

    [[pipelined]] void PipelinedGetHelper(uint32 threadIndex, uint32 keyMultiplier)
    {
        uint8 key = cast<uint8>(threadIndex * keyMultiplier);

        TestShapeInfo shape = _roCache.get(key, MySimpleKeyHash(key));
        if (!ValidateResult(shape, key))
        {
            print("PipelinedGetHelper invalid result for key {key}\n");
            read_error = true;
        }
    }

    bool TestRoCacheConcurrencyUniqueKeys()
    {
        bool result = true;

        _roCache.initialize();
        MissesPerKey = {};
        read_error = false;

        // do 4 fetches of 4 different keys
        // expect 4 cache misses
        PipelinedGetHelper(4, 1);
        if (read_error)
        {
            result = false;
        }

        // Check the cache miss counts.  The first 4 keys should have all
        // missed exactly once.
        for (const auto i : KeyCount)
        {
            uint8 expectedMissCount;
            switch(i)
            {
                case 0:
                    expectedMissCount = 1;
                    break;

                case 1:
                    expectedMissCount = 1;
                    break;

                case 2:
                    expectedMissCount = 1;
                    break;

                case 3:
                    expectedMissCount = 1;
                    break;

                default:
                    expectedMissCount = 0;
                    break;
            }
            if (!ValidateMissCount(expectedMissCount, i))
            {
                result = false;
            }
        }

        return result;
    }

    bool TestRoCacheConcurrencySameKey()
    {
        bool result = true;

        _roCache.initialize();
        MissesPerKey = {};
        read_error = false;

        // do 8 fetches of the same key
        // expect at least 1 cache miss
        PipelinedGetHelper(8, 0);
        if (read_error)
        {
            result = false;
        }

        // Check the cache miss counts.  The first key should have
        // missed at least once, and the other keys should have
        // no misses at all.
        if (MissesPerKey[0] < 1)
        {
            print("Expected at least 1 miss for key 0, actual  {MissesPerKey[0]}\n");
            result = false;
        }

        for (const auto i : KeyCount - 1)
        {
            uint8 expectedMissCount = 0;
            if (!ValidateMissCount(expectedMissCount, checked_cast<uint8>(i + 1)))
            {
                result = false;
            }
        }

        return result;
    }

    bool TestDirectRoCache()
    {
        bool result = true;

        _directRoCache.initialize();
        MissesPerKey = {};

        // Get keys 3, 11.  Both should miss the brand new cache, but also note
        // that since this is a direct cache (not set associative) reading key 11
        // should have evicted key 3.
        TestShapeInfo shape = _directRoCache.get(3, MySimpleKeyHashDirect(3));
        if (!ValidateResult(shape, 3))
        {
            result = false;
        }

        shape = _directRoCache.get(11, MySimpleKeyHashDirect(11));
        if (!ValidateResult(shape, 11))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 3))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 11))
        {
            result = false;
        }

        // Get key 4 and 11 again. Former should miss, latter should not.
        shape = _directRoCache.get(4, MySimpleKeyHashDirect(4));
        if (!ValidateResult(shape, 4))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 4))
        {
            result = false;
        }

        shape = _directRoCache.get(11, MySimpleKeyHashDirect(11));
        if (!ValidateResult(shape, 11))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 11))
        {
            result = false;
        }

        // Because the get of 11 should have evicted 3, if we get 3 again, we expect
        // that to be a second cache miss for 3.
        shape = _directRoCache.get(3, MySimpleKeyHashDirect(3));
        if (!ValidateResult(shape, 3))
        {
            result = false;
        }

        // Should have been a second cache miss for 3.
        if (!ValidateMissCount(2, 3))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 11))
        {
            result = false;
        }

        return result;
    }

    bool TestWtCache()
    {
        bool result = true;

        _wtCache.initialize();
        MissesPerKey = {};
        WritesPerKey = {};
        backend_write_error = false;

        // Generate the value for key 3 and put() it to the cache.
        _wtCache.put(3, MySimpleKeyHash(3), GenerateShape(3));

        // Key 3 should have had 0 misses and 1 write.
        if (!ValidateMissCount(0, 3))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 3))
        {
            result = false;
        }

        // Since we directly put key 3 into the cache, we should be
        // able to get it now without generating a cache miss.
        TestShapeInfo shape = _wtCache.get(3, MySimpleKeyHash(3));
        if (!ValidateResult(shape, 3))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 3))
        {
            result = false;
        }

        // Put key 3 again.  This should result in a second call to the Write callback.
        _wtCache.put(3, MySimpleKeyHash(3), GenerateShape(3));
        if (!ValidateWriteCount(2, 3))
        {
            result = false;
        }

        // Do 3 more writes to fill this cache set.
        _wtCache.put(5, MySimpleKeyHash(5), GenerateShape(5));
        _wtCache.put(7, MySimpleKeyHash(7), GenerateShape(7));
        _wtCache.put(9, MySimpleKeyHash(9), GenerateShape(9));

        // Keys 5, 7, 9 should all be in the cache now, and a get()
        // shouldn't cause a cache miss for any of them.
        shape = _wtCache.get(5, MySimpleKeyHash(5));
        if (!ValidateResult(shape, 5))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 5))
        {
            result = false;
        }

        shape = _wtCache.get(7, MySimpleKeyHash(7));
        if (!ValidateResult(shape, 7))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 7))
        {
            result = false;
        }

        shape = _wtCache.get(9, MySimpleKeyHash(5));
        if (!ValidateResult(shape, 9))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 9))
        {
            result = false;
        }

        // One more write will force entry 3 out of the cache.
        _wtCache.get_or_put(false, 11, MySimpleKeyHash(11), GenerateShape(11));

        // Keys 5, 7, 9, and 11 should have been written once each now.
        if (!ValidateWriteCount(1, 5))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 7))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 9))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 11))
        {
            result = false;
        }

        // Key 3 should have been evicted by the 4 put() calls above, so
        // calling get() again should result in the first cache miss for 3.
        shape = _wtCache.get(3, MySimpleKeyHash(3));
        if (!ValidateResult(shape, 3))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 3))
        {
            result = false;
        }

        // Get() of 3 should have evicted 5, so keys 7, 9, and 11 should
        // still be in the cache.  Get() those keys and ensure that their
        // miss counts remain at zero.

        shape = _wtCache.get(7, MySimpleKeyHash(7));
        if (!ValidateResult(shape, 7))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 7))
        {
            result = false;
        }

        shape = _wtCache.get(9, MySimpleKeyHash(9));
        if (!ValidateResult(shape, 9))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 9))
        {
            result = false;
        }

        shape = _wtCache.get(11, MySimpleKeyHash(11));
        if (!ValidateResult(shape, 11))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 11))
        {
            result = false;
        }

        if (backend_write_error)
        {
            result = false;
        }

        return result;
    }

    bool TestDirectWtCache()
    {
        bool result = true;

        _directWtCache.initialize();
        MissesPerKey = {};
        WritesPerKey = {};
        backend_write_error = false;

        // Generate the value for key 3 and put() it to the cache.
        _directWtCache.put(3, MySimpleKeyHashDirect(3), GenerateShape(3));

        // Key 3 should have had 0 misses and 1 write.
        if (!ValidateMissCount(0, 3))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 3))
        {
            result = false;
        }

        // Since we directly put key 3 into the cache, we should be
        // able to get it now without generating a cache miss.
        TestShapeInfo shape = _directWtCache.get(3, MySimpleKeyHashDirect(3));
        if (!ValidateResult(shape, 3))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 3))
        {
            result = false;
        }

        // Put key 3 again.  This should result in a second call to the Write callback.
        _directWtCache.put(3, MySimpleKeyHashDirect(3), GenerateShape(3));
        if (!ValidateWriteCount(2, 3))
        {
            result = false;
        }

        // Do 3 more writes to fill this cache set.
        _directWtCache.put(5, MySimpleKeyHashDirect(5), GenerateShape(5));
        _directWtCache.put(7, MySimpleKeyHashDirect(7), GenerateShape(7));
        _directWtCache.put(8, MySimpleKeyHashDirect(8), GenerateShape(8));

        // Keys 5, 7, 8 should all be in the cache now, and a get()
        // shouldn't cause a cache miss for any of them.
        shape = _directWtCache.get(5, MySimpleKeyHashDirect(5));
        if (!ValidateResult(shape, 5))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 5))
        {
            result = false;
        }

        shape = _directWtCache.get(7, MySimpleKeyHashDirect(7));
        if (!ValidateResult(shape, 7))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 7))
        {
            result = false;
        }

        shape = _directWtCache.get(8, MySimpleKeyHashDirect(8));
        if (!ValidateResult(shape, 8))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 8))
        {
            result = false;
        }

        // Key 11 will force entry 3 out of the cache.
        _directWtCache.get_or_put(false, 11, MySimpleKeyHashDirect(11), GenerateShape(11));

        // Keys 5, 7, 8, and 11 should have been written once each now.
        if (!ValidateWriteCount(1, 5))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 7))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 8))
        {
            result = false;
        }

        if (!ValidateWriteCount(1, 11))
        {
            result = false;
        }

        // Key 3 should have been evicted by the key 11 put() call above, so
        // calling get() again should result in the first cache miss for 3.
        shape = _directWtCache.get(3, MySimpleKeyHashDirect(3));
        if (!ValidateResult(shape, 3))
        {
            result = false;
        }

        if (!ValidateMissCount(1, 3))
        {
            result = false;
        }

        // Get() of 3 should have evicted 11, so keys 5, 7, 8 should
        // still be in the cache.  Get() those keys and ensure that their
        // miss counts remain at zero.

        shape = _directWtCache.get(5, MySimpleKeyHashDirect(5));
        if (!ValidateResult(shape, 5))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 5))
        {
            result = false;
        }

        shape = _directWtCache.get(7, MySimpleKeyHashDirect(7));
        if (!ValidateResult(shape, 7))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 7))
        {
            result = false;
        }

        shape = _directWtCache.get(8, MySimpleKeyHashDirect(8));
        if (!ValidateResult(shape, 8))
        {
            result = false;
        }

        if (!ValidateMissCount(0, 8))
        {
            result = false;
        }

        if (backend_write_error)
        {
            result = false;
        }

        return result;
    }

public:

    inline void Run()
    {
        unit::check<1>(TestRoCacheEviction);
        unit::check<2>(TestRoCacheConcurrencyUniqueKeys);
        unit::check<3>(TestRoCacheConcurrencySameKey);
        unit::check<4>(TestDirectRoCache);
        unit::check<5>(TestWtCache);
        unit::check<6>(TestDirectWtCache);
    }
}

// Function: Main test entry point for unit tests of data.cache classes.
//
// Returns - 0 if test passed, test-dependent non-zero value if test failed.
inline void test_main()
{
    static CacheTests _tests;

    _tests.Run();
}

