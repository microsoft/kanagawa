// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.cache.read_only
    { cache
    }

import data.cache.internal
import data.memory.pipelined

//| Read-only cache.
template
    < typename Key          //< The type of key for looking up a value in the cache.
    , typename Value        //< The type of value stored in the cache.
    , typename LUtime       //< The type to use for storing the time a cache entry was most recently used.
                            // Using a wider type makes LRU eviction more accurate for a set associative
                            // cache, but for a direct cache where LRU does not apply, using uint1 saves space.
    , auto Associativity    //< The number of entries to store for a given hash value.  Use 1 to create a directly mapped cache.
    , auto Depth            //< The total number of entries to cache.  Must be a multiple of `Associativity`.
    , auto Banks = 1        //< The number of memories used to implement the data store.  Higher values makes of large caches placement easier.
    >
class cache
{
private:
    //| The callback function to invoke when a `get` call causes a cache miss.
    (Key) -> Value load;

    const auto _setCount = Depth / Associativity;
    cache_tags<Key, LUtime, Associativity, Depth> _tags;

    pipelined_memory<Value, Depth, Banks> _data;

public:
    using set_index_t = index_t<_setCount>;

    //| Initialize or re-initialize a cache object.
    // Re-initializing the cache invalidates all entries.
    // The caller must ensure that intialize is not called concurrently with `get`.
    void initialize()
    {
        _tags.initialize();
    }

    //| Get a value from the cache.
    Value get
        ( Key key               //< The key to lookup.
        , set_index_t set_index //< The hashed value of the key.
        )
    {
        auto getResult = _tags.get(key, set_index, false);

        Value val;

        if (!getResult.hit)
        {
            val = load(key);
        }

        // | below should be an addition if non-power of 2 associativity is supported
        static assert(0 == (Associativity & (Associativity - 1)));

        index_t<Depth> dataIndex = set_index * Associativity | getResult.idx;

        bool hit = getResult.hit;

        auto result = _data.atomically(dataIndex, [val, hit](Value prev)
        {
            Value next = prev;

            if (!hit)
            {
                // Update data in the cache
                next = val;
            }

            return next;
        });

        return result.second;
    }
}