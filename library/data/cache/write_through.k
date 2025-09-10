// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.cache.write_through
    { cache
    }

import data.cache.internal

//| A cache that immediately writes data to the backing store.
template
< typename Key          //< The type of key for looking up a value in the cache.
, typename Value        //< The type of value stored in the cache.
, typename LUtime       //< The type to use for storing the time a cache entry was most recently used.
                        // Using a wider type makes LRU eviction more accurate for a set associative
                        // cache, but for a direct cache where LRU does not apply, using uint1 saves space.
, auto Associativity    //< The number of entries to store for a given hash value.  Use 1 to create a directly mapped cache.
, auto Depth            //< The total number of entries to cache.  Must be a multiple of `Associativity`.
>
class cache
{
private:
    //| The callback function to invoke when a `get` call causes a cache miss.
    (Key key) -> Value load;

    //| The callback function to invoke in a `put` call to store the new value
    //  to the backend store.
    (Key key, Value value) -> void store;

    const auto _setCount = Depth / Associativity;
    const auto _entryIndexBits = (clog2(Associativity) > 0) ? clog2(Associativity) : 1;
    using entry_index_t = uint<_entryIndexBits>;

public:
    using set_index_t = index_t<_setCount>;

private:
    cache_tags<Key, LUtime, Associativity, Depth> _tags;

    memory <Value, Depth> m_data;

    //| Stores a value in the cache, or reads the existing value if `readFromCache` is true.
    // Called by both `get` and `put`, and serves as a single place with a single atomic
    // block for manipulating the contents of the cached data.
    // Returns the value now in the cache, which may have just been written there.
    inline Value store_value_in_cache
        ( set_index_t set_index       //< The hashed value of the key.
        , entry_index_t entry_index   //< The index within the cache line for the given entry.
        , Value newValue              //< The value to store into the cache.
        , bool readFromCache          //< Set to true if this function should return the existing value from the cache.
        )
    {
        Value result;

        auto dataIndex = set_index * Associativity + entry_index;
        atomic
        {
            Value dataInCache = m_data[dataIndex];
            if (readFromCache)
            {
                // Snap the data from the cache.
                result = dataInCache;
            }
            else
            {
                // Update the cache.
                dataInCache = newValue;
                result = newValue;
            }
            m_data[dataIndex] = dataInCache;
        }

        return result;
    }

public:
    //| Gets or puts a value from/to the cache.
    Value get_or_put
    ( bool is_get               //< Whether this is a get operation.  If false, this is a put operation.
        , Key key               //< The key to read or write.
        , set_index_t set_index //< The hashed value of the key.
        , Value value           //< The new value to put.  Ignored if `is_get` is true.
        )
    {
        bool read_from_cache;

        auto getResult = _tags.get(key, set_index, false);

        if (is_get)
        {
            if (!getResult.hit)
            {
                value = load(key);
            }

            read_from_cache = getResult.hit;
        }
        else
        {
            store(key, value);
            read_from_cache = false;
        }

        return store_value_in_cache(set_index, getResult.idx, value, read_from_cache);
    }

    //| Initialize or re-initialize a cache object.
    // Re-initializing the cache invalidates all entries.
    // The caller must ensure that intialize is not called concurrently with get or put.
    void initialize()
    {
        _tags.initialize();
    }

    //| Gets a value from the cache.
    Value get
        ( Key key               //< The key to lookup.
        , set_index_t set_index //< The hashed value of the key.
        )
    {
        Value null_value;

        return get_or_put(true, key, set_index, null_value);
    }

    //| Puts a value into the cache.
    void put
        ( Key key               //< The key to write.
        , set_index_t set_index //< The hashed value of the key.
        , Value value           //< The new value to put.
        )
    {
        get_or_put(false, key, set_index, value);
    }
}