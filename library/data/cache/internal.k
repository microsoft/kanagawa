// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.cache.internal
    { cache_tags
    }

import data.array
import data.bits

template <typename Key, typename LUtime>
struct cache_entry
{
    bool _valid;
    bool _unwritten;
    LUtime _lastUsed;
    Key _key;
}

template <typename LUtime, typename entry_index_t>
struct cache_entry_with_index
{
    bool _valid;
    bool _unwritten;
    LUtime _lastUsed;
    entry_index_t _index;
}

template <typename Key, typename LUtime, auto EntriesPerTag>
struct cache_tag
{
    cache_entry<Key, LUtime>[EntriesPerTag] _entries;
}


// Function: ReduceEntryForInsertion
//
// Given two cache_entry_with_index structs, return the one that would be a better candidate
// for inserting a new entry in the cache.  First choice is to find an unused (invalid)
// entry, but if both candidates are valid, then the least recently used is chosen.
template <typename LUtime, typename entry_index_t>
inline cache_entry_with_index<LUtime, entry_index_t> ReduceEntryForInsertion(cache_entry_with_index<LUtime, entry_index_t> x, cache_entry_with_index<LUtime, entry_index_t> y)
{
    cache_entry_with_index<LUtime, entry_index_t> result;

    if (!x._valid)
    {
        // If x is invalid, we can store a new entry here without evicting any cached data.
        result = x;
    }
    else if (!y._valid)
    {
        result = y;
    }
    else if (x._lastUsed < y._lastUsed)
    {
        // Both x and y are valid, but y was used more recently.  Evict x.
        result = x;
    }
    else
    {
        result = y;
    }

    return result;
}

template <typename entry_index_t, typename Key>
struct cache_tags_get_result
{
    entry_index_t idx;
    bool hit;
    optional<Key> key_to_write;
}


//| Handles metadata related to entries in the cache, but does not store data.
template
    < typename Key       //< The type of key for looking up a value in the cache.
    , typename LUtime    //< The type to use for storing the time a cache entry was
                         // most recently used. Using a wider type makes LRU eviction
                         // more accurate for a set associative cache, but for a direct
                         // cache where LRU does not apply, using `uint1` saves space.
    , auto Associativity //< The number of entries to store for a given hash value,
                         // keeping the most recently used values. Pass 1 to create
                         // a directly mapped cache.
    , auto Depth         //< The total number of entries to cache. Must be a multiple
                         // of `Associativity`.
    >
class cache_tags
{
private:
    const auto _setCount = Depth / Associativity;
    const auto _entryIndexBits = (clog2(Associativity) > 0) ? clog2(Associativity) : 1;

public:
    using set_index_t = index_t<_setCount> ;
    using entry_index_t = uint<_entryIndexBits> ;

private:
    memory_norep<cache_tag<Key, LUtime, Associativity>, _setCount> m_tags;

    // Function: IsValidMatch
    //
    // Maps from a cache_entry and a Key to a boolean indicating whether the
    // given entry is a valid match for the key.
    inline bool IsValidMatch(cache_entry<Key, LUtime> entry, Key key)
    {
        return (entry._valid && entry._key == key);
    }

    // Function: FindKey
    //
    // Given a key, find its index within the given tag.
    // If the key is not found, the is_valid field of the result is false.
    inline optional<entry_index_t> FindKey(cache_tag<Key, LUtime, Associativity> tag, Key key)
    {
        bool[Associativity] matchBitmap;

        // Conceptually this is a map operation, but map doesn't pass another parameter,
        // the key in this case, to the map function.
        static for(const auto i : Associativity)
        {
            matchBitmap[i] = IsValidMatch(tag._entries[i], key);
        }

        // There should never be more than 1 valid matching key, but this code affects
        // HW performance.  Uncomment this line if necessary when debugging the Cache.
        // assert(pop_count<entry_index_t>(cast<uint<Associativity>>(matchBitmap)) < 2);

        // highest_one does not support being called for a single bit array, which causes the
        // following line to not compile in the direct map case (Associativity == 1) without
        // the + 1 term seen below.
        optional<index_t<Associativity>> highestOne = highest_one(matchBitmap);

        return make_optional<entry_index_t>(highestOne.is_valid, highestOne.value);
    }

    // Function: MapToEntryWithIndex
    //
    // Maps a entry to a cache_entry_with_index.  The latter structure doesn't have the Key
    // field, but adds an index field indicating where it lies in the tag._entries array.
    inline cache_entry_with_index<LUtime, entry_index_t> MapToEntryWithIndex(cache_entry<Key, LUtime> entry, entry_index_t idx)
    {
        cache_entry_with_index<LUtime, entry_index_t> result;

        result._valid = entry._valid;
        result._unwritten = entry._unwritten;
        result._lastUsed = entry._lastUsed;
        result._index = idx;

        return result;
    }

    // Function: GetIndexForNewEntry
    //
    // Find the index of the best slot to store a new cache line entry.
    // This will either be an entry that is not valid or else the valid
    // entry that has been least recently used.
    inline optional<entry_index_t> GetIndexForNewEntry(cache_tag<Key, LUtime, Associativity> tag)
    {
        cache_entry_with_index<LUtime, entry_index_t>[Associativity] indexed_entries;

        // Conceptually this is a map operation, but map doesn't pass another parameter,
        // the index in this case, to the map function.
        static for(const auto i : Associativity)
        {
            indexed_entries[i] = MapToEntryWithIndex(tag._entries[i], i);
        }

        auto destination = reduce(ReduceEntryForInsertion<LUtime, entry_index_t>, indexed_entries);
        return make_optional(true, destination._index);
    }

    // Function: initializeCacheTag
    //
    // Initializes a single cache_tag.
    [[pipelined]] void InitializeCacheTag(set_index_t threadId)
    {
        cache_tag<Key, LUtime, Associativity> tag;
        static for (const auto j : Associativity)
        {
            cache_entry<Key, LUtime> entry;
            entry._valid = false;
            entry._unwritten = false;
            entry._lastUsed = 0; // not strictly necessary, but makes for cleaner debug output
            tag._entries[j] = entry;
        }
        m_tags[threadId] = tag;
    }

public:

    //| Initialize or re-initialize a cache object.
    // Re-initializing the cache invalidates all entries.
    // The caller must ensure that intialize is not called concurrently with `get`.
    void initialize()
    {
        // If this assert fires, Depth is not a multiple of Associativity.
        assert(Depth == _setCount * Associativity);

        InitializeCacheTag(_setCount);
    }

    //| Get the index of a value from the `cache_tags`, if present.
    cache_tags_get_result<entry_index_t, Key> get
        ( Key key                //< The key to lookup.
        , set_index_t set_index  //< The hashed value of the key.
        , bool mark_as_unwritten //< Mark the cache entry as unwritten, meaning it hasn't
                                 // been written to the backing store yet.
        )
    {
        bool hit = false;
        optional<entry_index_t> idx;
        cache_entry<Key, LUtime> entry;
        cache_tags_get_result<entry_index_t, Key> result;
        auto cycleCount = cycles();
        Key key_to_write;
        bool valid_key_to_write = false;
        atomic
        {
            cache_tag<Key, LUtime, Associativity> tag = m_tags[set_index];
            idx = FindKey(tag, key);
            if (idx.is_valid)
            {
                hit = true;
                entry = tag._entries[idx.value];
            }
            else
            {
                hit = false;

                idx = GetIndexForNewEntry(tag);
                assert(idx.is_valid);

                if (tag._entries[idx.value]._unwritten)
                {
                    assert(tag._entries[idx.value]._valid);
                    // Before we overwrite an unwritten value in the cache, capture the key
                    // that's about to be evicted.
                    key_to_write = tag._entries[idx.value]._key;
                    valid_key_to_write = true;
                }

                // In the cache miss case, we overwrite every field of the cache_tag, so
                // there is no need to copy the existing value from the tag._entries array.
                entry._valid = true;
                entry._key = key;
            }

            // Update the last used time for this cache entry
            assert(idx.is_valid);
            entry._lastUsed = cast<LUtime>(cycleCount); // dropping high bits is expected
            entry._unwritten = mark_as_unwritten;

            tag._entries[idx.value] = entry;
            m_tags[set_index] = tag;
        }

        result.idx = idx.value;
        result.hit = hit;
        result.key_to_write = make_optional<Key>(valid_key_to_write, key_to_write);

        return result;
    }

    //| Returns the key at the given (set_index, entry_index) tuple as well as whether the
    //  value is unwritten.  Marks the key as written.
    //  Must not be called concurrently with `get`.
    optional<Key> get_and_clear_unwritten_key
        ( set_index_t set_index     //< Set index of the key to lookup.
        , entry_index_t entry_index //< Way index of the key to lookup.
        )
    {
        optional<Key> result = {};

        atomic
        {
            auto set = m_tags[set_index];

            auto entry = set._entries[entry_index];

            result = make_optional<Key>(entry._unwritten, entry._key);

            entry._unwritten = false;

            set._entries[entry_index] = entry;

            m_tags[set_index] = set;
        }

        return result;
    }
}