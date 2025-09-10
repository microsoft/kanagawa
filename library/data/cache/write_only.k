// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.cache.write_only
    { cache
    }

import data.array
import data.cache.internal
import data.memory.pipelined

//| A cache that only supports write operations with word-enable bits.
template
    < typename Key                                      //< The type of key for looking up a value in the cache.
    , typename Word                                     //< The type of a single word stored in the cache.
    , auto WordCount                                    //< The maximum number of words that can be written at one time.
    , typename LUtime                                   //< The type to use for storing the time a cache entry was most recently used.
                                                        //  Using a wider type makes LRU eviction more accurate for a set associative
                                                        //  cache, but for a direct cache where LRU does not apply, using uint1 saves space.
    , auto Associativity                                //< The number of entries to store for a given hash value, keeping the most
                                                        //  recently used values.  Pass 1 to create a directly mapped cache.
    , auto Depth                                        //< The total number of entries to cache.  Must be a multiple of `Associativity`.
    , auto Banks = 1                                    //< The number of memory banks cache memory is split into.
    >
class cache
{
private:
    //| Function used to write to the backing store.
    (Key key, optional<Word>[WordCount] data) -> void store;

    const auto SetCount = Depth / Associativity;

    // An integer that represents a single set of `Associativity` values in the cache.
    using set_index_t = index_t<SetCount>;

    cache_tags<Key, LUtime, Associativity, Depth> _tags;

    pipelined_memory<optional<Word>[WordCount], Depth, Banks, memory_norep> _data;

    inline index_t<Depth> get_data_index(set_index_t set_index, index_t<Associativity> way_index)
    {
        return (set_index * Associativity) + way_index;
    }

public:
    //| Initialize the cache.
    //  This must be called before using the cache.
    //  The caller must ensure that `intialize` is not called concurrently with other methods.
    void initialize()
    {
        _tags.initialize();
    }

    //| Write any unwritten data out of the cache.
    //  The caller must ensure that `flush` is not called concurrently with other methods.
    void flush()
    {
        // For each tag
        pipelined_for (Depth, [](index_t<Depth> tid)
        {
            auto set_index = tid / Associativity;
            auto entry_index = tid % Associativity;

            optional<Key> key = _tags.get_and_clear_unwritten_key(set_index, entry_index);

            if (key.is_valid)
            {
                auto data_index = get_data_index(set_index, entry_index);

                optional<Word>[WordCount] value_and_enable = _data.read(data_index);

                store(key.value, value_and_enable);
            }
        });
    }

    //| Write an array of words into the cache.
    void write
        ( Key key                           //< Key to lookup.
        , set_index_t set_index             //< Hashed value of `key`.
        , optional<Word>[WordCount] words   //< Values to store.
        )
    {
        // Tag lookup
        auto tag_result = _tags.get(key, set_index, true);

        auto data_index = get_data_index(set_index, tag_result.idx);

        bool hit = tag_result.hit;

        // Data read-modify-write
        auto prev = first(_data.atomically(data_index,
            [words, hit](optional<Word>[WordCount] prev)
            {
                return zip_with(
                    [hit](optional<Word> prev, optional<Word> word)
                    {
                        return make_optional(word.is_valid || (prev.is_valid && hit), word.is_valid ? word.value : prev.value);
                    },
                    prev,
                    words);
            }));

        // Write out evicted cache line
        if (tag_result.key_to_write.is_valid)
        {
             store(tag_result.key_to_write.value, prev);
        }
    }
}