// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
A data structure that maps keys to values using hashing.
*/

module data.hash_table
    { hash_table
    , toeplitz_hash
    }

import data.array
import data.bits
import data.counter
import data.memory.pipelined
import data.optional
import data.random.toeplitz
import data.tuple
import sync.atomic
import sync.atomic.init
import sync.lock

//| Computes a hash of input keys using an instance of the `toeplitz` class.
template
    < typename Hash //< Output hash type
    , typename Key  //< Input key type
    >
inline Hash toeplitz_hash(Key key)
{
    static toeplitz
        < 1
        , bitsizeof Hash
        , bitsizeof Key
        , bitsizeof Hash
        , 0x6D5A1BA6540E36AE7384C94779710E89BAB5778362E9B302C3A2CF202B5615A9DD5E8EF2E2EF40444F7C23BBB76A508BF48BE900D8A33DAE8829FB3C643771A6
        > _toeplitz;

    return _toeplitz.calc_hash(key);
}

//| A mapping from `Key` to `Value`.
// Use `insert_or_update` to insert, lookup, modify, and reset the mapping.
// The only way to remove entries from the table is to reset the entire table to the initial empty state.
template
    < typename Key
    , typename Value
    , auto Depth                                                //< Maximum number of elements that can be stored.
    , auto Associativity                                        //< Number of locations to consider on each search iteration.
                                                                // Larger values lead to more resource consumption and higher throughput.
    , (Key)->index_t<Depth / Associativity> hash_fn
        = toeplitz_hash<index_t<Depth / Associativity>, Key>    //< Function that maps a key to a location in the hash table.
    , auto GenerationBits = 4                                   //< Number of bits to ammortize reset costs.
                                                                // Higher values lead to more resource consumption, and lower the average cost of reset.
    , auto MaxIterations = Depth                                //< Maximum number of search iterations before returning an error
    , auto DataBanks = 1                                        //< Number of banks to store values.
                                                                // If `Depth` is high, then increasing `DataBanks` can improve clock frequency.
    , auto HistorySize = 1                                      //< Size of arrays used to detect hazards.  Larger values increase frequency and resource usage.
    , auto MaxThreads = 512                                     //< Maximum number of threads that can be present inside of `insert_or_update` concurrently.
                                                                // Caller does not need to ensure this limit is respected.
                                                                // Higher values enable more concurrency which can improve performance if
                                                                // the allocation function compiles to a deep pipeline or if `DataBanks` is set to a large value.
                                                                // Lower values save resources by using narrower counters to track in-flight threads.
    >
class hash_table
{
private:
    using generation_id_t = uint<GenerationBits>;
    using element_index_t = index_t<Depth>;
    using way_index_t = index_t<Associativity>;

    // Avoid declaring arrays with 0 size
    const bool UseHistory = (HistorySize > 0);

    const auto HistoryArraySize = UseHistory ? HistorySize : 1;

    struct tag
    {
        generation_id_t gen_id;
        Key key;
    }

    rwlock<MaxThreads> _tag_lock;

    const auto NumSets = Depth / Associativity;

    memory<tag, NumSets>[Associativity] _tags;

    pipelined_memory<Value, Depth, DataBanks> _data;

    counter<MaxThreads, 0> _threads_in_flight;

    // The type that keys are hashed to
    using set_index_t = index_t<Depth / Associativity>;

    using history_tuple = tuple3<set_index_t, generation_id_t, bool>;

    using history_entry_t = optional<history_tuple>;

    history_entry_t[HistoryArraySize] _outer_history;

public:
    //| All sets must have the same number of elements
    static assert(0 == (Depth % Associativity));

    //| Set count must be a power of 2
    static assert(0 == (NumSets & (NumSets - 1)));

    //| MaxIterations must be positive, otherwise no lookup operation could ever succeed
    static assert(MaxIterations > 0);

    //| Return value on a successful `insert_or_update`
    struct result
    {
        //| True if this is the first time the associated key has been used since the hash table was reset.
        bool inserted;

        //| Values in the hash table before and after the lookup
        // When an key is first inserted into the table, `value.first` is
        // the value returned by `allocate_fn`.
        pair<Value, Value> value;
    }

private:
    struct lookup_one_set_result
    {
        bool continue_searching;

        // True if a new key/value pair was inserted
        bool inserted;

        // is_valid == true iff an existing key/value pair was found
        // or if a new one was inserted.
        // False if the search failed.
        optional<element_index_t> element_index;
    }

    //| Used to block the first thread of each generation until `_threads_in_flight` is zero
    inline bool test_thread_count(bool reset)
    {
        bool result = false;

        if (!reset || (_threads_in_flight.count() == 0))
        {
            result = true;

            _threads_in_flight.add(1);
        }

        return result;
    }

public:

    //| Combined function that can be used to:
    //
    // 1. Insert a new `(Key, Value)` pair.
    // 2. Lookup the `Value` associated with a `Key` that is already present in the table.
    // 3. Modify the `Value` associated with a `Key` that is already present in the table.
    // 4. Reset the table to the initial state.
    //
    // Inserts, lookups, and modifications use an iterative search.
    // A hash of the input key is used to determine the starting location for the search.
    // On each iteration, `Associativity` locations are checked in parallel.  If a matching key is found
    // then the hash table runs at full throughput.  Otherwise, the search loop proceeds,
    // searching `Associativity` new locations on each iteration.
    // Callers must ensure there are never concurrent calls to this function
    // from separate call sites.
    // Calls to `allocate_fn` occur in the same order as calls to `insert_or_update`.
    inline optional<result> insert_or_update
        ( Key key                           //< Key used to find a location to store the value
        , bool reset                        //< True if all elements in the hash table should be removed before performing the search.
        , bool insert_on_miss               //< True if a new `(Key, Value)` pair should be inserted into the table if a pair with a matching `key` is not already present.
        , ()->Value allocate_fn             //< Function that is called when a key is inserted into the hash table.
                                            // Returns a value that is passed to `access_fn`.
        , (Value, bool)->Value access_fn    //< Function that accepts a previous value and a bool indicating if an insert occured.
                                            // Returns a new value to store in the hash table associated with the key.
                                            // If `key` was not already present in the table then,
                                            // the first parameter is the value returned by `allocate_fn` and the second parameter is `true`.
                                            // Otherwise, the first parameter is the value currently stored in the table and the second parameter is `false`.
        )
    {
        // Update generation ID if resetting
        auto generation_id = init_generational<GenerationBits>(reset);

        // Generation ID 0 is the reset value
        assert(generation_id.second != 0);

        // Lock the tags
        // If generation_id.first is true, then acquire exclusive access
        // which will block until preceding threads have drained
        _tag_lock.lock(!generation_id.first);

        if (generation_id.first)
        {
            // Reset all tags to generation ID 0 (invalid generation ID)
            pipelined_for(NumSets, [](set_index_t set_index)
            {
                static for (const auto way_index : Associativity)
                {
                    _tags[way_index][set_index] = {};

                    // Allow writes to be pipelined
                    barrier;
                }
            });

            // Reset hazard tracking history
            static for (const auto i : HistorySize)
            {
                _outer_history[i].is_valid = false;
            }
        }

        lookup_one_set_result internal_result;

        static if (MaxIterations == 1)
        {
            // only perform 1 lookup, no loop necessary
            internal_result = lookup_one_set(key, generation_id.second, insert_on_miss, 0);
        }
        else
        {
            // Linear probing.
            // It is important to ensure that all threads that are probing
            // have the same generation id.  If threads with different generation IDs
            // can be probing at the same time, then a thread could incorrectly
            // determine that a slot was unsued (because of a generation id mismatch).
            // When reset=true, block until all younger threads have finished probing.
            atomic do; while(!test_thread_count(reset));

            count_t<NumSets> offset = 0;

            // allocate_fn is not called inside of this loop
            // to ensure that calls to allocate_fn occur in the same order as calls
            // to insert_or_update.
            do
            {
                // will be cast to set_index_t
                assert(offset < NumSets);

                internal_result = lookup_one_set(key, generation_id.second, insert_on_miss, cast<set_index_t>(offset));

                offset++;

                // internal_result.continue_searching term is for the case where insert_on_miss is false.
                // In this case the search can stop as soon as 1 emtpy tag is found.
                // (offset < NumSets) term prevents infinite loop if the table is full.
            } while(!internal_result.element_index.is_valid && internal_result.continue_searching && (offset < NumSets) && (offset < MaxIterations));

            _threads_in_flight.decrement();
        }

        optional<result> r = {};

        if (internal_result.element_index.is_valid)
        {
            Value initial_value;

            if (internal_result.inserted)
            {
                // First time this key has been inserted
                // Get initial value
                initial_value = allocate_fn();
            }

            bool inserted = internal_result.inserted;

            auto update_result = _data.atomically(
                internal_result.element_index.value,
                [access_fn, inserted, initial_value](Value prev)
                {
                    return access_fn(inserted ? initial_value : prev, inserted);
                });

            r.is_valid = true;
            r.value.inserted = internal_result.inserted;
            r.value.value.first = inserted ? initial_value : update_result.first;
            r.value.value.second = update_result.second;
        }

        // Unlock the tags
        _tag_lock.unlock(!generation_id.first);

        return r;
    }

private:
    inline lookup_one_set_result lookup_one_set
        ( Key key
        , generation_id_t generation_id
        , bool insert_on_miss
        , set_index_t set_offset)
    {
        // Determine which set to lookup in
        // Compute the hash, and then add the provided probing offset
        set_index_t set_index = static_cast(hash_fn(key) + set_offset);
        assert(set_index < NumSets);

        // Search all tags in this set
        struct Context
        {
            bool found_tag;
            bool inserted;
            bool found_empty_tag;
            way_index_t way_index;
        }

        Context ctx = {};

        bool[HistoryArraySize] outer_hazard_array = {};

        static if (UseHistory)
        {
            // Get the memory address, generation id, and insert_on_miss
            // from recent threads
            history_entry_t[HistorySize] snapped_history;

            atomic
            {
                snapped_history = _outer_history;

                history_entry_t[HistorySize] new_history = snapped_history;

                static for (const auto i : HistorySize - 1)
                {
                    new_history[i] = snapped_history[i + 1];
                }

                // The only time is_valid is false is after tags are reset
                new_history[HistorySize - 1] = template just<history_tuple>({set_index, generation_id, insert_on_miss});

                _outer_history = new_history;
            }

            // Determine if a recent thread
            // could possible write to the tag
            // that this thread will read from
            outer_hazard_array = map(
                [set_index, generation_id](history_entry_t src)
                {
                    return src.is_valid && (src.value.first == set_index) && (src.value.second == generation_id) && src.value.third;
                },
                snapped_history);
        }

        static for (const auto way_index : Associativity)
        {
            optional<index_t<HistoryArraySize>> hazard_index = {};

            if (UseHistory)
            {
                // Filter the hazard array based on ctx.found_tag
                // If ctx.found_tag was true for a recent thread
                // at this point, then that thread will not update the tag
                // for this way of the table
                bool[HistoryArraySize] not_found_tag_history = first(atomically([ctx](bool[HistoryArraySize] prev)
                {
                    bool[HistoryArraySize] new;

                    static for (const auto i : HistoryArraySize - 1)
                    {
                        new[i] = prev[i + 1];
                    }

                    new[HistoryArraySize - 1] = !ctx.found_tag;

                    return new;
                }));

                bool[HistoryArraySize] hazard_array = zip_with(
                    [](bool x, bool y){return x && y;},
                    outer_hazard_array,
                    not_found_tag_history);

                // If there is more than one hazard
                // select the one associated with the most recent thread
                hazard_index = highest_one(cast<uint<HistoryArraySize>>(hazard_array));
            }

            // If a recent thread used the same memory address and generation ID
            // then the [[schedule]] block below will not modify any tags
            // There are three relevant cases:
            // 1) The recent thread found a used, and non-matching tag
            //    The recent thread will not modify the tag
            //
            // 2) The recent thread found a used, matching tag
            //    The recent thread will not modify the tag
            //
            // 3) The recent thread found an unused tag
            //    The recent thread will modify the tag (marking it as used)
            //
            // In each of these cases, the current thread
            // will never see an unused tag.
            // The current thread may detect a match with the tag
            // but that detection will occur after the [[schedule]] block
            // If generation IDs have changed
            // then the current thread can overwrite the tag
            // without seeing the tag updates from the recent thread
            // because the current thread will see the generation ID
            // mismatch either way
            bool hazard_detected = hazard_index.is_valid;

            // If UseHistory is false
            // then hazards should never be detected
            assert(UseHistory || !hazard_detected);

            tag curr_tag = {};

            bool set_way_index = false;

            [[schedule(HistorySize + 1)]]
            {
                curr_tag = _tags[way_index][set_index];

                if (!ctx.found_tag && !hazard_detected)
                {
                    // Tag has not been found, search this tag
                    assert(!ctx.inserted);

                    if (curr_tag.gen_id != generation_id)
                    {
                        // This tag is unoccupied.
                        ctx.found_empty_tag = true;

                        if (insert_on_miss)
                        {
                            curr_tag.gen_id = generation_id;
                            curr_tag.key = key;

                            ctx.found_tag = true;
                            ctx.inserted = true;

                            set_way_index = true;
                        }
                    }
                    else if (curr_tag.key == key)
                    {
                        // Tag matches

                        // If an empty tag has been found in a previous slot
                        // then a matching tag should not be found
                        assert(!ctx.found_empty_tag);

                        ctx.found_tag = true;

                        set_way_index = true;
                    }

                    _tags[way_index][set_index] = curr_tag;
                }
            }

            if (UseHistory)
            {
                // Record the most recent [HistorySize] tags read/written by the [[schedule]] block above
                tag[HistoryArraySize] tag_history = first(atomically([curr_tag, hazard_detected, hazard_index](tag[HistoryArraySize] prev)
                {
                    tag[HistoryArraySize] new_history;

                    static for (const auto i : HistoryArraySize - 1)
                    {
                        new_history[i] = prev[i + 1];
                    }

                    // If a hazard was detected, then curr_tag could have stale data.
                    // Use history from previous threads instead
                    new_history[HistoryArraySize - 1] = hazard_detected ? prev[hazard_index.value] : curr_tag;

                    return new_history;
                }));

                // If tag checking and modification was skipped in the [[schedule]] block above
                // then compare against tags read/written by recent threads
                if (!ctx.found_tag && hazard_detected)
                {
                    tag hazard_tag = tag_history[hazard_index.value];

                    if ((hazard_tag.gen_id == generation_id) && (hazard_tag.key == key))
                    {
                        ctx.found_tag = true;
                        assert(!ctx.inserted);

                        set_way_index = true;
                    }
                }
            }

            if (set_way_index)
            {
                assert(ctx.found_tag);

                ctx.way_index = way_index;
            }
        }

        lookup_one_set_result r = {};

        if (ctx.found_tag)
        {
            assert(ctx.way_index < Associativity);

            element_index_t data_addr = checked_cast((set_index * Associativity) + ctx.way_index);

            r.inserted = ctx.inserted;
            r.continue_searching = false;
            r.element_index = make_optional(true, data_addr);
        }
        else
        {
            // Matching tag was not found
            // and the key was not inserted
            // Continue searching only if all tags were
            // valid and contained non-matching keys
            r.continue_searching = !ctx.found_empty_tag;
        }

        return r;
    }
}
