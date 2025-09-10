// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner

// Regression tests for initializing floats with negative values (unary minus applied at compile time)
class Test534
{
private:
    float32 foo534 = -1.0;

public:
    void run(unit::tag_t tag)
    {
        println(foo534);
        barrier;
        unit::assert(tag, cast<uint32>(foo534) == 0xbf800000);
    }
}

// repro of a bug with nested template classes
template <uint32 MAX_SIZE>
class PriorityQueue535 {
public:
    uint32 queueid;

    inline void SetId(uint32 i)
    {
        queueid = i;
    }

    inline uint32 GetId()
    {
        return queueid;
    }
}

const auto QUERY_LANES = 2;

template<typename data_t, uint32 MAX_NEIGHBORS, uint32 MAX_DIMENSIONS, uint32 LANE_WIDTH>
class KnnCore535 {

public:

	PriorityQueue535<MAX_NEIGHBORS> pqueue;

	uint32 core_id;

	// Reset the priority queues
	void reset() {
        print("Calling setId {core_id}\n");
        pqueue.SetId(core_id);
	}

    void validate(unit::tag_t tag)
    {
        print("validate {core_id} {pqueue.GetId()}\n");
        barrier;

        unit::assert(tag, core_id == pqueue.GetId());
    }
}

/*
	A basic k-means accelerator implementation
	- dataset is stored off chip
	- centroids are stored on chip
	TODO: turn this into a class-based implementation
*/

// Instantiation individual kNN processing units
class Test535
{
private:
    using data_t_535=float;
    using knn_core_t_535=KnnCore535<data_t_535, 3, 4, 5>;
    knn_core_t_535[QUERY_LANES] cores;

public:
    void run(unit::tag_t tag)
    {
    	static for (const auto Q : QUERY_LANES) {
    		cores[Q].core_id = Q;
    		cores[Q].reset();
    	}
    
        static for (const auto Q : QUERY_LANES) {
    		cores[Q].validate(tag);
    	}
    }
}

// nested template bug
template<uint32 NUM_COLUMNS, typename T>
class CU_stage539 {
public:
    inline uint32 execute(T[NUM_COLUMNS] input) {
        uint32 result = 1 + NUM_COLUMNS;

        return result;
    }
}

template<uint32 NUM_COLUMNS>
class CU539
{
public:
    CU_stage539<NUM_COLUMNS, int32> stage0;

    inline uint32 go()
    {
        int32[NUM_COLUMNS] data;
        return stage0.execute(data);
    }
}

class Test539
{
private:
    CU539<16> cu_inst539;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, cu_inst539.go() == 17);
    }
}

// member variable used before it is declared
class Foo540
{
public:
    uint32 get()
    {
        return x;
    }

private:
    uint32 x = 7;
}

class Test540
{
private:
    Foo540 g_foo540;

public:
    void run(unit::tag_t tag)
    {
        unit::assert(tag, g_foo540.get() == 7);
    }
}

// memory that is only ever read from
class Test541
{
private:
    [[memory]] uint32[512] g_mem541;

public:
    void run(unit::tag_t tag)
    {
        println(g_mem541[3]);
    }
}

// atomic with no compute inside
class Test542
{
private:
    [[memory]] bool[512] m542;

public:
    [[pipelined]] void Clear542(uint32 i)
    {
        m542[i] = false;
    }

    [[pipelined]] void Helper542(uint32 i, unit::tag_t tag)
    {
        bool snapped;

        atomic
        {
            auto val = m542[i];
            snapped = val;
            val = true;
            m542[i] = val;
        }

        unit::assert(tag, snapped == false);
    }

    void run(unit::tag_t tag)
    {
        Clear542(512);

        Helper542(512, tag);
    }
}

// Literal which is the largest 64-bit integer
const int64 MAX_RLE_VALUE_544 = 9223372036854775807;

inline void TestCase544(unit::tag_t tag)
{
    int64 v = MAX_RLE_VALUE_544;

    println(v);

    barrier;

    unit::assert(tag, v == 9223372036854775807);
}

// Literal which is the largest 64-bit integer
const int64 MIN_RLE_VALUE_545 = -9223372036854775807;

inline int64 Foo545(uint2 idx)
{
    return mux(idx, 0, MIN_RLE_VALUE_545, 0, 0);
}

inline void TestCase545(unit::tag_t tag)
{
    unit::assert(tag, MIN_RLE_VALUE_545 == Foo545(1));
}

// uint<N>
const size_t Width546 = 3;

inline void TestCase546(unit::tag_t tag)
{
    uint<Width546> x;

    x = 10;

    unit::assert(tag, x == 2);
    unit::assert(tag, 3 == bitsizeof(x));
}

// uint<N>
const size_t Width547 = 4;

inline void TestCase547(unit::tag_t tag)
{
    int<Width547> x;

    x = -127;

    unit::assert(tag, x == 1);
    unit::assert(tag, 4 == bitsizeof(x));
}

class Helper548
{
private:
    uint32 _valid = 0;
    uint32 _invalid = 0;

    (uint32)->uint32 read_mem;

public:
    [[async, pipelined]] void Read(uint32 tid)
    {
        uint32 val = read_mem(tid);

        if (val == (tid + 2))
        {
            atomic
            {
                _valid++;
            }
        }
        else
        {
            atomic
            {
                _invalid++;
            }
        }
    }

    inline uint32 GetValid()
    {
        return _valid;
    }

    inline uint32 GetInvalid()
    {
        return _invalid;
    }

    inline void Wait(uint32 expected)
    {
        atomic do; while((_valid + _invalid) != expected);
    }
}

// [[memory, non_replicated]]
class Test548
{
private:
    [[memory, non_replicated]] uint32[512] _mem548;

    [[pipelined]] void Init548(uint32 tid)
    {
        _mem548[tid] = tid + 2;
    }

    inline uint32 ReadMemory(uint32 addr)
    {
        return _mem548[addr];
    }

    Helper548[2] g_helper548 = 
    {
        {.read_mem = ReadMemory},
        {.read_mem = ReadMemory}
    };

public:
    void run(unit::tag_t tag)
    {
        // fill the memory
        Init548(512);

        //  Start reads from both helpers
        static for (const auto i : 2)
        {
            g_helper548[i].Read(512);

            // ensure the reads start at different times
            static for(const auto j : 10)
            {
                stages<2>();
            }
        }

        // Wait for all operations to finish
        static for (const auto i : 2)
        {
            g_helper548[i].Wait(512);
        }

        uint32 totalValid = 0;
        uint32 totalInvalid = 0;

        static for (const auto i : 2)
        {
            print("Instance: {i} valid: {g_helper548[i].GetValid()} invalid: {g_helper548[i].GetInvalid()}\n");

            totalValid += g_helper548[i].GetValid();
            totalInvalid += g_helper548[i].GetInvalid();
        }

        // in the worst case, all read operations conflict
        // therefore, exactly half of the reads will return invalid results
        unit::assert(tag, totalValid >= 512);

        // Because a no-rep memory is used, some reads should conflict
        unit::assert(tag, totalInvalid > 0);

        unit::assert(tag, (totalValid + totalInvalid) == 1024);
    }
}

class Helper549
{
private:
    uint32 _valid = 0;
    uint32 _invalid = 0;

    (uint32, uint2)->uint32 read_mem;

public:
    [[async, pipelined]] void Read(uint32 tid)
    {
        uint32 localValid = 0;
        uint32 localInvalid = 0;

        static for(const auto i : 4)
        {
            uint32 val = read_mem(tid, i);

            if (val == (tid + 2 + i))
            {
                localValid++;
            }
            else
            {
                localInvalid++;
            }
        }

        atomic
        {
            _valid += localValid;
            _invalid += localInvalid;
        }
    }

    inline uint32 GetValid()
    {
        return _valid;
    }

    inline uint32 GetInvalid()
    {
        return _invalid;
    }

    inline void Wait(uint32 expected)
    {
        atomic do; while((_valid + _invalid) != expected);
    }
}

// [[memory, non_replicated]] with an array of memories
class Test549
{
private:
    memory_norep<uint32, 512>[4] _mem549;

    [[pipelined]] void Init549(uint32 tid)
    {
        static for(const auto i : 4)
        {
            _mem549[i][tid] = tid + 2 + i;
        }
    }
    
    inline uint32 ReadMemory(uint32 addr, uint2 i)
    {
        return _mem549[i][addr];
    }

    Helper549[2] g_helper549 = 
    {
        {.read_mem = ReadMemory},

        {.read_mem = ReadMemory}
    };

public:
    void run(unit::tag_t tag)
    {
        // fill the memory
        Init549(512);

        //  Start reads from both helpers
        static for (const auto i : 2)
        {
            g_helper549[i].Read(512);

            // ensure the reads start at different times
            static for(const auto j : 10)
            {
                stages<2>();
            }
        }

        // Wait for all operations to finish
        static for (const auto i : 2)
        {
            g_helper549[i].Wait(2048);
        }

        uint32 totalValid = 0;
        uint32 totalInvalid = 0;

        static for (const auto i : 2)
        {
            print("Instance: {i} valid: {g_helper549[i].GetValid()} invalid: {g_helper549[i].GetInvalid()}\n");

            totalValid += g_helper549[i].GetValid();
            totalInvalid += g_helper549[i].GetInvalid();
        }

        // assert after print
        barrier;

        // in the worst case, all read operations conflict
        // therefore, exactly half of the reads will return invalid results
        unit::assert(tag, totalValid >= 2048);

        // Because a no-rep memory is used, some reads should conflict
        unit::assert(tag, totalInvalid > 0);

        unit::assert(tag, (totalValid + totalInvalid) == 4096);
    }
}

inline void test_main()
{
    unit::test<534>(unit::fixture<Test534>());
    unit::test<535>(unit::fixture<Test535>());
    unit::test<539>(unit::fixture<Test539>());
    unit::test<540>(unit::fixture<Test540>());
    unit::test<541>(unit::fixture<Test541>());
    unit::test<542>(unit::fixture<Test542>());
    unit::test<544>(TestCase544);
    unit::test<545>(TestCase545);
    unit::test<546>(TestCase546);
    unit::test<547>(TestCase547);
    unit::test<548>(unit::fixture<Test548>());
    unit::test<549>(unit::fixture<Test549>());
}
