// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.runner
import test.unit as unit

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//
// Array of memories
//
// using array syntax

class data_memory_test
{
private:
    memory<uint32, 512>[5] array_of_memories1;

    // using array
    array<memory<uint32, 512>, 5> array_of_memories2;

    //
    // Memory of arrays
    //
    // using array syntax
    memory<uint8[4], 512> memory_of_arrays1;

    // using array
    memory<array<uint8, 4>, 512> memory_of_arrays2;

    //
    // Array of memories of arrays
    //
    // using array syntax
    memory<uint8[4], 512>[5] array_of_memories_of_arrays1;

    // using array
    array<memory<array<uint8, 4>, 512>, 5> array_of_memories_of_arrays2;

public:
    void run(unit::tag_t tag)
    {

        bool result = true;


        for (const auto i : 5)
        {
            for (const auto j : 512)
            {
                array_of_memories1[i][j] = i * j;
                array_of_memories2[i][j] = i * j;

                array_of_memories_of_arrays1[i][j] = cast<uint8[4]>(cast<uint32>(i * j));
                array_of_memories_of_arrays2[i][j] = cast<uint8[4]>(cast<uint32>(i * j));
            }
        }

        for (const auto j : 512)
        {
            memory_of_arrays1[j] = cast<uint8[4]>(array_of_memories1[0][j]);
            memory_of_arrays2[j] = cast<uint8[4]>(array_of_memories2[1][j]);
        }

        for (const auto j : 512)
        {
            for (const auto k : 4)
            {
                result &&= memory_of_arrays1[j][k] == array_of_memories_of_arrays1[0][j][k];
                assert(result);
                result &&= memory_of_arrays2[j][k] == array_of_memories_of_arrays2[1][j][k];
                assert(result);
            }
        }

        unit::assert_equal(tag, true, result);
    }
}

inline void test_main()
{
    unit::test<1>(unit::fixture<data_memory_test>());
}
