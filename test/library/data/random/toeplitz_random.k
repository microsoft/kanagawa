// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.random.toeplitz

class ToeplitzRandomWrapper
{
private:
    toeplitz_random<32, 256, 0x6D5A1BA6540E36AE7384C94779710E89BAB5778362E9B302C3A2CF202B5615A9, 1> _random;

public:
    uint32 NextRandom(bool set_seed, uint32 seed)
    {
        return _random.next(set_seed, seed);
    }
}

export ToeplitzRandomWrapper;
