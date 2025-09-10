// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.random.toeplitz

const auto DataWidth = 242;
const auto AlignedDataWidth = 128;
const auto BigHashWidth = 512;
const auto MediumHashWidth = 49;
const auto SmallHashWidth = 16;
const auto BigKeyWidth = 512;
const auto SmallKeyWidth = 16;
const auto MediumKeyWidth = 64;
const uint<BigKeyWidth> BigKey = 0x6D5A1BA6540E36AE7384C94779710E89BAB5778362E9B302C3A2CF202B5615A9DD5E8EF2E2EF40444F7C23BBB76A508BF48BE900D8A33DAE8829FB3C643771A6;
const uint<SmallKeyWidth> SmallKey = 0x6d5a;
const uint<MediumKeyWidth> MediumKey = 0x6D5A1BA691A843CF;

class ToeplitzWrapper
{
private:
    // toeplitz<PipelineCycles, HashWidth, DataWidth, KeyWidth, Key>
    toeplitz<3, BigHashWidth, DataWidth, BigKeyWidth, BigKey> _bhbkmpc;
    toeplitz<3, MediumHashWidth, DataWidth, MediumKeyWidth, MediumKey> _mhmkmpc;
    // Change the MultipleCycles below to 1 when the Kanagawa compiler supports static for with zero loop count.
    toeplitz<3, SmallHashWidth, DataWidth, SmallKeyWidth, SmallKey> _shskmpc;
    toeplitz<4, SmallHashWidth, AlignedDataWidth, SmallKeyWidth, SmallKey> _shskmpc_aligned;
    toeplitz<1, SmallHashWidth, DataWidth, SmallKeyWidth, SmallKey> _shskopc;

public:
    uint<BigHashWidth> BigHashBigKeyMultiCycles(uint<DataWidth> data)
    {
        return _bhbkmpc.calc_hash(data);
    }

    uint<MediumHashWidth> MediumHashMediumKeyMultiCycles(uint<DataWidth> data)
    {
        return _mhmkmpc.calc_hash(data);
    }

    uint<SmallHashWidth> SmallHashSmallKeyMultiCycles(uint<DataWidth> data)
    {
        return _shskmpc.calc_hash(data);
    }

    uint<SmallHashWidth> SmallHashSmallKeyMultiCyclesAligned(uint<AlignedDataWidth> data)
    {
        return _shskmpc_aligned.calc_hash(data);
    }

    uint<SmallHashWidth> SmallHashSmallKeyOneCycle(uint<DataWidth> data)
    {
        return _shskopc.calc_hash(data);
    }
}

export ToeplitzWrapper;
