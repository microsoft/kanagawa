// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import type.cstdint

const auto MaxPackets = 128; // Max packets in Parse
using     pkt_id_t=uint<clog2(MaxPackets)>;
using mem_index_t=uint<bitsizeof(pkt_id_t)+2>;
using parse_type_t=uint5  ;
using eth_type_t=uint16 ;

const eth_type_t etherTypeIpV6 = 0x86DD;

// Parse Headers sent to Lookup
struct SingleHeaderGroup
{
    uint32 dummy;
};

struct ParseExtractedData
{
	parse_type_t _parseType; // 5 bits
};

struct SingleParseResult
{
	ParseExtractedData _extractedData;
};

const auto MaxParseFlits = 8;

inline bool IsBitSet(uint32 data, uint6 bit)
{
    const uint32 mask = 1 << bit;
    return (data & mask) == mask;
}

inline SingleParseResult ParseSingleHeaderGroup(
    pkt_id_t packetId,
    bool direction,
    uint2 headerGroupIndex,
    uint8_t headerOffsetIn, // from last iteration
    uint16_t vxLanId,
    bool isIp)
{
    SingleParseResult result = {};

    uint8[78] header;

    parse_type_t parseType = 0; // 5 bits
    uint8 ipPtype = 0;
    uint16 grePType = 0;
    bool greC = false;
    bool greK = false;
    bool greS = false;
    bool ipFrag = false;
    bool l4Vxlan = false;
    bool l2Vtag = false;
    bool l3Ipv6 = false;

    uint32 greKey = 0;

    const uint8 ethernetHeaderLength = 14;
    const uint8 ipV4HeaderLength = 20;
    const uint8 ipV6HeaderLength = 40;
    const uint8 tcpHeaderLength = 20;
    const uint8 udpHeaderLength = 8;
    const uint8 vxlanHeaderLength = 16;

    const uint8 vtagSize = 18;

    uint8[3] headerOffset = {};

    headerOffset[0] = ethernetHeaderLength;

    // Extract etherType from eithernet frame
    // While extracting, perform byte-reversal (ntohs)
    const eth_type_t etherType = 0;

    // Extract IP version
    const uint8 ipVer = header[0] >> 4; // 4 bits

    // Compute the parse type based on the headers
    if (isIp == false)
    {
        // The first header is an ethernet header
        uint8 greByte = 0;

        if (etherType == etherTypeIpV6)
        {
            // IPv6
            ipPtype = header[20];

            // Extract and flip (ntohs) grePtype bytes
            grePType = concat(header[56], header[57]);
            greByte = header[54];
        }
        else
        {
            // IPv4
            ipPtype = header[23];

            // Extract and flip (ntohs) grePtype bytes
            grePType = concat(header[ipV4HeaderLength + 16], header[ipV4HeaderLength + 17]);
            greByte = header[ipV4HeaderLength + 14];
        }

        //result._ecn = header[15];

        // True if checksum is present
        greC = IsBitSet(greByte, 7);

        // True if a key is present
        greK = IsBitSet(greByte, 5);

        // True if a sequence number is present
        greS = IsBitSet(greByte, 4);

        switch (etherType)
        {
        case 0x8100: // VLAN_TAG
 
            break;

        case 0x0800: // IPv4
            
            break;

        case 0x86DD: // IPv6
        {

            
        }
        break;
        }
    }

    SingleHeaderGroup parseHeaders;
    result._extractedData._parseType = parseType;

    return result;
}

inline void ParseConstPropTest(unit::tag_t tag)
{
    parse_type_t[1] parseTypes = {};

    const uint32 headerGroupIndex = 0;

    const SingleParseResult singleParseResult = ParseSingleHeaderGroup(
        0,
        false,
        0,
        0,
        0,
        false);

    parseTypes[headerGroupIndex] = singleParseResult._extractedData._parseType;

    {
        const auto result = IsBitSet(parseTypes[0], 2);
        print("Parse: IsBitSet(parseTypes[0], 2) {result}\n"); 
        unit::assert_equal(tag, false, result);
    }
}

inline void test_main()
{
    unit::test<1>(ParseConstPropTest);
}
