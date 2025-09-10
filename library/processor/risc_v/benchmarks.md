# RISC-V Core performance benchmarks

## Agilex Fmax and area

All configurations benchmarked below share the following parameters:

    IMEM            DMEM            BTB depth
    --------------------------------------------------------------------------
    64 KB           8 KB            1024

The following compiler settings are used:

    --rmw-memory-write-delay=MemDelay --rmw-hardened-write-delay=MemDelay --register-ratio=3

The following are results for a 4-seed VPP project build for Agilex FPGA.

    ISA       RV32I    RV32I    RV32I    RV32I    RV32I    RV32I    RV32I    RV32I    RV32I    RV32I   RV32IM   RV32IM   RV32IM   RV32IM   RV32IM   RV32IM
    Harts         1        1        2        2        2        2        4        4        8        8        1        2        4        4        8        8
    Config              mmio            custom    fetch     fast    small     fast    small     fast                      small     fast    small     fast
    ------------------------------------------------------------------------------------------------------------------------------------------------------
    Fmax        301      300      320      335      327      383      401      506      398      505      288      293      358      438      361      481
    ALMs      1,341    1,503    1,302    1,587    1,632    1,447    1,392    1,507    1,571    1,627    1,458    1,444    1,439    1,571    1,585    1,672
    M20K         34       30       34       32       30       35       36       37       32       32       34       36       36       37       32       33
    DSP           0        0        0        0        0        0        0        0        0        0        4        4        4        4        4        4

    MemDelay      0        0        0        0        0        1        0        1        0        1        0        0        0        1        0        1

Empty VPP project with the same exports is 202 ALMs.

## Coremark compiled for RV32IM

Time measurement in the test is via `rdtime` instruction which in our
implementation is the same as `rdcycle` and reports number of clock cycles.
Number of dynamic instructions obtained via `rdinstret` instruction.

Average dynamic instructions per iteration: 266,871

### 1-hart configuration

    2K performance run parameters for coremark.
    CoreMark Size    : 666
    Total ticks      : 1526104924
    Total instr      : 1005175646
    Total time (secs): 15.261049
    Iterations/Sec   : 262.105176
    Iterations       : 4000
    Compiler version : GCC10.1.0
    Compiler flags   : -O3
    Memory location  : STACK
    seedcrc          : 0xe9f5
    [0]crclist       : 0xe714
    [0]crcmatrix     : 0x1fd7
    [0]crcstate      : 0x8e3a
    [0]crcfinal      : 0x65c5
    Correct operation validated. See README.md for run and reporting rules.
    CoreMark 1.0 : 262.105176 / GCC10.1.0 -O3 / STACK

Coremark score/MHz: `2.62`

Average Instructions per cycle: `0.66`

### 2-hart configuration

    2K performance run parameters for coremark.
    CoreMark Size    : 666
    Total ticks      : 1757363740
    Total instr      : 753882079
    Total time (secs): 17.573637
    Iterations/Sec   : 170.710248
    Iterations       : 3000
    Compiler version : GCC10.1.0
    Compiler flags   : -O3
    Memory location  : STACK
    seedcrc          : 0xe9f5
    [0]crclist       : 0xe714
    [0]crcmatrix     : 0x1fd7
    [0]crcstate      : 0x8e3a
    [0]crcfinal      : 0xcc42
    Correct operation validated. See README.md for run and reporting rules.
    CoreMark 1.0 : 170.710248 / GCC10.1.0 -O3 / STACK

Coremark score/MHz: `1.71` per hart * `2` harts = `3.41`

Average Instructions per hart cycle: `0.86`

### 4-hart configuration

    2K performance run parameters for coremark.
    CoreMark Size    : 666
    Total ticks      : 1135199396
    Total instr      : 276423239
    Total time (secs): 11.351994
    Iterations/Sec   : 96.899276
    Iterations       : 1100
    Compiler version : GCC10.1.0
    Compiler flags   : -O3
    Memory location  : STACK
    seedcrc          : 0xe9f5
    [0]crclist       : 0xe714
    [0]crcmatrix     : 0x1fd7
    [0]crcstate      : 0x8e3a
    [0]crcfinal      : 0x33ff
    Correct operation validated. See README.md for run and reporting rules.
    CoreMark 1.0 : 96.899276 / GCC10.1.0 -O3 / STACK

Coremark score/MHz: `0.97` per hart * `4` harts = `3.88`

Average Instructions per hart cycle: `0.97`

## Coremark compiled for RV32I

Note that when "M" extension is not used  Coremark score is highly limited by
the matrix multiply component of the test.

Average dynamic instructions per iteration: 712,491

### 1-hart configuration, RV32I

    2K performance run parameters for coremark.
    CoreMark Size    : 666
    Total ticks      : 1876039010
    Total instr      : 1382851990
    Total time (secs): 18.760390
    Iterations/Sec   : 106.607591
    Iterations       : 2000
    Compiler version : GCC10.1.0
    Compiler flags   : -O3
    Memory location  : STACK
    seedcrc          : 0xe9f5
    [0]crclist       : 0xe714
    [0]crcmatrix     : 0x1fd7
    [0]crcstate      : 0x8e3a
    [0]crcfinal      : 0x4983
    Correct operation validated. See README.md for run and reporting rules.
    CoreMark 1.0 : 106.607591 / GCC10.1.0 -O3 / STACK

Coremark score/MHz: `1.07`

Average Instructions per cycle: `0.74`

### 2-hart configuration, RV32I

    2K performance run parameters for coremark.
    CoreMark Size    : 666
    Total ticks      : 1778586528
    Total instr      : 760568439
    Total time (secs): 17.785865
    Iterations/Sec   : 61.846865
    Iterations       : 1100
    Compiler version : GCC10.1.0
    Compiler flags   : -O3
    Memory location  : STACK
    seedcrc          : 0xe9f5
    [0]crclist       : 0xe714
    [0]crcmatrix     : 0x1fd7
    [0]crcstate      : 0x8e3a
    [0]crcfinal      : 0x33ff
    Correct operation validated. See README.md for run and reporting rules.
    CoreMark 1.0 : 61.846865 / GCC10.1.0 -O3 / STACK

Coremark score/MHz: `0.62` per hart * `2` harts = `1.24`

Average Instructions per hart cycle: `0.86`

### 4-hart configuration, RV32I

    2K performance run parameters for coremark.
    CoreMark Size    : 666
    Total ticks      : 1799850436
    Total instr      : 414855850
    Total time (secs): 17.998504
    Iterations/Sec   : 33.336103
    Iterations       : 600
    Compiler version : GCC10.1.0
    Compiler flags   : -O3
    Memory location  : STACK
    seedcrc          : 0xe9f5
    [0]crclist       : 0xe714
    [0]crcmatrix     : 0x1fd7
    [0]crcstate      : 0x8e3a
    [0]crcfinal      : 0xbd59
    Correct operation validated. See README.md for run and reporting rules.
    CoreMark 1.0 : 33.336103 / GCC10.1.0 -O3 / STACK

Coremark score/MHz: `0.33` per hart * `4` harts = `1.33`

Average Instructions per hart cycle: `0.92`
