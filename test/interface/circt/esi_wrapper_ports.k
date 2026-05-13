// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Source for the cli.esi_wrapper_ports test. The class below mixes the six
// combinations of ESI wrapper port shapes that EsiWrapper now produces:
//
//   regular export      -> input  bundle (FIFO + ValidReady channels)
//   [[async]] export    -> input  bare channel (FIFO)
//   no_backpressure exp -> input  bundle (ValidOnly channels)
//   regular callback    -> output bundle (FIFO + ValidReady channels)
//   [[async]] callback  -> output bare channel (FIFO)
//   no_backpressure cb  -> output bare channel (ValidOnly)
//
// `check_esi_wrapper_ports.py` greps the emitted CIRCT MLIR for each
// expected port declaration.

class Mixed
{
private:
    (uint32)->uint32 reg_cb;

    [[async]] (uint32)->void async_cb;

    [[no_backpressure]] [[async]] (uint32)->void nbp_cb;

public:
    uint32 RegExport(uint32 x)
    {
        return reg_cb(x);
    }

    [[async]] void AsyncExport(uint32 x)
    {
        async_cb(x);
    }

    [[no_backpressure]] uint32 NbpExport(uint32 x)
    {
        nbp_cb(static_cast(x + 1));
        return static_cast(x + 4);
    }
}

export Mixed;
