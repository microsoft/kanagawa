// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

`timescale 1ps/1ps

// Contains a pure-virtual base class and some concrete subclasses to
// implement random stall interval and lengths. Designed for use with the
// <KanagawaSimStaller> module.

package KanagawaSimStallerPolicies;

virtual class StallPolicy;
    virtual function void next();
    endfunction

    virtual function void set_seed(int seed = 100);
        srandom(seed);
    endfunction

    virtual function int get_interval();
        return 0;
    endfunction

    virtual function int get_duration();
        return 0;
    endfunction
endclass

class NullStallPolicy extends StallPolicy;
endclass

class ConstrainedStallPolicy extends StallPolicy;
    rand int interval;
    rand int duration;

    constraint c_interval { interval == 0; };
    constraint c_duration { duration == 0; };

    virtual function void next();
        assert(randomize());
    endfunction

    virtual function int get_interval();
        return interval;
    endfunction

    virtual function int get_duration();
        return duration;
    endfunction
endclass

class InclusiveRangeStallPolicy
#(
    parameter INTERVAL_MIN_CYCLES,
    parameter INTERVAL_MAX_CYCLES,
    parameter DURATION_MIN_CYCLES,
    parameter DURATION_MAX_CYCLES
)
    extends StallPolicy;

    int interval;
    int duration;

    virtual function void next();
        interval = $urandom_range(INTERVAL_MIN_CYCLES, INTERVAL_MAX_CYCLES);
        duration = $urandom_range(DURATION_MIN_CYCLES, DURATION_MAX_CYCLES);
    endfunction

    virtual function int get_interval();
        return interval;
    endfunction

    virtual function int get_duration();
        return duration;
    endfunction
endclass

endpackage : KanagawaSimStallerPolicies
