// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


// Generates stalls or enables using a policy specified by an instance of a
// subclass of KanagawaSimStallerPolicy. Used during simulation to generate
// constrained random stalls.

module KanagawaSimStaller
#(
    parameter type STALL_POLICY,
    parameter STALL_ON_RESET = 1,
    parameter ENABLED_ON_RESET = 1,
    parameter SEED = 1,
    parameter VERBOSITY = 0
)
(
    input   wire    clk,
    input   wire    rst,
    output wire     stalled_out,
    output wire     not_stalled_out
);

    bit                     stalls_enabled_ff;
    bit                     stalled_ff;
    int                     interval_ff;
    int                     duration_ff;
    STALL_POLICY            stall_policy = new;

    initial begin
        stall_policy.set_seed(SEED);
    end

    function void enable_stalls();
        stalls_enabled_ff = 1'b1;
    endfunction

    function void disable_stalls();
        stalls_enabled_ff = 1'b0;
    endfunction

    function bit stalls_enabled();
        return stalls_enabled_ff;
    endfunction

    always_ff @(posedge clk) begin
        if (rst) begin
            stalled_ff <= STALL_ON_RESET ? 1'b1 : 1'b0;
            stalls_enabled_ff <= ENABLED_ON_RESET ? 1'b1 : 1'b0;
            stall_policy.next();
            if (VERBOSITY > 0) begin
                $display("stall_policy.next(): interval=%0d, duration=%0d", stall_policy.get_interval(), stall_policy.get_duration());
            end
            duration_ff <= 0;
            interval_ff <= 0;
        end
        else if (!stalls_enabled_ff) begin
            stalled_ff <= 1'b0;
            stall_policy.next();
            if (VERBOSITY > 0) begin
                $display("stall_policy.next(): interval=%0d, duration=%0d", stall_policy.get_interval(), stall_policy.get_duration());
            end
            duration_ff <= 0;
            interval_ff <= 0;
        end
        else begin
            if (stalled_ff) begin
                interval_ff <= 0;
                duration_ff <= duration_ff + 1;
                if ((duration_ff+1) >= stall_policy.get_duration()) begin
                    stall_policy.next();
                    if (VERBOSITY > 0) begin
                        $display("stall_policy.next(): interval=%0d, duration=%0d", stall_policy.get_interval(), stall_policy.get_duration());
                    end
                    stalled_ff <= 1'b0;
                end
            end
            else begin
                duration_ff <= 0;
                if ((interval_ff+1) >= stall_policy.get_interval()) begin
                    if (stall_policy.get_duration() != 0) begin
                        stalled_ff <= 1'b1;
                    end
                    else if (interval_ff >= stall_policy.get_interval()) begin
                        interval_ff <= 0;
                    end
                end
                else begin
                    interval_ff <= interval_ff + 1;
                end
            end
        end
    end

    assign stalled_out = stalled_ff;
    assign not_stalled_out = ~stalled_ff;

endmodule // KanagawaSimStaller
