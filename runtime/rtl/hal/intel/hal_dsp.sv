// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Modules that implements DSP intrinsics for Intel FPGAs
//
`default_nettype none

module KanagawaLutMul
# (
    parameter int unsigned  DW_X = 8,
    parameter int unsigned  DW_Y = 8,
    parameter bit           X_SIGNED = 1'b0,
    parameter bit           Y_SIGNED = 1'b0,
    parameter int unsigned  DW_Z = 16
)
(
    input wire  [DW_X-1:0]  x_in,
    input wire  [DW_Y-1:0]  y_in,
    output logic [DW_Z-1:0] z_out
);

    localparam bit SIGNED = X_SIGNED || Y_SIGNED;

    // LutMul has 0-cycle latency for now
    generate
        if (SIGNED) begin: gen_signed_mul
            assign z_out = $signed({X_SIGNED && x_in[DW_X-1], x_in}) * (* multstyle = "logic" *) $signed({Y_SIGNED && y_in[DW_Y-1], y_in});
        end
        else begin: gen_unsigned_mul
            assign z_out = x_in * (* multstyle = "logic" *) y_in;
        end
    endgenerate

endmodule

module _hardware_dsp__umul27
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [26:0] op_x_in,
    input wire [26:0] op_y_in,

    output logic [53:0] op_result_out,

    input wire op_valid_in
);
    generate
        if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            tennm_mac
            #(
                .operation_mode("m27x27"),
                .signed_max("false"),
                .signed_may("false"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .signed_mcx("false"),
                .signed_mcy("false"),
                .signed_mdx("false"),
                .signed_mdy("false"),
                .sub_clken("no_reg"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .scan_out_width(18),
                .ax_width(27),
                .ax_clken("0"),
                .bx_clken("no_reg"),
                .cx_clken("no_reg"),
                .dx_clken("no_reg"),
                .ay_scan_in_width(27),
                .ay_scan_in_clken("0"),
                .by_clken("no_reg"),
                .cy_clken("no_reg"),
                .dy_clken("no_reg"),
                .result_a_width(54),
                .output_clken("0"),
                .input_systolic_clken("no_reg"),
                .operand_source_may("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .az_clken("no_reg"),
                .bz_clken("no_reg"),
                .operand_source_max("input"),
                .operand_source_mbx("input"),
                .coef_sel_a_clken("no_reg"),
                .coef_sel_b_clken("no_reg"),
                .coef_a_0(0),
                .coef_a_1(0),
                .coef_a_2(0),
                .coef_a_3(0),
                .coef_a_4(0),
                .coef_a_5(0),
                .coef_a_6(0),
                .coef_a_7(0),
                .coef_b_0(0),
                .coef_b_1(0),
                .coef_b_2(0),
                .coef_b_3(0),
                .coef_b_4(0),
                .coef_b_5(0),
                .coef_b_6(0),
                .coef_b_7(0),
                .accumulate_clken("no_reg"),
                .load_const_clken("no_reg"),
                .negate_clken("no_reg"),
                .enable_double_accum("false"),
                .load_const_value(0),
                .use_chainadder("false"),
                .input_pipeline_clken("no_reg"),
                .second_pipeline_clken("no_reg"),
                .accum_pipeline_clken("no_reg"),
                .accum_2nd_pipeline_clken("no_reg"),
                .load_const_pipeline_clken("no_reg"),
                .load_const_2nd_pipeline_clken("no_reg"),
                .clear_type("none")
            )
            agx_mac
            (
                .ax(op_x_in),
                .ay(op_y_in),
                .clr({1'b0,1'b0}),
                .clk(clk),
                .ena(3'b1),
                .resulta(op_result_out),

                .accumulate(1'b0),
                .negate(1'b0),
                .sub(1'b0),

                .scanout(),
                .resultb(),
                .chainin(),
                .chainout(),
                .disable_scanin(),
                .disable_chainout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .scanin(),
                .loadconst(),
                .dy(),
                .dx(),
                .cy(),
                .cx(),
                .bx(),
                .by(),
                .bz(),
                .az(),
                .coefsela(),
                .coefselb()
            );
        end

        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            fourteennm_mac
            #(
                .ax_width(27),
                .ay_scan_in_width(27),
                .az_width(1),
                .bx_width(0),
                .by_width(0),
                .scan_out_width(1),
                .result_a_width(54),
                .result_b_width(0),
                .operation_mode("m27x27"),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .signed_max("false"),
                .signed_may("false"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .output_clock("0")
            )
            s10_mac
            (
                .ax(op_x_in),
                .ay(op_y_in),
                .clr(2'b00),
                .clk({clk, clk, clk}),
                .ena(3'b1),
                .resulta(op_result_out),

                .accumulate(1'b0),
                .chainin(64'b0),
                .loadconst(1'b0),
                .negate(1'b0),
                .sub(1'b0),
                .chainout(),
                .dftout(),
                .resultb(),
                .scanout(),

                .dfxmisrena(),
                .dfxlfsrena(),
                .scanin(),
                .coefsela(),
                .coefselb(),
                .bx(),
                .by(),
                .az(),
                .bz()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            twentynm_mac
            #(
                .ax_width(27),
                .ay_scan_in_width(27),
                .az_width(1),
                .bx_width(1),
                .by_width(1),
                .scan_out_width(1),
                .result_a_width(54),
                .result_b_width(1),
                .operation_mode("m27x27"),
                .mode_sub_location(0),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .signed_max("false"),
                .signed_may("false"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .output_clock("0")
            )
            a10_mac
            (
                .accumulate(1'b0),
                .aclr(2'b0),
                .ax(op_x_in),
                .ay(op_y_in),
                .chainin(64'b0),
                .clk({clk, clk, clk}),
                .ena(3'b1),
                .loadconst(1'b0),
                .negate(1'b0),
                .scanin(27'b0),
                .sub(1'b0),
                .az(),
                .bx(),
                .by(),
                .bz(),
                .chainout(),
                .dftout(),
                .resulta(op_result_out),
                .resultb(),
                .scanout(),
                .coefsela(),
                .coefselb()
            );
        end

        if (DEVICE_FAMILY == "StratixV") begin
            stratixv_mac
            #(
                .ax_width(27),
                .ay_scan_in_width(27),
                .az_width(1),
                .bx_width(1),
                .by_width(1),
                .scan_out_width(1),
                .result_a_width(54),
                .result_b_width(1),
                .operation_mode("m27x27"),
                .mode_sub_location(0),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .signed_max("false"),
                .signed_may("false"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .output_clock("0")
            )
            sv_mac
            (
                .accumulate(1'b0),
                .aclr(2'b0),
                .ax(op_x_in),
                .ay(op_y_in),
                .chainin(64'b0),
                .cin(1'b0),
                .clk({clk, clk, clk}),
                .complex(1'b0),
                .ena(3'b1),
                .loadconst(1'b0),
                .negate(1'b0),
                .scanin(27'b0),
                .sub(1'b0),
                .chainout(),
                .cout(),
                .dftout(),
                .resulta(op_result_out),
                .resultb(),
                .scanout()
            );
        end
    endgenerate

endmodule

module _hardware_dsp__imul27
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [26:0] op_x_in,
    input wire [26:0] op_y_in,
    input wire op_negate_in,

    output logic [53:0] op_result_out,
    input wire op_valid_in
);
    generate
        if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            tennm_mac
            #(
                .operation_mode("m27x27"),
                .signed_max("true"),
                .signed_may("true"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .signed_mcx("false"),
                .signed_mcy("false"),
                .signed_mdx("false"),
                .signed_mdy("false"),
                .sub_clken("no_reg"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .scan_out_width(18),
                .ax_width(27),
                .ax_clken("0"),
                .bx_clken("no_reg"),
                .cx_clken("no_reg"),
                .dx_clken("no_reg"),
                .ay_scan_in_width(27),
                .ay_scan_in_clken("0"),
                .by_clken("no_reg"),
                .cy_clken("no_reg"),
                .dy_clken("no_reg"),
                .result_a_width(54),
                .output_clken("0"),
                .input_systolic_clken("no_reg"),
                .operand_source_may("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .az_clken("no_reg"),
                .bz_clken("no_reg"),
                .operand_source_max("input"),
                .operand_source_mbx("input"),
                .coef_sel_a_clken("no_reg"),
                .coef_sel_b_clken("no_reg"),
                .coef_a_0(0),
                .coef_a_1(0),
                .coef_a_2(0),
                .coef_a_3(0),
                .coef_a_4(0),
                .coef_a_5(0),
                .coef_a_6(0),
                .coef_a_7(0),
                .coef_b_0(0),
                .coef_b_1(0),
                .coef_b_2(0),
                .coef_b_3(0),
                .coef_b_4(0),
                .coef_b_5(0),
                .coef_b_6(0),
                .coef_b_7(0),
                .accumulate_clken("no_reg"),
                .load_const_clken("no_reg"),
                .negate_clken("0"),
                .enable_double_accum("false"),
                .load_const_value(0),
                .use_chainadder("false"),
                .input_pipeline_clken("no_reg"),
                .second_pipeline_clken("no_reg"),
                .accum_pipeline_clken("no_reg"),
                .accum_2nd_pipeline_clken("no_reg"),
                .load_const_pipeline_clken("no_reg"),
                .load_const_2nd_pipeline_clken("no_reg"),
                .clear_type("none")
            )
            agx_mac
            (
                .ax (op_x_in),
                .ay (op_y_in),
                .negate(op_negate_in),
                .clr ({1'b0,1'b0}),
                .clk(clk),
                .ena (3'b1),
                .resulta (op_result_out),

                .accumulate(1'b0),
                .sub(1'b0),

                .scanout(),
                .resultb(),
                .chainin(),
                .chainout(),
                .disable_scanin(),
                .disable_chainout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .scanin(),
                .loadconst(),
                .dy(),
                .dx(),
                .cy(),
                .cx(),
                .bx(),
                .by(),
                .bz(),
                .az(),
                .coefsela(),
                .coefselb()
            );
        end
        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            fourteennm_mac
            #(
                .ax_width(27),
                .ay_scan_in_width(27),
                .az_width(1),
                .bx_width(0),
                .by_width(0),
                .scan_out_width(1),
                .result_a_width(54),
                .result_b_width(0),
                .operation_mode("m27x27"),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .signed_max("true"),
                .signed_may("true"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .negate_clock("0"),
                .output_clock("0")
            )
            s10_mac
            (
                .accumulate(1'b0),
                .clr(2'b0),
                .ax(op_x_in),
                .ay(op_y_in),
                .chainin(64'b0),
                .clk({clk, clk, clk}),
                .ena(3'b1),
                .loadconst(1'b0),
                .negate(op_negate_in),
                .sub(1'b0),
                .resulta(op_result_out),

                .chainout(),
                .dftout(),
                .resultb(),
                .scanout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .scanin(),
                .coefsela(),
                .coefselb(),
                .az(),
                .bx(),
                .by(),
                .bz()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            twentynm_mac
            #(
                .ax_width(27),
                .ay_scan_in_width(27),
                .az_width(1),
                .bx_width(1),
                .by_width(1),
                .scan_out_width(1),
                .result_a_width(54),
                .result_b_width(1),
                .operation_mode("m27x27"),
                .mode_sub_location(0),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .signed_max("true"),
                .signed_may("true"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .negate_clock("0"),
                .output_clock("0")
            )
            a10_mac
            (
                .accumulate(1'b0),
                .aclr(2'b0),
                .ax(op_x_in),
                .ay(op_y_in),
                .chainin(64'b0),
                .clk({clk, clk, clk}),
                .ena(3'b1),
                .loadconst(1'b0),
                .negate(op_negate_in),
                .scanin(27'b0),
                .sub(1'b0),

                .az(),
                .bx(),
                .by(),
                .bz(),

                .chainout(),
                .dftout(),
                .resulta(op_result_out),
                .resultb(),
                .scanout(),
                .coefsela(),
                .coefselb()
            );
        end

        if (DEVICE_FAMILY == "StratixV") begin
            stratixv_mac
            #(
                .ax_width(27),
                .ay_scan_in_width(27),
                .az_width(1),
                .bx_width(1),
                .by_width(1),
                .scan_out_width(1),
                .result_a_width(54),
                .result_b_width(1),
                .operation_mode("m27x27"),
                .mode_sub_location(0),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .signed_max("true"),
                .signed_may("true"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .negate_clock("0"),
                .output_clock("0")
            )
            sv_mac
            (
                .accumulate(1'b0),
                .aclr(2'b0),
                .ax(op_x_in),
                .ay(op_y_in),
                .chainin(64'b0),
                .cin(1'b0),
                .clk({clk, clk, clk}),
                .complex(1'b0),
                .ena(3'b1),
                .loadconst(1'b0),
                .negate(op_negate_in),
                .scanin(27'b0),
                .sub(1'b0),
                .chainout(),
                .cout(),
                .dftout(),
                .resulta(op_result_out),
                .resultb(),
                .scanout()
            );
        end
    endgenerate

endmodule

module _hardware_dsp__umul18x2
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [35:0] op_x_in,

    input wire [35:0] op_y_in,

    output logic [71:0] op_result_out,

    input wire op_valid_in
);
    logic [36:0] result0_37;
    logic [36:0] result1_37;

    assign op_result_out = {result1_37[35:0], result0_37[35:0]};

    generate
        if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            tennm_mac
            #(
                .operation_mode("m18x18_full"),
                .signed_max("false"),
                .signed_may("false"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .signed_mcx("false"),
                .signed_mcy("false"),
                .signed_mdx("false"),
                .signed_mdy("false"),
                .sub_clken("no_reg"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .scan_out_width(18),
                .ax_width(18),
                .ax_clken("0"),
                .bx_width(18),
                .bx_clken("0"),
                .cx_clken("no_reg"),
                .dx_clken("no_reg"),
                .ay_scan_in_width(18),
                .ay_scan_in_clken("0"),
                .by_width(18),
                .by_clken("0"),
                .cy_clken("no_reg"),
                .dy_clken("no_reg"),
                .result_a_width(37),
                .result_b_width(37),
                .output_clken("0"),
                .input_systolic_clken("no_reg"),
                .operand_source_may("input"),
                .operand_source_mby("input"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .az_clken("no_reg"),
                .bz_clken("no_reg"),
                .operand_source_max("input"),
                .operand_source_mbx("input"),
                .coef_sel_a_clken("no_reg"),
                .coef_sel_b_clken("no_reg"),
                .coef_a_0(0),
                .coef_a_1(0),
                .coef_a_2(0),
                .coef_a_3(0),
                .coef_a_4(0),
                .coef_a_5(0),
                .coef_a_6(0),
                .coef_a_7(0),
                .coef_b_0(0),
                .coef_b_1(0),
                .coef_b_2(0),
                .coef_b_3(0),
                .coef_b_4(0),
                .coef_b_5(0),
                .coef_b_6(0),
                .coef_b_7(0),
                .accumulate_clken("no_reg"),
                .load_const_clken("no_reg"),
                .negate_clken("no_reg"),
                .enable_double_accum("false"),
                .load_const_value(0),
                .use_chainadder("false"),
                .input_pipeline_clken("no_reg"),
                .second_pipeline_clken("no_reg"),
                .accum_pipeline_clken("no_reg"),
                .accum_2nd_pipeline_clken("no_reg"),
                .load_const_pipeline_clken("no_reg"),
                .load_const_2nd_pipeline_clken("no_reg"),
                .clear_type("none")
            )
            agx_mac
            (
                .ax (op_x_in[17:0]),
                .ay (op_y_in[17:0]),
                .bx (op_x_in[35:18]),
                .by (op_y_in[35:18]),
                .clr ({1'b0,1'b0}),
                .clk(clk),
                .ena (3'b1),
                .resulta (result0_37),
                .resultb (result1_37),

                .accumulate(1'b0),
                .negate(1'b0),
                .sub(1'b0),

                .scanout(),
                .chainin(),
                .chainout(),
                .disable_scanin(),
                .disable_chainout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .scanin(),
                .loadconst(),
                .dy(),
                .dx(),
                .cy(),
                .cx(),
                .bz(),
                .az(),
                .coefsela(),
                .coefselb()
            );
        end

        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
            end

            fourteennm_mac
            #(
                .ax_clock("0"),
				.ay_scan_in_clock("0"),
				.az_clock("none"),
				.output_clock("0"),
				.bx_clock("0"),
				.accumulate_clock("none"),
				.accum_pipeline_clock("none"),
				.bz_clock("none"),
				.by_clock("0"),
				.coef_sel_a_clock("none"),
				.coef_sel_b_clock("none"),
				.sub_clock("none"),
				.negate_clock("none"),
				.accum_2nd_pipeline_clock("none"),
				.load_const_clock("none"),
				.load_const_pipeline_clock("none"),
				.load_const_2nd_pipeline_clock("none"),
				.input_pipeline_clock("0"),
				.second_pipeline_clock("none"),
				.input_systolic_clock("none"),
				.preadder_subtract_a("false"),
				.preadder_subtract_b("false"),
				.delay_scan_out_ay("false"),
				.delay_scan_out_by("false"),
				.ay_use_scan_in("false"),
				.by_use_scan_in("false"),
				.operand_source_may("input"),
				.operand_source_mby("input"),
				.operand_source_max("input"),
				.operand_source_mbx("input"),
				.signed_max("false"),
				.signed_may("false"),
				.signed_mbx("false"),
				.signed_mby("false"),
				.operation_mode("m18x18_full"),
				.clear_type("none"),
				.ax_width(18),
				.bx_width(18),
				.ay_scan_in_width(18),
				.by_width(18),
				.result_a_width(37),
				.result_b_width(37),
				.load_const_value(0)
            )
            s10_mac
            (
                .ax(op_x_in[17:0]),
                .ay(op_y_in[17:0]),
                .bx(op_x_in[35:18]),
                .by(op_y_in[35:18]),
                .ena(3'b1),
                .clr(2'b00),
                .clk({clk, clk, clk}),
                .resulta(result0_37),
                .resultb(result1_37),

                .accumulate(1'b0),
                .chainin(64'b0),
                .loadconst(1'b0),
                .negate(1'b0),
                .sub(1'b0),
                .chainout(),
                .dftout(),
                .scanout(),

                .dfxmisrena(),
                .dfxlfsrena(),
                .scanin(),
                .coefsela(),
                .coefselb(),
                .az(),
                .bz()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 2) else $fatal("Expected LATENCY = 2");
            end

            twentynm_mac
            #(
                .ax_width(18),
                .ay_scan_in_width(18),
                .bx_width(18),
                .by_width(18),
                .operation_mode("m18x18_full"),
                .mode_sub_location(0),
                .operand_source_max("input"),
                .operand_source_may("input"),
                .operand_source_mbx("input"),
                .operand_source_mby("input"),
                .signed_max("false"),
                .signed_may("false"),
                .signed_mbx("false"),
                .signed_mby("false"),
                .preadder_subtract_a("false"),
                .preadder_subtract_b("false"),
                .ay_use_scan_in("false"),
                .by_use_scan_in("false"),
                .delay_scan_out_ay("false"),
                .delay_scan_out_by("false"),
                .use_chainadder("false"),
                .enable_double_accum("false"),
                .load_const_value(0),
                .ax_clock("0"),
                .ay_scan_in_clock("0"),
                .az_clock("none"),
                .bx_clock("0"),
                .by_clock("0"),
                .bz_clock("none"),
                .coef_sel_a_clock("none"),
                .coef_sel_b_clock("none"),
                .sub_clock("none"),
                .sub_pipeline_clock("none"),
                .negate_clock("none"),
                .negate_pipeline_clock("none"),
                .accumulate_clock("none"),
                .accum_pipeline_clock("none"),
                .load_const_clock("none"),
                .load_const_pipeline_clock("none"),
                .input_pipeline_clock("none"),
                .output_clock("0"),
                .scan_out_width(18),
                .result_a_width(37),
                .result_b_width(37)
            )
            a10_mac
            (
                .ax(op_x_in[17:0]),
                .ay(op_y_in[17:0]),
                .bx(op_x_in[35:18]),
                .by(op_y_in[35:18]),
                .resulta(result0_37),
                .resultb(result1_37),

                .clk({clk, clk, clk}),

                .az(),
                .bz(),

                .aclr(2'b0),
                .accumulate(1'b0),
                .chainin(64'b0),
                .ena(3'b1),
                .loadconst(1'b0),
                .negate(1'b0),
                .scanin(18'b0),
                .sub(1'b0),
                .chainout(),
                .dftout(),
                .scanout(),
                .coefsela(),
                .coefselb()
            );
        end
    endgenerate

endmodule

module _hardware_dsp__fmul32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    generate
         if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            tennm_fp_mac
            #(
                .operation_mode("fp32_mult"),
                .fp16_mode("flushed"),
                .fp16_input_width(16),
                .use_chainin("false"),
                .fp32_adder_subtract("false"),
                .fp16_adder_subtract("false"),
                .clear_type("none"),
                .accumulate_clken("no_reg"),
                .accum_adder_clken("no_reg"),
                .adder_input_clken("no_reg"),
                .adder_pl_clken("no_reg"),
                .fp16_mult_input_clken("no_reg"),
                .fp32_adder_a_clken("no_reg"),
                .fp32_adder_b_clken("no_reg"),
                .fp32_mult_a_clken("0"),
                .fp32_mult_b_clken("0"),
                .fp32_adder_a_chainin_pl_clken("no_reg"),
                .fp32_adder_a_chainin_2nd_pl_clken("no_reg"),
                .output_clken("0"),
                .accum_pipeline_clken("no_reg"),
                .mult_pipeline_clken("0"),
                .accum_2nd_pipeline_clken("no_reg"),
                .mult_2nd_pipeline_clken("0")
            ) sp_mult (
               .clr ({1'b0,1'b0}),
               .clk(clk),
               .ena ({1'b0, 1'b0, 1'b1}),
               .accumulate(1'b0),
               .fp32_mult_a (op_x_in),
               .fp32_mult_b (op_y_in),
               .fp32_result (op_result_out),

               .fp32_adder_a(),
               .fp32_adder_b(),
               .fp16_mult_top_a(),
               .fp16_mult_top_b(),
               .fp16_mult_bot_a(),
               .fp16_mult_bot_b(),
               .dfxmisrena(),
               .dfxlfsrena(),
               .fp32_chainin(),

               .fp32_chainout(),
               .fp32_adder_inexact(),
               .fp32_adder_invalid(),
               .fp32_adder_overflow(),
               .fp32_adder_underflow(),
               .fp32_mult_inexact(),
               .fp32_mult_invalid(),
               .fp32_mult_overflow(),
               .fp32_mult_underflow(),
               .fp16_adder_inexact(),
               .fp16_adder_invalid(),
               .fp16_adder_infinite(),
               .fp16_adder_zero(),
               .fp16_adder_overflow(),
               .fp16_adder_underflow(),
               .fp16_mult_top_inexact(),
               .fp16_mult_top_invalid(),
               .fp16_mult_top_infinite(),
               .fp16_mult_top_zero(),
               .fp16_mult_top_overflow(),
               .fp16_mult_top_underflow(),
               .fp16_mult_bot_inexact(),
               .fp16_mult_bot_invalid(),
               .fp16_mult_bot_infinite(),
               .fp16_mult_bot_zero(),
               .fp16_mult_bot_overflow(),
               .fp16_mult_bot_underflow()
            );
        end

        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            fourteennm_fp_mac
            #(
                .ax_clock("NONE"),
                .ay_clock("0"),
                .az_clock("0"),
                .accumulate_clock("NONE"),
                .ax_chainin_pl_clock("NONE"),
                .accum_pipeline_clock("NONE"),
                .mult_pipeline_clock("0"),
                .mult_2nd_pipeline_clock("0"),
                .output_clock("0"),
                .adder_input_clock("NONE"),
                .accum_adder_clock("NONE"),
                .use_chainin("false"),
                .operation_mode("sp_mult"),
                .adder_subtract("false")
            ) sp_mult (
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .clr({1'b0,1'b0}),
                .ay(op_x_in),
                .az(op_y_in),

                .chainin(32'b0),
                .resulta(op_result_out),

                .chainout(),
                .ax(),
                .dftout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .mult_underflow(),
                .mult_overflow(),
                .mult_invalid(),
                .mult_inexact(),
                .adder_underflow(),
                .adder_overflow(),
                .adder_invalid(),
                .adder_inexact(),
                .accumulate()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
            end

            twentynm_fp_mac
            #(
                .ax_clock("NONE"),
                .ay_clock("0"),
                .az_clock("0"),
                .output_clock("0"),
                .accumulate_clock("NONE"),
                .ax_chainin_pl_clock("NONE"),
                .accum_pipeline_clock("NONE"),
                .mult_pipeline_clock("0"),
                .adder_input_clock("0"),
                .accum_adder_clock("NONE"),
                .use_chainin("false"),
                .operation_mode("sp_mult"),
                .adder_subtract("false")
            ) sp_mult (
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .aclr({1'b0,1'b0}),
                .ay(op_x_in),
                .az(op_y_in),

                .chainin(32'b0),
                .resulta(op_result_out),

                .ax(),

                .chainout(),
                .dftout(),
                .chainout_invalid(),
                .chainout_inexact(),
                .chainout_underflow(),
                .chainout_overflow(),
                .invalid(),
                .inexact(),
                .underflow(),
                .overflow(),
                .accumulate(),
                .chainin_invalid(),
                .chainin_inexact(),
                .chainin_underflow(),
                .chainin_overflow()
            );
        end
    endgenerate

endmodule

module _hardware_dsp__fmac32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,
    input wire op_accumulate_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    generate
        if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            tennm_fp_mac
            #(
                .operation_mode("fp32_mult_acc"),
                .fp16_mode("flushed"),
                .fp16_input_width(16),
                .use_chainin("false"),
                .fp32_adder_subtract("false"),
                .fp16_adder_subtract("false"),
                .clear_type("none"),
                .accumulate_clken("0"),
                .accum_adder_clken("0"),
                .adder_input_clken("0"),
                .adder_pl_clken("no_reg"),
                .fp16_mult_input_clken("no_reg"),
                .fp32_adder_a_clken("no_reg"),
                .fp32_adder_b_clken("no_reg"),
                .fp32_mult_a_clken("0"),
                .fp32_mult_b_clken("0"),
                .fp32_adder_a_chainin_pl_clken("no_reg"),
                .fp32_adder_a_chainin_2nd_pl_clken("no_reg"),
                .output_clken("0"),
                .accum_pipeline_clken("0"),
                .mult_pipeline_clken("no_reg"),
                .accum_2nd_pipeline_clken("no_reg"),
                .mult_2nd_pipeline_clken("0")
            ) sp_mult_acc (
               .clr ({1'b0,1'b0}),
               .clk(clk),
               .ena ({1'b0, 1'b0, 1'b1}),
               .accumulate(op_valid_in ? op_accumulate_in : 1'b1),
               .fp32_mult_a (op_valid_in ? op_x_in : 32'b0),
               .fp32_mult_b (op_valid_in ? op_y_in : 32'b0),
               .fp32_result (op_result_out),

               .fp32_adder_a(),
               .fp32_adder_b(),
               .fp16_mult_top_a(),
               .fp16_mult_top_b(),
               .fp16_mult_bot_a(),
               .fp16_mult_bot_b(),
               .dfxmisrena(),
               .dfxlfsrena(),
               .fp32_chainin(),

               .fp32_chainout(),
               .fp32_adder_inexact(),
               .fp32_adder_invalid(),
               .fp32_adder_overflow(),
               .fp32_adder_underflow(),
               .fp32_mult_inexact(),
               .fp32_mult_invalid(),
               .fp32_mult_overflow(),
               .fp32_mult_underflow(),
               .fp16_adder_inexact(),
               .fp16_adder_invalid(),
               .fp16_adder_infinite(),
               .fp16_adder_zero(),
               .fp16_adder_overflow(),
               .fp16_adder_underflow(),
               .fp16_mult_top_inexact(),
               .fp16_mult_top_invalid(),
               .fp16_mult_top_infinite(),
               .fp16_mult_top_zero(),
               .fp16_mult_top_overflow(),
               .fp16_mult_top_underflow(),
               .fp16_mult_bot_inexact(),
               .fp16_mult_bot_invalid(),
               .fp16_mult_bot_infinite(),
               .fp16_mult_bot_zero(),
               .fp16_mult_bot_overflow(),
               .fp16_mult_bot_underflow()
            );
        end

        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            fourteennm_fp_mac  #(
                .ax_clock("NONE"),
                .ay_clock("0"),
                .az_clock("0"),
                .output_clock("0"),
                .accumulate_clock("0"),
                .ax_chainin_pl_clock("NONE"),
                .accum_pipeline_clock("0"),
                .mult_pipeline_clock("0"),
                .adder_input_clock("0"),
                .accum_adder_clock("0"),
                .use_chainin("false"),
                .operation_mode("sp_mult_acc"),
                .adder_subtract("false")
            ) sp_mult_acc (
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .clr({1'b0,1'b0}),
                .accumulate(op_valid_in ? op_accumulate_in : 1'b1),
                .ay(op_valid_in ? op_x_in : 32'b0),
                .az(op_valid_in ? op_y_in : 32'b0),

                .chainin(32'b0),
                .resulta(op_result_out),

                .chainout(),
                .ax(),
                .dftout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .mult_underflow(),
                .mult_overflow(),
                .mult_invalid(),
                .mult_inexact(),
                .adder_underflow(),
                .adder_overflow(),
                .adder_invalid(),
                .adder_inexact()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            twentynm_fp_mac  #(
                .ax_clock("NONE"),
                .ay_clock("0"),
                .az_clock("0"),
                .output_clock("0"),
                .accumulate_clock("0"),
                .ax_chainin_pl_clock("NONE"),
                .accum_pipeline_clock("0"),
                .mult_pipeline_clock("0"),
                .adder_input_clock("0"),
                .accum_adder_clock("0"),
                .use_chainin("false"),
                .operation_mode("sp_mult_acc"),
                .adder_subtract("false")
            ) sp_mult_acc (
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .aclr({1'b0,1'b0}),
                .accumulate(op_valid_in ? op_accumulate_in : 1'b1),
                .ay(op_valid_in ? op_x_in : 32'b0),
                .az(op_valid_in ? op_y_in : 32'b0),

                .chainin(32'b0),
                .resulta(op_result_out),

                .ax(),

                .chainout(),
                .dftout(),
                .chainout_invalid(),
                .chainout_inexact(),
                .chainout_underflow(),
                .chainout_overflow(),
                .invalid(),
                .inexact(),
                .underflow(),
                .overflow(),
                .chainin_invalid(),
                .chainin_inexact(),
                .chainin_underflow(),
                .chainin_overflow()
            );
        end
    endgenerate

endmodule

module _hardware_dsp__fadd32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    generate
        if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
            end

            tennm_fp_mac
            #(
                .operation_mode("fp32_add"),
                .fp16_mode("flushed"),
                .fp16_input_width(16),
                .use_chainin("false"),
                .fp32_adder_subtract("false"),
                .fp16_adder_subtract("false"),
                .clear_type("none"),
                .accumulate_clken("no_reg"),
                .accum_adder_clken("no_reg"),
                .adder_input_clken("0"),
                .adder_pl_clken("no_reg"),
                .fp16_mult_input_clken("no_reg"),
                .fp32_adder_a_clken("0"),
                .fp32_adder_b_clken("0"),
                .fp32_mult_a_clken("no_reg"),
                .fp32_mult_b_clken("no_reg"),
                .fp32_adder_a_chainin_pl_clken("no_reg"),
                .fp32_adder_a_chainin_2nd_pl_clken("no_reg"),
                .output_clken("0"),
                .accum_pipeline_clken("no_reg"),
                .mult_pipeline_clken("no_reg"),
                .accum_2nd_pipeline_clken("no_reg"),
                .mult_2nd_pipeline_clken("no_reg")
            ) sp_add (
               .clr ({1'b0,1'b0}),
               .clk(clk),
               .ena ({1'b0, 1'b0, 1'b1}),
               .accumulate(1'b0),
               .fp32_adder_a (op_x_in),
               .fp32_adder_b (op_y_in),
               .fp32_result (op_result_out),

               .fp32_mult_a(),
               .fp32_mult_b(),
               .fp16_mult_top_a(),
               .fp16_mult_top_b(),
               .fp16_mult_bot_a(),
               .fp16_mult_bot_b(),
               .dfxmisrena(),
               .dfxlfsrena(),
               .fp32_chainin(),

               .fp32_chainout(),
               .fp32_adder_inexact(),
               .fp32_adder_invalid(),
               .fp32_adder_overflow(),
               .fp32_adder_underflow(),
               .fp32_mult_inexact(),
               .fp32_mult_invalid(),
               .fp32_mult_overflow(),
               .fp32_mult_underflow(),
               .fp16_adder_inexact(),
               .fp16_adder_invalid(),
               .fp16_adder_infinite(),
               .fp16_adder_zero(),
               .fp16_adder_overflow(),
               .fp16_adder_underflow(),
               .fp16_mult_top_inexact(),
               .fp16_mult_top_invalid(),
               .fp16_mult_top_infinite(),
               .fp16_mult_top_zero(),
               .fp16_mult_top_overflow(),
               .fp16_mult_top_underflow(),
               .fp16_mult_bot_inexact(),
               .fp16_mult_bot_invalid(),
               .fp16_mult_bot_infinite(),
               .fp16_mult_bot_zero(),
               .fp16_mult_bot_overflow(),
               .fp16_mult_bot_underflow()
            );
        end

        if (DEVICE_FAMILY == "Stratix10NX") begin
            initial begin
                assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
            end

            logic [95:0] data_in;

            logic [37:0] result_l;

            assign data_in[31:0] = op_x_in;
            assign data_in[63:32] = op_y_in;
            assign data_in[95:64] = '0;

            assign op_result_out = result_l[31:0];

            fourteennm_dsp_prime #(
               .dsp_mode("scalar_fp32"),
               .dsp_sel_int4("select_int8"),
               .dsp_cascade("cascade_disabled"))
            fp32_dsp
            (
               // Outputs
               .cascade_weight_out  (),
               .cascade_data_out    (),
               .result_h            (),
               .result_l            (result_l),
               // Inputs
               .clk                 (clk),
               .clr                 ({1'b0, 1'b0}),
               .cascade_weight_in   (),
               .cascade_data_in     (96'b0),
               .data_in             (data_in),
               .shared_exponent     (8'b0),
               .ena                 (1'b1),
               .zero_en             (1'b0),
               .feed_sel            (2'b00),
               .load_bb_one         (1'b0),
               .load_bb_two         (1'b0),
               .acc_en              (1'b0),
               .load_buf_sel        (1'b0),
               .mode_switch         (1'b0)
           );
        end

        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
            end

            fourteennm_fp_mac  #(
                .ax_clock("0"),
                .ay_clock("0"),
                .az_clock("NONE"),
                .output_clock("0"),
                .accumulate_clock("NONE"),
                .ax_chainin_pl_clock("NONE"),
                .accum_pipeline_clock("NONE"),
                .mult_pipeline_clock("NONE"),
                .adder_input_clock("0"),
                .accum_adder_clock("NONE"),
                .use_chainin("false"),
                .operation_mode("sp_add"),
                .adder_subtract("false")
            ) sp_add (
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .clr({1'b0,1'b0}),
                .ax(op_x_in),
                .ay(op_y_in),

                .chainin(32'b0),
                .resulta(op_result_out),

                .az(),
                .accumulate(),
                .chainout(),
                .dftout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .mult_underflow(),
                .mult_overflow(),
                .mult_invalid(),
                .mult_inexact(),
                .adder_underflow(),
                .adder_overflow(),
                .adder_invalid(),
                .adder_inexact()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
            end

            twentynm_fp_mac  #(
                .ax_clock("0"),
                .ay_clock("0"),
                .az_clock("NONE"),
                .output_clock("0"),
                .accumulate_clock("NONE"),
                .ax_chainin_pl_clock("NONE"),
                .accum_pipeline_clock("NONE"),
                .mult_pipeline_clock("NONE"),
                .adder_input_clock("0"),
                .accum_adder_clock("NONE"),
                .use_chainin("false"),
                .operation_mode("sp_add"),
                .adder_subtract("false")
            ) sp_add (
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .aclr({1'b0,1'b0}),
                .ax(op_x_in),
                .ay(op_y_in),

                .chainin(32'b0),
                .resulta(op_result_out),

                .az(),

                .chainout(),
                .dftout(),
                .chainout_invalid(),
                .chainout_inexact(),
                .chainout_underflow(),
                .chainout_overflow(),
                .invalid(),
                .inexact(),
                .underflow(),
                .overflow(),
                .accumulate(),
                .chainin_invalid(),
                .chainin_inexact(),
                .chainin_underflow(),
                .chainin_overflow()
            );
        end

    endgenerate

endmodule

module _hardware_dsp__fsub32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    _hardware_dsp__fadd32
    #(
		.DEVICE_FAMILY(DEVICE_FAMILY),
        .LATENCY(LATENCY)
	) add
    (
        .clk(clk),
        .rst(rst),

        .op_x_in(op_x_in),
        .op_y_in(op_y_in ^ 32'h80000000), // flip the sign bit

        .op_result_out(op_result_out),
        .op_valid_in(op_valid_in)
    );

endmodule

module _hardware_dsp__fmad32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire [31:0] op_y_in,
    input wire [31:0] op_z_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    generate
        if (DEVICE_FAMILY == "Agilex") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            tennm_fp_mac
            #(
                .operation_mode("fp32_mult_add"),
                .fp16_mode("flushed"),
                .fp16_input_width(16),
                .use_chainin("false"),
                .fp32_adder_subtract("false"),
                .fp16_adder_subtract("false"),
                .clear_type("none"),
                .accumulate_clken("no_reg"),
                .accum_adder_clken("no_reg"),
                .adder_input_clken("0"),
                .adder_pl_clken("no_reg"),
                .fp16_mult_input_clken("no_reg"),
                .fp32_adder_a_clken("0"),
                .fp32_adder_b_clken("no_reg"),
                .fp32_mult_a_clken("0"),
                .fp32_mult_b_clken("0"),
                .fp32_adder_a_chainin_pl_clken("0"),
                .fp32_adder_a_chainin_2nd_pl_clken("no_reg"),
                .output_clken("0"),
                .accum_pipeline_clken("no_reg"),
                .mult_pipeline_clken("no_reg"),
                .accum_2nd_pipeline_clken("no_reg"),
                .mult_2nd_pipeline_clken("0")
            ) sp_mult_add (
               .clr ({1'b0,1'b0}),
               .clk(clk),
               .accumulate(1'b0),
               .ena ({1'b0, 1'b0, 1'b1}),
               .fp32_adder_a(op_x_in),
               .fp32_mult_a(op_y_in),
               .fp32_mult_b(op_z_in),
               .fp32_result (op_result_out),

               .fp32_adder_b(),
               .fp16_mult_top_a(),
               .fp16_mult_top_b(),
               .fp16_mult_bot_a(),
               .fp16_mult_bot_b(),
               .dfxmisrena(),
               .dfxlfsrena(),
               .fp32_chainin(),

               .fp32_chainout(),
               .fp32_adder_inexact(),
               .fp32_adder_invalid(),
               .fp32_adder_overflow(),
               .fp32_adder_underflow(),
               .fp32_mult_inexact(),
               .fp32_mult_invalid(),
               .fp32_mult_overflow(),
               .fp32_mult_underflow(),
               .fp16_adder_inexact(),
               .fp16_adder_invalid(),
               .fp16_adder_infinite(),
               .fp16_adder_zero(),
               .fp16_adder_overflow(),
               .fp16_adder_underflow(),
               .fp16_mult_top_inexact(),
               .fp16_mult_top_invalid(),
               .fp16_mult_top_infinite(),
               .fp16_mult_top_zero(),
               .fp16_mult_top_overflow(),
               .fp16_mult_top_underflow(),
               .fp16_mult_bot_inexact(),
               .fp16_mult_bot_invalid(),
               .fp16_mult_bot_infinite(),
               .fp16_mult_bot_zero(),
               .fp16_mult_bot_overflow(),
               .fp16_mult_bot_underflow()
            );
        end


        if (DEVICE_FAMILY == "Stratix10") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            fourteennm_fp_mac  #(
                .ax_clock("0"),
                .ay_clock("0"),
                .az_clock("0"),
                .output_clock("0"),
                .accumulate_clock("NONE"),
                .ax_chainin_pl_clock("0"),
                .accum_pipeline_clock("NONE"),
                .mult_pipeline_clock("0"),
                .adder_input_clock("0"),
                .accum_adder_clock("NONE"),
                .use_chainin("false"),
                .operation_mode("sp_mult_add"),
                .adder_subtract("false")
            ) sp_mult_add(
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .clr({1'b0,1'b0}),
                .ax(op_x_in),
                .ay(op_y_in),
                .az(op_z_in),

                .chainin(32'b0),
                .resulta(op_result_out),
                .chainout(),

                .accumulate(),
                .dftout(),
                .dfxmisrena(),
                .dfxlfsrena(),
                .mult_underflow(),
                .mult_overflow(),
                .mult_invalid(),
                .mult_inexact(),
                .adder_underflow(),
                .adder_overflow(),
                .adder_invalid(),
                .adder_inexact()
            );
        end

        if (DEVICE_FAMILY == "Arria10") begin
            initial begin
                assert(LATENCY == 4) else $fatal("Expected LATENCY = 4");
            end

            twentynm_fp_mac  #(
                .ax_clock("0"),
                .ay_clock("0"),
                .az_clock("0"),
                .output_clock("0"),
                .accumulate_clock("NONE"),
                .ax_chainin_pl_clock("0"),
                .accum_pipeline_clock("NONE"),
                .mult_pipeline_clock("0"),
                .adder_input_clock("0"),
                .accum_adder_clock("NONE"),
                .use_chainin("false"),
                .operation_mode("sp_mult_add"),
                .adder_subtract("false")
            ) sp_mult_add(
                .clk({1'b0,1'b0,clk}),
                .ena({1'b0,1'b0,1'b1}),
                .aclr({1'b0,1'b0}),
                .ax(op_x_in),
                .ay(op_y_in),
                .az(op_z_in),

                .chainin(32'b0),
                .resulta(op_result_out),

                .chainout(),
                .dftout(),
                .chainout_invalid(),
                .chainout_inexact(),
                .chainout_underflow(),
                .chainout_overflow(),
                .invalid(),
                .inexact(),
                .underflow(),
                .overflow(),
                .accumulate(),
                .chainin_invalid(),
                .chainin_inexact(),
                .chainin_underflow(),
                .chainin_overflow()
            );
        end
    endgenerate
endmodule

module _hardware_dsp__facc32
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [31:0] op_x_in,
    input wire op_accumulate_in,

    output logic [31:0] op_result_out,
    input wire op_valid_in
);
    initial begin
        assert(DEVICE_FAMILY == "Stratix10NX") else $fatal(0, "%m: facc32 should only be used on Stratix10NX");
        assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
    end

    logic [95:0] acc_dsp_input;
    assign acc_dsp_input[31:0] = op_valid_in ? op_x_in : 32'b0;
    assign acc_dsp_input[95:32] = '0;

    logic [37:0] acc_dsp_output_l;
    assign op_result_out = acc_dsp_output_l[31:0];

    fourteennm_dsp_prime
    #(
        .dsp_mode           ("scalar_fp32"),
        .dsp_sel_int4       ("select_int8"),
        .dsp_fp32_sub_en    ("float_sub_disabled"),
        .dsp_cascade        ("cascade_disabled")
    )
    fp32_acc (
        .ena                (1'b1),
        .clk                (clk),
        .data_in            (acc_dsp_input),
        .clr                ({rst, rst}),
        .result_h           (),
        .result_l           (acc_dsp_output_l),

        .load_buf_sel       (1'b0),
        .mode_switch        (1'b0),
        .load_bb_one        (1'b0),
        .load_bb_two        (1'b0),
        .feed_sel           (2'b0),
        .zero_en            (1'b0),
        .shared_exponent    (8'b0),
        .cascade_weight_in  (),
        .cascade_data_in    (96'b0),
        .acc_en             (op_valid_in ? op_accumulate_in : 1'b1),

        .cascade_weight_out (),
        .cascade_data_out   ()
    );
endmodule

module _hardware_dsp__addbf16x3
#(
    parameter DEVICE_FAMILY,
    parameter LATENCY
)
(
    input wire clk,
    input wire rst,

    input wire [47:0] op_x_in,

    input wire [47:0] op_y_in,

    output logic [47:0] op_result_out,

    input wire op_valid_in
);
    initial begin
        assert(DEVICE_FAMILY == "Stratix10NX") else $fatal(0, "%m: faaddbf16x3cc32 should only be used on Stratix10NX");
        assert(LATENCY == 3) else $fatal("Expected LATENCY = 3");
    end

    logic [95:0] data_in;
    assign data_in =
    {
        op_x_in[47:32], op_y_in[47:32],
        op_x_in[31:16], op_y_in[31:16],
        op_x_in[15:0], op_y_in[15:0]
    };

    logic [37:0] dsp_out_l;
    logic [36:0] dsp_out_h;

    fourteennm_dsp_prime
    #(
        .dsp_mode             ("scalar_bf16"),
        .dsp_sel_int4         ("select_int8"),
        .dsp_cascade          ("cascade_disabled")
     )
     addbf16x3
     (
       .cascade_weight_out   (),
       .cascade_data_out     (),
       .result_h             (dsp_out_h),
       .result_l             (dsp_out_l),
       // Inputs
       .clk                  (clk),
       .clr                  ({1'b0, 1'b0}),
       .cascade_weight_in    (),
       .cascade_data_in      (96'b0),
       .data_in              (data_in),
       .shared_exponent      (8'b0),
       .ena                  (1'b1),
       .zero_en              (1'b0),
       .feed_sel             (2'b00),
       .load_bb_one          (1'b0),
       .load_bb_two          (1'b0),
       .acc_en               (1'b0),
       .load_buf_sel         (1'b0),
       .mode_switch          (1'b0)
       );

    assign op_result_out  = {dsp_out_h[15:0], dsp_out_l[31:16], dsp_out_l[15:0]};
endmodule

// Use a DSP to implement 3 or 4 pipeline stages for 36 bits
module KanagawaHALDSPPipeline36
#(
    parameter DEVICE_FAMILY,
    parameter PIPELINE_STAGES
)
(
    input wire clk,
    input wire [35:0] data_in,
    output logic [35:0] data_out
);
    logic [35:0] data_no_x;
    logic [35:0] data_ff [0:PIPELINE_STAGES-1];
    logic [35:0] data_dsp;
    logic [35:0] data_random;

    initial
    begin
        assert(PIPELINE_STAGES == 3 || PIPELINE_STAGES == 4);
        assert(DEVICE_FAMILY == "Stratix10") else $fatal(0, "%m: _hardware_dsp__delay36 is only supported for Stratix10");
    end

    // In simulation, any byte with a bit set to 'x will produce output an
    // output byte that is all 'x. We handle that by replacing the 'x before
    // sending it to the DSP.

    always_comb
    begin
        data_no_x = data_in;
        // Replace 'x in original data with random data
        //synopsys translate_off
        assert(randomize(data_random));
        for (int i = 0; i < 36; i++)
        begin
            if (data_in[i] === 'x)
            begin
                data_no_x[i] = data_random[i];
            end
        end
        //synopsys translate_on
    end

    //synopsys translate_off
    always_ff @(posedge clk)
    begin
        // Save original data
        data_ff[PIPELINE_STAGES - 1] <= data_in;
        for (int i = 0; i < PIPELINE_STAGES - 1; i++)
        begin
            data_ff[i] <= data_ff[i + 1];
        end
    end
    //synopsys translate_on

    always_comb
    begin
        for (int i = 0; i < 36; i++)
        begin
            data_out[i] = data_dsp[i];

            // Restore 'x in output data
            //synopsys translate_off
            if (data_ff[0][i] === 'x)
            begin
                data_out[i] = 1'bx;
            end
            //synopsys translate_on
        end
    end

    fourteennm_mac
    #(
        .ax_clock("0"),
        .ay_scan_in_clock("0"),
        .az_clock("none"),
        .output_clock("0"),
        .bx_clock("0"),
        .accumulate_clock("none"),
        .accum_pipeline_clock("none"),
        .bz_clock("none"),
        .by_clock("0"),
        .coef_sel_a_clock("none"),
        .coef_sel_b_clock("none"),
        .sub_clock("none"),
        .negate_clock("none"),
        .accum_2nd_pipeline_clock("none"),
        .load_const_clock("none"),
        .load_const_pipeline_clock("none"),
        .load_const_2nd_pipeline_clock("none"),
        .input_pipeline_clock("0"),
        .second_pipeline_clock(PIPELINE_STAGES == 3 ? "none" : "0"),
        .input_systolic_clock("none"),
        .preadder_subtract_a("false"),
        .preadder_subtract_b("false"),
        .delay_scan_out_ay("false"),
        .delay_scan_out_by("false"),
        .ay_use_scan_in("false"),
        .by_use_scan_in("false"),
        .operand_source_may("input"),
        .operand_source_mby("input"),
        .operand_source_max("input"),
        .operand_source_mbx("input"),
        .signed_max("false"),
        .signed_may("false"),
        .signed_mbx("false"),
        .signed_mby("false"),
        .operation_mode("m18x18_full"),
        .clear_type("none"),
        .ax_width(18),
        .bx_width(18),
        .ay_scan_in_width(1),
        .by_width(1),
        .result_a_width(18),
        .result_b_width(18),
        .load_const_value(0)
    )
    s10_mac
    (
        .ax(data_no_x[35:18]),
        .ay(1'd1),
        .bx(data_no_x[17:0]),
        .by(1'd1),
        .ena(3'b1),
        .clr(2'b00),
        .clk({clk, clk, clk}),
        .resulta(data_dsp[35:18]),
        .resultb(data_dsp[17:0]),

        .accumulate(1'b0),
        .chainin(64'b0),
        .loadconst(1'b0),
        .negate(1'b0),
        .sub(1'b0),
        .chainout(/*unused*/),
        .dftout(/*unused*/),
        .scanout(/*unused*/),

        .dfxmisrena(/*unused*/),
        .dfxlfsrena(/*unused*/),
        .scanin(/*unused*/),
        .coefsela(/*unused*/),
        .coefselb(/*unused*/),
        .az(/*unused*/),
        .bz(/*unused*/)
    );
endmodule

// Use DSPs to implement pipeline registers
module KanagawaHALDSPPipeline
#(
    parameter DEVICE_FAMILY,
    parameter WIDTH,
    parameter PIPELINE_STAGES
)
(
    input wire clk,

    input wire [WIDTH-1:0] data_in,
    output logic [WIDTH-1:0] data_out
);

    initial
    begin
        assert(DEVICE_FAMILY == "Stratix10") else $fatal(0, "%m: _hardware_dsp__pipeline is only supported for Stratix10");
    end

    // Pipeline stages handled by one DSP
    localparam DSP_DEPTH = 4;
    // Depth in number of DSPs
    localparam NUM_DSPS_DEPTH = PIPELINE_STAGES / 4;
    localparam NUM_EXTRA_PIPELINE_STAGES = PIPELINE_STAGES % 4;

    // Width of one DSP (_hardware_dsp__pipeline36)
    localparam DSP_WIDTH = 36;
    // Width in number of DSPs
    localparam NUM_DSPS_WIDTH = (WIDTH / DSP_WIDTH) + (WIDTH % DSP_WIDTH == 0 ? 0 : 1);
    localparam ROUNDUP_WIDTH = DSP_WIDTH * NUM_DSPS_WIDTH;

    logic [ROUNDUP_WIDTH-1:0] input_expanded;
    logic [ROUNDUP_WIDTH-1:0] output_expanded;

    genvar i, j;
    generate
        // Add one pipeline stage at input if 1 or 2 extra pipeline stages are needed
        if (NUM_EXTRA_PIPELINE_STAGES == 1 || NUM_EXTRA_PIPELINE_STAGES == 2)
        begin
            always_ff @(posedge clk)
            begin
                input_expanded[WIDTH - 1 : 0] <= data_in;
            end
        end
        else
        begin
            assign input_expanded[WIDTH - 1 : 0] = data_in;
        end

        // Add one pipeline stage at output if 2 extra pipeline stages are needed
        if (NUM_EXTRA_PIPELINE_STAGES == 2)
        begin
            always_ff @(posedge clk)
            begin
                data_out <= output_expanded[WIDTH - 1 : 0];
            end
        end
        else
        begin
            assign data_out = output_expanded[WIDTH - 1 : 0];
        end

        for (i = 0; i < NUM_DSPS_WIDTH; i++)
        begin : generate_dsp_width
            logic [DSP_WIDTH-1:0] dsp_intermediate [0:NUM_DSPS_DEPTH];
            assign dsp_intermediate[0] = input_expanded[i * DSP_WIDTH +: DSP_WIDTH];

            for (j = 0; j < NUM_DSPS_DEPTH; j++)
            begin : generate_dsp_depth
                KanagawaHALDSPPipeline36
                #(
                    .DEVICE_FAMILY   (DEVICE_FAMILY),
                    .PIPELINE_STAGES (4)
                )
                dsp4
                (
                    .clk      (clk),
                    .data_in  (dsp_intermediate[j]),
                    .data_out (dsp_intermediate[j + 1])
                );
            end

            // Use one extra DSP with 3 pipeline stages
            if (NUM_EXTRA_PIPELINE_STAGES == 3)
            begin
                KanagawaHALDSPPipeline36
                #(
                    .DEVICE_FAMILY   (DEVICE_FAMILY),
                    .PIPELINE_STAGES (3)
                )
                dsp3
                (
                    .clk      (clk),
                    .data_in  (dsp_intermediate[NUM_DSPS_DEPTH]),
                    .data_out (output_expanded[i * DSP_WIDTH +: DSP_WIDTH])
                );
            end
            else
            begin
                assign output_expanded[i * DSP_WIDTH +: DSP_WIDTH] = dsp_intermediate[NUM_DSPS_DEPTH];
            end
        end
    endgenerate

endmodule
