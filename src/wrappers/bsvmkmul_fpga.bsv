// Bluespec wrapper, created by Import BVI Wizard
// Created on: Thu Apr 12 09:13:36 IST 2018
// Created by: neel
// Bluespec version: 2017.07.A 2017-07-21 1da80f1


interface Ifc_mul_fpga;
	(*always_ready , always_enabled*)
	method Action iA (Bit#(32) a);
	(*always_ready , always_enabled*)
	method Action iB (Bit#(32) b);
	(*always_enabled*)
	method Bit#(64) oP ();
endinterface

import "BVI" mul_fpga =
module mkmul_fpga  (Ifc_mul_fpga);

	default_clock clk_CLK;
	default_reset rst;

	input_clock clk_CLK (CLK)  <- exposeCurrentClock;
	input_reset rst (/* empty */) clocked_by(clk_CLK)  <- exposeCurrentReset;


	method iA (A /*31:0*/)
		 enable((*inhigh*)iA_enable) clocked_by(clk_CLK) reset_by(rst);
	method iB (B /*31:0*/)
		 enable((*inhigh*)iB_enable) clocked_by(clk_CLK) reset_by(rst);
	method P /* 63 : 0 */ oP ()
		 clocked_by(clk_CLK) reset_by(rst);

	schedule iA C iA;
	schedule iA CF iB;
	schedule iA CF oP;
	schedule iB C iB;
	schedule iB CF oP;
	schedule oP CF oP;
endmodule


