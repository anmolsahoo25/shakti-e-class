// Bluespec wrapper, created by Import BVI Wizard
// Created on: Tue Mar 27 11:26:01 IST 2018
// Created by: aditya
// Bluespec version: 2017.07.A 2017-07-21 1da80f1

package bsvmkmult_gen_0;
interface Ifc_mult_gen#(numeric type xlen);
  (*always_ready , always_enabled*)
  method Action iA (Bit#(xlen) a);
  (*always_ready , always_enabled*)
  method Action iB (Bit#(xlen) b);
  (*always_enabled*)
  method Bit#(TSub#(TMul#(2,xlen),1)) oP ();
endinterface

import "BVI" mult_gen_0 =
module mkmult_gen_0  (Ifc_mult_gen#(xlen));

  parameter XLEN = valueOf(xlen);

  default_clock clk_CLK;
  default_reset rst;

  input_clock clk_CLK (CLK)  <- exposeCurrentClock;
  input_reset rst (/* empty */) clocked_by(clk_CLK)  <- exposeCurrentReset;


  method iA (A /*XLEN-1:0*/)
    enable((*inhigh*)iA_enable) clocked_by(clk_CLK) reset_by(rst);
  method iB (B /*XLEN-1:0*/)
    enable((*inhigh*)iB_enable) clocked_by(clk_CLK) reset_by(rst);
  method P /* 2*XLEN-1 : 0 */ oP ()
    clocked_by(clk_CLK) reset_by(rst);

  schedule iA C iA;
  schedule iA CF iB;
  schedule iA CF oP;
  schedule iB C iB;
  schedule iB CF oP;
  schedule oP CF oP;
endmodule
endpackage
