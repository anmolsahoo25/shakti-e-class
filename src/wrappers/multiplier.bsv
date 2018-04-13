// Bluespec wrapper, created by Import BVI Wizard
// Created on: Thu Apr 12 09:13:36 IST 2018
// Created by: neel
// Bluespec version: 2017.07.A 2017-07-21 1da80f1

package multiplier;
  interface Ifc_multiplier#(numeric type width);
  	(*always_ready , always_enabled*)
  	method Action iA (Bit#(width) a);
  	(*always_ready , always_enabled*)
  	method Action iB (Bit#(width) b);
  	(*always_enabled*)
  	method Bit#(TMul#(width, 2)) oP ();
  endinterface:Ifc_multiplier
  
  import "BVI" multiplier =
  module mkmultiplier  (Ifc_multiplier#(width));
  
  	default_clock clk_CLK;
  	default_reset rst;
  
  	input_clock clk_CLK (CLK)  <- exposeCurrentClock;
  	input_reset rst (/* empty */) clocked_by(clk_CLK)  <- exposeCurrentReset;
  
  
  	method iA (A /*31:0*/)
  		 enable((*inhigh*)iA_enable) clocked_by(clk_CLK) reset_by(rst);
  	method iB (B /*31:0*/)
  		 enable((*inhigh*)iB_enable) clocked_by(clk_CLK) reset_by(rst);
  	method P /* 32 : 0 */ oP ()
  		 clocked_by(clk_CLK) reset_by(rst);
  
  	schedule iA C iA;
  	schedule iA CF iB;
  	schedule iA CF oP;
  	schedule iB C iB;
  	schedule iB CF oP;
  	schedule oP CF oP;
  endmodule: mkmultiplier
endpackage: multiplier 
  
