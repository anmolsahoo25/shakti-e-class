// Bluespec wrapper, created by Import BVI Wizard
// Created on: Sat Apr 14 08:55:11 IST 2018
// Created by: neel
// Bluespec version: 2017.03.beta1 2017-03-16 35049

package divider;
  interface Ifc_divider#(numeric type width);
  	(*always_enabled*)
  	method Action is_axis_divisor_tdata (Bit#(width) s_axis_divisor_tdata);
  	(*always_enabled*)
  	method Action is_axis_dividend_tdata (Bit#(width) s_axis_dividend_tdata);
  	method Bit#(TMul#(2, width)) om_axis_dout_tdata ();
  endinterface
  
  import "BVI" divider =
  module mkdivider  (Ifc_divider#(width));
  
  	default_clock clk_aclk;
  	default_reset rst;
  
  	input_clock clk_aclk (aclk)  <- exposeCurrentClock;
  	input_reset rst (/* empty */) clocked_by(clk_aclk)  <- exposeCurrentReset;
  
  
  	method is_axis_divisor_tdata (s_axis_divisor_tdata /*63:0*/)
  		 enable(s_axis_divisor_tvalid) clocked_by(clk_aclk) reset_by(rst);
  	method is_axis_dividend_tdata (s_axis_dividend_tdata /*63:0*/)
  		 enable(s_axis_dividend_tvalid) clocked_by(clk_aclk) reset_by(rst);
  	method m_axis_dout_tdata /* 127 : 0 */ om_axis_dout_tdata ()
  		 ready(m_axis_dout_tvalid) clocked_by(clk_aclk) reset_by(rst);
  
  	schedule is_axis_divisor_tdata C is_axis_divisor_tdata;
  	schedule is_axis_divisor_tdata CF is_axis_dividend_tdata;
  	schedule is_axis_divisor_tdata CF om_axis_dout_tdata;
  	schedule is_axis_dividend_tdata C is_axis_dividend_tdata;
  	schedule is_axis_dividend_tdata CF om_axis_dout_tdata;
  	schedule om_axis_dout_tdata CF om_axis_dout_tdata;
  endmodule:mkdivider

endpackage:divider
