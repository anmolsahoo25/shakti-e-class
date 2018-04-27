/* 
Copyright (c) 2018, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and/or other materials provided 
  with the distribution.  
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Module name: Multiplier module.  
author name: Neel Gala
Email id: neelgala@gmail.com
*/
package muldiv;
  import multiplier::*;
//  import divider::*;
  import common_types::*;
  `include "common_params.bsv"

	interface Ifc_muldiv;
		method ActionValue#(Tuple2#(Bool, ALU_OUT)) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2, 
        Bit#(3) funct3, Bool word32);
		method ActionValue#(ALU_OUT) delayed_output;//returning the result
	endinterface:Ifc_muldiv

  (*synthesize*)
	module mkmuldiv(Ifc_muldiv);
    Ifc_multiplier#(XLEN) mult <- mkmultiplier;
//    Ifc_divider#(XLEN) divider <- mkdivider;
    Reg#(Bit#(TLog#(TAdd#(TMax#(`MULSTAGES, `DIVSTAGES), 1)))) rg_count <-mkReg(0);
    Reg#(Bool) mul_div <-mkReg(False); // False = Mul, True = Div.
    Reg#(Bool) rg_upperbits <- mkReg(False);
    Reg#(Bool) rg_complement <- mkReg(False);
    Reg#(Bool) rg_word32 <- mkReg(False);

    rule increment_counter(rg_count!=0);
      if((rg_count== fromInteger(`MULSTAGES) && !mul_div) || 
          (rg_count== fromInteger(`DIVSTAGES) && mul_div)) begin
        $display($time, "\tALU: got output from Mul/Div. mul_div: %b", mul_div);
        rg_count<= 0;
      end
      else begin
        $display($time, "\tALU: Waiting for mul/div to respond. Count: %d", rg_count);
        rg_count<= rg_count+ 1;
      end
    endrule

		method ActionValue#(Tuple2#(Bool, ALU_OUT)) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2,
        Bit#(3) funct3,  Bool word32) if(rg_count==0);
      Bool lv_upperbits = unpack(|funct3);

      Bit#(XLEN) op1= ((funct3[0]^funct3[1])==1 && operand1[valueOf(XLEN)-1]==1)? 
                                                                            (~operand1)+1:operand1;
	    Bit#(XLEN) op2= (funct3[1:0]==1 && operand2[valueOf(XLEN)-1]==1)? (~operand2)+1:operand2;
	    Bool lv_take_complement = False;
    	if(funct3[1:0]==1)
		    lv_take_complement=unpack(operand1[valueOf(XLEN)-1]^operand2[valueOf(XLEN)-1]);
    	else if(funct3[1:0]==2)
		    lv_take_complement=unpack(operand1[valueOf(XLEN)-1]);
	

      if(funct3[2]==0)begin // multiplication operation
        $display($time, "\tALU: Sending inputs to multiplier. A: %h B: %h funct3: %b", op1, op2,
        funct3);
        mult.iA(op1);
        mult.iB(op2);
      end
//      else begin
//        divider.is_axis_divisor_tdata(operand1);
//        divider.is_axis_dividend_tdata(operand2);
//      end
      if(`MULSTAGES!=0)begin
        rg_count<= rg_count+ 1;
        rg_upperbits<= lv_upperbits;
        rg_complement<= lv_take_complement;
        rg_word32<= word32;
      end
      return tuple2(`MULSTAGES==0, tuple3(REGULAR, lv_upperbits?truncateLSB(mult.oP): 
                                                                            truncate(mult.oP), ?));
    endmethod
		method ActionValue#(ALU_OUT) delayed_output if((rg_count== fromInteger(`MULSTAGES) && !mul_div) || 
                                          (rg_count== fromInteger(`DIVSTAGES) && mul_div));
      let prod = rg_complement?~mult.oP+ 1:mult.oP;
      Bit#(XLEN) product=rg_word32?signExtend(prod[31:0]):rg_upperbits?truncateLSB(prod): 
                                                                                    truncate(prod);
      $display($time, "\tALU: Output from Multiplier: %h updated: %h", mult.oP, product);                                          
      return tuple3(REGULAR, mul_div? ?/* div result*/: product, ?); 
    endmethod
	endmodule:mkmuldiv

  module mkTb(Empty);
    Ifc_muldiv muldiv <-mkmuldiv;
    Reg#(Bit#(32)) rg_count <- mkReg(0);
    rule increment_counter;
      rg_count<= rg_count+1;
      if(rg_count=='d100)
        $finish(0);
    endrule
    rule check(rg_count==20);
      let x<-muldiv.get_inputs(zeroExtend(rg_count), zeroExtend(rg_count), 0,  False);
      $display($time, "\t Giving inputs: %d", rg_count);
    endrule
    rule check_output;
      let {committype, out, paddr} <- muldiv.delayed_output;
      $display($time, "\tOutput: %d", out);
      $finish(0);
    endrule
  endmodule

endpackage:muldiv
