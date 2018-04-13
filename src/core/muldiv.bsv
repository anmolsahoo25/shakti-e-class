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
  import common_types::*;
  `include "common_params.bsv"

	interface Ifc_muldiv;
		method ActionValue#(Tuple2#(Bool, ALU_OUT)) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2, 
        Bit#(3) funct3, Bool word32);
		method ALU_OUT delayed_output;//returning the result
	endinterface:Ifc_muldiv

  (*synthesize*)
	module mkmuldiv(Ifc_muldiv);
    Ifc_multiplier#(XLEN) mult <- mkmultiplier;
    Reg#(Bit#(TLog#(TAdd#(`MULSTAGES, 1)))) rg_count <-mkReg(0);

    rule increment_counter(rg_count!=0);
      if(rg_count== fromInteger(`MULSTAGES))
        rg_count<= 0;
      else
        rg_count<= rg_count+ 1;
    endrule

		method ActionValue#(Tuple2#(Bool, ALU_OUT)) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2,
        Bit#(3) funct3,  Bool word32) if(rg_count==0);
      mult.iA(operand1);
      mult.iB(operand2);
      if(`MULSTAGES!=0)
        rg_count<= rg_count+ 1;
      return tuple2(`MULSTAGES==0, tuple3(REGULAR, truncate(mult.oP), ?));
    endmethod
		method ALU_OUT delayed_output if(rg_count== fromInteger(`MULSTAGES));
      return tuple3(REGULAR, truncate(mult.oP), ?);
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
      let {committype, out, paddr} = muldiv.delayed_output;
      $display($time, "\tOutput: %d", out);
      $finish(0);
    endrule
  endmodule

endpackage:muldiv
