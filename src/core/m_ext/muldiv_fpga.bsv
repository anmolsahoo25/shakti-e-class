/* 
Copyright (c) 2018, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
 * Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and / or other materials provided 
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

Module name : Multiplier module.  
author name : Neel Gala
Email id : neelgala@gmail.com
 */
package muldiv_fpga;
  `ifdef simulate
    import muldiv_fpga_sim::*;
  `else
  import multiplier::*;
  `endif
  import restoring_div::*;
  import common_types::*;
  `include "common_params.bsv"
  `include "Logger.bsv"

  interface Ifc_muldiv;
    method ActionValue#(ALU_OUT) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2, 
        Bit#(3) funct3 `ifdef RV64, Bool word32 `endif );
    method ActionValue#(ALU_OUT) delayed_output;//returning the result
  endinterface : Ifc_muldiv

  (*synthesize*)
  (*preempts="get_inputs, increment_counter"*)
  module mkmuldiv(Ifc_muldiv);

    String muldiv = "" ;

    Ifc_multiplier#(XLEN) mult <- mkmultiplier;
    Ifc_restoring_div divider <- mkrestoring_div();
    Reg#(Bit#(TLog#(TAdd#(TMax#(`MULSTAGES, `DIVSTAGES), 1)))) rg_count <- mkReg(0);
    Reg#(Bool) mul_div <- mkReg(False); // False = Mul, True = Div.
    Reg#(Bool) rg_upperbits <- mkReg(False);
    Reg#(Bool) rg_complement <- mkReg(False);
    `ifdef RV64 Reg#(Bool) rg_word32 <- mkReg(False); `endif
    Reg#(Bit#(1)) rg_sign_op1 <- mkReg(0);
    
    
    rule increment_counter(rg_count != 0);
      if((rg_count == fromInteger(`MULSTAGES) && !mul_div) || 
          (rg_count == fromInteger(`DIVSTAGES) + 1 && mul_div)) begin
        `logLevel( muldiv, 0, $format("MULDIV : got output from Mul/Div. mul_div: %b", mul_div))
        rg_count <= 0;
      end
      else begin
        `logLevel( muldiv, 0, $format("MULDIV : Waiting for mul/div to respond. Count: %d", 
                                      rg_count))
        rg_count <= rg_count + 1;
      end
    endrule

    method ActionValue#(ALU_OUT) get_inputs(Bit#(XLEN) operand1, Bit#(XLEN) operand2,
        Bit#(3) funct3 `ifdef RV64, Bool word32 `endif );
      // logic to choose the upper bits
      // in case of division,  this variable is set is the operation is a remainder operation
      mul_div <= unpack(funct3[2]);
      `ifdef RV64
        if(word32)begin
          operand1 = funct3[0] == 0? signExtend(operand1[31 : 0]) : zeroExtend(operand1[31 : 0]);
          operand2 = funct3[0] == 0? signExtend(operand2[31 : 0]) : zeroExtend(operand2[31 : 0]);
        end
      `endif
      Bool lv_upperbits = funct3[2] == 0?unpack(|funct3[1 : 0]) : unpack(funct3[1]);

      Bool invert_op1 = False;
      Bool invert_op2 = False;
      // in multiplication operations
      if(funct3[2] == 0 && (funct3[0]^funct3[1]) == 1 && operand1[valueOf(XLEN) - 1] == 1)
        invert_op1 = True;
      else if(funct3[2] == 1 && funct3[0] == 0 && operand1[valueOf(XLEN) - 1] == 1) // div op
        invert_op1 = True;
      
      if(funct3[2] == 0 && funct3[1 : 0] == 1 && operand2[valueOf(XLEN) - 1] == 1)// mult op
          invert_op2 = True;
      else if(funct3[2] == 1 && funct3[0] == 0 && operand2[valueOf(XLEN) - 1] == 1)// div op.
        invert_op2 = True;

      Bit#(XLEN) t1 = signExtend(pack(invert_op1));
      Bit#(XLEN) t2 = signExtend(pack(invert_op2));
      Bit#(XLEN) op1 = (t1^operand1) + zeroExtend(pack(invert_op1));
      Bit#(XLEN) op2 = (t2^operand2) + zeroExtend(pack(invert_op2));

      Bool lv_take_complement = False;
      if(funct3 == 1 || funct3 == 4) // in case of MULH or DIV
        lv_take_complement = unpack(operand1[valueOf(XLEN) - 1]^operand2[valueOf(XLEN) - 1]);
      else if(funct3 == 2)
        lv_take_complement = unpack(operand1[valueOf(XLEN) - 1]);
      else if(funct3 == 6)
        lv_take_complement = True;
      rg_sign_op1 <= operand1[valueOf(XLEN) - 1];	

      Bit#(XLEN) default_out = '1;
      Bool result_avail = False;
      if(funct3[2] == 0)begin // multiplication operation
          `logLevel( muldiv, 0, $format("MULDIV : Inps to Mul. A:%h B:%h f3: %b", op1, op2, funct3))
        mult.iA(op1);
        mult.iB(op2);
        if(`MULSTAGES == 0)begin
          result_avail = True;
          if(lv_upperbits)
            default_out = truncateLSB(mult.oP);
          else
            default_out = truncate(mult.oP);
        end
      end
      else begin
        if(|operand2 == 0)begin
          result_avail = True;
          if(funct3[1] == 1)
            default_out = operand1;
        end
        else
          divider.get_inputs(op1, op2, unpack(funct3[1])); // send inputs to the divider
      end
      `ifdef RV64
        if(word32)
          default_out = signExtend(default_out[31 : 0]);
      `endif
      if((funct3[2] == 0 && `MULSTAGES != 0) || (funct3[2] == 1 && `DIVSTAGES != 0) 
            && !result_avail)begin
        rg_count <= 1;
        rg_upperbits <= lv_upperbits;
        rg_complement <= lv_take_complement;
        `ifdef RV64 rg_word32 <= word32; `endif
      end
      return ALU_OUT{done : result_avail, cmtype : REGULAR, aluresult : zeroExtend(default_out), 
                      effective_addr:?, cause:?, redirect : False
                    `ifdef bpu, branch_taken: ?, redirect_pc: ? `endif };
    endmethod
    method ActionValue#(ALU_OUT) delayed_output if((rg_count == fromInteger(`MULSTAGES) && !mul_div)
                                        || (rg_count == (fromInteger(`DIVSTAGES) + 1) && mul_div));

      Bit#(TMul#(2, XLEN)) reslt = mul_div ? zeroExtend(divider.quo_rem) : mult.oP;
      if( (mul_div && rg_upperbits && rg_complement && reslt[valueOf(XLEN) - 1] != rg_sign_op1) || 
                      (mul_div && rg_complement && !rg_upperbits) ||(!mul_div && rg_complement))
        reslt=~reslt + 1;
      Bit#(XLEN) product = `ifdef RV64 rg_word32 ? signExtend(reslt[31 : 0]) : `endif 
          (!mul_div && rg_upperbits) ? truncateLSB(reslt) : truncate(reslt);
      `logLevel( muldiv, 0, $format("MULDIV: Responding with DelayedOut: %h", product))
      return ALU_OUT{done : True, cmtype : REGULAR, aluresult : zeroExtend(product), 
                    effective_addr:?, cause:?, redirect : False
                    `ifdef bpu, branch_taken: ?, redirect_pc: ? `endif };
    endmethod
  endmodule : mkmuldiv

endpackage : muldiv_fpga
