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
 */
package muldiv_asic_32bit;
  /*====== Package import ==== */
  import FIFOF::*;
  import SpecialFIFOs::*;
  import UniqueWrappers::*;
  /*==== Project Import=== */
  import common_types::*;
  `include "common_params.bsv"
  `include "Logger.bsv"
  /*====================== */

  `define UnrollMul 4 // this means the number of bits being analysed simultaneously
  `define UnrollDiv 1

  interface Ifc_muldiv;
    method ActionValue#(ALU_OUT) get_inputs(Bit#(XLEN) in1, Bit#(XLEN) in2, Bit#(3) funct3 );
    method ActionValue#(ALU_OUT) delayed_output;//returning the result
    method Action flush;
  endinterface
 
  function Bit#(XLEN) single_mult ( Bit#(XLEN) in1, Bit#(XLEN) in2,
                                              Bit#(3) funct3 `ifdef RV64, Bool word_flag `endif );
    Bool lv_take_complement = False;
    if(funct3 == 1 ) // in case of MULH or DIV
      lv_take_complement = unpack(in1[valueOf(XLEN) - 1]^in2[valueOf(XLEN) - 1]);
    else if(funct3 == 2)
      lv_take_complement = unpack(in1[valueOf(XLEN) - 1]);

    Bool invert_op1 = False;
    Bool invert_op2 = False;
    Bool lv_upperbits = funct3[2] == 0?unpack(|funct3[1 : 0]) : unpack(funct3[1]);
    if(funct3[2] == 0 && (funct3[0]^funct3[1]) == 1 && in1[valueOf(XLEN) - 1] == 1)
      invert_op1 = True;
    if(funct3[2] == 0 && funct3[1 : 0] == 1 && in2[valueOf(XLEN) - 1] == 1)
      invert_op2 = True;

    Bit#(XLEN) t1 = signExtend(pack(invert_op1));
    Bit#(XLEN) t2 = signExtend(pack(invert_op2));
    Bit#(XLEN) op1 = (t1^in1) + zeroExtend(pack(invert_op1));
    Bit#(XLEN) op2 = (t2^in2) + zeroExtend(pack(invert_op2));

    Bit#(TMul#(2, XLEN)) out = zeroExtend(op1) * zeroExtend(op2); 

    if(lv_take_complement)
      out=~out + 1;

    Bit#(XLEN) default_out;
    if(lv_upperbits)
      default_out = truncateLSB(out);
    else
      default_out = truncate(out);

    return default_out;
  endfunction

  function Bit#(41) func_mult(Bit#(9) op1, Bit#(33) op2);
    Bit#(41) lv_result=  signExtend(op1) * signExtend(op2);
    return lv_result;
  endfunction

  function Bool is_op_zero(Bit#(24) op, Bit#(3) count);
    Bool acc_7to0_is_zero  = op[7 : 0] == 0;
    Bool acc_15to8_is_zero = op[15 : 8] == 0;
    Bool acc_23to16_is_zero = op[23 : 16] == 0;

    Bool acc_15to0_is_zero = acc_15to8_is_zero  && acc_7to0_is_zero;

    Bool earlyout = False;
        if((count == 3) || (count == 2)) begin
      if(acc_23to16_is_zero && acc_15to0_is_zero)
        earlyout = True;
        end
    else begin 
      if(acc_15to0_is_zero)
        earlyout = True;
    end
    return earlyout;
  endfunction

  (*synthesize*)
  (*descending_urgency = "get_inputs, perform_n_restoring_steps"*)
  module mkmuldiv(Ifc_muldiv);

    String muldiv = "";

    let output_unavail = ALU_OUT{done : False, cmtype : REGULAR, aluresult : ?, effective_addr : ?,
              cause : ?, redirect : False `ifdef bpu, branch_taken : ?, 
              redirect_pc : ? `endif };

    let xlen = valueOf(XLEN);
    Wrapper2#(Bit#(41), Bit#(41), Bit#(41))   wrapper_add_1     <- mkUniqueWrapper2( \+ );
    Wrapper2#(Bit#(9), Bit#(33), Bit#(41))   wrapper_mul_1      <- mkUniqueWrapper2( func_mult );
    Wrapper2#(Bit#(24), Bit#(3), Bool)   wrapper_is_op_zero <- mkUniqueWrapper2( is_op_zero );
	
    Reg#(Bit#(33)) multiplicand_divisor <- mkReg(0);				// operand2
    Reg#(Bit#(73)) accumulator <- mkReg(0);  // holds the accumulated results over the iterations
    FIFOF#(Bit#(32)) ff_muldiv_result <- mkLFIFOF();					// to hold the final result
      FIFOF#(Tuple4#(Bit#(XLEN), Bit#(XLEN), Bit#(2),  Bit#(1))) ff_input <- mkLFIFOF();			
    Reg#(Bit#(3)) rg_count[2] <- mkCReg(2, 4);
    Reg#(Bool) rg_signed <- mkReg(False);
    Reg#(Bool) upper_bits <- mkReg(False);
    Reg#(Bit#(1)) temp_multiplier_sign <- mkReg(0);
    Reg#(Bit#(1)) rg_result_sign <- mkReg(0);
    Reg#(Bool) rg_is_mul <- mkReg(False);

    //Only DIV
    Reg#(Bit#(6)) rg_state_counter[2] <- mkCReg(2, 0);										// to count the number of iterations
    Reg#(Bit#(2)) rg_funct3 <- mkReg(0);

    rule unroll_multiplication(rg_is_mul && rg_count[1] != 4 && `MULSTAGES != 0);

      //Bit#(137) x = partial_prod_generator(multiplier_sign, multiplicand, accumulator[1]);
      Bit#(41) product <- wrapper_mul_1.func({temp_multiplier_sign, accumulator[7 : 0]}, 
                                            multiplicand_divisor);
      Bit#(41) new_accum <- wrapper_add_1.func(accumulator[72 : 32], product);
      Bit#(73) x = {new_accum, accumulator[31 : 0]};
      Int#(73) y = unpack(x);
      y = y>>8;
      x = pack(y);
      Bool earlyout = False;
      if(rg_count[1] == 3)
        earlyout <- wrapper_is_op_zero.func(accumulator[31 : 8], rg_count[1]);
      else
        earlyout <- wrapper_is_op_zero.func(accumulator[23 : 0], rg_count[1]);
      if(rg_count[1] == 0 || earlyout)begin
        y = unpack(x);
        x = pack(y>>({2'b0, rg_count[1]}*8));
        if(upper_bits)
          ff_muldiv_result.enq(x[2 * xlen - 1:xlen]);
        else
          ff_muldiv_result.enq(x[xlen - 1:0]);
        rg_count[1] <= 4;
      end
      else begin
        rg_count[1] <= rg_count[1] - 1;
        accumulator <= x;
      end
      if(rg_count[1] == 1 && rg_signed)
        temp_multiplier_sign <= rg_result_sign;
    endrule

    rule perform_n_restoring_steps(!rg_is_mul && rg_count[1] != 'd4);
      Bit#(XLEN) divisor = multiplicand_divisor[xlen - 1:0];
      Bit#(TAdd#(1, TMul#(2, XLEN))) remainder = truncate(accumulator);
      Bit#(TAdd#(1, XLEN)) sub;
      for (Integer i = 0;i<`UnrollDiv;i = i+1)begin
        remainder = remainder<<1;
        Bit#(41) lv_add_op1= {8'd0, remainder[2 * xlen : xlen]};
        Bit#(41) lv_add_op2 = signExtend(~divisor + 1); 
        let lv_added_inter_res <- wrapper_add_1.func(lv_add_op1, lv_add_op2);
        sub = truncate(lv_added_inter_res);
        if(remainder[2 * xlen - 1:xlen] >= divisor)begin	// if subtraction is positive
          remainder[0] = 1;
          remainder[2 * xlen : xlen] = sub;					// restore 
        end
      end
      //Bit#(TAdd#(1, xlen)) lv_to_add = signExtend(~multiplicand_divisor[63 : 0] + 1);
      sub = accumulator[64 : 32] + signExtend(~multiplicand_divisor[31 : 0] + 1);
      if((rg_state_counter[1] == (32 / `UnrollDiv)))begin // end of computation;
        rg_state_counter[1] <= 0;
        rg_count[1] <= 'd4;
        if(rg_funct3[1] == 1) // REM / REMU
            remainder = signExtend(remainder[63 : 32]);
        else // DIV / DIVU
            remainder = signExtend(remainder[31 : 0]);

        if(rg_funct3[1] == 0 && rg_signed) begin// DIVU
          remainder=~remainder + 1;
        end
        else if(rg_funct3[1 : 0] == 'b10 && remainder[xlen - 1] != rg_result_sign) begin// REM[U]
          remainder=~remainder + 1;
        end
          ff_muldiv_result.enq(remainder[xlen - 1:0]);	    
      end
      else begin
        accumulator[64 : 0] <= remainder;
        rg_state_counter[1] <= rg_state_counter[1] + 1;
      end
    endrule

    rule first_stage(rg_count[1] == 4);
      ff_input.deq;
      let {in1, in2, funct3, is_mul}=ff_input.first;
      Bit#(1) in2_sign = funct3[1 : 0] == 1? in2[31] : 0;
      Bit#(1) in1_sign = (funct3[1]^funct3[0]) & (in1[31]);

      Bit#(TAdd#(XLEN, 1)) op1;
      Bit#(TAdd#(XLEN, 1)) op2;
      if(is_mul == 1 && `MULSTAGES != 0) begin
        op1= {1'b0, in1};
        op2= {1'b0, in2};
      end
      else begin
        op1 = ({in1[31], in1[31 : 0]});
                op2 = ({in2[31], in2[31 : 0]});

        op1 = (funct3[0] == 0 && op1[xlen] == 1)?~op1[xlen - 1:0] + 1:op1[xlen - 1:0];
        op2 = (funct3[0] == 0 && op2[xlen] == 1)?~op2[xlen - 1:0] + 1:op2[xlen - 1:0];
      end

      //`ifdef RV64 rg_word_flag <= word_flag; `endif
      rg_is_mul <= unpack(is_mul);
      Bool op1_31_to_0_is_zero = (op1[31 : 0] == 0);
      Bool op2_31_to_0_is_zero = (op2[31 : 0] == 0);
      Bool op1_is_zero = (op1[31 : 0] == 0 && op1_31_to_0_is_zero);
      Bool op2_is_zero = (op2[31 : 0] == 0 && op2_31_to_0_is_zero);

      if(is_mul == 0 && op2_is_zero) begin
        if(funct3[1] == 1) begin	//REM / REMU operation
            ff_muldiv_result.enq(in1);
        end
        else begin				//DIV / DIVU operation
          ff_muldiv_result.enq('1);
        end
      end
      else if(op1_is_zero) begin
        ff_muldiv_result.enq(0);
        rg_signed <= False;
      end
      else begin 
        if(funct3 == 0) begin
          upper_bits <= False;		//used only for MUL
          if(is_mul == 1)
            rg_signed <= op1[xlen - 1] != op2[xlen - 1];
          else
            rg_signed <= op1[xlen] != op2[xlen];
        end
        else begin
          upper_bits <= True;	
          if(is_mul == 1)
            rg_signed <= unpack(in1_sign);
          else
            rg_signed <= False;
        end

        if(is_mul == 1 && `MULSTAGES != 0) begin
            rg_result_sign <= op1[xlen - 1];
            temp_multiplier_sign <= 0;
            multiplicand_divisor<={in2_sign, op2[31 : 0]};
            accumulator <= zeroExtend(op1[31 : 0]);
            rg_count[1] <= 3;
        end
        else begin
          accumulator <= zeroExtend(op1[31 : 0]);
          rg_state_counter[1] <= 1;
          rg_count[1] <= 3;
          multiplicand_divisor <= op2;
          rg_result_sign <= op1[xlen];
          rg_funct3 <= funct3;
        end
      end
    endrule

    method ActionValue#(ALU_OUT) get_inputs(Bit#(XLEN) in1, Bit#(XLEN) in2, Bit#(3) funct3);
    if(`MULSTAGES == 0 && funct3[2] == 0) begin
        let product = single_mult(in1, in2, funct3 `ifdef RV64, word_flag `endif );
        return ALU_OUT{done : True, cmtype : REGULAR, aluresult : zeroExtend(product), 
                      effective_addr : ?, cause : ?, redirect : False 
                      `ifdef bpu, branch_taken : ?, 
                      redirect_pc : ? `endif };
      end
      else begin
        ff_input.enq(tuple4(in1, in2, funct3[1 : 0], ~funct3[2]));
        return output_unavail;
      end
    endmethod
    method ActionValue#(ALU_OUT) delayed_output;//returning the result
      ff_muldiv_result.deq;
      let default_out = ff_muldiv_result.first();
      // zero extend is required when XLEN<ELEN
      return ALU_OUT{done : True, cmtype : REGULAR, aluresult : zeroExtend(default_out), 
                      effective_addr : ?, cause : ?, redirect : False 
                      `ifdef bpu, branch_taken : ?, 
                      redirect_pc : ? `endif };
    endmethod
    method Action flush;
      rg_count[0] <= 4;
      rg_state_counter[0] <= 0;
    endmethod
  endmodule
endpackage
