/* 
Copyright (c) 2013, IIT Madras All rights reserved.

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

Module name: Riscv_arithmetic_unit.  
author name: Neel Gala, Aditya Mathur 
Email id: neelgala@gmail.com

This module is the arithmetic execution unit for the RISCV ISA. 
It is a 64 bit implementation which is named as RV64.  The instruction with a "W" are RV64 
instructions which ignore the upper 32 bits and operate on the lower 32 bits.  
The arithmetic unit is implemented as a single case statement where the instruction bits define 
the various operations to be executed.

This module contains single cycle MUL instruction execution.

*/

package alu;

  // Project imports 
  import common_types::*;
  `include "common_params.bsv"
  
  //basic mul function
  (*noinline*)
  function Bit#(XLEN) mul_core(Bit#(XLEN) v1, Bit#(XLEN) v2, Bool complement);
  		let out =v1*v2;
  		out=(complement==True)?(~(out)+1):out;
  		return out;
  endfunction

	(*noinline*)
	function ALU_OUT fn_alu (Bit#(4) fn, Bit#(XLEN) op1, Bit#(XLEN) op2, Bit#(XLEN) imm_value, 
        Bit#(PADDR) pc, Instruction_type inst_type, Funct3 funct3, Access_type mem_access, 
          Bit#(5) rd, Bool word32);
	  /*========= Perform all the arithmetic ===== */
	  // ADD* ADDI* SUB* 
    Bit#(XLEN) inv = signExtend(fn[3]);
	  let inv_op2=op2^inv;
	  let op1_xor_op2=op1^inv_op2;
    let op1_add=op1;
    let op2_add=inv_op2;
    let carry = fn[3];
    if(inst_type== JAL_R || inst_type==BRANCH)begin
      carry=0;
    end
    if(inst_type== JAL_R || inst_type==BRANCH || inst_type==MEMORY)begin
      op2_add= imm_value;
    end
    if(inst_type==BRANCH)
      op1_add=zeroExtend(pc);
	  let adder_output=op1_add+op2_add+zeroExtend(carry);
    let inter=op1+ inv_op2+ zeroExtend(fn[3]);
	  // SLT SLTU
	  Bit#(1) compare_out=fn[0]^(
						(fn[3]==0)?pack(op1_xor_op2==0):
						(op1[valueOf(XLEN)-1]==op2[valueOf(XLEN)-1])?inter[valueOf(XLEN)-1]:
						(fn[1]==1)?op2[valueOf(XLEN)-1]:op1[valueOf(XLEN)-1]);
	  // SLL SRL SRA
    //word32 is bool, shift_amt is used to describe the amount of shift
	  Bit#(6) shift_amt={((!word32)?op2[5]:0), op2[4:0]};

	  `ifdef RV64
	  	Bit#(TDiv#(XLEN, 2)) upper_bits=word32?signExtend(fn[3]&op1[31]):op1[63:32];
	  	Bit#(XLEN) shift_inright={upper_bits, op1[31:0]};//size of 64 bit
	  `else
	  	Bit#(XLEN) shift_inright=zeroExtend(op1[31:0]);//size of 32bit
	  `endif

	  let shin = (fn==`FNSR || fn==`FNSRA)?shift_inright:reverseBits(shift_inright);
	  Int#(TAdd#(XLEN, 1)) t=unpack({(fn[3]&shin[valueOf(XLEN)-1]), shin});
	  Int#(XLEN) shift_r=unpack(pack(t>>shift_amt)[valueOf(XLEN)-1:0]);//shift right by shift_amt
	  let shift_l=reverseBits(pack(shift_r));//shift left
	  Bit#(XLEN) shift_output=((fn==`FNSR || fn==`FNSRA)?pack(shift_r):0) | 
                            ((fn==`FNSL)?pack(shift_l):0); 

	  // AND OR XOR
	  let logic_output=	((fn==`FNXOR || fn==`FNOR)?op1_xor_op2:0) |
	  						((fn==`FNOR || fn==`FNAND)?op1&op2:0);
	  let shift_logic=zeroExtend(pack(fn==`FNSEQ || fn==`FNSNE || fn >= `FNSLT)&compare_out) |
	  					 logic_output|shift_output;

		// mul and div inst
	/*	
    Data operand1=?;Data operand2=?;Bool is32Bit=False;
    Data_S sop1 = unpack(op1), sop2 = unpack(op2);
    Data_U uop1 = unpack(op1), uop2 = unpack(op2);
    Data res = ?;
    //Data_2 res_2 = ?;//Data_2 is 128 bit wide
    Bit #(1) sn_op1 = op1[valueof(XLEN)-1], sn_op2 = op2[valueof(XLEN)-1];
    Bool take_complement = !(sn_op1 == sn_op2);

    //see how to reduce number of adder
		Data mop1 = (sn_op1 == 1) ? (~op1+1) : op1;
		Data mop2 = (sn_op2 == 1) ? (~op2+1) : op2;  

     
    if(funct3==f3_MUL)begin operand1=op1;operand2=op2;is32Bit=False; end
    else if(funct3==f3_MULHSU)begin operand1=mop1;operand2=op2;is32Bit=take_complement; end
    else if(funct3==f3_MULH)begin operand1=mop1;operand2=mop2;is32Bit=take_complement; end
    else if(funct3==f3_MULHU)begin operand1=op1;operand2=op2;is32Bit=False; end
    res=mul_core(operand1, operand2, is32Bit);//creates only single instance of the multiplier
*/
    Bit#(XLEN) final_output=(fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;
    //Bit#(XLEN) final_output=(fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;
    `ifdef RV64
  		if(word32)
	  		 final_output=signExtend(final_output[31:0]);
    `endif

		// Generate flush if prediction was wrong
		Flush_type flush=None;
		if((inst_type==BRANCH && final_output[0]==1) || inst_type==JAL_R )begin
			flush=Flush;
		end
		
    // generate the effective address to jump to 
		Bit#(PADDR) effective_address=truncate(adder_output);
		Bit#(PADDR) npc=pc+4;
		if(inst_type==JAL_R)
			 final_output=zeroExtend(npc);
		`ifdef simulate
			if(inst_type==BRANCH)
				final_output=0;
		`endif

//	Trap_type exception=tagged None;
//	if((inst_type==JAL_R) && effective_address[1]!=0)
//		exception=tagged Exception Inst_addr_misaligned;
    
    Commit_type committype = REGULAR;
    if(inst_type==MEMORY)
      committype = MEMORY;
    else if(inst_type == SYSTEM_INSTR)
      committype = SYSTEM_INSTR;
	
	  Bit#(XLEN) op1_result = committype == SYSTEM_INSTR?op1: final_output;
	  Bit#(TAdd#(PADDR, 1)) effaddr_csrdata = (flush == Flush)? zeroExtend({1'b1, effective_address}): 
                                          zeroExtend({funct3, imm_value[16:0]});

	  return tuple3(committype, op1_result, effaddr_csrdata);
	endfunction
endpackage:alu
