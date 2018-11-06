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

  `ifdef muldiv
    `ifdef muldiv_fpga
      import muldiv_fpga::*; 
    `else
      import muldiv_asic::*;
    `endif
  `endif
  import common_types::*;
  `include "common_params.bsv"

	(*noinline*)
    function ALU_OUT fn_alu(ALU_Inputs inp `ifdef RV64 , Bool word32 `endif , Bit#(1) misa_c);
    let {fn, op1, op2, imm_value, op3, inst_type, funct3, memaccess}=inp;

	  /*========= Perform all the arithmetic ===== */
	  // ADD* ADDI* SUB* 
    Bit#(XLEN) inv = signExtend(fn[3]);
	  let inv_op2=op2^inv;
	  let op1_xor_op2=op1^inv_op2;
    let op1_add=op1;
    let op2_add=inv_op2;
    let adder_output=op1+ inv_op2+ zeroExtend(fn[3]);
	  // SLT SLTU
	  Bit#(1) compare_out=fn[0]^(
						(fn[3]==0)?pack(op1_xor_op2==0):
						(op1[valueOf(XLEN)-1]==op2[valueOf(XLEN)-1])?adder_output[valueOf(XLEN)-1]:
						(fn[1]==1)?op2[valueOf(XLEN)-1]:op1[valueOf(XLEN)-1]);
	  // SLL SRL SRA
    //word32 is bool, shift_amt is used to describe the amount of shift
	  Bit#(6) shift_amt={( `ifdef RV64 (!word32)?op2[5]: `endif 0), op2[4:0]};

	  `ifdef RV64
	  	Bit#(TDiv#(XLEN, 2)) upper_bits=`ifdef RV64 word32?signExtend(fn[3]&op1[31]): `endif op1[63:32];
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

		Bit#(XLEN) final_output=(fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;
    `ifdef RV64
  		if(word32)
	  		 final_output=signExtend(final_output[31:0]);
    `endif

		// Generate flush if prediction was wrong
		Flush_type flush=None;
		if((inst_type==BRANCH && final_output[0]==1) || inst_type==JALR || inst_type==JAL )begin
			flush=Flush;
		end
		
    // generate the effective address to jump to 
		Bit#(XLEN) effective_address=op3+ signExtend(imm_value);
    if(inst_type==JALR)
      effective_address[0]=0;
    // The following can be placed here since we are not using a branch predictor yet.
    Trap_type exception=tagged None;
	  if((inst_type==JAL || inst_type==JALR || (inst_type==BRANCH && final_output[0]==1)) 
                                                     && effective_address[1]!=0 && misa_c==0)
	    exception=tagged Exception Inst_addr_misaligned;
    else
    if(inst_type==MEMORY && ((funct3[1:0]==1 && effective_address[0]!=0) || (funct3[1:0]==2 &&
        effective_address[1:0]!=0) `ifdef RV64 || (funct3[1:0]==3 && effective_address[2:0]!=0)
        `endif ))begin
      exception = memaccess==Load? tagged Exception Load_addr_misaligned: 
                                   tagged Exception Store_addr_misaligned;
    end
    // there is priority implied here.
    else if(valueOf(XLEN)>valueOf(PADDR)) begin
      Bit#(TSub#(XLEN, PADDR)) temp=effective_address[valueOf(XLEN)-1:valueOf(PADDR)];
      if(inst_type==MEMORY && (|temp)==1)
        if(memaccess==Load)
          exception= tagged Exception Load_access_fault;
        else
          exception = tagged Exception Store_access_fault;
    end
    
    Commit_type committype = REGULAR;
    if(inst_type==MEMORY)
      committype = MEMORY;
    else if(inst_type == SYSTEM_INSTR)
      committype = SYSTEM_INSTR;
	
	  Bit#(TAdd#(PADDR, 1)) effaddr_csrdata = (inst_type==SYSTEM_INSTR)? 
                                            zeroExtend({funct3, imm_value[16:0]}): 
                                            {pack(flush), effective_address[valueOf(PADDR)-1:0]};

		`ifdef simulate
			if(inst_type==BRANCH)
				final_output=0;
		`endif

	  return tuple4(committype, final_output, effaddr_csrdata, exception);
	endfunction

`ifdef muldiv
  interface Ifc_alu;
    method ActionValue#(Tuple2#(Bool, ALU_OUT)) get_inputs(ALU_Inputs inp 
        `ifdef RV64 , Bool word32 `endif , Bit#(1) misa_c);
		method ActionValue#(ALU_OUT) delayed_output;
  endinterface:Ifc_alu

  (*synthesize*)
  module mkalu(Ifc_alu);
    Ifc_muldiv muldiv <- mkmuldiv;
    method ActionValue#(Tuple2#(Bool, ALU_OUT)) get_inputs(ALU_Inputs inp 
        `ifdef RV64 , Bool word32 `endif , Bit#(1) misa_c );
      let {fn, op1, op2, imm_value, op3, inst_type, funct3, memaccess}=inp;
      if(inst_type==MULDIV)begin
        let product <- muldiv.get_inputs(op1, op2, funct3 `ifdef RV64 , word32 `endif );
        return product;
      end
      else
        return tuple2(True, fn_alu(inp `ifdef RV64 , word32 `endif ,misa_c));
    endmethod
		method delayed_output=muldiv.delayed_output;
  endmodule
`endif
endpackage:alu
