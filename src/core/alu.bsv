/* 
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Module name: Riscv_arithmetic_unit.
author name: Neel Gala
Email id:    neelgala@gmail.com

This module is the arithmetic execution unit for the RISCV ISA. It is a 64 bit implementation which is named as RV64.
The instruction with a "W" are RV64 instructions which ignore the upper 32 bits and operate on the lower 32 bits.
The arithmetic unit is implemented as a single case statement where the instruction bits define the various operations to be executed.

This module contains single cycle MUL instruction execution.

*/

package alu;

/****************************changes to be made to ISAdefs*************************/
typedef 64 XLEN;
typedef 128 XLEN_2;
typedef Bit#(XLEN) Data;
typedef Bit#(XLEN_2) Data_2;
typedef Int #(XLEN) Data_S;   // Signed register data
typedef UInt#(XLEN) Data_U;
typedef Bit #(3)  Funct3;
typedef Bit #(7)  Funct7;
Funct3 f3_MUL     = 3'b000;     // 0
Funct7 f7_MUL     = 7'b0000001; // 1
Funct3 f3_MULH    = 3'b001;     // 1
Funct7 f7_MULH    = 7'b0000001; // 1
Funct3 f3_MULHSU  = 3'b010;     // 2
Funct7 f7_MULHSU  = 7'b0000001; // 1
Funct3 f3_MULHU   = 3'b011;     // 3
Funct7 f7_MULHU   = 7'b0000001; // 1
Funct3 f3_DIV     = 3'b100;     // 4
Funct7 f7_DIV     = 7'b0000001; // 1
Funct3 f3_DIVU    = 3'b101;     // 5
Funct7 f7_DIVU    = 7'b0000001; // 1
/************************************************/


`define VADDR 	39
`define PADDR   32
import defined_types::*;
import defined_parameters::*;
`include "defined_parameters.bsv"
`include "decode.defines"

//basic mul function
(*noinline*)
function Bit#(XLEN) mul_core(Bit#(XLEN) v1,Bit#(XLEN) v2,Bool compliment);
		let out =v1*v2;
		if(compliment==True) out=~out+1;
		return out;
endfunction

//basic div function
(*noinline*)
function Bit#(XLEN) div_core(Bit#(XLEN) v1,Bit#(XLEN) v2,Bool compliment);
		let out=v1/((v2==0)?1:v2);
		if(compliment==True) out=~out+1;
		return out;
endfunction

	(*noinline*)
		function Tuple3#(Execution_output, Bit#(`VADDR),Flush_type) fn_alu(Bit#(4) fn, Bit#(64) op1, Bit#(64) op2, Bit#(64) immediate_value, Bit#(`VADDR) pc , 
																												Instruction_type inst_type, Bit#(`VADDR) npc, Funct3 funct3,Funct7 funct7, Access_type mem_access, Bit#(5) rd,Bool word32);
// TODO: use the pc of the previous stage for next-pc. This will save space in the FIFOs																													
// But what if the instruction in the previous stage has not yet been enqueued (in cases of page/cache misses)
// In this case you will have to wait untill that instruction arrives before progressing. NEED TO THINK THIS THROUGH
		/*========= Perform all the arithmetic ===== */
		// ADD* ADDI* SUB* 
		let inv_op2=(fn[3]==1)?~op2:op2;
		let op1_xor_op2=op1^inv_op2;
		let adder_output=op1+inv_op2+zeroExtend(fn[3]);
		// SLT SLTU
		Bit#(1) compare_out=fn[0]^(
							(fn[3]==0)?pack(op1_xor_op2==0):
							(op1[64-1]==op2[64-1])?adder_output[64-1]:
							(fn[1]==1)?op2[64-1]:op1[64-1]);
		// SLL SRL SRA
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};//word32 is bool,shift_amt is used to describe the amount of shift
		Bit#(TDiv#(XLEN,2)) upper_bits=word32?signExtend(fn[3]&op1[31]):op1[63:32];
		Bit#(XLEN) shift_inright={upper_bits,op1[31:0]};
		let shin = (fn==`FNSR || fn==`FNSRA)?shift_inright:reverseBits(shift_inright);
		Int#(TAdd#(64,1)) t=unpack({(fn[3]&shin[64-1]),shin});
		Int#(XLEN) shift_r=unpack(pack(t>>shift_amt)[64-1:0]);//shift right by shift_amt
		let shift_l=reverseBits(pack(shift_r));//shift left
		Bit#(XLEN) shift_output=((fn==`FNSR || fn==`FNSRA)?pack(shift_r):0) | ((fn==`FNSL)?pack(shift_l):0); 
		// AND OR XOR
		let logic_output=	((fn==`FNXOR || fn==`FNOR)?op1_xor_op2:0) |
								((fn==`FNOR || fn==`FNAND)?op1&op2:0);//why is FNOR checked in the second condition
		let shift_logic=zeroExtend(pack(fn==`FNSEQ || fn==`FNSNE || fn >= `FNSLT)&compare_out) |
							 logic_output|shift_output;
		//Bit#(XLEN) final_output = (fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;
		
		

		//********************************mul and div inst***************************
		Data v1 = op1, v2 = op2;//"Data" is defined in ISA_Defs.bsv,it is raw(unsigned) data reg of size 64
       	Data_S sop1 = unpack(v1), sop2 = unpack(v2);
      	Data_U uop1 = unpack(v1), uop2 = unpack(v2);
      	Data res = ?;
      	Data_2 res_2 = ?;//Data_2 is 128 bit wide
      	Bit #(1) sn_op1 = v1[valueof(XLEN)-1], sn_op2 = v2[valueof(XLEN)-1];
      	Bool take_complement = !(sn_op1 == sn_op2);
      	Data mop1 = (sn_op1 == 1) ? (~v1+1) : v1;
      	Data mop2 = (sn_op2 == 1) ? (~v2+1) : v2;    
      	case ({funct3, funct7})
         // MUL/DIV Instructions
        	{f3_MUL,    f7_MUL}     : begin res = mul_core(v1, v2, False); end
        	//{f3_MULS,    f7_MULS}   : begin res = mul_core(mop1, mop2, take_complement);end
        	{f3_DIVU,   f7_DIVU}	: begin res = div_core(v1,v2,False);end
         	{f3_DIV,    f7_DIV}     : begin res = div_core(mop1, mop2,take_complement);end
        endcase
        Bit#(128) final_output=zeroExtend(res_2);
            //**********************************************

		if(word32)
			final_output=signExtend(final_output[31:0]);
		if(inst_type==MEMORY && mem_access==Atomic) // TODO see if this can be avoided
			final_output=zeroExtend(op1);
		/*============================================ */
		/*====== generate the effective address to jump to ====== */  
		Bit#(`VADDR) branch_address=truncate(immediate_value)+pc;
		Bit#(`VADDR) next_pc=pc+4;
		Bit#(`VADDR) effective_address=0;
		Bit#(3) new_state=0;
		if(inst_type==JAL || inst_type==JALR)
			new_state='b11;
		else if(final_output[0]==1)begin
			if(new_state<3)
				new_state=new_state+1;
		end
		else begin
			if(new_state>0)
				new_state=new_state-1;
		end

		if((inst_type==BRANCH && final_output[0]==1) || inst_type==JAL)
			effective_address=branch_address;
		else if(inst_type==FENCEI || (inst_type==BRANCH && final_output[0]==0))
			effective_address=next_pc;
		else begin
			effective_address=truncate(final_output);
		end
		if(inst_type==JAL || inst_type==JALR)
			final_output=signExtend(next_pc);
		`ifdef simulate
			if(inst_type==BRANCH)
				final_output=0;
		`endif
		/*======================================================== */
		/*==== Generate flush if prediction was wrong or FENCEI ========== */
		Flush_type flush=None;
		if((inst_type==BRANCH || inst_type==JAL || inst_type==JALR) && effective_address!=npc)begin
			flush=AccessFlush;
		end
		else if(inst_type==FENCEI)
			flush=Fence;
		/*================================================================ */
	Trap_type exception=tagged None;
	if((inst_type==JALR || inst_type==JAL) && effective_address[1]!=0)
		exception=tagged Exception Inst_addr_misaligned;
 	Execution_output result;
		if(inst_type==MEMORY || inst_type==FENCE || inst_type == FENCEI)begin
			result= tagged MEMORY (Memout{
				address:final_output[31:0],
				memory_data:immediate_value[31:0],
				transfer_size:zeroExtend(funct3[1:0]),
				signextend:~funct3[2],
				mem_type:(inst_type==FENCE || inst_type==FENCEI)?Fence:mem_access
				`ifdef atomic ,atomic_op:{pack(word32),fn} `endif	});
		end
		else if(inst_type==SYSTEM_INSTR)begin
			result=tagged SYSTEM (CSRInputs{rs1:op1[31:0],rs2:op2[31:0],rs1_addr:immediate_value[16:12],funct3:funct3,csr_address:immediate_value[11:0]});	
		end
		else
			result=tagged RESULT (Arithout{aluresult:final_output[31:0],fflags:0});
		return tuple3(result,effective_address,flush);
	endfunction

//	module mkTb(Empty);
//
//	rule test_alu;
//		Bit#(64) op1='h8000000000004123;
//		Int#(64) in1=unpack(op1);
//		Bit#(64) op2='h8000000000004123;
//		let {x,ea,flush}<-fn_alu(`FNSNE,op1,op2,'d0,'d0,BRANCH,'h800,'d3,Load,False);
//		//$display("Output is: :%h Excepted: %h",x,(op1!=op2));
//		$finish(0);
//	endrule
//	endmodule
	
endpackage