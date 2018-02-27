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
author name: Neel Gala, Aditya Mathur
Email id:    neelgala@gmail.com

This module is the arithmetic execution unit for the RISCV ISA. It is a 64 bit implementation which is named as RV64.
The instruction with a "W" are RV64 instructions which ignore the upper 32 bits and operate on the lower 32 bits.
The arithmetic unit is implemented as a single case statement where the instruction bits define the various operations to be executed.

This module contains single cycle MUL instruction execution.

*/

package alu;

/****************************changes to be made to ISAdefs*************************/
/*==== Project imports ====*/
import isa_defs::*;
import common_types::*;
`include "common_params.bsv"
/*=========================*/
/************************************************/


//basic mul function
(*noinline*)
function Bit#(XLEN) mul_core(Bit#(XLEN) v1,Bit#(XLEN) v2,Bool complement);
		let out =v1*v2;
		out=(complement==True)?(~(out)+1):out;
		return out;
endfunction
	(*noinline*)
		function Tuple3#(Execution_output, Bit#(PADDR),Flush_type) fn_alu(Bit#(4) fn, Bit#(XLEN) op1, Bit#(XLEN) op2, Bit#(XLEN) immediate_value, Bit#(PADDR) pc , 
											Instruction_type inst_type, Funct3 funct3,Access_type mem_access, Bit#(5) rd,Bool word32);
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
								((fn==`FNOR || fn==`FNAND)?op1&op2:0);//why is `FNOR checked in the second condition
		let shift_logic=zeroExtend(pack(fn==`FNSEQ || fn==`FNSNE || fn >= `FNSLT)&compare_out) |
							 logic_output|shift_output;

		/********************************mul and div inst***************************/
				//Data v1 = op1, v2 = op2;//"Data" is defined in ISA_Defs.bsv,it is raw(unsigned) data reg of size 64
		/*		Data operand1=?;Data operand2=?;Bool is32Bit=False;

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
       			res=mul_core(operand1,operand2,is32Bit);
*/
     /**********************************************/
     	//Bit#(XLEN) final_output=(inst_type==MUL)?signExtend(res):(fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;
     	Bit#(XLEN) final_output=(fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;

		if(word32)
			 final_output=signExtend(final_output[31:0]);
		if(inst_type==MEMORY && mem_access==Atomic) // TODO see if this can be avoided
			 final_output=zeroExtend(op1);
		/*============================================ */
		/*====== generate the effective address to jump to ====== */  
		Bit#(PADDR) branch_address=truncate(immediate_value)+pc;
		Bit#(PADDR) effective_address=0;
		if((inst_type==BRANCH && final_output[0]==1) || inst_type==JAL)
			effective_address=branch_address;

//		if(inst_type==JAL || inst_type==JALR)
//			 final_output=signExtend(npc);/*next_pc*/
		`ifdef simulate
			if(inst_type==BRANCH)
				final_output=0;
		`endif
		/*======================================================== */
		/*==== Generate flush if prediction was wrong or FENCEI ========== */
		Flush_type flush=None;
		if((inst_type==BRANCH || inst_type==JAL || inst_type==JALR))begin
			flush=Flush;
		end
		/*================================================================ */
	Trap_type exception=tagged None;
	if((inst_type==JALR || inst_type==JAL) && effective_address[1]!=0)
		exception=tagged Exception Inst_addr_misaligned;
 	Execution_output result;
		if(inst_type==MEMORY)begin
			result= tagged MEMORY (Memout{
				address:truncate(final_output),
				memory_data:immediate_value,
				transfer_size:zeroExtend(funct3[1:0]),
				signextend:~funct3[2],
				mem_type:mem_access
				`ifdef atomic ,atomic_op:{pack(word32),fn} `endif	});
		end
		else if(inst_type==SYSTEM_INSTR)begin
			result=tagged SYSTEM (CSRInputs{rs1:op1,rs2:op2,rs1_addr:immediate_value[16:12],funct3:funct3,csr_address:immediate_value[11:0]});	
		end
		else
			result=tagged RESULT (Arithout{aluresult:final_output,fflags:0});
		return tuple3(result,effective_address,flush);
	endfunction
module mkTb(Empty);
	Reg#(Bit#(5)) test_counter <- mkReg(0);
	Bool word32=False;
	rule test1;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNADD,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1+op2;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
		test_counter<=test_counter+1;
	endrule
	rule test2;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNSUB,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1-op2;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test3;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNSNE,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=zeroExtend(pack(op1!=op2));
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test4;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNSEQ,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=zeroExtend(pack(op1==op2));
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test5;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};
		let {a,b,c} =fn_alu(`FNSR,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1>>shift_amt;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test6;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};
		let {a,b,c} =fn_alu(`FNSRA,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1>>shift_amt;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test7;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};
		let {a,b,c} =fn_alu(`FNSL,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=reverseBits(op1>>shift_amt);
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test8;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNXOR,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1^op2;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test9;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNOR,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1|op2;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test10;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNSLT,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=zeroExtend(pack(op1<op2));
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test11;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNAND,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=op1&op2;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
				 
	rule test12;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		Bit#(PADDR) next_pc='h07000000+4;
		let {a,b,c} =fn_alu(`FNAND,op1,op2,op1,'h07000000,JAL,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=signExtend(next_pc);
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test13;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		Bit#(PADDR) next_pc='h07000000+4;
		let {a,b,c} =fn_alu(`FNAND,op1,op2,op1,'h07000000,JALR,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=signExtend(next_pc);
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	rule test14;
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		let {a,b,c} =fn_alu(`FNAND,op1,op2,op1,'h07000000,BRANCH,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output=0;
		if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		begin
			$display("output matches with the golden output at test number=%d",test_counter);
			$finish(0);
		end
		else
		begin
			$display("output does't match with the golden output at test number=%d",test_counter);
			$finish(0);
		end
	endrule
	endmodule
endpackage
