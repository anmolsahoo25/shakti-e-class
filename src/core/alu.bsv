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
// noinline creates these functions as seperate modules during synthesis in vivado
	(*noinline*)
		function Tuple5#(Instruction_type,Bit#(XLEN),Bit#(XLEN),Bit#(5),Bit#(15))
				fn_alu (Bit#(4) fn, Bit#(XLEN) op1, Bit#(XLEN) op2, Bit#(XLEN) immediate_value, Bit#(PADDR) pc , 
						Instruction_type inst_type, Funct3 funct3,Access_type mem_access, Bit#(5) rd,Bool word32);
								/*========= Perform all the arithmetic ===== */
		// ADD* ADDI* SUB* 
		let inv_op2=(fn[3]==1)?~op2:op2;
		let op1_xor_op2=op1^inv_op2;
		let adder_output=op1+inv_op2+zeroExtend(fn[3]);
		// SLT SLTU
		Bit#(1) compare_out=fn[0]^(
							(fn[3]==0)?pack(op1_xor_op2==0):
							(op1[valueOf(XLEN)-1]==op2[valueOf(XLEN)-1])?adder_output[valueOf(XLEN)-1]:
							(fn[1]==1)?op2[valueOf(XLEN)-1]:op1[valueOf(XLEN)-1]);
		// SLL SRL SRA
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};//word32 is bool,shift_amt is used to describe the amount of shift
		`ifdef RV64
			Bit#(TDiv#(XLEN,2)) upper_bits=word32?signExtend(fn[3]&op1[31]):op1[63:32];
			Bit#(XLEN) shift_inright={upper_bits,op1[31:0]};//size of 64 bit
		`else
			Bit#(XLEN) shift_inright=zeroExtend(op1[31:0]);//size of 32bit
		`endif
		let shin = (fn==`FNSR || fn==`FNSRA)?shift_inright:reverseBits(shift_inright);
		Int#(TAdd#(XLEN,1)) t=unpack({(fn[3]&shin[valueOf(XLEN)-1]),shin});
		Int#(XLEN) shift_r=unpack(pack(t>>shift_amt)[valueOf(XLEN)-1:0]);//shift right by shift_amt
		let shift_l=reverseBits(pack(shift_r));//shift left
		Bit#(XLEN) shift_output=((fn==`FNSR || fn==`FNSRA)?pack(shift_r):0) | ((fn==`FNSL)?pack(shift_l):0); 
		// AND OR XOR
		let logic_output=	((fn==`FNXOR || fn==`FNOR)?op1_xor_op2:0) |
								((fn==`FNOR || fn==`FNAND)?op1&op2:0);
		let shift_logic=zeroExtend(pack(fn==`FNSEQ || fn==`FNSNE || fn >= `FNSLT)&compare_out) |
							 logic_output|shift_output;

		/********************************mul and div inst***************************/
				//Data v1 = op1, v2 = op2;//"Data" is defined in ISA_Defs.bsv,it is raw(unsigned) data reg of size 64
				/*Data operand1=?;Data operand2=?;Bool is32Bit=False;

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
       			res=mul_core(operand1,operand2,is32Bit);//creates only single instance of the multiplier
*/
     /**********************************************/
     	Bit#(XLEN) final_output=(fn==`FNADD || fn==`FNSUB)?adder_output:shift_logic;
		if(word32)
			 final_output=signExtend(final_output[31:0]);

		/*============================================ */
		/*==== Generate flush if prediction was wrong ========== */
		Flush_type flush=None;
		if((inst_type==BRANCH && final_output[0]==1) || inst_type==JAL_R )begin
			flush=Flush;
		end
		/*================================================================ */
		/*====== generate the effective address to jump to ====== */  
		Bit#(PADDR) effective_address=truncate(immediate_value)+pc;
		Bit#(PADDR) npc=pc+4;
		if(inst_type==JAL_R)
			 final_output=signExtend(npc);
		`ifdef simulate
			if(inst_type==BRANCH)
				final_output=0;
		`endif
		/*======================================================== */
//	Trap_type exception=tagged None;
//	if((inst_type==JAL_R) && effective_address[1]!=0)
//		exception=tagged Exception Inst_addr_misaligned;


	// Explanation of the return type:
	// First field		:  Instruction_type: This fields holds the type of instruction being executed.
	// Second field	:	Bit#(XLEN)	:	if instruction is SYSTEM_INSTR then this holds op1. If the instruction is MEMORY then the lower
	//												PADDR bits hold the address of the Load/Store. Else it holds the result of the ALU operation.
	// Third field		:	Bit#(XLEN)	:	if instruction is SYSTEM_INSTR then this holds op2. if instruction is MEMORY then this holds
	//												the data for Store operations. Else it holds the effective branch address in case of BRANCH or JAL operations
	// Fourth field	:	Bit#(5)		:  if instruction is SYSTEM_INSTR then this holds the address of rs1. Else this field holds the concatenation
	//												of {Flush (if pc needs to change), transfer_size, signextend and type of mem_access}
	// Fifth	field		:	Bit#(15)		:	This holds the concatenation of funct3 and the 12-bit CSR address.
	
	Bit#(XLEN) address_op1_result=inst_type==SYSTEM_INSTR?op1:final_output;
	Bit#(XLEN) data_op2_effaddr  =inst_type==SYSTEM_INSTR?op2:inst_type==MEMORY?immediate_value:zeroExtend(effective_address);
	Bit#(5)	  meminfo_rs1addr	  =inst_type==SYSTEM_INSTR?immediate_value[16:12]:{pack(flush),funct3[1:0],~funct3[2],pack(mem_access)};
	Bit#(15)   funct3_addr	 ={funct3,immediate_value[11:0]};

	return tuple5(inst_type,address_op1_result,data_op2_effaddr,meminfo_rs1addr,funct3_addr);
	endfunction
module mkTb(Empty);
	Reg#(Bit#(5)) test_counter <- mkReg(1);
	Bool word32=False;
	Reg#(Bool) fire1<-mkReg(True);
	Reg#(Bool) fire2<-mkReg(True);
	Reg#(Bool) fire3<-mkReg(True);
	Reg#(Bool) fire4<-mkReg(True);
	Reg#(Bool) fire5<-mkReg(True);
	Reg#(Bool) fire7<-mkReg(True);
	Reg#(Bool) fire8<-mkReg(True);
	Reg#(Bool) fire9<-mkReg(True);
	Reg#(Bool) fire10<-mkReg(True);
	Reg#(Bool) fire11<-mkReg(True);
	Reg#(Bool) fire12<-mkReg(True);
	Reg#(Bool) fire13<-mkReg(True);
	(*descending_urgency="test1,test2,test3,test4,test5,test7,test8,test9,test10,test11,test12,test13"*)

	(*fire_when_enabled*)
	rule test1(fire1);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNADD,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output1=op1+op2;
		//if(a matches tagged RESULT .inter &&& inter.aluresult==correct_output)
		if(!(b==correct_output1))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output1);
			$finish(0);
		end
		fire1<=False;
		test_counter<=test_counter+1;
	endrule
	rule test2(fire2);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNSUB,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output2=op1-op2;
		if(!(b==correct_output2))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output2);	
			$finish(0);
		end
		fire2<=False;
		test_counter<=test_counter+1;
	endrule
	rule test3(fire3);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNSNE,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output3=zeroExtend(pack(op1!=op2));
		if(!(b==correct_output3))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output3);			
			$finish(0);
		end
		fire3<=False;
		test_counter<=test_counter+1;
	endrule
	rule test4(fire4);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNSEQ,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output4=zeroExtend(pack(op1==op2));
		if(!(b==correct_output4))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output4);
			$finish(0);
		end
		fire4<=False;
		test_counter<=test_counter+4;
	endrule
	rule test5(fire5);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};
		let {a,b,c,d,e} =fn_alu(`FNSR,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output5=op1>>shift_amt;
		if(!(b==correct_output5))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output5);
			$finish(0);
		end
		fire5<=False;
		test_counter<=test_counter+2;//added two bcaz the next test is skipped
	endrule
	/*rule test6(fire6);
		Bit#(64) op1='h8000000000001234;
		Bit#(64) op2='h8000000000001234;
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};
		let {a,b,c,d,e} =fn_alu(`FNSRA,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(64) correct_output6=op1>>shift_amt;
		if(!(b==correct_output6))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output6);
			$finish(0);
		end
		fire6<=False;
		test_counter<=test_counter+1;
	endrule*/
	rule test7(fire7);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		Bit#(6) shift_amt={((!word32)?op2[5]:0),op2[4:0]};
		let {a,b,c,d,e} =fn_alu(`FNSL,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output=reverseBits(op1>>shift_amt);
		if(!(b==correct_output))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output);
			$finish(0);
		end
		test_counter<=test_counter+1;
	endrule
	rule test8(fire8);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNXOR,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output8=op1^op2;
		if(!(b==correct_output8))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output8);
			$finish(0);
		end
		test_counter<=test_counter+1;
		fire8<=False;
	endrule
	rule test9(fire9);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNOR,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output9=op1|op2;
		if(!(b==correct_output9))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output9);
			$finish(0);
		end
		fire9<=False;
		test_counter<=test_counter+1;
	endrule
	rule test10(fire10);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNSLT,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output10=zeroExtend(pack(op1<op2));
		if(!(b==correct_output10))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output10);
			$finish(0);
		end
		fire10<=False;
		test_counter<=test_counter+1;
	endrule
	rule test11(fire11);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNAND,op1,op2,op1,'h07000000,ALU,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output11=op1&op2;
		if(!(b==correct_output11))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output11);
			$finish(0);
		end
		fire11<=False;
		test_counter<=test_counter+1;
	endrule
				 
	rule test12(fire12);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		Bit#(PADDR) npc='h07000000+4;
		let {a,b,c,d,e} =fn_alu(`FNADD,op1,op2,op1,'h07000000,JAL_R,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output12=signExtend(npc);
		if(!(b==correct_output12))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output12);
			$finish(0);
		end
		fire12<=False;
	endrule
	rule test13(fire13);
		Bit#(XLEN) op1='h8000000000001234;
		Bit#(XLEN) op2='h8000000000001234;
		let {a,b,c,d,e} =fn_alu(`FNAND,op1,op2,op1,'h07000000,BRANCH,f3_MUL,Load,'h07,word32);
		Bit#(XLEN) correct_output13=truncate(op1)+'h07000000;
		if(!(b==correct_output13))
		begin
			$display("output does't match with the golden output at test number=%d,b=%d,correct_output=%d",test_counter,b,correct_output13);
			$finish(0);
		end
		fire13<=False;
		$display("test finished,test_number=%d",test_counter);
		$finish(0);
		test_counter<=test_counter+1;
	endrule
endmodule:mkTb
endpackage:alu
