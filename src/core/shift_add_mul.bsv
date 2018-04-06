package shift_add_mul_fsm;
	import common_types::*;
	import StmtFSM::*;
	`include "common_params.bsv"

	interface Ifc_mul;
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2,Funct3 funct3);
		method ActionValue#(Bit#(XLEN_2)) rst(Bool word32);
	endinterface
(*synthesize*)
	module mkMul(Ifc_mul);
		Reg#(Bit#(XLEN)) op1<-mkRegU;
		Reg#(Bit#(XLEN)) op2<-mkRegU;
		Reg#(Bit#(7)) rg_counter <-mkReg(0);
		Reg#(Bit#(XLEN_2)) result <-mkReg(0);
		Reg#(Bool) is32Bit<-mkReg(False);
		rule shift_add(rg_counter!=0&&rg_counter<=64);
			if(op2[0]==1) 
					begin
						result<=result+zeroExtend(op1);//addition
						op1<=op1<<1;//left shift
					end
				else if(op2[0]==0)
					begin	
						op1<=op1<<1;//left shift
					end
				op2<=op2>>1;//right shift
				if(rg_counter<=64) rg_counter<=rg_counter+1;
		endrule

		//interface methods
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) 
			operand2,Funct3 funct3) if(rg_counter==0)	;
			Data_S sop1 = unpack(operand1), sop2 = unpack(operand2);
      		Data_U uop1 = unpack(operand1), uop2 = unpack(operand2);
      		Data res = ?;
      		Bit #(1) sn_op1 = operand1[valueof(XLEN)-1], sn_op2 = operand2[valueof(XLEN)-1];
      		Bool take_complement = !(sn_op1 == sn_op2);
      		Data mop1 = (sn_op1 == 1) ? (~operand1+1) : operand1;
		    Data mop2 = (sn_op2 == 1) ? (~operand2+1) : operand2; 
		    if(funct3==f3_MUL)begin op1<=operand1;op2<=operand2;is32Bit<=False; end
       		else if(funct3==f3_MULHSU)begin op1<=mop1;op2<=operand2;is32Bit<=take_complement; end
       		else if(funct3==f3_MULH)begin op1<=mop1;op2<=mop2;is32Bit<=take_complement; end
       		else if(funct3==f3_MULHU)begin is32Bit<=False; end
			rg_counter<=1;
		endmethod
		method ActionValue#(Bit#(XLEN_2)) rst(Bool word32) if(rg_counter==64);
			Bit#(XLEN_2) final_rst=result;
			if(!word32) return final_rst;
			else return zeroExtend(final_rst[31:0])	;
		endmethod
	endmodule
	module mktest(Empty);
		Ifc_mul dut<- mkMul;
		Reg#(Bit#(XLEN)) a<-mkReg(2);
		Reg#(Bit#(XLEN)) b<-mkReg(3);
		rule put_values;
			dut.get_values(a,b,f3_MUL);
			let result=dut.rst();
		//	$display(result);
			$finish(0);
		endrule
	endmodule

endpackage
