package shift_add_mul_fsm;
	import common_types::*;
	import StmtFSM::*;
	`include "common_params.bsv"

	interface Ifc_mul;
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2,Funct3 funct3);
		method ActionValue#(Bit#(TSub#(TMul#(2,XLEN),1))) rst();
	endinterface

	module mkMul(Ifc_mul);
		Reg#(Bit#(XLEN)) op1<-mkRegU;
		Reg#(Bit#(XLEN)) op2<-mkRegU;
		Reg#(Bit#(7)) rg_counter <-mkReg(0);
		Reg#(Bit#(TSub#(TMul#(2,XLEN),1))) result <-mkReg(0);
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
			op1<=operand1;
			op2<=operand2;
			rg_counter<=1;
		endmethod
		method ActionValue#(Bit#(TSub#(TMul#(2,XLEN),1))) rst() if(rg_counter==64);
			Bit#(TSub#(TMul#(2,XLEN),1)) final_rst=result;
			return final_rst;
		endmethod
	endmodule

endpackage
