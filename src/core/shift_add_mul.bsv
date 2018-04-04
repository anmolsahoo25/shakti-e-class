package shift_add_mul;

import common_types::*;
	`include "common_params.bsv"

	interface Ifc_mul;
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2);
		method ActionValue#(Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool)) rst();//returning the result and the done bit
	endinterface
(*synthesize*)
	module mkmuldiv(Ifc_mul);
		Reg#(Bit#(XLEN)) op1 <-mkRegU;
		Reg#(Bit#(XLEN)) op2 <-mkRegU;
		Reg#(Bit#(TSub#(TMul#(2,XLEN),1))) result <-mkReg(0);
		Reg#(Bool) done<-mkReg(False);
		Reg#(Bool) start<-mkReg(True);

	for(Integer i=valueOf(XLEN)-1;i>=0;i=i-1)
		begin
			rule shift_add(!start);
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
			endrule
		end

	method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2) if(start==True)	;
		op1<=operand1;
		op2<=operand2;
		start<=False;
	endmethod
	method ActionValue#(Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool)) rst();
		Bit#(TSub#(TMul#(2,XLEN),1)) final_rst=result;
		done<=True;
		Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool) rst_out=tuple2(final_rst,done);
		return rst_out;
	endmethod
	endmodule:mkmuldiv
endpackage:shift_add_mul