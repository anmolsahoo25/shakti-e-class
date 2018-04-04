package shift_add_mul;

import common_types::*;
	`include "common_params.bsv"

	interface Ifc_mul;
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2);
		method ActionValue#(Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool)) rst();//returning the result and the done bit
	endinterface

	module mkmuldiv(Ifc_mul);
		Reg#(Bit#(XLEN)) op1 <-mkRegU;
		Reg#(Bit#(XLEN)) op2 <-mkRegU;
		Reg#(Bit#(TSub#(TMul#(2,XLEN),1))) result <-mkReg(0);

	for(Integer i=valueOf(XLEN)-1;i>=0;i=i-1)
		begin
			rule shift_add;
				Bit#(XLEN) sf_op2=op2;
				Bit#(XLEN) sf_op1=op1;
				if(sf_op2[0]==1) 
					begin
						result<=result+zeroExtend(sf_op1);//addition
						sf_op1=sf_op1<<1;//left shift
					end
				else if(sf_op2[0]==0)
					begin	
						sf_op1=sf_op1<<1;//left shift
					end
				sf_op2=sf_op2>>1;//right shift
			endrule
		end


	method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2)	;
		op1<=operand1;
		op2<=operand2;
	endmethod
	method ActionValue#(Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool)) rst();
		Bit#(TSub#(TMul#(2,XLEN),1)) final_rst=result;
		Bool done=True;
		Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool) rst_out=tuple2(final_rst,done);
		return rst_out;
	endmethod
	endmodule:mkmuldiv
endpackage:shift_add_mul