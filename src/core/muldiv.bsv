package muldiv;
import bsvmkmult_gen_0::*;
import common_types::*;
  `include "common_params.bsv"

	interface Ifcmuldiv;
	(*always_ready , always_enabled*)
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2);
	(*always_ready , always_enabled*)	
		method ActionValue#(Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool)) result();//returning the result and the done bit
	endinterface:Ifcmuldiv

	module mkMuldiv(Ifcmuldiv);
		//Reg#(Bool) done<-mkReg(False);
		Ifc_mult_gen#(XLEN) biv_mul <- mkmult_gen_0;

		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2);
			biv_mul.iA(operand1);
			biv_mul.iB(operand2);
		endmethod
		method ActionValue#(Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool)) result();
			Bool done =True;
			Bit#(TSub#(TMul#(2,XLEN),1)) output_mul = biv_mul.oP();
			Tuple2#(Bit#(TSub#(TMul#(2,XLEN),1)),Bool) mul=tuple2(output_mul,done);
			return mul;
		endmethod

	endmodule:mkMuldiv

endpackage:muldiv
