package shift_add_mul_fsm;
	import common_types::*;
	import StmtFSM::*;
	`include "common_params.bsv"

	interface Ifc_mul;
		method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2,Funct3 funct3);
		method Bit#(XLEN_2) rst(Bool word32);
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
       		else if(funct3==f3_MULHU)begin op1<=operand1;op2<=operand2;is32Bit<=False; end
			rg_counter<=1;
		endmethod
		method Bit#(XLEN_2) rst(Bool is32) if(rg_counter==64);
			Bit#(XLEN_2) final_rst=result;
			return (is32)?~final_rst+1:final_rst;
		endmethod
	endmodule

	//test bench
	module mktest(Empty);
		Ifc_mul dut<- mkMul;
		Reg#(Bit#(XLEN)) a<-mkRegU;
		Reg#(Bit#(XLEN)) b<-mkRegU;
		Reg#(Bit#(2)) count<-mkReg(0);
		//firing order
		Reg#(Bool) fire1<-mkReg(True);
  		Reg#(Bool) fire2<-mkReg(True);
  		Reg#(Bool) fire3<-mkReg(True);
  		Reg#(Bool) fire4<-mkReg(True);

		(*execution_order="one,two,three,four"*)
		(*fire_when_enabled*)
		rule one(fire1);
			fire1<=False;
			a<='h0000000000001234;
			b<='h0000000000001234;

			Bit #(1) sn_op1 = a[valueof(XLEN)-1], sn_op2 = b[valueof(XLEN)-1];
      		Bool take_complement = !(sn_op1 == sn_op2);
      		a<=(unpack(sn_op1))?(~a+1):a;
      		b<=(unpack(sn_op2))?(~b+1):b;

			Bit#(XLEN) gold_res1=a*b;
			dut.get_values(a,b,f3_MUL);
			let res1=dut.rst(False);
			if(res1!=signExtend(gold_res1)) begin $display("error in test one");$finish(0); end
		endrule
		rule two(fire2);
			fire2<=False;
			a<='hffffffffffff1234;
			b<='h0000000000001234;

			Bit #(1) sn_op1 = a[valueof(XLEN)-1], sn_op2 = b[valueof(XLEN)-1];
      		Bool take_complement = !(sn_op1 == sn_op2);
      		a<=(unpack(sn_op1))?(~a+1):a;
      		b<=(unpack(sn_op2))?(~b+1):b;

			Bit#(XLEN) gold_res2=a*b;
			gold_res2=(take_complement)?~gold_res2+1:gold_res2;
			dut.get_values(a,b,f3_MULHSU);
			let res2=dut.rst(take_complement);
			if(res2!=signExtend(gold_res2))begin $display("error in test two"); $finish(0); end
		endrule
		rule three(fire3);
			fire3<=False;
			a<='hffffffffffff1234;
			b<='hffffffffffff1234;

			Bit #(1) sn_op1 = a[valueof(XLEN)-1], sn_op2 = b[valueof(XLEN)-1];
      		Bool take_complement = !(sn_op1 == sn_op2);
      		a<=(unpack(sn_op1))?(~a+1):a;
      		b<=(unpack(sn_op2))?(~b+1):b;

			Bit#(XLEN) gold_res3=a*b;
			gold_res3=(take_complement)?~gold_res3+1:gold_res3;
			dut.get_values(a,b,f3_MULH);
			let res3=dut.rst(take_complement);
			if(res3!=signExtend(gold_res3))begin $display("error in test three"); $finish(0); end
		endrule
		rule four(fire4);
			fire4<=False;
			a<='h8000000000001234;
			b<='h8000000000001234;

			Bit #(1) sn_op1 = a[valueof(XLEN)-1], sn_op2 = b[valueof(XLEN)-1];
      		Bool take_complement = !(sn_op1 == sn_op2);
      		a<=(unpack(sn_op1))?(~a+1):a;
      		b<=(unpack(sn_op2))?(~b+1):b;

			Bit#(XLEN) gold_res4=a*b;
			dut.get_values(a,b,f3_MULHU);
			let res4=dut.rst(False);
			if(res4!=signExtend(gold_res4))begin $display("error in test four"); $finish(0); end
			$finish(0);
		endrule
	endmodule

endpackage
