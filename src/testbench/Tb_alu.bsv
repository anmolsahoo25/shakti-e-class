package Tb_alu;
  // import project packages
  import alu::*;
  import common_types::*;
  `include "common_params.bsv"

  module mkTb_alu(Empty);
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
  		Bit#(`paddr) npc='h07000000+4;
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
  endmodule:mkTb_alu;
endpackage

