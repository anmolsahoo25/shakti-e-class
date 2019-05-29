package shift_add_mul;
  import common_types::*;
  `include "common_params.bsv"

  interface Ifc_mul;
    method Action get_values(Bit#(XLEN) operand1,Bit#(XLEN) operand2,Funct3 funct3);
    method ALU_OUT rst;
  endinterface
(*synthesize*)
  module mkMul(Ifc_mul);
    Reg#(Bit#(TMul#(2, XLEN))) op1<-mkReg(0);
    Reg#(Bit#(XLEN)) op2<-mkRegU;
    Reg#(Bit#(7)) rg_counter <-mkReg(0);
    Reg#(Bit#(TMul#(2, XLEN))) rg_product <-mkReg(0);
    Reg#(Bool) is32Bit<-mkReg(False);

    rule shift_add(rg_counter!=0&&rg_counter<=64);
      if(op2[0]==1) 
        rg_product<=rg_product+ op1;//addition
      op1<=op1<<1;//left shift
      op2<=op2>>1;//right shift
      if(rg_counter<=64) 
        rg_counter<=rg_counter+1;
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
      if(funct3==f3_MUL)begin op1<=zeroExtend(operand1);op2<=operand2;is32Bit<=False; end
      else if(funct3==f3_MULHSU)begin op1<=zeroExtend(mop1);op2<=operand2;
                                      is32Bit<=take_complement; end
      else if(funct3==f3_MULH)begin op1<=zeroExtend(mop1);op2<=mop2;is32Bit<=take_complement; end
      else if(funct3==f3_MULHU)begin op1<=zeroExtend(operand1);op2<=operand2;is32Bit<=False; end
      rg_counter<=1;
    endmethod
    method ALU_OUT rst if(rg_counter==64);
      Bit#(XLEN_2) final_rst=rg_product;
      return tuple3(REGULAR, truncate((is32Bit)?~final_rst+1:final_rst), ?);
    endmethod
  endmodule

  //test bench
  (*synthesize*)
  module mktest(Empty);
    Ifc_mul dut<- mkMul;
    Reg#(Bit#(2)) count<-mkReg(0);
    //firing order
      Reg#(Bool) fire1<-mkReg(True);
      Reg#(Bool) fire2<-mkReg(False);
      Reg#(Bool) fire3<-mkReg(False);
      Reg#(Bool) fire4<-mkReg(False);

    rule one;
      fire1<=False;
      Bit#(XLEN) a1='h1000000000001234;
      Bit#(XLEN) b1='h0100000000001234;

      $display($time, "inputs: %h %h", a1, b1);
      Bit #(1) sn_op1 = a1[valueof(XLEN)-1], sn_op2 = b1[valueof(XLEN)-1];
          Bool take_complement = !(sn_op1 == sn_op2);
          a1=(unpack(sn_op1))?(~a1+1):a1;
          b1=(unpack(sn_op2))?(~b1+1):b1;

      Bit#(XLEN) gold_res1=a1*b1;
      dut.get_values(a1,b1,f3_MUL);
    endrule

    rule read_output;
      let res1=dut.rst;
      $display("Ouput: %h ", res1);
      $finish(0);
    endrule
  endmodule

endpackage
