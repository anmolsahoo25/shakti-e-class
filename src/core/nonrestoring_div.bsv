package nonrestoring_div;
  import common_types::*;
  `include "common_params.bsv"

  interface Ifc_restoringDiv;
    method Action put_values(Bit#(XLEN) op1,Bit#(XLEN) op2);
    method ALU_OUT rst_div();
  endinterface
  (*synthesize*)
  module mknr_div(Ifc_restoringDiv);
    Reg#(Bit#(XLEN)) q<-mkRegU;
    Reg#(Bit#(XLEN)) m<-mkRegU;
    Reg#(Bit#(XLEN)) a<-mkReg(0);
    Reg#(Bit#(XLEN)) a_new<-mkRegU;
    Reg#(Bit#(XLEN)) q_new<-mkRegU;
    Reg#(Bit#(TMul#(XLEN,2))) x<-mkRegU;
    Reg#(Bit#(7)) rg_counter<-mkReg(0);
    rule shift_add(rg_counter!=0&&rg_counter<=64);
    //concatinating and shifting so that the MSB of q goes into the 
    //LSB of a
      x<={a,q}<<1;
      q_new<=x[valueOf(TSub#(XLEN,1)):0];
    //if a is negative then add m to it or if a is positive
    // subtract m from a (or add 2's complement of m)
      a_new<=(a[valueOf(XLEN)-1]==1)?
      (x[valueOf(TSub#(TMul#(XLEN,2),1)):valueOf(XLEN)]+m):
      x[valueOf(TSub#(TMul#(XLEN,2),1)):valueOf(XLEN)]+(~m+1);
      //updating the value of a
      a<=a_new;
    //updating q and changing the last bit of q_new with respect to the 
    //sign of a; if a is positive then q[0]=1 else q[0]=0
      q<=(a_new[valueOf(XLEN)-1]==1)?{q_new[valueOf(XLEN)-1:1],1'b0}:
        {q_new[valueOf(XLEN)-1:1],1'b1} ;
				
      if(rg_counter<=64)	
      rg_counter<=rg_counter+1;	
    endrule
    method Action put_values(Bit#(XLEN) op1,Bit#(XLEN) op2) if(rg_counter==0);
      m<=op1;//divisor
      q<=op2;//dividend
      rg_counter<=1;
    endmethod 
    method ALU_OUT rst_div()if(rg_counter==64&&rg_counter!=0);
      Bit#(XLEN) div_rst=q;//sending only q for the testing purpose
      return tuple3(REGULAR, truncate(div_rst), ?);
    endmethod
  endmodule
endpackage
