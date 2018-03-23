package opfetch_execute;
  // packages to be imported
  import GetPut::*;
  import RegFile::*;

  // files to be included
  import isa_defs::*;
  import common_types::*;
  import TxRx ::*;
  import alu::*;
  `include "common_params.bsv"

  interface Inf_op_ex;
    //rs1,rs2,rd,fn,funct3,instruction_type are given by the fetch and decode unit
    interface RXe#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
        Tuple5#(Operand_type,Operand_type,Instruction_type,Access_type,Bit#(32)))) from_fetch_decode_unit;
    
    //result being transfered to the memory and write back
    interface TXe#(Bit#(XLEN)) to_mem_wb_unit;
  
    //rd,valid and value given back by the mem and wb unit for eliminating congestion
    interface Put#(Tuple3#(Bit#(5),Bool,Bit#(64))) wb_mem_to_opfetch;
  
    //rd and value given back by the write back unit
    interface Put#(Tuple2#(Bit#(5),Bit#(64))) wb_to_regFile;
  
    //interface method for the regfile
    method ActionValue#(Tuple2#(Bit#(64),Bit#(64))) operand_provider(Bit#(5) rs1_addr, 
                        Operand_type rs1_type, Bit#(5) rs2_addr, Operand_type rs2_type, 
                        Bit#(PADDR) pc, Bit#(XLEN) imm);
  
    //write in general purpose register
    method Action write_rd(Bit#(5) r, Bit#(XLEN) d, Operand_type rdtype);
  
    //reading the general purpose register
    method Bit#(XLEN) read_igpr(Bit#(5) r);
  
  endinterface
  
  (*synthesize*)
  module mkOpfetch_execute(Inf_op_ex);
  
    // generating the register file
    RegFile#(Bit#(5),Bit#(XLEN)) integer_rf <-mkRegFileWCF(0,31);
    Reg#(Bool) initialize<-mkReg(True);
    Reg#(Bit#(5)) rg_index<-mkReg(0);
  
    // rule to initialize all the registers to 0
    rule initialize_regfile(initialize);
      integer_rf.upd(rg_index,0);
      rg_index<=rg_index+1;
      if(rg_index=='d31)
        initialize<=False;
    endrule 
    // TXRX interface instantiation
    TX#(Bit#(64)) tx<-mkTX;
    RX#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
        Tuple5#(Operand_type,Operand_type,Instruction_type,Access_type,Bit#(32)))) rx<-mkRX;
  
    // receiving the decoded data from the previous stage
    let {fn,rs1_addr_in,rs2_addr_in,rd_addr_in,immediate,word32,funct3,rs1_type_in,rs2_type_in,
         insttype,mem_access,pc_in}=rx.u.first;
  
    // rs1,rs2 will be passed to the register file and the recieve value along with the other 
    // parameters reqiured by the alu function will be passed
    let {op1,op2}=operand_provider(rs1_addr_in,rs1_type_in,rs2_addr_in,rs2_type_in,
                                           pc_in,immediate);
  
    // using the alu function
    // how to obtain pc value from the previous stage,a seperate interface is required or 
    // the output of the previous stage has to be changed
    let {instType,address_op1_result,data_op2_effaddr,meminfo_rs1addr,funct3_addr}=
                      fn_alu(fn,op1,op2,immediate_value,pc_in,insttype,funct3,mem_access,rd,word32);
  
    //passing the result to next stage via fifo
    tx.u.enq(address_op1_result);//passing the obtained result
  
    // interface definition
    interface from_fetch_decode_unit=rx.e;
    
    interface to_mem_wb_unit=tx.e;
    
    interface wb_mem_to_opfetch=interface Put
      method Action put (Tuple3#(Bit#(5),Bool,Bit#(XLEN)) from_mem_to_opfetch );
        {rd,valid,value}<=from_mem_to_opfetch;
        if(rs1==rd&&valid)
          let rs1_value=value;
        else
          rs1_value<=rs1;
        if(rs2==rd&&valid)  
          let rs2_value=value;
        else
          rs2_value<=rs2;   
      endmethod 
    endinterface;
    
    interface wb_to_regFile=interface Put
      method Action put (Tuple2#(Bit#(5),Bit#(XLEN)) from_mem_to_rf );
        {rd,value}<=from_mem_to_rf;
      endmethod
    endinterface;
    
    method ActionValue#(Tuple2#(Bit#(64),Bit#(64))) operand_provider(Bit#(5) rs1_addr, 
      Operand_type rs1_type, Bit#(5) rs2_addr, Operand_type rs2_type, Bit#(PADDR) pc, 
        Bit#(XLEN) imm);
      
      Bit#(XLEN) rs1=0;
      Bit#(XLEN) rs2=0;
    
      if(rs1_type==PC)
        rs1=signExtend(pc);
      else if(rs1_addr==0 && rs1_type==IntegerRF)
        rs1=0;
      else if(rs1_type==IntegerRF)
        rs1=integer_rf.sub(rs1_addr);

      if(rs2_type==Immediate)
        rs2=imm;
      else if(rs2_addr==0 && rs2_type==IntegerRF)
        rs2=0;
      else if(rs2_type==IntegerRF)
        rs2=integer_rf.sub(rs2_addr);
      
      `ifdef verbose $display($time,"\nReg1 :%d : ",rs1_addr,fshow(rs1),"\nReg2 : %d : ",
                                                                      rs2_addr,fshow(rs2)); `endif
      return tuple2(rs1,rs2);
    endmethod

    method Action write_rd(Bit#(5) r, Bit#(XLEN) d, Operand_type rdtype) if(!initialize); 
      `ifdef verbose $display($time,"\tRF: Writing reg: :%d data: %h ",r,d,fshow(rdtype)); `endif
      if(rdtype==IntegerRF)
        if(r!=0)
          integer_rf.upd(r,d);
    endmethod

    method Bit#(XLEN)read_igpr(Bit#(5) r);  
      return integer_rf.sub(r);
    endmethod

  endmodule:mkOpfetch_execute
endpackage
