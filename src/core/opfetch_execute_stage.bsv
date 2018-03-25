/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and/or other materials provided 
 with the distribution.  
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Author: Neel Gala, Aditya Mathur
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package opfetch_execute_stage;
  // packages to be imported
  import GetPut::*;
  import RegFile::*;
  import FIFOF::*;

  // files to be included
  import isa_defs::*;
  import common_types::*;
  import TxRx ::*;
  import alu::*;
  `include "common_params.bsv"

  interface Ifc_opfetch_execute_stage;
    //rs1,rs2,rd,fn,funct3,instruction_type are given by the fetch and decode unit
    interface RXe#(PIPE1_DS)  from_fetch_decode_unit;
    
    //result being transfered to the memory and write back
    interface TXe#(PIPE2_DS) to_mem_wb_unit;
  
    //rd,valid and value given back by the mem and wb unit for eliminating congestion
    interface Put#(OpFwding) operand_fwding;
  
    //rd and value given back by the write back unit
    interface Put#(Tuple2#(Bit#(5),Bit#(XLEN))) commit_rd;
    
    // memory request interface in case of Load/Store instruction
    interface Get#(MemoryRequest) memory_request;
  
    method Action flush_from_wb(Bool fl);
  endinterface:Ifc_opfetch_execute_stage
  
  (*synthesize*)
  module mkopfetch_execute_stage(Ifc_opfetch_execute_stage);
     
    // generating the register file
    RegFile#(Bit#(5),Bit#(XLEN)) integer_rf <-mkRegFileWCF(0,31);
    Reg#(Bool) initialize<-mkReg(True);
    Reg#(Bit#(5)) rg_index<-mkReg(0);
    Reg#(Bit#(1)) rg_epoch[2] <- mkCReg(2,0);
    Wire#(OpFwding) wr_opfwding <- mkDWire(unpack(0));
    //Wire#(Maybe#(Tuple2#(Bit#(PADDR),Bool))) wr_memory_request <- mkDWire(tagged Invalid);
    FIFOF#(MemoryRequest) ff_memory_request <- mkSizedFIFOF(2);
  
    function (Tuple3#(Bit#(XLEN),Bit#(XLEN),Bool)) operand_provider(Bit#(5) rs1_addr, Operand1_type 
               rs1_type, Bit#(5) rs2_addr, Operand2_type rs2_type, Bit#(PADDR) pc, Bit#(XLEN) imm);
     
      let {rd,valid,rd_value}=wr_opfwding;
      Bit#(XLEN) rs1=0;
      Bit#(XLEN) rs2=0;
    
      if(rs1_type==PC)
        rs1=signExtend(pc);
      else if(rs1_addr==0)
        rs1=0;
      else if(rs1_addr == rd)
        rs1=rd_value;
      else
        rs1=integer_rf.sub(rs1_addr);

      if(rs2_type==Immediate)
        rs2=imm;
      else if(rs2_addr==0)
        rs2=0;
      else if(rs2_addr == rd)
        rs2 = rd_value;
      else
        rs2=integer_rf.sub(rs2_addr);

      Bool operands_avail=True;
      if( (rs1_addr == rd || rs2_addr == rd) && !valid && rd!=0)
        operands_avail=False;
      return tuple3(rs1,rs2,operands_avail);
    endfunction

  
    // rule to initialize all the registers to 0 on reset
    rule initialize_regfile(initialize);
      integer_rf.upd(rg_index,0);
      rg_index<=rg_index+1;
      if(rg_index=='d31)
        initialize<=False;
    endrule 

    // TXRX interface instantiation
    RX#(PIPE1_DS) rx<-mkRX;
    TX#(PIPE2_DS) tx<-mkTX;
  
    rule fetch_execute_pass(!initialize);
      // receiving the decoded data from the previous stage
      let {fn, rs1, rs2, rd, imm, word32, funct3, rs1_type, rs2_type, insttype, mem_access, 
                                                                  pc, exception, epoch}=rx.u.first;
      // rs1,rs2 will be passed to the register file and the recieve value along with the other 
      // parameters reqiured by the alu function will be passed
      let {op1, op2, available}=operand_provider(rs1, rs1_type, rs2, rs2_type, pc, imm);
      let {committype, reslt, funct3_rs1_csr} = fn_alu(fn, op1, op2, imm, pc, insttype,
                                                                   funct3, mem_access, rd, word32);
      if(epoch==rg_epoch[0])begin
        //passing the result to next stage via fifo
        if(available)begin
          rx.u.deq;
          if(committype == MEMORY)
            ff_memory_request.enq(tuple5(truncate(reslt), imm, mem_access,
                                                                        funct3[1:0], ~funct3[2]));
          tx.u.enq(tuple6(committype,reslt, funct3_rs1_csr, pc, rd, rg_epoch[0]));
        end
      end
      else
        rx.u.deq;
    endrule
  
    // interface definition
    interface from_fetch_decode_unit=rx.e;
    
    interface to_mem_wb_unit=tx.e;
   
    // the memory_wb stage has to ensure that it sends only 0 when there is no data
    // to be forwarded
    interface operand_fwding=interface Put
      method Action put (Tuple3#(Bit#(5),Bool,Bit#(XLEN)) from_mem_to_opfetch );
        wr_opfwding <= from_mem_to_opfetch;
      endmethod 
    endinterface;
    
    interface commit_rd=interface Put
      method Action put (Tuple2#(Bit#(5),Bit#(XLEN)) from_mem_to_rf ) if(!initialize);
        let {rd,value} = from_mem_to_rf;
        if(rd!=0)
          integer_rf.upd(rd,value);
      endmethod
    endinterface;
    
    interface memory_request = interface Get
      method ActionValue#(MemoryRequest) get ;
        ff_memory_request.deq;
        return ff_memory_request.first;
      endmethod
    endinterface;

    method Action flush_from_wb(Bool fl);
      if(fl)
        rg_epoch[1]<=~rg_epoch[1];
    endmethod
  endmodule:mkopfetch_execute_stage
endpackage:opfetch_execute_stage
