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

Author: Neel Gala
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package mem_wb_stage;
  // imports related to the project
  import common_types::*;
  `include "common_params.bsv"
  import TxRx::*;

  // package imports
  import GetPut::*; 

  interface Ifc_mem_wb_stage;
    interface RXe#(PIPE2_DS) from_execute;
    interface Put#(Tuple2#(Bit#(XLEN),Bool)) memory_response;
    interface Get#(Tuple2#(Bit#(5),Bit#(XLEN))) commit_rd;
    interface Get#(OpFwding) operand_fwding;
    method Tuple2#(Bit#(PADDR),Bool) flush;
  endinterface:Ifc_mem_wb_stage

  (*synthesize*)
  module mkmem_wb_stage(Ifc_mem_wb_stage);

    RX#(PIPE2_DS) rx<-mkRX;
   
    // wire that captures the response coming from the external memory or cache.
    Wire#(Maybe#(Tuple2#(Bit#(XLEN),Bool))) wr_memory_response <- mkDWire(tagged Invalid);

    // wire that carriues the information for operand forwarding
    Wire#(OpFwding) wr_operand_fwding <- mkDWire(tuple3(0,False,?));

    // wire that carries the commit data that needs to be written to the integer register file.
    Wire#(Maybe#(Tuple2#(Bit#(5),Bit#(XLEN)))) wr_commit <- mkDWire(tagged Invalid);

    // wire which signals the entire pipe to be flushed.
    Wire#(Tuple2#(Bit#(PADDR),Bool)) wr_flush <- mkDWire(tuple2(?,False));

    // the local epoch register
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);

    rule instruction_commit;
      let {committype, reslt,funct3_rs1_csr,pc,rd, epoch}=rx.u.first;

      // continue commit only if epochs match. Else deque the ex fifo
      if(rg_epoch==epoch)begin

        // if it is a branch/JAL_R instruction generate a flush signal to the pipe. 
        Flush_type fl = unpack(funct3_rs1_csr[20]);
        wr_flush<=tuple2(truncate(reslt),(fl==Flush));

        // in case of a flush also flip the local epoch register.
        if(fl==Flush)
          rg_epoch <= ~rg_epoch;

        // if instruction is of memory type then wait for response from memory
        if(committype == MEMORY) begin
          if (wr_memory_response matches tagged Valid .resp)begin
            let {data,err}=resp;
            if(!err) // no bus error
              wr_operand_fwding <= tuple3(rd,True,data);
            rx.u.deq;
          end
          else begin
            // is response is not available then indicate that the rd is not yet available.
            wr_operand_fwding <= tuple3(rd,False,?);
          end
        end
        else if(committype == SYSTEM_INSTR)begin
          $display("CSR SUPPORT NOR PRESENT YET");
          $finish(0);
          wr_operand_fwding <= tuple3(rd,False,?);
          rx.u.deq;
        end
        else begin
          // in case of regular instruction simply update RF and forward the data.
          wr_operand_fwding <= tuple3(rd,True, reslt);
          wr_commit <= tagged Valid (tuple2(rd,reslt));
          rx.u.deq;
        end
      end
      else begin
        rx.u.deq;
      end
    endrule

    interface  memory_response= interface Put
      method Action put (Tuple2#(Bit#(XLEN),Bool) response);
        wr_memory_response <= tagged Valid response;
      endmethod
    endinterface;

    interface from_execute=rx.e;

    interface commit_rd = interface Get
      method ActionValue#(Tuple2#(Bit#(5),Bit#(XLEN))) get if(wr_commit matches tagged Valid .data);
        return data;
      endmethod
    endinterface;

    interface operand_fwding = interface Get
      method ActionValue#(OpFwding) get;
        return wr_operand_fwding;
      endmethod
    endinterface;

    method flush=wr_flush;

  endmodule:mkmem_wb_stage
endpackage:mem_wb_stage
