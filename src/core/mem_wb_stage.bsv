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
  import csr::*;

  // package imports
  import GetPut::*; 
  import FIFO:: *;
  import SpecialFIFOs:: *;

  interface Ifc_mem_wb_stage;
    interface RXe#(PIPE2_DS) from_execute;
    interface Put#(Tuple2#(Bit#(XLEN), Bool)) memory_response;
    interface Get#(Tuple2#(Bit#(5), Bit#(XLEN))) commit_rd;
    interface Get#(OpFwding) operand_fwding;
    method Tuple2#(Bit#(PADDR), Bool) flush;
    method CSRtoDecode csrs_to_decode;
	  method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
  endinterface:Ifc_mem_wb_stage

  (*synthesize*)
  module mkmem_wb_stage(Ifc_mem_wb_stage);

    RX#(PIPE2_DS) rx<-mkRX;
    Ifc_csr csr <- mkcsr();

    // wire that captures the response coming from the external memory or cache.
    Wire#(Maybe#(Tuple2#(Bit#(XLEN), Bool))) wr_memory_response <- mkDWire(tagged Invalid);

    // wire that carriues the information for operand forwarding
    Wire#(OpFwding) wr_operand_fwding <- mkDWire(tuple3(0, False, 0));

    // wire that carries the commit data that needs to be written to the integer register file.
    Wire#(Maybe#(Tuple2#(Bit#(5), Bit#(XLEN)))) wr_commit <- mkDWire(tagged Invalid);

    // wire which signals the entire pipe to be flushed.
    Wire#(Tuple2#(Bit#(PADDR), Bool)) wr_flush <- mkDWire(tuple2(?, False));

    // the local epoch register
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);

    `ifdef simulate
      FIFO#(DumpType) dump_ff <- mkLFIFO;
      let prv=tpl_1(csr.csrs_to_decode);
    `endif

    Integer verbosity = `VERBOSITY;

    rule instruction_commit;
      let {committype, reslt, effaddr_csrdata, pc, rd, epoch, trap `ifdef simulate , inst `endif } = 
                                                                                        rx.u.first;
      if(verbosity!=0)
        $display($time, "\tSTAGE3: PC: %h committype: ", pc, fshow(committype), " result: %h",
            reslt);
        $display($time, "\t        csr_data: %h", effaddr_csrdata, " rd: %d", rd, " epoch: %b", 
            epoch, " trap: ", fshow(trap));
      Bit#(PADDR) jump_address=truncate(effaddr_csrdata);
      Flush_type fl = unpack(effaddr_csrdata[valueOf(PADDR)]);
      // continue commit only if epochs match. Else deque the ex fifo
      if(trap matches tagged Interrupt .in)begin
        let newpc<-  csr.take_trap(trap, pc, ?);
        wr_flush<=tuple2(newpc, True);
        rg_epoch <= ~rg_epoch;
        rx.u.deq;
        if(verbosity!=0)
          $display($time, "\tSTAGE3: Received Interrupt: ", fshow(trap));
      end
      else if(rg_epoch==epoch)begin
        // in case of a flush also flip the local epoch register.
        // if instruction is of memory type then wait for response from memory
        if(trap matches tagged Exception .ex)begin
          jump_address<- csr.take_trap(trap, pc, ?);
          fl= Flush;
          rx.u.deq;
          if(verbosity!=0)
            $display($time, "\tSTAGE3: Received Exception: ", fshow(trap));
        end
        else if(committype == MEMORY) begin
          if (wr_memory_response matches tagged Valid .resp)begin
            let {data, err}=resp;
            if(!err)begin // no bus error
              wr_operand_fwding <= tuple3(rd, True, data);
              wr_commit <= tagged Valid (tuple2(rd, data));
              `ifdef simulate 
                dump_ff.enq(tuple5(prv, zeroExtend(pc), inst, rd, data));
              `endif
            end
            else begin
              if(verbosity!=0)
                $display($time, "\tSTAGE3: Received Exception from Memory: ", fshow(resp));
              jump_address<- csr.take_trap(trap, pc, truncate(effaddr_csrdata));
              fl= Flush;
            end
            rx.u.deq;
          end
          else begin
            // is response is not available then indicate that the rd is not yet available.
            wr_operand_fwding <= tuple3(rd, False, 0);
          end
        end
        else if(committype == SYSTEM_INSTR)begin
          let {drain, newpc, dest}<-csr.system_instruction(effaddr_csrdata[11:0], 
                                              effaddr_csrdata[16:12], reslt, effaddr_csrdata[19:17]);
          jump_address=newpc;
          if(drain) 
            fl=Flush;
          `ifdef simulate 
          else
            dump_ff.enq(tuple5(prv, zeroExtend(pc), inst, rd, dest));
          `endif
          wr_operand_fwding <= tuple3(rd, True, dest);
          wr_commit <= tagged Valid (tuple2(rd, dest));
          rx.u.deq;
        end
        else begin
          // in case of regular instruction simply update RF and forward the data.
          wr_operand_fwding <= tuple3(rd, True, reslt);
          wr_commit <= tagged Valid (tuple2(rd, reslt));
          rx.u.deq;
          `ifdef simulate 
            dump_ff.enq(tuple5(prv, zeroExtend(pc), inst, rd, reslt));
          `endif
        end
        
        // if it is a branch/JAL_R instruction generate a flush signal to the pipe. 
        wr_flush<=tuple2(jump_address, (fl==Flush));
        if(fl==Flush)
          rg_epoch <= ~rg_epoch;

      end
      else begin
        if(verbosity!=0)
          $display($time, "\tSTAGE3: Dropping instruction");
        rx.u.deq;
      end
    endrule

    rule increment_instruction_counter(wr_commit matches tagged Valid .x);
      csr.incr_minstret;
    endrule

    interface  memory_response= interface Put
      method Action put (Tuple2#(Bit#(XLEN), Bool) response);
        wr_memory_response <= tagged Valid response;
      endmethod
    endinterface;

    interface from_execute=rx.e;

    interface commit_rd = interface Get
      method ActionValue#(Tuple2#(Bit#(5), Bit#(XLEN)))get if(wr_commit matches tagged Valid .data);
        return data;
      endmethod
    endinterface;

    interface operand_fwding = interface Get
      method ActionValue#(OpFwding) get;
        return wr_operand_fwding;
      endmethod
    endinterface;

    method flush=wr_flush;
    method csrs_to_decode = csr.csrs_to_decode;

	  method Action clint_msip(Bit#(1) intrpt);
      csr.clint_msip(intrpt);
    endmethod
		method Action clint_mtip(Bit#(1) intrpt);
      csr.clint_mtip(intrpt);
    endmethod
		method Action clint_mtime(Bit#(XLEN) c_mtime);
      csr.clint_mtime(c_mtime);
    endmethod
		method Action externalinterrupt(Bit#(1) intrpt);
      csr.externalinterrupt(intrpt);
    endmethod
    `ifdef simulate
      interface dump = interface Get
        method ActionValue#(DumpType) get ;
          dump_ff.deq;
          return dump_ff.first;
        endmethod
      endinterface;
    `endif
  endmodule:mkmem_wb_stage
endpackage:mem_wb_stage
