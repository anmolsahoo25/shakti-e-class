/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
 * Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and / or other materials provided 
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

Author : Neel Gala
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
 */
package stage3;

  // library imports
  import GetPut :: *; 
  import FIFO :: *;
  import SpecialFIFOs :: *;
  import DReg :: *;
  import TxRx :: *;
  import Vector :: *;

  // project imports
  import common_types::*;
  import csr::*;
  `include "common_params.bsv"
  `include "Logger.bsv"

  interface Ifc_stage3;
    interface RXe#(Stage3Common)  rx_stage3_common;
    interface RXe#(Stage3Type)    rx_stage3_type;

  `ifdef rtldump
    interface RXe#(TraceDump)   rx_stage3_dump ;
  `endif

    interface Put#(MemoryResponse) memory_response;
    interface Get#(CommitPacket) commit_rd;
    interface Get#(OpFwding) operand_fwding;
    method Tuple2#(Bit#(`vaddr), Bool) flush;

    method CSRtoDecode mv_csr_decode;
    method Bit#(1) mv_csr_misa_c;

    // side band connections to the csr
    method Action clint_msip(Bit#(1) intrpt);
    method Action clint_mtip(Bit#(1) intrpt);
    method Action clint_mtime(Bit#(64) c_mtime);
    method Action ext_interrupt(Bit#(1) i);
    method Bool csr_updated;
  `ifdef rtldump
    interface Get#(DumpType) dump;
  `endif
  `ifdef triggers
    method Vector#(`trigger_num, TriggerData) mv_trigger_data1;
    method Vector#(`trigger_num, Bit#(XLEN))  mv_trigger_data2;
    method Vector#(`trigger_num, Bool)        mv_trigger_enable;
  `endif
    method Bit#(2) mv_curr_priv;
  endinterface : Ifc_stage3

  (*synthesize*)
  module mkstage3(Ifc_stage3);

    let stage3 = "" ;

    RX#(Stage3Common) ff_stage3_common <- mkRX();
    RX#(Stage3Type)   ff_stage3_type   <- mkRX();
  `ifdef rtldump
    RX#(TraceDump)  ff_stage3_dump   <- mkRX();
  `endif

    Ifc_csr csr <- mkcsr();
    Wire#(Bool) wr_csr_updated <- mkDWire(False);

    // wire that captures the response coming from the external memory or cache.
    Wire#(Maybe#(MemoryResponse)) wr_memory_response <- mkDReg(tagged Invalid);

    // wire that carriues the information for operand forwarding
    Wire#(OpFwding) wr_operand_fwding <- mkDWire(unpack(0));

    // wire that carries the commit data that needs to be written to the integer register file.
    Wire#(Maybe#(CommitPacket)) wr_commit <- mkDWire(tagged Invalid);

    // wire which signals the entire pipe to be flushed.
    Wire#(Tuple2#(Bit#(`vaddr), Bool)) wr_flush <- mkDWire(tuple2(?, False));

    // the local epoch register
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);

    Reg#(Bool) rg_rerun <- mkReg(False);

  `ifdef rtldump
    FIFO#(DumpType) dump_ff <- mkLFIFO;
    Privilege_mode prv = unpack(csr.mv_curr_priv);
  `endif

    function Action deq_rx = action
      ff_stage3_common.u.deq;
      ff_stage3_type.u.deq;
    `ifdef rtldump
      ff_stage3_dump.u.deq;
    `endif
    endaction;

    rule instruction_commit;
      let s3common = ff_stage3_common.u.first;
      let s3type = ff_stage3_type.u.first;
    `ifdef rtldump
      let dump = ff_stage3_dump.u.first();
      `logLevel( stage3, 0, $format("STAGE3: ", fshow(dump)))
    `endif
      `logLevel( stage3, 0, $format("STAGE3: ", fshow(s3common)))
      `logLevel( stage3, 0, $format("STAGE3: ", fshow(s3type)))

      // continue commit only if epochs match. Else deque the ex fifo

      if(rg_epoch == s3common.epoch)begin
        if (rg_rerun) begin
          wr_flush <= tuple2(s3common.pc , True);
          rg_epoch <= ~rg_epoch;
          deq_rx;
          rg_rerun <= False;
          `logLevel( stage3, 0, $format("STAGE3: Rerun Instruction"))
        end
        else begin
          if(s3type matches tagged Trap .t) begin
            let newpc <- csr.take_trap(t.cause, s3common.pc, t.badaddr);
            wr_flush <= tuple2(newpc, True);
            rg_epoch <= ~rg_epoch;
            deq_rx;
            `logLevel( stage3, 0, $format("STAGE3 : Jumping to PC:%h", newpc))
          end

          if(s3type matches tagged Regular .r)begin
            let data = r.rdvalue;
            if(s3common.rd == 0)
                data = 0;

            wr_operand_fwding <= OpFwding{rdaddr : s3common.rd, valid : True, rdvalue : data};
            wr_commit <= tagged Valid (CommitPacket{rdaddr : s3common.rd, rdvalue : data});
            deq_rx;
          `ifdef rtldump 
            dump_ff.enq(tuple5(prv, dump.pc, dump.instruction, s3common.rd, data));
          `endif
          end

          if(s3type matches tagged System .sys) begin
            let {drain, newpc, dest} <- csr.system_instruction(sys.csr_address, sys.rs1_imm, 
                                                                sys.funct3, sys.lpc);
            if(!drain)
              rg_rerun <= True;
            else begin
              wr_flush <= tuple2(newpc, True);
              rg_epoch <= ~rg_epoch;
            end
            if(s3common.rd == 0)
              dest = 0;
            wr_commit <= tagged Valid (CommitPacket{rdaddr : s3common.rd, rdvalue : dest});
            deq_rx;
          `ifdef rtldump 
            dump_ff.enq(tuple5(prv, dump.pc, dump.instruction, s3common.rd, dest));
          `endif
          end

          if(s3type matches tagged Memory .mem)begin
            if(mem.memaccess == Fence)begin
              wr_commit <= tagged Valid (CommitPacket{rdaddr : 0, rdvalue : 0});
              deq_rx;
              rg_rerun <= True;
            `ifdef rtldump 
              dump_ff.enq(tuple5(prv, dump.pc, dump.instruction, 0, 0));
            `endif
            end
            else if(wr_memory_response matches tagged Valid .resp &&& resp.epoch == rg_epoch)begin
              let data = resp.data;
              if( !resp.err )begin
                if(s3common.rd == 0)
                  data = 0;
                wr_operand_fwding <= OpFwding{rdaddr : s3common.rd, valid : True, rdvalue : data};
                wr_commit <= tagged Valid (CommitPacket{rdaddr : s3common.rd, rdvalue : data});
                deq_rx;
              `ifdef rtldump 
                dump_ff.enq(tuple5(prv, dump.pc, dump.instruction, s3common.rd, data));
              `endif
              end
              else begin
                let newpc <- csr.take_trap(mem.memaccess == Load ? `Load_access_fault:
                                            `Store_access_fault, s3common.pc, data);
                wr_flush <= tuple2(newpc, True);
                rg_epoch <= ~rg_epoch;
                deq_rx;
                `logLevel( stage3, 0, $format("STAGE3 : Jumping to PC:%h", newpc))
              end
            end
            else begin
              `logLevel( stage3, 1, $format("STAGE3 : Waiting for response from Fabric"))
            end
          end
        end
      end
      else begin
        deq_rx;
        `logLevel( stage3, 0, $format("STAGE3 : Dropping instruction"))
      end
    endrule

    rule increment_instruction_counter(wr_commit matches tagged Valid .x);
      csr.incr_minstret;
    endrule

    interface rx_stage3_common  = ff_stage3_common.e;
    interface rx_stage3_type    = ff_stage3_type.e;

  `ifdef rtldump
    interface rx_stage3_dump = ff_stage3_dump.e;
  `endif
    interface  memory_response = interface Put
      method Action put (MemoryResponse response);
        wr_memory_response <= tagged Valid response;
      endmethod
    endinterface;


    interface commit_rd = interface Get
      method ActionValue#(CommitPacket)get if(wr_commit matches tagged Valid .data);
        return data;
      endmethod
    endinterface;

    interface operand_fwding = interface Get
      method ActionValue#(OpFwding) get;
        return wr_operand_fwding;
      endmethod
    endinterface;

    method flush = wr_flush;
    method mv_csr_decode = csr.mv_csr_decode;
    method Bool csr_updated = wr_csr_updated;

    method Action clint_msip(Bit#(1) intrpt);
      csr.clint_msip(intrpt);
    endmethod
    method Action clint_mtip(Bit#(1) intrpt);
      csr.clint_mtip(intrpt);
    endmethod
    method Action clint_mtime(Bit#(64) c_mtime);
      csr.clint_mtime(c_mtime);
    endmethod
    method Action ext_interrupt(Bit#(1) ex_i);
      csr.ext_interrupt(ex_i);
    endmethod
    `ifdef rtldump 
      interface dump = interface Get
        method ActionValue#(DumpType) get ;
          dump_ff.deq;
          return dump_ff.first;
        endmethod
      endinterface;
    `endif
    method mv_csr_misa_c = csr.mv_csr_misa_c;
  `ifdef triggers
    method mv_trigger_data1 =   csr.mv_trigger_data1;
    method mv_trigger_data2 =   csr.mv_trigger_data2;
    method mv_trigger_enable =  csr.mv_trigger_enable;
  `endif
    method mv_curr_priv = csr.mv_curr_priv;
  endmodule : mkstage3
endpackage : stage3
