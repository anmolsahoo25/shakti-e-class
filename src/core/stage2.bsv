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

Author : Neel Gala, Aditya Mathur
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
 */
package stage2;
  // library packages 
  import GetPut::*;
  import RegFile::*;
  import FIFOF::*;
  import DReg::*;
  import UniqueWrappers ::*;
  import TxRx ::*;
  import Vector :: *;
  import SpecialFIFOs :: *;

  // project packages
  import common_types::*;
  import alu::*;
  `include "common_params.bsv"
  `include "Logger.bsv"

  interface Ifc_stage2;
    // interface to send decoded instruction to the next stage
    interface RXe#(STAGE1_operands)   rx_stage1_operands;
    interface RXe#(STAGE1_meta)       rx_stage1_meta;
    interface RXe#(STAGE1_control)    rx_stage1_control;
    
    // interface to send the alu result to the next stage.
    interface TXe#(Stage3Common)      tx_stage3_common;
    interface TXe#(Stage3Type)        tx_stage3_type;

  `ifdef rtldump
    interface RXe#(TraceDump)       rx_stage1_dump ;
    interface TXe#(TraceDump)       tx_stage3_dump;  
  `endif
  
    // memory request interface in case of Load / Store instruction
    interface Get#(MemoryRequest) memory_request;
   
    //rd, valid and value given back by the mem and wb unit for eliminating congestion
    interface Put#(OpFwding) operand_fwding;
    // receive flush form stage3 in case of traps
    method Action ma_update_wEpoch;
    // receive the 'c' bit value of the misa-csr from stage3
    method Action ma_csr_misa_c (Bit#(1) c);
  `ifdef muldiv
    method DelayedOut mv_delayed_output;
  `endif
  
  `ifdef triggers
    (*always_ready, always_enabled*)
    method Action ma_trigger_data1(Vector#(`trigger_num, TriggerData) t);
    (*always_ready, always_enabled*)
    method Action ma_trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);
    (*always_ready, always_enabled*)
    method Action ma_trigger_enable(Vector#(`trigger_num, Bool) t);
  `endif

    method Tuple2#(Bit#(`vaddr), Bool) mv_redirection;
  `ifdef arith_trap
    method Action ma_arithtrap_en(Bit#(1) arith_en);
  `endif
  `ifdef perfmonitors
    method Bit#(1) mv_event_jumps;
    method Bit#(1) mv_event_branch_taken;
    method Bit#(1) mv_event_branch_nottaken;
    method Bit#(1) mv_event_muldiv;
    method Bit#(1) mv_event_csr_ops;
    method Bit#(1) mv_event_raw_stalls;
    method Bit#(1) mv_event_redirection;
  `endif
  endinterface : Ifc_stage2
  
  (*synthesize*)
  module mkstage2(Ifc_stage2);

    function Tuple3#(Bool, Bit#(XLEN), Bit#(XLEN)) get_latest_ops(Bit#(5) rs1addr, Bit#(XLEN) op1, 
                                                Op1type rs1type, Bit#(5) rs2addr, Bit#(XLEN) op2, 
                                                Op2type rs2type, Bit#(5) rdaddr, Bit#(XLEN) rd,
                                                                  Bool valid);
      Bit#(XLEN) rs1 = (rs1addr == rdaddr) ? rd : op1;
      Bit#(XLEN) rs2 = (rs2addr == rdaddr) ? rd : op2;
      Bool avail = !(((rs1addr == rdaddr ) || 
                      (rs2addr == rdaddr )) && !valid && rdaddr != 0);
      return tuple3(avail, rs1, rs2);
    endfunction

    let stage2 = "";

    Wire#(Bit#(1)) wr_misa_c <- mkWire();
     
    // generating the register file
    Reg#(Bit#(1)) rg_wEpoch <- mkReg(0);
    Reg#(Bit#(1)) rg_eEpoch <- mkReg(0);

    Reg#(OpFwding) wr_opfwding <- mkDWire(unpack(0));
    FIFOF#(MemoryRequest) ff_memory_request <- mkLFIFOF();

    Ifc_alu alu <- mkalu();

  `ifdef atomic
    Reg#(Maybe#(Bit#(`paddr))) rg_loadreserved_addr <- mkReg(tagged Invalid);
  `endif
  
    // the fifo to communicate with the previous stage.
    RX#(STAGE1_operands) ff_stage1_operands <- mkRX;
    RX#(STAGE1_meta) ff_stage1_meta <- mkRX;
    RX#(STAGE1_control) ff_stage1_control <- mkRX;

    TX#(Stage3Common) ff_stage3_common <- mkTX;
    TX#(Stage3Type) ff_stage3_type <- mkTX;

    Wire#(Tuple2#(Bit#(`vaddr), Bool)) wr_redirection <- mkDWire(tuple2(?,False));
    
    Bit#(2) curr_epoch = {rg_eEpoch, rg_wEpoch};


  `ifdef rtldump
    RX#(TraceDump) ff_stage1_dump <- mkRX;
    TX#(TraceDump) ff_stage3_dump <- mkTX;
  `endif
  
  `ifdef triggers
    Vector#(`trigger_num, Wire#(TriggerData)) v_trigger_data1 <- replicateM(mkWire());
    Vector#(`trigger_num, Wire#(Bit#(XLEN))) v_trigger_data2 <- replicateM(mkWire());
    Vector#(`trigger_num, Wire#(Bool)) v_trigger_enable <- replicateM(mkWire());
  `endif
  
  `ifdef perfmonitors
    Wire#(Bit#(1)) wr_event_jumps             <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_branch_taken      <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_branch_nottaken   <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_muldiv            <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_csr_ops           <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_raw_stalls        <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_redirection       <- mkDWire(0);
  `endif

    
    function Action deq_rx = action
      ff_stage1_operands.u.deq;
      ff_stage1_meta.u.deq;
      ff_stage1_control.u.deq;
    `ifdef rtldump  
      ff_stage1_dump.u.deq; 
    `endif          
    endaction;

    rule fetch_execute_pass;

      let ops = ff_stage1_operands.u.first;
      let meta = ff_stage1_meta.u.first.meta;
      let opaddr = ff_stage1_meta.u.first.op_addr;
      let optype = ff_stage1_meta.u.first.op_type;
    `ifdef compressed
      let is_compressed = ff_stage1_meta.u.first.compressed;
    `endif
      let control = ff_stage1_control.u.first;
    `ifdef rtldump
      let dump = ff_stage1_dump.u.first;
    `endif
      let {valid, op1, op2} = get_latest_ops(opaddr.rs1addr, ops.op1, optype.rs1type, 
                                              opaddr.rs2addr, ops.op2, optype.rs2type, 
                                              wr_opfwding.rdaddr, wr_opfwding.rdvalue, 
                                              wr_opfwding.valid);
      Bit#(XLEN) _op1 = (optype.rs1type == PC)? control.pc : op1;
      Bit#(XLEN) _op2 = (optype.rs2type == Constant4)? 'd4: (optype.rs2type == Constant2)? 'd2:
                        (optype.rs2type == Immediate)? signExtend(meta.immediate) : op2;
      Bit#(XLEN) op3  = meta.inst_type == MEMORY || meta.inst_type == JALR? op1 : control.pc;
      Bit#(3) funct3  = truncate(meta.funct);
      Bit#(4) fn      = truncateLSB(meta.funct);
    `ifdef rtldump      
      `logLevel( stage2, 0, $format("STAGE2 : ", fshow(dump)))
    `endif
      `logLevel( stage2, 1, $format("STAGE2 : OPs: ", fshow(ops)))
      `logLevel( stage2, 1, $format("STAGE2 : Meta: ", fshow(meta)))
      `logLevel( stage2, 1, $format("STAGE2 : OpAddr: ", fshow(opaddr)))
      `logLevel( stage2, 1, $format("STAGE2 : OpType: ", fshow(optype)))
      `logLevel( stage2, 1, $format("STAGE2 : Control: ", fshow(control)))
      `logLevel( stage2, 1, $format("STAGE2 : Fwding : Valid:%b Op1:%h Op2:%h", valid, op1, op2)) 

      // TODO: are sure you want to trigger mul when inputs are not available?
      // TODO: you are sending inputs to mul even if epochs do not match? 
      let aluout <- alu.inputs(fn, _op1, _op2, op3, signExtend(meta.immediate), 
                              meta.inst_type, funct3, meta.memaccess, 
                              `ifdef RV64 meta.word32, `endif wr_misa_c, truncate(control.pc)
                              `ifdef triggers
                                ,readVReg(v_trigger_data1), readVReg(v_trigger_data2)
                                ,readVReg(v_trigger_enable)
                              `endif ); 

      `logLevel( stage2, 1, $format("STAGE2 : AluOut: ", fshow(aluout)))

      Bit#(2) epoch = control.epoch;
      if(epoch == curr_epoch)begin
        if(valid)begin
          deq_rx; 
          wr_redirection <= tuple2(aluout.effective_addr, aluout.redirect);
          if(aluout.redirect)
            rg_eEpoch <= ~rg_eEpoch;
        `ifdef atomic
          if(meta.inst_type == MEMORY) begin 
            if({funct3[0],fn} == 'b00101 && meta.memaccess == Atomic) begin // LR
              rg_loadreserved_addr <= tagged Valid truncate(aluout.effective_addr);
              meta.memaccess = Load;
              `logLevel( stage2, 1, $format("STAGE2: Reserving Addr: %h", aluout.effective_addr))
            end
            else
              rg_loadreserved_addr <= tagged Invalid;

            if({funct3[0],fn} == 'b00111 && meta.memaccess == Atomic)begin // SC
              `logLevel( stage2, 1, $format("STAGE2: SC-ADDR:%h RES-ADDR: ",
                                                aluout.effective_addr, fshow(rg_loadreserved_addr)))
              if(rg_loadreserved_addr matches tagged Valid .a &&& 
                                                      truncate(aluout.effective_addr) != a)begin
                aluout.cmtype = REGULAR;
                aluout.aluresult = 1;
              end
              else if(rg_loadreserved_addr matches tagged Invalid) begin
                aluout.cmtype = REGULAR;
                aluout.aluresult = 1;
              end
              else begin
                meta.memaccess = Store;
              end
            end
          end
        `endif
          if(aluout.cmtype == MEMORY && meta.memaccess != Fence)
            ff_memory_request.enq(MemoryRequest{addr : aluout.effective_addr, data : _op2, 
                                                memaccess : meta.memaccess, size : funct3, 
                                                epoch : rg_wEpoch
                                              `ifdef atomic
                                                ,atomic_op: {funct3[0],fn}
                                              `endif });
        // -------------------------- Derive types for Next stage --------------------------- //
            let s3common = Stage3Common{pc      : control.pc, 
                                        rd      : opaddr.rd,
                                        epoch   : rg_wEpoch};
            let s3memory = Stage3Memory{memaccess   : meta.memaccess
                                      `ifdef triggers
                                        ,address     : aluout.effective_addr
                                        ,size        : funct3[1 : 0]
                                      `endif };
            let s3trap = Stage3Trap { cause     : aluout.cause,
                                      badaddr   : aluout.effective_addr };

            let s3regular = Stage3Regular { rdvalue   : aluout.aluresult 
                                          `ifdef muldiv
                                            ,delayed   : !aluout.done
                                          `endif };

            let s3system = Stage3System { funct3     : funct3,
                                          lpc        : truncate(control.pc),
                                          rs1_imm    : funct3[2] == 1?
                                                              zeroExtend(meta.immediate[16 : 12]):
                                                              truncate(aluout.aluresult),
                                          csr_address : truncate(meta.immediate) };

            Stage3Type s3type = case(aluout.cmtype) matches 
                                  REGULAR       : tagged Regular s3regular;
                                  SYSTEM_INSTR  : tagged System s3system;
                                  TRAP          : tagged Trap s3trap;
                                  MEMORY        : tagged Memory s3memory;  endcase;
        // ---------------------------------------------------------------------------------- //
            ff_stage3_common.u.enq(s3common);
            ff_stage3_type.u.enq(s3type);
          `ifdef rtldump
            ff_stage3_dump.u.enq(dump);
          `endif
          `ifdef perfmonitors
            wr_event_jumps <= pack(meta.inst_type==JALR || meta.inst_type == JAL);
            wr_event_branch_taken <=  pack (meta.inst_type == BRANCH && aluout.redirect);
            wr_event_branch_nottaken <= pack (meta.inst_type == BRANCH && !aluout.redirect);
          `ifdef muldiv
            wr_event_muldiv <=  pack(meta.inst_type == MULDIV);
          `endif
            wr_event_csr_ops <= pack(meta.inst_type == SYSTEM_INSTR);
            wr_event_redirection <= pack(aluout.redirect);
          `endif
        end
      end
      else begin
        `logLevel( stage2, 0, $format("STAGE2 : Dropping instruction due to mis - match"))
        deq_rx;
      `ifdef perfmonitors
        wr_event_raw_stalls <= 1;
      `endif
      end
    endrule
 
    // interface definition

    // interface to receive decoded instruction to the previous stage
    interface rx_stage1_operands  = ff_stage1_operands.e;
    interface rx_stage1_meta      = ff_stage1_meta.e;
    interface rx_stage1_control   = ff_stage1_control.e;
		
    interface tx_stage3_common    = ff_stage3_common.e;
    interface tx_stage3_type      = ff_stage3_type.e;

  `ifdef rtldump
    interface rx_stage1_dump = ff_stage1_dump.e;
    interface tx_stage3_dump = ff_stage3_dump.e;  
  `endif
   
    // the memory_wb stage has to ensure that it sends only 0 when there is no data
    // to be forwarded
    interface operand_fwding = interface Put
      method Action put (OpFwding fwd);
        wr_opfwding <= fwd;
      endmethod 
    endinterface;
    
    interface memory_request = interface Get
      method ActionValue#(MemoryRequest) get ;
        ff_memory_request.deq;
        return ff_memory_request.first;
      endmethod
    endinterface;

    method Action ma_update_wEpoch; //fence integration
      `logLevel( stage2, 0, $format("STAGE2: Received Flush from WB"))
      rg_wEpoch <= ~rg_wEpoch;
      ff_memory_request.clear();
    endmethod

    method Action ma_csr_misa_c (Bit#(1) c);
      wr_misa_c <= c;
    endmethod

    method mv_redirection = wr_redirection;
  `ifdef muldiv
    method mv_delayed_output = alu.mv_delayed_output;
  `endif

  `ifdef triggers
    method Action ma_trigger_data1(Vector#(`trigger_num, TriggerData) t);
      for(Integer i = 0; i<`trigger_num; i = i+1)
        v_trigger_data1[i] <= t[i];
    endmethod
    method Action ma_trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);
      for(Integer i = 0; i<`trigger_num; i = i+1)
        v_trigger_data2[i] <= t[i];
    endmethod
    method Action ma_trigger_enable(Vector#(`trigger_num, Bool) t);
      for(Integer i = 0; i<`trigger_num; i = i+1)
        v_trigger_enable[i] <= t[i];
    endmethod
  `endif

  `ifdef arith_trap
    method Action ma_arithtrap_en(Bit#(1) arith_en);
      alu.ma_arithtrap_en(arith_en);
    endmethod
  `endif
  `ifdef perfmonitors
    method  mv_event_jumps           = wr_event_jumps;
    method  mv_event_branch_taken    = wr_event_branch_taken;
    method  mv_event_branch_nottaken = wr_event_branch_nottaken;
    method  mv_event_muldiv          = wr_event_muldiv;
    method  mv_event_csr_ops         = wr_event_csr_ops;
    method  mv_event_raw_stalls      = wr_event_raw_stalls;
    method  mv_event_redirection     = wr_event_redirection;
  `endif
  endmodule : mkstage2
endpackage : stage2
