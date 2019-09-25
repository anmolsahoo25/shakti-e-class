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

Author : Neel Gala, Aditya Mathur, Deepa Sarma
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
 */
package stage1;

  // library packages 
  import GetPut::*;
  import TxRx	::*;
  import FIFOF ::*;
  import SpecialFIFOs ::*;
  import Vector :: *;

  // project packages
  import common_types::*;
  import decode::*;
  import registerfile :: *;
  `include "common_params.bsv"
  `include "Logger.bsv"
`ifdef debug
  import debug_types :: *; // for importing the debug abstract interface
`endif
`ifdef compressed
  import decompress :: *;
`endif

  // ----------------------------- local type definitions -------------------------------------- //
  typedef enum {CheckPrev, None} ActionType deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(16) instruction;
    Bit#(2) epoch;
  } PrevMeta deriving(Eq, Bits, FShow);
  // ------------------------------------------------------------------------------------------- //

  // Interface for the fetch and decode unit
  interface Ifc_stage1;

    //(* doc = "subifc: interface to send request to the fabric " *)
    interface Get#(InstRequest)         inst_request;

    //(* doc = "subifc: interface to receive response from fabric " *)
    interface Put#(InstResponse)        inst_response;

    //(*doc = "subifc: interface to send operands to the next stage" *)
    interface TXe#(STAGE1_operands)     tx_stage1_operands;
    //(*doc = "subifc: interface to send op-type and op-addr to the next stage" *)
    interface TXe#(STAGE1_meta)         tx_stage1_meta;
    // (*doc = "subifc: interface to send instruction control information to the next stage" *)
    interface TXe#(STAGE1_control)      tx_stage1_control;
  `ifdef rtldump
    interface TXe#(TraceDump)         tx_stage1_dump ;
  `endif
  
    // (*doc = "subifc: rd and value given back by the write back unit" *)
    interface Put#(CommitPacket)        commit_rd;

    // current csr status registers
    (*always_ready*)
    // (*doc = "method: receives flush from later stages of the pipe" *)
    method Action ma_flush( Bit#(`vaddr) newpc);
    (*always_ready, always_enabled*)
    // (*doc = "method: receives the current value of the 'c' field of the MISA CSR" *)
    method Action ma_csr_misa_c (Bit#(1) c);
    (*always_ready, always_enabled*)
    // (*doc = "method: receives the current value of any interrupt activity on the core" *)
    method Action ma_interrupt(Bool i);
    (*always_ready, always_enabled*)
    // (*doc = "method: receives multiple csrs for decode purposes" *)
    method Action ma_csr_decode (CSRtoDecode c);

    // (*doc = "method: recives epoch updates from stage2" *)
    method Action ma_update_eEpoch();
    // (*doc = "method: recives epoch updates from stage3" *)
    method Action ma_update_wEpoch();

  `ifdef debug
    // (*doc = "method: recives registerfile access packet from the debugger" *)
    method ActionValue#(Bit#(XLEN)) mav_debug_access_gprs(AbstractRegOp cmd);

    // debug related info checking interrupts
    (*always_enabled, always_ready*)
    // (*doc = "method: recives the current status information from the Debugger" *)
    method Action ma_debug_status (DebugStatus status);
  `endif
  `ifdef triggers
    (*always_ready, always_enabled*)
    //(*doc = "method: receives the TDATA1 from the csrs" *)
    method Action ma_trigger_data1(Vector#(`trigger_num, TriggerData) t);

    (*always_ready, always_enabled*)
    //(*doc = "method:  receives the TDATA2 register from the csrs for comparison" *)
    method Action ma_trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);

    (*always_ready, always_enabled*)
    // (*doc = "method: receives a vector indicating which triggers are currently enabled" *)
    method Action ma_trigger_enable(Vector#(`trigger_num, Bool) t);
  `endif

  endinterface : Ifc_stage1

  (*synthesize*)
  (*preempts = "ma_flush, wait_for_interrupt"*)
  (*preempts = "ma_flush, process_instruction"*)
`ifdef debug
  (*conflict_free="mav_debug_access_gprs,commit_rd_put"*)
`endif
  (*doc = "module: This Module implements the instruction-fetch + decode + operand-fetch \
  functionality of the pipeline. \
  \
  1. Instruction-Fetch phase:  It generates the new PC, sends the PC to the fabric and in \
  return expects the instruction response from the fabric. The PC is updated\
  either when a flush is received from any of the later stages or is simple incremented by 4 or 2\
  (in case of compressed instructions)\
  \
  2. Decode phase: Once the instruction is received, it is checked if the instruction is a\
  compressed instruction or not. If so, then it passes through a decompressor which then converts\
  the compressed instruction into its 32-bit equivalent encoding. The 32-bit instructions are\
  then decoded to capture various informations. Most of the decoded information which holds\
  control-flow information is passed on to the next stage for execution.\
  \
  3. Operand Fetch: The operand addresses are generated by the decoder and then used to access the\
  register file. The registerfile itself forward the data of the commit happening in the same\
  cycle. The fetched operands are then passed on to the next stage.\
  \
  4. The debugger also is given access to the registerfile through this module.\
  \
  5. Triggers are also supported to capture events related to program counter or instruction match\
  "*)
  module mkstage1#(Bit#(`vaddr) resetpc)(Ifc_stage1);

    let stage1 = ""; // for logger

    // ------------------------------------ Start instantiations --------------------------------//

    (*doc = "wire: this wire carries the current value of the misa_c csr field" *)
    Wire#(Bit#(1)) wr_csr_misa_c <- mkWire();

    (*doc = "wire: this wire caries the current value of certain csrs" *)
    Wire#(CSRtoDecode) wr_csr_decode <- mkWire();

    (*doc = "reg: register to hold the address of the next request to the fabric." *)
    Reg#(Bit#(`vaddr)) rg_fabric_request[2] <- mkCReg(2, 0);

    (*doc = "reg: register to hold the PC value of the instruction to be decoded." *)
    Reg#(Bit#(`vaddr)) rg_pc <- mkReg(0);

    (*doc = "reg: holds the current epoch values controlled by the stage2."*)
    Reg#(Bit#(1)) rg_eEpoch <- mkReg(0);

    (*doc = "reg: holds the current epoch values controlled by the stage3."*)
    Reg#(Bit#(1)) rg_wEpoch <- mkReg(0);

    (*doc = "reg: This register implements a simple state - machine which indicates how the \
    instruction should be extracted from the cache response." *)
    Reg#(ActionType) rg_action <- mkReg(None);

  `ifdef compressed
    Reg#(Bool) rg_discard_lower <- mkReg(False);
    Reg#(PrevMeta) rg_prev <- mkReg(unpack(0));
  `endif

    (*doc = "wire: This wire will be set if any interrupts have been detected by the core" *)
    Wire#(Bool) wr_interrupt <- mkWire();

    (*doc = "reg: this is register it set to True when a WFI instruction is executed. It set to \
    False, when an interrupt has been received or there is a flush from the write - back stage." *)
    Reg#(Bool) rg_wfi <- mkReg(False);

    (*doc = "fifo: to hold the instruction response from the fabric" *)
    FIFOF#(InstResponse) ff_memory_response <- mkSizedFIFOF(2);
    
    (*doc = "submod: operand register file" *)
    Ifc_registerfile integer_rf <- mkregisterfile;
    
    (*doc = "reg: register to indicate that the RegFile is being initialized to all zeros" *)
    Reg#(Bool) rg_initialize<-mkReg(True);

    (*doc = "reg: index into the Regfile during initialization sequence." *)
    Reg#(Bit#(5)) rg_index<-mkReg(0);

    // the fifo to communicate with the next stage.
    TX#(STAGE1_operands) ff_stage1_operands <- mkTX;
    TX#(STAGE1_meta) ff_stage1_meta <- mkTX;
    TX#(STAGE1_control) ff_stage1_control <- mkTX;
  `ifdef rtldump
    TX#(TraceDump) ff_stage1_dump <- mkTX;
  `endif

  
  `ifdef debug
    (*doc = "wire: This wire will capture info about the current debug state of the core" *)
    Wire#(DebugStatus) wr_debug_info <- mkWire();

    (*doc = "reg: This register indicates when an instruction passed the decode stage after a \
    resume request is received while is step is set." *)
    Reg#(Bool) rg_step_done <- mkReg(False);
  `endif

  `ifdef triggers
    (*doc = "vector: Array of wires capturing the tdata1 values from csr" *)
    Vector#(`trigger_num, Wire#(TriggerData)) v_trigger_data1 <- replicateM(mkWire());
    (*doc = "vector: Array of wires capturing the tdata2 values from csr" *)
    Vector#(`trigger_num, Wire#(Bit#(XLEN))) v_trigger_data2 <- replicateM(mkWire());
    (*doc = "vector: Array of wires capturing which triggers are enabled currently" *)
    Vector#(`trigger_num, Wire#(Bool)) v_trigger_enable <- replicateM(mkWire());
  `endif

    Bit#(2) curr_epoch = {rg_eEpoch, rg_wEpoch};
    // ----------------------------------End instantiations ------------------------------------ //

  `ifdef triggers
    // (*doc = "func: function to check triggers" *)
    function ActionValue#(Tuple2#(Bool, Bit#(`causesize))) fn_check_trigger (Bit#(`vaddr) pc, 
                          Bit#(32) instr `ifdef compressed, Bool compressed `endif ) = actionvalue
      Bool trap = False;
      Bit#(`causesize) cause = `Breakpoint;
      Bit#(XLEN) compare_value ;
      Bool chain = False;
      for(Integer i = 0; i < `trigger_num; i = i+1)begin
        `logLevel( stage1, 3, $format("STAGE1 : Trigger[%2d] Data1: ", i, 
                                      fshow(v_trigger_data1[i])))
        `logLevel( stage1, 3, $format("STAGE1 : Trigger[%2d] Data2: ", i, 
                                      fshow(v_trigger_data2[i])))
        `logLevel( stage1, 3, $format("STAGE1 : Trigger[%2d] Enable: ", i, 
                                      fshow(v_trigger_enable[i])))
        if(v_trigger_enable[i] &&& v_trigger_data1[i] matches tagged MCONTROL .mc &&& 
                              ((!trap && !chain) || (chain && trap)) &&& mc.execute == 1)begin
          Bit#(XLEN) trigger_compare = `ifdef compressed 
                    (compressed && mc.size == 2) ? zeroExtend(v_trigger_data2[i][15 : 0]) : `endif 
                                                  v_trigger_data2[i];
          if(mc.select == 0)
            compare_value = pc;
          else
            compare_value = zeroExtend(instr);

          if(mc.matched == 0)begin
            if(trigger_compare == compare_value)
              trap = True;
            else if(chain)
              trap = False;
          end
          if(mc.matched == 2)begin
            if(compare_value >= trigger_compare)
              trap = True;
            else if(chain)
              trap = False;
          end
          if(mc.matched == 3)begin
            if(compare_value < trigger_compare)
              trap = True;
            else if(chain)
              trap = False;
          end

        `ifdef debug
          if(trap && mc.action_ == 1)begin
            cause = `HaltTrigger;
            cause[`causesize - 1] = 1;
          end
        `endif
          chain = unpack(mc.chain);
        end
      end
      return tuple2(trap, cause);
    endactionvalue;
  `endif

    // (*doc = "func: function to access the register file and return the operands" *)
    function STAGE1_operands fn_access_rf (Bit#(5) rs1addr, Bit#(5) rs2addr);
                                        
      Bit#(XLEN) rs1irf = integer_rf.sub(rs1addr);
      Bit#(XLEN) rs2irf = integer_rf.sub(rs2addr);

      return STAGE1_operands{op1 : rs1irf, op2 : rs2irf};
    endfunction
    // ---------------------- End local function definitions ------------------//

    // -------------------------------------- start rules ------------------------------------- //

    (*doc = "rule: initialize all the registers to 0 on reset" *)
    rule initialize_regfile(rg_initialize);
      `logLevel( stage1, 1, $format("STAGE1: Initializing the RF. Index: %d", rg_index))
      integer_rf.upd(rg_index,0);
      rg_index<=rg_index+1;
      if(rg_index=='d31) begin
        rg_initialize<=False;
        rg_fabric_request[1] <= resetpc;
        rg_pc <= resetpc;
      end
    endrule 

    (*doc = "rule:This rule is fired when the core has executed the WFI instruction and waiting \
    for an intterupt to the core to resume fetch" *)
    rule wait_for_interrupt(rg_wfi && !rg_initialize);
      if(wr_interrupt)
        rg_wfi <= False;
      `logLevel( stage1, 0, $format("STAGE1 : Waiting for Interrupt. wr_interrupt: %b",
                                    wr_interrupt))
    endrule

    (* doc = "rule:This rule will receive the instruction from the memory subsystem and decide if \
    the instruction is compressed or not. The final instruction is then sent to the next stage.\
    To extract the instruction from the memory response a state machine is implemented.\
    \
    1. First the epochs are compared and if a mis - match is observed then the response is \
    dropped without any other changes to the state of the module.\
    2. if rg_discard is set and compressed is enabled then the lower 16 - bits of the\
    resposne are discarded and the upper 16 - bits are probed to check if it is a compressed\
    instruction. If so, then the instruction is sent to the next stage. However is it is not a\
    compressed instruction it means the upper 16 - bits of the response refer to the lower 16 -\
    bits of a 32 - bit instruction and thus we will have to wait for the next response from the \
    cache to form the instruction is send to the next stage. To ensure the concatenation happens \
    in the next response we set rg_action to ChecPrev and set enque_instruction to False.\
    3. if rg_action is set to None, then we simply probe the lower 2 - bits to the response to\
    check if it is compressed. If so then the lower 16 bits form an instruction which is sent to\
    the next stage, the upper 16 - bits are stored to rg_instruction and rg_action is set to\
    CheckPrev to ensure that in the next resposne we first probe rg_instruction.\
    4. if rg_Action if set to CheckPrev then we first probe the lower 2 - bits of the \
    rg_instruction which leads to two possibilities. Either rg_instruction could hold a\
    compressed instruction from the previous response, in which case the current memory response\
    is not dequed and rg_instruction is sent to the next stage. This can happen due to state - 3\
    mentioned above. The other possibility is that rg_instruction holds the lower 16 - bits of a\
    32 - bit isntruction, in which case we have concatenate the lower 16 - bits of the response \
    with rg_instruction and send to the next, and also store the upper 16 - bits of the response \
    into rg_instruction. rg_Action in this case will remain CheckPrev so that the upper bits of \
    this repsonse are probed in the next cycle." *)
    rule process_instruction(!rg_wfi && !rg_initialize);
        let resp = ff_memory_response.first;
        Bit#(32) final_instruction = 0;
        Bool compressed = False;
        Bool perform_decode = True;

      `ifdef compressed
        PrevMeta lv_prev = rg_prev;
      `endif

        if(curr_epoch != resp.epoch)begin
          ff_memory_response.deq;
          rg_action <= None;
          perform_decode = False;
          `logLevel( stage1, 1, $format("STAGE1 : Dropping Instruction from Cache"))
        end
      `ifdef compressed
        else if(rg_action == CheckPrev && rg_prev.epoch == curr_epoch)begin
          if(rg_prev.instruction[1 : 0] == 2'b11)begin
            final_instruction={resp.inst[15 : 0], rg_prev.instruction};
            lv_prev.instruction = truncateLSB(resp.inst);
            ff_memory_response.deq;
          end
          else begin
            compressed = True;
            final_instruction = zeroExtend(rg_prev.instruction);
            rg_action <= None;
          end
        end
        else if(rg_discard_lower && wr_csr_misa_c == 1)begin
          rg_discard_lower <= False;
          ff_memory_response.deq;
          if(resp.inst[17 : 16] == 2'b11)begin
            lv_prev.instruction = resp.inst[31 : 16];
            rg_action <= CheckPrev;
            perform_decode = False;
          end
          else begin
            compressed = True;
            final_instruction = zeroExtend(resp.inst[31 : 16]);
          end
        end
      `endif
        else begin
          ff_memory_response.deq;
          if(resp.inst[1 : 0] == 'b11)begin
            final_instruction = resp.inst;
          end
        `ifdef compressed
          else if(wr_csr_misa_c == 1) begin
            compressed = True;
            final_instruction = zeroExtend(resp.inst[15 : 0]);
            lv_prev.instruction = truncateLSB(resp.inst);
            rg_action <= CheckPrev;
          end
        `endif
        end
      `ifdef compressed
        lv_prev.epoch = curr_epoch;
        rg_prev <= lv_prev;
        `logLevel( stage1, 1, $format("STAGE1 : rg_action: ",fshow(rg_action), " Prev: ",
                                              fshow(rg_prev) , " rg_discard:%b", rg_discard_lower))
      `endif
        Bit#(32) decode_instruction = final_instruction;
      `ifdef compressed
        if (compressed)
          decode_instruction = fn_decompress(final_instruction[15:0]);
        `logLevel( stage1, 0, $format("STAGE1: Decompressed: %h", decode_instruction))
      `endif
        let y <- decoder_func(decode_instruction, resp.err, wr_csr_decode
                              `ifdef compressed ,compressed `endif
                              `ifdef debug ,wr_debug_info, rg_step_done `endif );
        if(y.meta.inst_type == WFI && perform_decode) begin
          rg_wfi <= True;
          perform_decode = False;
        end
      `ifdef triggers
        let {trigger_err, trigger_cause} <- fn_check_trigger(rg_pc, decode_instruction
                                        `ifdef compressed , compressed `endif );
        if(trigger_err) begin
          y.meta.inst_type = TRAP;
          y.meta.funct = zeroExtend(trigger_cause);
        end
      `endif

      Bit#(`vaddr) offset = 4;
      `ifdef compressed
        if(compressed  && perform_decode && wr_csr_misa_c == 1)begin
          offset = 2;
        end
      `endif
      let _ops = fn_access_rf(y.op_addr.rs1addr, y.op_addr.rs2addr);

    `ifdef rtldump
      TraceDump dump = TraceDump {pc : rg_pc, instruction : final_instruction};
    `endif
      if(perform_decode) begin
      `ifdef debug
        `logLevel( stage1, 0, $format("STAGE1: step_done:%b", rg_step_done))
        if(rg_step_done && wr_debug_info.core_is_halted)
          rg_step_done <= False;
        else
          rg_step_done <= !wr_debug_info.core_is_halted && wr_debug_info.step_set
                                                      && wr_debug_info.debugger_available;
      `endif
        ff_stage1_operands.u.enq(_ops);
        ff_stage1_meta.u.enq(y);
        ff_stage1_control.u.enq(STAGE1_control{ epoch : curr_epoch, pc : rg_pc});
      `ifdef rtldump
        ff_stage1_dump.u.enq(dump);
      `endif
        rg_pc <= rg_pc + offset;
        `ifdef rtldump
          `logLevel( stage1, 0, $format("STAGE1 : ", fshow(dump)))
        `endif
        `logLevel( stage1, 1, $format("STAGE1 : compressed: %b perform_decode: %b curr_epoch: %b",
                                        compressed, perform_decode, curr_epoch))
      end
    endrule
    interface inst_request = interface Get
      method ActionValue#(InstRequest) get if(!rg_initialize);
        rg_fabric_request[0] <= rg_fabric_request[0] + 4; 
        return InstRequest{addr:rg_fabric_request[0], epoch:curr_epoch};
      endmethod
    endinterface;

    interface inst_response = interface Put
      method Action put (InstResponse resp);
        ff_memory_response.enq(resp);
      endmethod
    endinterface;

    interface tx_stage1_operands = ff_stage1_operands.e;
  
    interface tx_stage1_meta = ff_stage1_meta.e;

    interface tx_stage1_control = ff_stage1_control.e;

  `ifdef rtldump
    interface tx_stage1_dump = ff_stage1_dump.e;
  `endif

    method Action ma_update_eEpoch;
      rg_eEpoch <= ~rg_eEpoch;
    endmethod

    method Action ma_update_wEpoch;
      rg_wEpoch <= ~rg_wEpoch;
    endmethod

    method Action ma_flush( Bit#(`vaddr) newpc); 
      rg_pc <= newpc;
      rg_fabric_request[1]<={truncateLSB(newpc), 2'b0};
    `ifdef compressed
      if(newpc[1 : 0] != 0)
        rg_discard_lower <= True;
      else 
        rg_discard_lower <= False;
    `endif
      rg_wfi <= False;
      `logLevel( stage1, 0, $format("STAGE1 : Received Flush. PC: %h ",newpc)) 
    endmethod

    // This method captures the "c" of misa csr
    method Action ma_csr_misa_c (Bit#(1) c);
      wr_csr_misa_c <= c;
    endmethod
    method Action ma_interrupt(Bool i);
      wr_interrupt <= i;
    endmethod
    method Action ma_csr_decode (CSRtoDecode c);
      wr_csr_decode <= c;
    endmethod
  `ifdef debug
    method ActionValue#(Bit#(XLEN)) mav_debug_access_gprs(AbstractRegOp cmd) if(!rg_initialize);
      let _x <- integer_rf.mav_debug_access_gprs(cmd);
      return _x;
    endmethod
    method Action ma_debug_status (DebugStatus status);
      wr_debug_info <= status;
    endmethod
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
    
    interface commit_rd = interface Put
      method Action put (CommitPacket wbinfo ) if(!rg_initialize);
        `logLevel( stage1, 0, $format("STAGE1: Writing RF[%d]:%h",wbinfo.rdaddr, wbinfo.rdvalue))
        integer_rf.upd( wbinfo.rdaddr, wbinfo.rdvalue );
      endmethod
    endinterface;
    
  endmodule : mkstage1
endpackage : stage1
