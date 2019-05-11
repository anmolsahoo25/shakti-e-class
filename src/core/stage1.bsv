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

  // project packages
	import common_types::*;
  import decode::*;
  `include "common_params.bsv"
  `include "Logger.bsv"

  // ----------------------------- local type definitions -------------------------------------- //
  typedef enum {CheckPrev, None} ActionType deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(`vaddr) pc;
    Bit#(16) instruction;
    Bit#(1) epoch;
  } PrevMeta deriving(Eq, Bits, FShow);
  // ------------------------------------------------------------------------------------------- //

  // Interface for the fetch and decode unit
	interface Ifc_stage1;

    // interface to send request to the fabric
	  interface Get#(Tuple2#(Bit#(`paddr), Bit#(1))) inst_request;

    // interface to receive response from fabric
	  interface Put#(Tuple3#(Bit#(32), Bool, Bit#(1))) inst_response;

    // interface to send decoded instruction to the next stage
    interface TXe#(PIPE1_DS) to_nextstage;

    // flush from stage3
    (*always_ready*)
    method Action ma_flush( Bit#(`paddr) newpc);
    
    // csrs from the csrfile.
    (*always_ready,always_enabled*)
    method Action ma_csr_misa_c (Bit#(1) c);

    // interrupt from csr mip register
    (*always_ready,always_enabled*)
    method Action ma_interrupt(Bool i);

    // csrs for decoder
    (*always_ready,always_enabled*)
    method Action ma_csr_decode (CSRtoDecode c);
  
  `ifdef triggers
    // receives the TDATA1 from the csrs
    method Action trigger_data1(Vector#(`trigger_num, TriggerData) t);

    // receives the TDATA2 register from the csrs for comparison
    method Action trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);

    // receives the info on which triggers are enabled
    method Action trigger_enable(Vector#(`trigger_num, Bool) t);
  `endif
	endinterface : Ifc_stage1

	(*synthesize*)
  (*preempts = "ma_flush, wait_for_interrupt"*)
  (*preempts = "ma_flush, process_instruction"*)
	module mkstage1#(parameter Bit#(`paddr) resetpc)(Ifc_stage1);

    let stage = ""; // for logger

    // ------------------------------------ Start instantiations --------------------------------//

    // this wire carries the current value of the misa_c csr field
    Wire#(Bit#(1)) wr_csr_misa_c <- mkWire();

    // this wire caries the current value of certain csrs
    Wire#(CSRtoDecode) wr_csr_decode <- mkWire();

    // register to hold the address of the next request to the fabric.
    Reg#(Bit#(`paddr)) rg_fabric_request[2] <- mkCReg(2, (resetpc));

    // register to hold the PC value of the instruction to be decoded.
    Reg#(Bit#(`paddr)) rg_pc <- mkReg((resetpc));

    // holds the curren epoch values of the pipe.
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);

    // This register implements a simple state - machine which indicates how the instruction should be
    // extracted from the cache response.
    Reg#(ActionType) rg_action <- mkReg(None);
  `ifdef compressed
    Reg#(Bool) rg_discard_lower <- mkReg(False);
    Reg#(PrevMeta) rg_prev <- mkReg(?);
  `endif

    // This wire will be set if any interrupts have been detected by the core
    Wire#(Bool) wr_interrupt <- mkWire();

    // this is register it set to True when a WFI instruction is executed. It set to False, when an
    // interrupt has been received or there is a flush from the write - back stage.
    Reg#(Bool) rg_wfi <- mkReg(False);

    // fifo to hold the instruction response from the fabric
    FIFOF#(Tuple3#(Bit#(32), Bool, Bit#(1))) ff_memory_response <- mkSizedFIFOF(2);

    // the fifo to communicate with the next stage.
		TX#(PIPE1_DS) tx_nextstage <- mkTX;
  
  `ifdef triggers
    Vector#(`trigger_num, Wire#(TriggerData)) v_trigger_data1 <- replicateM(mkWire());
    Vector#(`trigger_num, Wire#(Bit#(XLEN))) v_trigger_data2 <- replicateM(mkWire());
    Vector#(`trigger_num, Wire#(Bool)) v_trigger_enable <- replicateM(mkWire());
  `endif
    // ----------------------------------End instantiations ------------------------------------ //

  `ifdef triggers

    function ActionValue#(Tuple2#(Bool, Bit#(`causesize))) check_trigger (Bit#(`vaddr) pc, 
                           Bit#(32) instr `ifdef compressed, Bool compressed `endif ) = actionvalue
      Bool trap = False;
      Bit#(`causesize) cause = `Breakpoint;
      Bit#(XLEN) compare_value ;
      Bool chain = False;
      for(Integer i = 0; i < `trigger_num; i = i+1)begin
        `logLevel( stage1, 3, $format("STAGE1 : Trigger[%2d] Data1: ", i, fshow(v_trigger_data1[i])))
        `logLevel( stage1, 3, $format("STAGE1 : Trigger[%2d] Data2: ", i, fshow(v_trigger_data2[i])))
        `logLevel( stage1, 3, $format("STAGE1 : Trigger[%2d] Enable: ", i, fshow(v_trigger_enable[i])))
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
    // ---------------------- End local function definitions ------------------//

    // ---------------------------------------- rules ------------------------------------------ //

    // RuleName : wait_for_interrupt
    // Explicit Conditions : rg_wfi == True
    // Implicit Conditions : wr_interrupt should be written in the same cycle
    // Desciption : This rule is fired when the core has executed the WFI instruction and waiting for
    // an intterupt to the core to resume fetch;
    rule wait_for_interrupt(rg_wfi);
      if(wr_interrupt)
        rg_wfi <= False;
      `logLevel( stage1, 0, $format("STAGE1 : Waiting for Interrupt. wr_interrupt: %b",wr_interrupt))
    endrule


    // RuleName : process_instruction
    // Explicit Conditions : rg_wfi == False
    // Implicit Conditions: 
    //    1. ff_memory_response.notEmpty
    //    2. wr_csr_decode is written in the same cycle
    //    3. wr_csr_misa_c is written in the same cycle
    //    4. tostage FIFO notFull
    // Schedule Conflicts : This rule will not fire if there is flush from the write - back stage. A
    // flush from the write - back stage will cause a change in the rg_pc and rg_discard,
    // both of which are being updated in this method as well. This schedule is acceptable since
    // anyways the response from the memory currently to be handled in this rule will match epochs
    // and will be dropped.
    //
    // Details : This rule will receive the instruction from the memory subsystem and decide if the
    // instruction is compressed or not. The final instruction is then sent to the next stage.
    // To extract the instruction from the memory response a state machine is implemented.
    // 
    // 1. First the epochs are compared and if a mis - match is observed then the response is dropped
    // without any other changes to the state of the module.
    // 2. if rg_discard is set and compressed is enabled then the lower 16 - bits of the
    // resposne are discarded and the upper 16 - bits are probed to check if it is a compressed
    // instruction. If so, then the instruction is sent to the next stage. However is it is not a
    // compressed instruction it means the upper 16 - bits of the response refer to the lower 16 - bits
    // of a 32 - bit instruction and thus we will have to wait for the next response from the cache to
    // form the instruction is send to the next stage. To ensure the concatenation happens in the
    // next response we set rg_action to ChecPrev and set enque_instruction to False.
    // 3. if rg_action is set to None, then we simply probe the lower 2 - bits to the response to
    // check if it is compressed. If so then the lower 16 bits form an instruction which is sent to
    // the next stage, the upper 16 - bits are stored to rg_instruction and rg_action is set to
    // CheckPrev to ensure that in the next resposne we first probe rg_instruction.
    // 4. if rg_Action if set to CheckPrev then we first probe the lower 2 - bits of the 
    // rg_instruction which leads to two possibilities. Either rg_instruction could hold a
    // compressed instruction from the previous response, in which case the current memory response
    // is not dequed and rg_instruction is sent to the next stage. This can happen due to state - 3
    // mentioned above. The other possibility is that rg_instruction holds the lower 16 - bits of a
    // 32 - bit isntruction, in which case we have concatenate the lower 16 - bits of the response with
    // rg_instruction and send to the next, and also store the upper 16 - bits of the response into
    // rg_instruction. rg_Action in this case will remain CheckPrev so that the upper bits of this
    // repsonse are probed in the next cycle.
    rule process_instruction(!rg_wfi);
        let {cache_response, err, epoch}=ff_memory_response.first;
        Bit#(32) final_instruction = 0;
        Bool compressed = False;
        Bool perform_decode = True;

        PrevMeta lv_prev = rg_prev;

        if(rg_epoch != epoch)begin
          ff_memory_response.deq;
          rg_action <= None;
          perform_decode = False;
          `logLevel( stage1, 1, $format("STAGE1 : Dropping Instruction from Cache"))
        end
      `ifdef compressed
        else if(rg_action == CheckPrev && rg_prev.epoch == rg_epoch)begin
          if(rg_prev.instruction[1 : 0] == 2'b11)begin
            final_instruction={cache_response[15 : 0], rg_prev.instruction};
            lv_prev.instruction = truncateLSB(cache_response);
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
          if(cache_response[17 : 16] == 2'b11)begin
            lv_prev.instruction = cache_response[31 : 16];
            rg_action <= CheckPrev;
            perform_decode = False;
          end
          else begin
            compressed = True;
            final_instruction = zeroExtend(cache_response[31 : 16]);
          end
        end
      `endif
        else begin
          ff_memory_response.deq;
          if(cache_response[1 : 0] == 'b11)begin
            final_instruction = cache_response;
          end
        `ifdef compressed
          else if(wr_csr_misa_c == 1) begin
            compressed = True;
            final_instruction = zeroExtend(cache_response[15 : 0]);
            lv_prev.instruction = truncateLSB(cache_response);
            rg_action <= CheckPrev;
          end
        `endif
        end
        lv_prev.epoch = rg_epoch;
        `logLevel( stage1, 1, $format("STAGE1 : rg_action: ",fshow(rg_action), " Prev: ",fshow(rg_prev)))
        `logLevel( stage1, 1, $format("STAGE1 : compressed: %b perform_decode: %b rg_epoch: %b",
                                        compressed, perform_decode, rg_epoch))

        PIPE1_DS x = decoder_func_16(final_instruction[15 : 0], rg_pc, epoch, err, wr_csr_decode);
        PIPE1_DS y = decoder_func(final_instruction, rg_pc, epoch, err, wr_csr_decode);
        if(!compressed && final_instruction[14 : 12] == 0 && final_instruction[31 : 25] == 'b001000 &&
              final_instruction[6 : 2] == 'b11100 && perform_decode) begin
          rg_wfi <= True;
          perform_decode = False;
        end
        if(compressed  && perform_decode && wr_csr_misa_c == 1)begin
          rg_pc <= rg_pc + 2;
          tx_nextstage.u.enq(x);
        end
        else if(perform_decode)begin
          rg_pc <= rg_pc + 4;
          tx_nextstage.u.enq(y);
        end
        `logLevel( stage1, 0, $format("STAGE1 : PC: %h Inst: %h, Err: %b Epoch: %b", 
                                        rg_pc, final_instruction, err, epoch))
        rg_prev <= lv_prev;
    endrule
    
    interface inst_request = interface Get
      method ActionValue#(Tuple2#(Bit#(`paddr), Bit#(1))) get;
				rg_fabric_request[1] <= rg_fabric_request[1] + 4; 
        return tuple2(rg_fabric_request[1], rg_epoch);
      endmethod
    endinterface;

    interface inst_response = interface Put
	    method Action put (Tuple3#(Bit#(32), Bool, Bit#(1)) resp);
        ff_memory_response.enq(resp);
	    endmethod
    endinterface;

    //providing the output of the decoder function to the opfetch unit via TX interface
		interface to_nextstage = tx_nextstage.e;

    method Action ma_flush( Bit#(`paddr) newpc); 
      rg_pc <= newpc;
      rg_epoch<=~rg_epoch;
      rg_fabric_request[0]<={truncateLSB(newpc), 2'b0};
      if(newpc[1 : 0] != 0)
        rg_discard_lower <= True;
      rg_wfi <= False;
      `logLevel( stage1, 0, $format("STAGE1 : Received Flush. PC: %h Flush: ",newpc)) 
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
  `ifdef triggers
    method Action trigger_data1(Vector#(`trigger_num, TriggerData) t);
      for(Integer i=0; i<`trigger_num; i=i+1)
        v_trigger_data1[i] <= t[i];
    endmethod
    method Action trigger_data2(Vector#(`trigger_num, Bit#(XLEN)) t);
      for(Integer i=0; i<`trigger_num; i=i+1)
        v_trigger_data2[i] <= t[i];
    endmethod
    method Action trigger_enable(Vector#(`trigger_num, Bool) t);
      for(Integer i=0; i<`trigger_num; i=i+1)
        v_trigger_enable[i] <= t[i];
    endmethod
  `endif
	endmodule : mkstage1
endpackage : stage1
