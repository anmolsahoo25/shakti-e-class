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
package riscv;

  // library imports
  import GetPut::*;
  import Connectable::*;
  import FIFO::*;
  import FIFOF::*;
  import SpecialFIFOs :: *;
  import TxRx :: *;
  import Vector :: *;

  // project imports
  import common_types :: *;
  import stage1 :: *;
  import stage2 :: *;
  import stage3 :: *;
  `include "common_params.bsv"
  `include "Logger.bsv"
`ifdef debug
  import debug_types ::*;
`endif

  interface Ifc_riscv;
    // interface between fetch and fabric
    interface Get#(InstRequest) inst_request;
    interface Put#(InstResponse) inst_response;

    // interface between memory - stage and fabric
    interface Get#(MemoryRequest) memory_request;
    interface Put#(MemoryResponse) memory_response;

    // side - band connections to the csr
    method Action clint_msip(Bit#(1) intrpt);
    method Action clint_mtip(Bit#(1) intrpt);
    method Action clint_mtime(Bit#(64) c_mtime);
    method Action ext_interrupt(Bit#(1) intrpt);

    `ifdef rtldump
      interface Get#(DumpType) dump;
    `endif
  
  `ifdef debug
    // interface to interact with debugger
    method ActionValue#(Bit#(XLEN)) mav_debug_access_gprs(AbstractRegOp cmd);
    method ActionValue#(Bit#(XLEN)) mav_debug_access_csrs(AbstractRegOp cmd);
    method Action ma_debug_halt_request(Bit#(1) ip);
    method Action ma_debug_resume_request(Bit#(1) ip);
    method Action ma_debugger_available (Bit#(1) avail);
    method Bit#(1) mv_core_is_halted;
    method Bit#(1) mv_core_debugenable;
  `endif

    method Bit#(2) mv_curr_priv;
    method Bool mv_trap;
`ifdef perfmonitors
    method Action ma_event_loads(Bit#(1) e);
    method Action ma_event_stores(Bit#(1) e);
  `ifdef simulate
    method Tuple2#(Vector#(`counters, Bit#(XLEN)), Vector#(`counters, Bit#(64))) counter_values;
  `endif
`endif
  `ifdef pmp
    method Vector#(`PMPSIZE, Bit#(8)) mv_pmp_cfg;
    method Vector#(`PMPSIZE, Bit#(TSub#(`paddr,2))) mv_pmp_addr;
  `endif
  endinterface : Ifc_riscv

  (*synthesize*)
  module mkriscv#(Bit#(`vaddr) resetpc) (Ifc_riscv);
    // instantiate each stage here
    Ifc_stage1              stage1          <- mkstage1(resetpc);
    Ifc_stage2              stage2          <- mkstage2();
    Ifc_stage3              stage3          <- mkstage3();

  `ifdef debug
    Wire#(Bool) wr_debugger_available <- mkWire();
  `endif

  `ifdef perfmonitors
    Wire#(Bit#(1)) wr_event_load <- mkDWire(0);
    Wire#(Bit#(1)) wr_event_store <- mkDWire(0);
  `endif

    mkChan(mkLFIFOF()   , stage1.tx_stage1_operands , stage2.rx_stage1_operands);
    mkChan(mkLFIFOF()   , stage1.tx_stage1_control  , stage2.rx_stage1_control);
    mkChan(mkLFIFOF()   , stage1.tx_stage1_meta     , stage2.rx_stage1_meta);

    mkChan(mkLFIFOF()   , stage2.tx_stage3_common   , stage3.rx_stage3_common);
    mkChan(mkLFIFOF()   , stage2.tx_stage3_type     , stage3.rx_stage3_type);

  `ifdef rtldump
    mkChan(mkLFIFOF()   , stage1.tx_stage1_dump     , stage2.rx_stage1_dump);
    mkChan(mkLFIFOF()   , stage2.tx_stage3_dump     , stage3.rx_stage3_dump);
  `endif
    
    mkConnection(stage3.commit_rd         , stage1.commit_rd);
    mkConnection(stage3.operand_fwding    , stage2.operand_fwding);

    mkConnection(stage1.ma_csr_decode     , stage3.mv_csr_decode);
    mkConnection(stage1.ma_csr_misa_c     , stage3.mv_csr_misa_c);
    mkConnection(stage2.ma_csr_misa_c     , stage3.mv_csr_misa_c);
    mkConnection(stage1.ma_interrupt      , stage3.mv_interrupt);
  `ifdef triggers
    mkConnection(stage1.ma_trigger_data1  , stage3.mv_trigger_data1);
    mkConnection(stage1.ma_trigger_data2  , stage3.mv_trigger_data2);
    mkConnection(stage1.ma_trigger_enable , stage3.mv_trigger_enable);
    mkConnection(stage2.ma_trigger_data1  , stage3.mv_trigger_data1);
    mkConnection(stage2.ma_trigger_data2  , stage3.mv_trigger_data2);
    mkConnection(stage2.ma_trigger_enable , stage3.mv_trigger_enable);
  `endif

  `ifdef muldiv
    mkConnection(stage2.mv_delayed_output , stage3.ma_delayed_output);
  `endif

  `ifdef arith_trap
    mkConnection(stage2.ma_arithtrap_en   , stage3.mv_arithtrap_en);
  `endif

    let {newpc, trap}=stage3.flush; 
    let {redirect_pc, redirect} = stage2.mv_redirection;

  `ifdef perfmonitors
    rule connect_events;
      stage3.ma_events({stage2.mv_event_raw_stalls, stage2.mv_event_redirection, wr_event_store, 
                        wr_event_load, stage2.mv_event_jumps, stage2.mv_event_csr_ops, 
                        stage2.mv_event_muldiv, stage2.mv_event_branch_nottaken, 
                        stage2.mv_event_branch_taken,
                        stage3.mv_event_interrupts, stage3.mv_event_exceptions});
    endrule
  `endif
  
  `ifdef debug
    (*fire_when_enabled*)
    rule connect_debug_info;
      stage1.ma_debug_status(DebugStatus {debugger_available : wr_debugger_available ,
                                          core_is_halted     : unpack(stage3.mv_core_is_halted),
                                          step_set           : unpack(stage3.mv_step_is_set),
                                          step_ie            : unpack(stage3.mv_step_ie),
                                          core_debugenable   : unpack(stage3.mv_core_debugenable)});
    endrule
  `endif

    rule gen_new_pc(trap || redirect);
      if(trap)
        stage1.ma_flush(newpc);
      else
        stage1.ma_flush(redirect_pc);
    endrule

    rule redirection_from_stage2(redirect);
      stage1.ma_update_eEpoch;
    endrule

    rule flush_from_writeback(trap); 
      stage2.ma_update_wEpoch;
      stage1.ma_update_wEpoch;
    endrule

    interface inst_request = stage1.inst_request;
    interface inst_response = stage1.inst_response;

    interface memory_request = stage2.memory_request;
    interface memory_response = stage3.memory_response;

    method Action clint_msip(Bit#(1) intrpt);
      stage3.clint_msip(intrpt);
    endmethod
    method Action clint_mtip(Bit#(1) intrpt);
      stage3.clint_mtip(intrpt);
    endmethod
    method Action clint_mtime(Bit#(64) c_mtime);
      stage3.clint_mtime(c_mtime);
    endmethod
    method Action ext_interrupt(Bit#(1) intrpt);
      stage3.ext_interrupt(intrpt);
    endmethod
    `ifdef rtldump
      interface dump = stage3.dump;
    `endif
    method mv_curr_priv = stage3.mv_curr_priv;
  `ifdef debug
    method mav_debug_access_gprs    = stage1.mav_debug_access_gprs;
    method mav_debug_access_csrs    = stage3.mav_debug_access_csrs;
    method ma_debug_halt_request    = stage3.ma_debug_halt_request;
    method ma_debug_resume_request  = stage3.ma_debug_resume_request;
    method mv_core_is_halted        = stage3.mv_core_is_halted;
    method mv_core_debugenable      = stage3.mv_core_debugenable;
    method Action ma_debugger_available (Bit#(1) avail);
      wr_debugger_available <= unpack(avail);
    endmethod
  `endif
    method mv_trap = trap;
`ifdef perfmonitors
    method Action ma_event_loads(Bit#(1) e);
      wr_event_load <= e;
    endmethod
    method Action ma_event_stores(Bit#(1) e);
      wr_event_store <= e;
    endmethod
  `ifdef simulate
    method counter_values = stage3.counter_values;
  `endif
`endif
  `ifdef pmp
    method mv_pmp_cfg  = stage3.mv_pmp_cfg;
    method mv_pmp_addr = stage3.mv_pmp_addr;
  `endif
  endmodule : mkriscv
endpackage : riscv
