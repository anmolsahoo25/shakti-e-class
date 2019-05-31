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
Details : This module contains the methods and functions which will perform tasks related to CSRs:
Trap handling and Updating the CSRs for system - instruction

--------------------------------------------------------------------------------------------------
 */

package csr;
  // project related imports
  import common_types::*;
  `include "common_params.bsv"
  `include "csr.defines"
  import ConcatReg::*;
  import csrfile::*;
  import Vector::*;
  `include "Logger.bsv"

`ifdef debug
  import debug_types::*;
`endif

  // package imports 
  import ConfigReg::*;
	
  interface Ifc_csr;
    method ActionValue#(Tuple3#(Bool, Bit#(`vaddr), Bit#(XLEN))) system_instruction(
            Bit#(12) csr_address, Bit#(XLEN) op1, Bit#(3) funct3, Bit#(2) lpc);
    method CSRtoDecode mv_csr_decode;
    method ActionValue#(Bit#(`vaddr)) take_trap(Bit#(`causesize) type_cause, Bit#(`vaddr) pc, 
                                                Bit#(`vaddr) badaddr);
    method Action clint_msip(Bit#(1) intrpt);
    method Action clint_mtip(Bit#(1) intrpt);
    method Action clint_mtime(Bit#(64) c_mtime);
    method Action incr_minstret;
    method Action ext_interrupt(Bit#(1) ex_i);
    method Bit#(1) mv_csr_misa_c;
    (*always_ready, always_enabled*)
    method Bool mv_interrupt;
  `ifdef arith_trap
    method Bit#(1) mv_arithtrap_en;
  `endif
    method Bit#(2) mv_curr_priv;
    method Bit#(XLEN) csr_mstatus;
  `ifdef pmp
    method Vector#(`PMPSIZE, Bit#(8)) mv_pmp_cfg;
    method Vector#(`PMPSIZE, Bit#(TSub#(`paddr,2))) mv_pmp_addr;
  `endif

  `ifdef debug
    method ActionValue#(Bit#(XLEN)) mav_debug_access_csrs(AbstractRegOp cmd);
    method Action ma_debug_halt_request(Bit#(1) ip);
    method Action ma_debug_resume_request(Bit#(1) ip);
    method Bit#(1) mv_core_is_halted;
    method Bit#(1) mv_step_is_set;
    method Bit#(1) mv_step_ie;
    method Bit#(1) mv_core_debugenable;
  `endif
  `ifdef triggers
    method Vector#(`trigger_num, TriggerData) mv_trigger_data1;
    method Vector#(`trigger_num, Bit#(XLEN))  mv_trigger_data2;
    method Vector#(`trigger_num, Bool)        mv_trigger_enable;
  `endif
`ifdef perfmonitors
    method Action ma_events(Bit#(SizeOf#(Events)) e);
  `ifdef simulate
    method Tuple2#(Vector#(`counters, Bit#(XLEN)), Vector#(`counters, Bit#(64))) counter_values;
  `endif
`endif
  endinterface : Ifc_csr


  (*synthesize*)
  (*mutually_exclusive="system_instruction, take_trap"*)
  (*conflict_free="system_instruction, ext_interrupt"*)
  (*conflict_free="take_trap, ext_interrupt"*)
  module mkcsr(Ifc_csr);
  
    Ifc_csrfile csrfile <- mkcsrfile();
    method ActionValue#(Tuple3#(Bool, Bit#(`vaddr), Bit#(XLEN))) system_instruction(
        Bit#(12) csr_address, Bit#(XLEN) op1, Bit#(3) funct3, Bit#(2) lpc);
      Bool flush = False;
      Bit#(`vaddr) jump_add = 0;
      let csrread <- csrfile.read_csr(csr_address);
      Bit#(XLEN) writecsrdata = 0;
      Bit#(XLEN) destination_value = 0;
      `logLevel( csr, 2, $format("CSR : Operation csr: %h op1: %h, funct3: %b csr_read: %h", 
                                  csr_address, op1, funct3, csrread))

      case(funct3)
        'd0 : case (csr_address[11 : 8])
              'h0, 'h3 : begin // URET, SRET, MRET
                let temp <- csrfile.upd_on_ret( `ifdef non_m_traps 
                                                  unpack(csr_address[9 : 8]) 
                                                `endif );
                jump_add = temp;
                flush = True;
                `logLevel( csr, 1, $format("CSR : RET Function: %h",csr_address))
              end
            endcase
        default : begin
          destination_value = csrread;
          if(funct3[1 : 0] == 'd1)
            writecsrdata = op1;
          else if(funct3[1 : 0] == 'd2)
            writecsrdata = op1|csrread;
          else
            writecsrdata = ~op1 & csrread;
          csrfile.write_csr(csr_address, writecsrdata,  lpc);
        end
      endcase
      return tuple3(flush, jump_add, destination_value);
    endmethod
	
    method ActionValue#(Bit#(`vaddr)) take_trap(Bit#(`causesize) type_cause, Bit#(`vaddr) pc, 
                                                Bit#(`vaddr) badaddr);
      let jump_address <- csrfile.upd_on_trap(type_cause, pc, badaddr); 
      return jump_address;
    endmethod

    method mv_csr_decode = csrfile.mv_csr_decode;
    method Action clint_msip(Bit#(1) intrpt);
      csrfile.clint_msip(intrpt);
    endmethod
    method Action clint_mtip(Bit#(1) intrpt);
      csrfile.clint_mtip(intrpt);
    endmethod
    method Action clint_mtime(Bit#(64) c_mtime);
      csrfile.clint_mtime(c_mtime);
    endmethod
    method incr_minstret = csrfile.incr_minstret;
    method Action ext_interrupt(Bit#(1) ex_i) = csrfile.ext_interrupt(ex_i);
    method mv_csr_misa_c = csrfile.mv_csr_misa_c;
    method mv_interrupt = csrfile.mv_interrupt;
 
  `ifdef arith_trap
    method mv_arithtrap_en = csrfile.mv_arithtrap_en;
  `endif
    method mv_curr_priv = csrfile.mv_curr_priv;
    method csr_mstatus = csrfile.csr_mstatus;
  `ifdef pmp
    method mv_pmp_cfg = csrfile.mv_pmp_cfg;
    method mv_pmp_addr = csrfile.mv_pmp_addr;
  `endif
  `ifdef debug
    method ActionValue#(Bit#(XLEN)) mav_debug_access_csrs(AbstractRegOp cmd);
      if(cmd.read_write) begin
        csrfile.write_csr(truncate(cmd.address), cmd.writedata, 'd0);
        return 0;
      end
      else begin
        let x <- csrfile.read_csr(truncate(cmd.address));
        return x;
      end
    endmethod
    method ma_debug_halt_request = csrfile.debug_halt_request;
    method ma_debug_resume_request = csrfile.debug_resume_request ;
    method mv_core_is_halted = csrfile.core_is_halted ;
    method mv_step_is_set = csrfile.step_is_set;
    method mv_step_ie = csrfile.step_ie;
    method mv_core_debugenable = csrfile.core_debugenable;
  `endif
  `ifdef triggers
    method mv_trigger_data1 =   csrfile.mv_trigger_data1;
    method mv_trigger_data2 =   csrfile.mv_trigger_data2;
    method mv_trigger_enable =  csrfile.mv_trigger_enable;
  `endif
`ifdef perfmonitors
    method ma_events = csrfile.ma_events;
  `ifdef simulate
    method counter_values = csrfile.counter_values;
  `endif
`endif
  endmodule
endpackage
