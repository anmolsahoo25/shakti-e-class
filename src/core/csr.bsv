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
    method CSRtoDecode csrs_to_decode;
    method ActionValue#(Bit#(`vaddr)) take_trap(Bit#(`causesize) type_cause, Bit#(`vaddr) pc, Bit#(`vaddr) badaddr);
	  method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(64) c_mtime);
    method Action incr_minstret;
		`ifdef supervisor
			method Bit#(XLEN) csr_satp;
		`endif
    `ifdef spfpu
      method Action update_fflags(Bit#(5) flags);
    `endif
	  method Action set_external_interrupt(Bit#(1) ex_i);
    method Bit#(1) csr_misa_c;
    method Bit#(3) mv_cacheenable;
  `ifdef arith_trap
   //This method returns value of csr_reg which enables or disables arithmetic exceptions
    method Bit#(1) arith_excep;
  `endif
    method Bit#(2) curr_priv;
    method Bit#(XLEN) csr_mstatus;
  `ifdef pmp
    method Vector#(`PMPSIZE, Bit#(8)) pmp_cfg;
    method Vector#(`PMPSIZE, Bit#(`paddr )) pmp_addr;
  `endif

  `ifdef debug
    method ActionValue#(Bit#(XLEN)) debug_access_csrs(AbstractRegOp cmd);
    method Action debug_halt_request(Bit#(1) ip);
    method Action debug_resume_request(Bit#(1) ip);
    method Bit#(1) core_is_halted;
    method Bit#(1) step_is_set;
    method Bit#(1) step_ie;
    method Bit#(1) core_debugenable;
  `endif
  `ifdef triggers
    method Vector#(`trigger_num, TriggerData) trigger_data1;
    method Vector#(`trigger_num, Bit#(XLEN)) trigger_data2;
    method Vector#(`trigger_num, Bool) trigger_enable;
  `endif
  endinterface : Ifc_csr


  (*synthesize*)
  (*mutually_exclusive="system_instruction, take_trap"*)
  (*conflict_free="system_instruction, set_external_interrupt"*)
  (*conflict_free="take_trap, set_external_interrupt"*)
  module mkcsr(Ifc_csr);
  
    Ifc_csrfile csrfile <- mkcsrfile();
	  method ActionValue#(Tuple3#(Bool, Bit#(`vaddr), Bit#(XLEN))) system_instruction(
         Bit#(12) csr_address, Bit#(XLEN) op1, Bit#(3) funct3, Bit#(2) lpc);
      Bool flush = False;
      Bit#(`vaddr) jump_add = 0;
	  	let csrread <- csrfile.read_csr(csr_address);
      Bit#(XLEN) writecsrdata = 0;
	  	Bit#(XLEN) destination_value = 0;
      `logLevel( csr, 2, $format("CSR : Operation csr: %h op1: %h, funct3: %b csr_read: %h", csr_address, 
            op1, funct3, csrread))

	  	case(funct3)
        'd0 : case (csr_address[11 : 8])
              'h0, `ifdef supervisor 'h1, `endif 'h3 : begin // URET, SRET, MRET
                let temp <- csrfile.upd_on_ret( `ifdef non_m_traps unpack(csr_address[9 : 8]) `endif );
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
	
    method ActionValue#(Bit#(`vaddr)) take_trap(Bit#(`causesize) type_cause, Bit#(`vaddr) pc, Bit#(`vaddr) badaddr);
      let jump_address <- csrfile.upd_on_trap(type_cause, pc, badaddr); 
		  return jump_address;
  	endmethod

    method csrs_to_decode = csrfile.csrs_to_decode;
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
		`ifdef supervisor
			method csr_satp = csrfile.csr_satp;
		`endif
    `ifdef spfpu
      method Action update_fflags(Bit#(5) flags);
        csrfile.update_fflags(flags);
      endmethod
    `endif
	  method Action set_external_interrupt(Bit#(1) ex_i) = csrfile.set_external_interrupt(ex_i);
    method csr_misa_c = csrfile.csr_misa_c;
    method mv_cacheenable = csrfile.mv_cacheenable;
 
  `ifdef arith_trap
    method arith_excep = csrfile.arith_excep;
  `endif
    method curr_priv = csrfile.curr_priv;
    method csr_mstatus = csrfile.csr_mstatus;
  `ifdef pmp
    method pmp_cfg = csrfile.pmp_cfg;
    method pmp_addr = csrfile.pmp_addr;
  `endif
  `ifdef debug
    method ActionValue#(Bit#(XLEN)) debug_access_csrs(AbstractRegOp cmd);
      if(cmd.read_write) begin
        csrfile.write_csr(truncate(cmd.address), cmd.writedata, 'd0);
        return 0;
      end
      else begin
        let x <- csrfile.read_csr(truncate(cmd.address));
        return x;
      end
    endmethod
    method debug_halt_request = csrfile.debug_halt_request;
    method debug_resume_request = csrfile.debug_resume_request ;
    method core_is_halted = csrfile.core_is_halted ;
    method step_is_set = csrfile.step_is_set;
    method step_ie = csrfile.step_ie;
    method core_debugenable = csrfile.core_debugenable;
  `endif
  `ifdef triggers
    method trigger_data1 = csrfile.trigger_data1;
    method trigger_data2 = csrfile.trigger_data2;
    method trigger_enable = csrfile.trigger_enable;
  `endif
  endmodule
endpackage
