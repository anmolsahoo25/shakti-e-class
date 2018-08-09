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
Details: This module contains the methods and functions which will perform tasks related to CSRs:
Trap handling and Updating the CSRs for system- instruction

--------------------------------------------------------------------------------------------------
*/

package csr;
  // project related imports
  import common_types::*;
  `include "common_params.bsv"
  import ConcatReg::*;
  import csrfile::*;

  // package imports 
  import ConfigReg::*;
	
  interface Ifc_csr;
	  method ActionValue#(Tuple3#(Bool, Bit#(PADDR), Bit#(XLEN))) system_instruction( Bit#(5) rd, 
            Bit#(12) csr_address, Bit#(5) rs1_addr, Bit#(XLEN) op1, Bit#(3) funct3);
    method CSRtoDecode csrs_to_decode;
    method ActionValue#(Bit#(PADDR)) take_trap(Trap_type trap, Bit#(PADDR) pc, Bit#(PADDR) badaddr);
	  method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    method Action incr_minstret;
    method Bool interrupt;
    `ifdef RV64 method Bool inferred_xlen; `endif // False-32bit,  True-64bit 
  endinterface:Ifc_csr


  (*synthesize*)
  (*mutually_exclusive="system_instruction, take_trap"*)
  module mkcsr(Ifc_csr);
    let verbosity=`VERBOSITY ;
  
    Ifc_csrfile csrfile <- mkcsrfile();
    
	  method ActionValue#(Tuple3#(Bool, Bit#(PADDR), Bit#(XLEN))) system_instruction( Bit#(5) rd, 
            Bit#(12) csr_address, Bit#(5) rs1_addr, Bit#(XLEN) op1, Bit#(3) funct3);
      if(verbosity>1)
        $display($time, "\tCSR: csr: %h rs1addr: %d, op1: %h, funct3: %b", csr_address, rs1_addr, 
            op1, funct3);

      Bool flush = False;
      Bit#(PADDR) jump_add=0;
	  	let csrread=csrfile.read_csr(csr_address);
      Bit#(XLEN) writecsrdata=0;
	  	Bit#(XLEN) destination_value=0;
	  	case(funct3)
        'd0:case (csr_address[11:8])
              'h0, 'h3:begin // URET,  MRET
                let temp<-csrfile.upd_on_ret( `ifdef USERTRAPS unpack(csr_address[9:8]) `endif );
                jump_add=temp;
                flush=True;
              end
	  		    endcase
        default: begin
          if(funct3[2] == 1)
            op1=zeroExtend(rs1_addr);
          if(rd!=0)
            destination_value=csrread;
          if(funct3[1:0] == 'd1)
            writecsrdata = op1;
          else if(funct3[1:0] == 'd2)
            writecsrdata = op1|csrread;
          else
            writecsrdata = ~op1&csrread;
          csrfile.write_csr(csr_address, writecsrdata);
        end
      endcase
	  	return tuple3(flush,jump_add,destination_value);
	  endmethod
	
    method ActionValue#(Bit#(PADDR)) take_trap(Trap_type trap, Bit#(PADDR) pc, Bit#(PADDR) badaddr);
		  if(trap matches tagged Exception .ex)begin
		  	if(ex==Inst_addr_misaligned || ex==Inst_access_fault)
		  		badaddr=pc;
		  	else if(ex==Illegal_inst)
		  		badaddr=0;
		  end
		  Bit#(6) cause = 0;
		  Bit#(TSub #(6, 1)) cause_code = 0;
		  Bit#(1) cause_type = 0;
		  case(trap) matches
		  	tagged Interrupt .i: begin cause_type = 1; cause_code = zeroExtend(pack(i)); end
		  	tagged Exception .e: begin cause_type = 0; cause_code = zeroExtend(pack(e)); end
		  endcase
			cause = {cause_type, cause_code};
      let jump_address<-csrfile.upd_on_trap(`ifdef USERTRAPS Machine, `endif cause, pc, badaddr); 
		  return jump_address;
  	endmethod

    method csrs_to_decode = csrfile.csrs_to_decode;
	  method Action clint_msip(Bit#(1) intrpt);
	  	csrfile.clint_msip(intrpt);
	  endmethod
	  method Action clint_mtip(Bit#(1) intrpt);
	  	csrfile.clint_mtip(intrpt);
	  endmethod
	  method Action clint_mtime(Bit#(XLEN) c_mtime);
	  	csrfile.clint_mtime(c_mtime);
	  endmethod
    method Action externalinterrupt(Bit#(1) intrpt);
      csrfile.externalinterrupt(intrpt);
    endmethod
    method incr_minstret=csrfile.incr_minstret;
    `ifdef RV64 method inferred_xlen = csrfile.inferred_xlen; `endif // False-32bit,  True-64bit 
    method  interrupt=csrfile.interrupt;
  endmodule
endpackage
