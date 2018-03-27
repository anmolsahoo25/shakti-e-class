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

package csr;
  // project related imports
  import common_types::*;
  `include "common_params.bsv"
  import ConcatReg::*;

  // package imports 
  import ConfigReg::*;
	
  interface Ifc_csr;
	  method ActionValue#(Tuple3#(Bool, Bit#(PADDR),Bit#(XLEN))) system_instruction(
            Bit#(12) csr_address, Bit#(5) rs1_addr, Bit#(XLEN) op1, Bit#(3) funct3);
    method CSRtoDecode csrs_to_decode;
    method ActionValue#(Bit#(PADDR)) take_trap(Trap_type trap, Bit#(PADDR) pc, Bit#(PADDR) badaddr);
		`ifdef CLINT
	  	method Action clint_msip(Bit#(1) intrpt);
			method Action clint_mtip(Bit#(1) intrpt);
			method Action clint_mtime(Bit#(XLEN) c_mtime);
		`endif
  endinterface:Ifc_csr

  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
       method t _read = r;
       method Action _write(t x) = noAction;
    endinterface);
  endfunction
  
  function Reg#(Bit#(a)) extInterruptReg(Reg#(Bit#(a)) r1, Reg#(Bit#(a)) r2);
    return (interface Reg;
            method Bit#(a) _read = r1 | r2;
            method Action _write(Bit#(a) x); 
            	r1._write(x);
				endmethod
        endinterface);
  endfunction


  (*synthesize*)
  module mkcsr(Ifc_csr);
    
    /////////////////////////////// Machine level register /////////////////////////
    // Current Privilege Level
	  Reg#(Privilege_mode) rg_prv <- mkConfigReg(Machine); // resets to machine mode
	  
	  Reg#(Bit#(XLEN)) csr_mvendorid = readOnlyReg(0);
    Reg#(Bit#(XLEN)) csr_marchid = readOnlyReg(0);
    Reg#(Bit#(XLEN)) csr_mimpid = readOnlyReg(0);
    Reg#(Bit#(XLEN)) csr_mhartid = readOnlyReg(0);
	  //misa fields
    `ifdef RV64
  	  Reg#(Bit#(2))  rg_mxl		= readOnlyReg(2);
    `else
  	  Reg#(Bit#(2))  rg_mxl		= readOnlyReg(1);
    `endif
	  Bit#(26) temp_misa='d0;
	  temp_misa[8]=1;
	  temp_misa[20]=1;
	  `ifdef atomic	temp_misa[0]=1; `endif
	  `ifdef dpfpu	temp_misa[3]=1; `endif
	  `ifdef spfpu	temp_misa[5]=1; `endif
	  `ifdef muldiv	temp_misa[12]=1; `endif
	  Reg#(Bit#(26)) rg_misa	<- mkReg(temp_misa);
	  Reg#(Bit#(XLEN)) csr_misa = concatReg3(rg_mxl,readOnlyReg(0),rg_misa);
     
    // trap vector fields (same as CSR without bottom 2 bits)
	  Reg#(Bit#(2)) rg_mode_m	<- mkReg(0); //0 if pc to base or 1 if pc to base + 4xcause
	  Reg#(Bit#(TSub#(PADDR,2))) rg_mtvec <- mkReg(0);
	  Reg#(Bit#(XLEN)) csr_mtvec=concatReg3(readOnlyReg(0),rg_mtvec, rg_mode_m);

    // mstatus fields
	  Reg#(Bit#(1)) rg_tsr	= readOnlyReg(0); // supervisor not supported
    Reg#(Bit#(1)) rg_tw	 	= readOnlyReg(0); // supervisor not supported
    Reg#(Bit#(1)) rg_tvm	= readOnlyReg(0); // supervisor not supported
    Reg#(Bit#(1)) rg_mxr  = readOnlyReg(0); // supervisor not supported
    Reg#(Bit#(1)) rg_sum  <- mkReg(0); // Not required
    Reg#(Bit#(1)) rg_mprv <- mkReg(0);
    Reg#(Bit#(2)) rg_xs	 	= readOnlyReg(0);
    Reg#(Bit#(2)) rg_fs	 	= readOnlyReg(2'b00);
    Reg#(Bit#(2)) rg_mpp	<- mkReg(2'b0);
    Reg#(Bit#(2)) rg_hpp	= readOnlyReg(0);
    Reg#(Bit#(1)) rg_spp	= readOnlyReg(0);
    Reg#(Bit#(1)) rg_mpie <- mkReg(0);
    Reg#(Bit#(1)) rg_hpie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_spie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_upie <- mkReg(0);
	  Reg#(Bit#(1)) rg_mie	<- mkReg(0);
    Reg#(Bit#(1)) rg_hie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_sie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_uie <- mkReg(0);
    Reg#(Bit#(1)) rg_sd	 	=  readOnlyReg(pack((rg_xs == 2'b11) || (rg_fs == 2'b11)));
    `ifdef RV64
    Reg#(Bit#(XLEN)) csr_mstatus =  concatReg24(
             rg_sd,
             readOnlyReg(0),
             rg_mxl, rg_mxl,          //sxl and uxl fields are hardwired to mxl in misa
             readOnlyReg(9'b0),
	  				 rg_tsr, rg_tw, rg_tvm,
             rg_mxr, rg_sum, rg_mprv, // memory privilege
             rg_xs, rg_fs, // coprocessor states
             rg_mpp, rg_hpp, rg_spp, // previous privileges
             rg_mpie, rg_hpie, rg_spie, rg_upie, // previous interrupt enables
             rg_mie, rg_hie, rg_sie, rg_uie); // interrupt enables
    `else
    Reg#(Bit#(XLEN)) csr_mstatus =  concatReg21(
             rg_sd,
             readOnlyReg(0),
	  				 rg_tsr, rg_tw, rg_tvm,
             rg_mxr, rg_sum, rg_mprv, // memory privilege
             rg_xs, rg_fs, // coprocessor states
             rg_mpp, rg_hpp, rg_spp, // previous privileges
             rg_mpie, rg_hpie, rg_spie, rg_upie, // previous interrupt enables
             rg_mie, rg_hie, rg_sie, rg_uie); // interrupt enables
    `endif
	  // trap delegation fields
    Reg#(Bit#(16)) rg_medeleg<-mkReg(0);
    Reg#(Bit#(15)) rg_mideleg<-mkReg(0);
    Reg#(Bit#(XLEN)) csr_medeleg = concatReg2(readOnlyReg(0),rg_medeleg);
    Reg#(Bit#(XLEN)) csr_mideleg = concatReg2(readOnlyReg(0),rg_mideleg);

	  // mie fields
    Reg#(Bit#(1)) rg_meie <- mkReg(0);
    Reg#(Bit#(1)) rg_heie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_seie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_ueie <- mkReg(0);
    Reg#(Bit#(1)) rg_mtie <- mkReg(0);
    Reg#(Bit#(1)) rg_htie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_stie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_utie <- mkReg(0);
    Reg#(Bit#(1)) rg_msie <- mkReg(0);
    Reg#(Bit#(1)) rg_hsie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_ssie = readOnlyReg(0);
    Reg#(Bit#(1)) rg_usie <- mkReg(0);
	  `ifdef Openocd
	  	Reg#(Bit#(1)) rg_dhalt<-mkReg(1);
	  	Reg#(Bit#(1)) rg_dresume<-mkReg(1);
	  `else
	  	Reg#(Bit#(1)) rg_dhalt<-mkReg(0);
	  	Reg#(Bit#(1)) rg_dresume<-mkReg(0);
	  `endif
	  Reg#(Bit#(1)) rg_dreset<-mkReg(0);
    Reg#(Bit#(XLEN)) csr_mie     =  concatReg16(
             readOnlyReg(0),
	  		     rg_dreset,rg_dresume,rg_dhalt,
             rg_meie, rg_heie, rg_seie, readOnlyReg(rg_ueie),
             rg_mtie, rg_htie, rg_stie, readOnlyReg(rg_utie),
             rg_msie, rg_hsie, rg_ssie, readOnlyReg(rg_usie));
	  Reg#(Bool) rg_nmi <- mkReg(True);

	  // mip fields
    Reg#(Bit#(1)) rg_meip <-  mkConfigReg(0);
    Reg#(Bit#(1)) rg_heip =  readOnlyReg(0);
    Reg#(Bit#(1)) rg_seips <-   mkReg(0);
    Reg#(Bit#(1)) rg_seipe <-   mkReg(0);
    Reg#(Bit#(1)) rg_ueips <-   mkReg(0);
    Reg#(Bit#(1)) rg_ueipe <-   mkReg(0);
    Reg#(Bit#(1)) rg_seip =   extInterruptReg(rg_seips,rg_seipe);
    Reg#(Bit#(1)) rg_ueip =   extInterruptReg(rg_ueips,rg_ueipe);
    Reg#(Bit#(1)) rg_mtip <-mkReg(0);
    Reg#(Bit#(1)) rg_htip =  readOnlyReg(0);
    Reg#(Bit#(1)) rg_stip <-  mkReg(0);
    Reg#(Bit#(1)) rg_utip <- mkReg(0);
	  Reg#(Bit#(1)) rg_msip <-  mkReg(0);
    Reg#(Bit#(1)) rg_hsip =  readOnlyReg(0);
    Reg#(Bit#(1)) rg_ssip <-  mkReg(0);
    Reg#(Bit#(1)) rg_usip <- mkReg(0);
 
	  `ifdef RV64
	  	Reg#(Bit#(XLEN)) csr_mcycle[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) csr_minstret[2]<-mkCReg(2,0);
	  `else
	  	Reg#(Bit#(XLEN)) csr_mcycle[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) csr_minstret[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) csr_mcycleh[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) csr_minstreth[2]<-mkCReg(2,0);
	  `endif

	  // Machine Trap Handling
	  Reg#(Bit#(XLEN)) 		 rg_mepc  		<- mkReg(0);
	  Reg#(Bit#(PADDR))	 rg_mtval  		<- mkReg(0);
	  Reg#(Bit#(XLEN)) csr_mscratch <- mkReg(0);
	  Reg#(Bit#(XLEN)) csr_mepc     = rg_mepc;
	  Reg#(Bit#(XLEN)) csr_mcause   <- mkReg(0);
	  Reg#(Bit#(XLEN)) csr_mtval		= concatReg2(readOnlyReg(0), rg_mtval);
	  Reg#(Bit#(XLEN)) csr_mip      =  concatReg13(
             readOnlyReg(0),
             readOnlyReg(rg_meip), readOnlyReg(rg_heip), rg_seip, rg_ueip,
             readOnlyReg(rg_mtip), readOnlyReg(rg_htip), rg_stip, rg_utip,
             readOnlyReg(rg_msip), readOnlyReg(rg_hsip), rg_ssip, rg_usip);
    
	  Reg#(Bit#(XLEN)) mip      =  concatReg13(
             readOnlyReg(0),
             rg_meip, rg_heip, rg_seip, rg_ueip,
             rg_mtip, rg_htip, rg_stip, rg_utip,
             rg_msip, rg_hsip, rg_ssip, rg_usip);
	  Reg#(Bit#(32)) reg_mcounteren<-mkReg(0);
  	Reg#(Bit#(XLEN)) csr_mcounteren=concatReg2(readOnlyReg(0),reg_mcounteren);
	  //////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////// User level registers ///////////////////////////////////
	  `ifdef RV64
	  	Reg#(Bit#(XLEN)) csr_uinstret=readOnlyReg(csr_minstret[1]);
	  	Reg#(Bit#(XLEN)) csr_ucycle=readOnlyReg(csr_mcycle[1]);
	  `else
	  	Reg#(Bit#(XLEN)) csr_uinstret=readOnlyReg(csr_minstret[1]);
	  	Reg#(Bit#(XLEN)) csr_ucycle=readOnlyReg(csr_mcycle[1]);
	  	Reg#(Bit#(XLEN)) csr_uinstreth=readOnlyReg(csr_minstreth[1]);
	  	Reg#(Bit#(XLEN)) csr_ucycleh=readOnlyReg(csr_mcycleh[1]);
	  `endif

	  Reg#(Bit#(XLEN)) rg_clint_mtime <-mkReg(0);
	  Reg#(Bit#(5)) rg_fflags<-mkReg(0);
	  Reg#(Bit#(3)) rg_frm<-mkReg(0);

    //////////////////////////////////////////////////////////////////////////////////////////
    /////////// Functions to access CSRs /////////////////////////////////////////////////////////////
	  function Reg#(Bit#(XLEN)) read_user_sro_registers(Bit#(8) addr);
	  	Reg#(Bit#(XLEN)) csr=(case (addr)
	  		`UCYCLE				:csr_ucycle;				
	  		`UTIME				  :readOnlyReg(rg_clint_mtime);  
	  		`UINSTRET			:csr_uinstret;
	  		`ifndef RV64 
	  			`UCYCLEH			:csr_ucycleh; 
	  			`UINSTRETH		:csr_uinstreth; 
	  		`endif
	  		default:readOnlyReg(0);
	  		endcase);
	  	return csr;
	  endfunction
	  function Reg#(Bit#(XLEN)) read_user_srw_registers(Bit#(8) addr);
	  	Reg#(Bit#(XLEN)) csr = readOnlyReg(0);
	  	return csr;
	  endfunction
	  function Reg#(Bit#(XLEN)) read_machine_srw_registers(Bit#(8) address);
	  	Reg#(Bit#(XLEN)) csr=(case(address)
	  		`MSTATUS			:csr_mstatus;		
	  		`MISA				:csr_misa;
	  		`MEDELEG			:csr_medeleg;
	  		`MIDELEG			:csr_mideleg;
	  		`MIE				:csr_mie;
	  		`MTVEC			:csr_mtvec;
	  		`MCOUNTEREN		:csr_mcounteren;
	  		`MSCRATCH		:csr_mscratch;
	  		`MEPC				:csr_mepc;
	  		`MCAUSE			:csr_mcause;
	  		`MTVAL			:csr_mtval;
	  		`MIP				:csr_mip;
	  		default: begin
	  			readOnlyReg(0);
	  		end
	  		endcase);
	  	return csr;
	  endfunction
	  function Reg#(Bit#(XLEN))read_machine_sro_registers(Bit#(8) address);
	  	Reg#(Bit#(XLEN)) csr=(case(address)
	  		`MVENDORID: csr_mvendorid;	
	  		`MARCHID: csr_marchid;
	  		`MIMPID: csr_mimpid;
	  		`MHARTID: csr_mhartid;
	  		default: readOnlyReg(0);
	  		endcase);
	  	return csr;
	  endfunction

	  function Reg#(Bit#(XLEN)) read_machine_counters(Bit#(8) address);
	  	Reg#(Bit#(XLEN)) csr=(case(address)
	  		`MCYCLE			:csr_mcycle[1];				
	  		`MINSTRET		:csr_minstret[1];
	  		`ifndef RV64
	  			`MCYCLEH		:csr_mcycleh[1];
	  			`MINSTRETH	:csr_minstreth[1];
	  		`endif
	  		default: begin
	  			readOnlyReg(0);
	  		end
	  		endcase);
	  	return csr;
	  endfunction

    function Reg#(Bit#(XLEN)) read_csr(Bit#(12) addr);
     Reg#(Bit#(XLEN)) csr=(
      case(addr[11:8])
	  	  'h3: read_machine_srw_registers(truncate(addr)); // machine standard read-write
	  	  'hB: read_machine_counters(truncate(addr)); // machine standard counters
	  	  'hC: read_user_sro_registers(truncate(addr)); // user standard read-only
	  	  'hF: read_machine_sro_registers(truncate(addr)); // machine standard read-only
        default: readOnlyReg(0);//read_perfcounter(address);
      endcase); 
      return csr;
    endfunction
	  
    function Bool hasCSRPermission(Bit#(12) address, Bool write);
      Bit#(12) csr_index = pack(address);
	  	Bool check_counter_permission = True;
      return ((pack(rg_prv) >= csr_index[9:8]) && !(write && csr_index[11:10]==2'b11) );
    endfunction
     
    // if the operand is not 0 then the instruction will perform a write on the CSR.
	  function Bool valid_csr_access(Bit#(12) csr_addr, Bit#(5) operand, Bit#(2) operation);
	  	Bool ret = hasCSRPermission(unpack(csr_addr), (operand != 0 || operation=='b01) ? True:False);
	  	return ret;
	  endfunction

    rule increment_cycle_counter;
	  	`ifdef RV64
      	csr_mcycle[0]<=csr_mcycle[0]+1;
	  	`else
	  		Bit#(64) new_cycle={csr_mcycleh[0],csr_mcycle[0]};
	  		new_cycle=new_cycle+1;
	  		csr_mcycle[0]<=new_cycle[31:0];
	  		csr_mcycleh[0]<=new_cycle[63:32];
	  	`endif
    endrule

	  method ActionValue#(Tuple3#(Bool, Bit#(PADDR),Bit#(XLEN))) system_instruction(
            Bit#(12) csr_address, Bit#(5) rs1_addr, Bit#(XLEN) op1, Bit#(3) funct3);

      Bool flush = False;
      Bit#(PADDR) jump_address=0;
	  	let csr_reg=read_csr(csr_address);
	  	Bit#(XLEN) destination_value=csr_reg;
	  	case(funct3)
        'd0:case (csr_address[11:8]) matches
	  		    'h3:begin  // MRET
	  			    Privilege_mode next_prv =unpack(rg_mpp);
        		  rg_mpie <= 1;
        		  rg_mpp <= pack(User);
        		  rg_prv <= next_prv;
        		  jump_address=truncate(csr_mepc);
              flush=True;
	  		  		rg_mie<=rg_mpie;
	  			  end
	  		  endcase
	  		'd1: csr_reg <= op1;					// CSRRW
      	'd2: if(rs1_addr!=0)  csr_reg <= op1 | csr_reg;			// CSRRS 
      	'd3: if(rs1_addr!=0)  csr_reg <= ~(op1) & csr_reg;    //	CSRRC 
      	'd5: csr_reg <= zeroExtend(rs1_addr);				// CSRRWI 
      	'd6: if(rs1_addr!=0)  csr_reg <= zeroExtend(rs1_addr) | csr_reg;		// CSRRSI 
      	'd7: if(rs1_addr!=0)  csr_reg <= ~(zeroExtend(rs1_addr)) & csr_reg; //CSRRCI 
      endcase
	  	return tuple3(flush,jump_address,destination_value);
	  endmethod
	
    method ActionValue#(Bit#(PADDR)) take_trap(Trap_type trap, Bit#(PADDR) pc, Bit#(PADDR) badaddr);
		  if(trap matches tagged Exception .ex)begin
		  	if(ex==Inst_addr_misaligned || ex==Inst_access_fault)
		  		badaddr=pc;
		  	else if(ex==Illegal_inst)
		  		badaddr=0;
		  end
		  Bit#(XLEN) cause = 0;
		  Bit#(TSub #(XLEN, 1)) cause_code = 0;
		  Bit#(1) cause_type = 0;
		  case(trap) matches
		  	tagged Interrupt .i: begin cause_type = 1; cause_code = zeroExtend(pack(i)); end
		  	tagged Exception .e: begin cause_type = 0; cause_code = zeroExtend(pack(e)); end
		  endcase
			cause = {cause_type, cause_code};
			rg_prv <= Machine;
			if(trap matches tagged Exception .ex)
				if(ex==Inst_addr_misaligned || ex==Inst_access_fault || ex==Illegal_inst || 
            ex==Load_access_fault || ex==Load_addr_misaligned ||  ex==Store_addr_misaligned 
              || ex==Store_access_fault)
  			  csr_mtval<=zeroExtend(badaddr);
			csr_mepc<=signExtend(pc);
			csr_mcause<=cause;
			rg_mie <= 0;
			rg_mpp <= pack(rg_prv);
			rg_mpie <= rg_mie;
		  return truncate(csr_mtvec);
  	endmethod

    method csrs_to_decode = tuple5(rg_prv, csr_mip, csr_mie, csr_mideleg, rg_mie);
	  `ifdef CLINT
	  	method Action clint_msip(Bit#(1) intrpt);
	  		rg_msip<=intrpt;
	  	endmethod
	  	method Action clint_mtip(Bit#(1) intrpt);
	  		rg_mtip<=intrpt;
	  	endmethod
	  	method Action clint_mtime(Bit#(`Reg_width) c_mtime);
	  		rg_clint_mtime<=c_mtime;
	  	endmethod
	  `endif
  endmodule
endpackage
