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
package csrfile;
  
  // project related imports
  import common_types::*;
  `include "common_params.bsv"
  import ConcatReg::*;

  interface Ifc_csrfile;
    method Bit#(XLEN) read_csr (Bit#(12) addr);
    method Action write_csr(Bit#(12) addr,  Bit#(XLEN) word);
    method CSRtoDecode csrs_to_decode;
	  method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    method ActionValue#(Bit#(PADDR)) upd_on_ret ( `ifdef USERTRAPS Privilege_mode prv `endif ) ;
    method ActionValue#(Bit#(PADDR)) upd_on_trap(`ifdef USERTRAPS Privilege_mode prv, `endif 
                                                  Bit#(6) cause, Bit#(PADDR) pc, Bit#(PADDR) tval);
    method Action incr_minstret;
    method Bool interrupt;
  endinterface
  
  function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
       method t _read = r;
       method Action _write(t x) = noAction;
    endinterface);
  endfunction

  (*synthesize*)
  (*mutually_exclusive="upd_on_ret, write_csr"*)
  (*mutually_exclusive="upd_on_trap, write_csr"*)
  (*preempts="write_csr, increment_cycle_counter"*)
  (*preempts="write_csr, incr_minstret"*)
  module mkcsrfile(Ifc_csrfile);
    let maxIndex=valueOf(XLEN);
    let paddr=valueOf(PADDR);

  
    /////////////////////////////// Machine level register /////////////////////////
    // Current Privilege Level
	  Reg#(Privilege_mode) rg_prv <- mkReg(Machine); // resets to machine mode
	  
	  Bit#(XLEN) csr_mvendorid  = 0;
    Bit#(XLEN) csr_marchid    = 0;
    Bit#(XLEN) csr_mimpid     = 0;
    Bit#(XLEN) csr_mhartid    = 0;

	  //MISA fields
    // TODO: it is difficult to write a binary which can transition from 64-bit to 32-bit or vice
    // versa. This is achieved by changing the mxl bit. This is doesn't seem to be happening in the
    // near future. So not integrating it right now.
    Reg#(Bit#(2)) rg_mxl = readOnlyReg(fromInteger(valueOf(XLEN)/32));
    Bit#(26) temp_misa='h141101;
	 // temp_misa[8]=1;
	 // temp_misa[20]=1;
	 // `ifdef atomic	temp_misa[0]=1; `endif
	 // `ifdef dpfpu	temp_misa[3]=1; `endif
	 // `ifdef spfpu	temp_misa[5]=1; `endif
	 // `ifdef muldiv	temp_misa[12]=1; `endif
	  Reg#(Bit#(26)) rg_misa	<- mkReg(temp_misa);
     
    //MTVEC trap vector fields
	  Reg#(Bit#(2)) rg_mode <- mkReg(0); //0 if pc to base or 1 if pc to base + 4xcause
	  Reg#(Bit#(TSub#(PADDR,2))) rg_mtvec <- mkReg(0);

    // mstatus fields
    Bit#(1) sd = 0;
	  Bit#(1) tsr	  = 0; // 0 if supervisor not supported
    Bit#(1) tw	 	= 0; // 0 if supervisor not supported
    Bit#(1) tvm	  = 0; // 0 if supervisor not supported
    Bit#(1) mxr   = 0; // 0 if supervisor not supported
    Bit#(1) sum   = 0; // 0 if supervisor not supported
    Reg#(Bit#(1)) rg_mprv <- mkReg(0);
    Bit#(2) xs	 	= 0;
    Bit#(2) fs	 	= 0;
    Reg#(Bit#(2)) rg_mpp	<- mkReg(2'b0);
    Bit#(2) hpp	= 0;
    Bit#(1) spp	= 0;
    Reg#(Bit#(1)) rg_mpie <- mkReg(0);
    Bit#(1) hpie = 0;
    Bit#(1) spie = 0;
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_upie <- mkReg(0);
    `else
      Bit#(1) rg_upie = 0;
    `endif
	  Reg#(Bit#(1)) rg_mie	<- mkReg(0);
    Bit#(1) hie = 0;
    Bit#(1) sie = 0;
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_uie <- mkReg(0);
    `else
      Bit#(1) rg_uie = 0;
    `endif

	  // mie fields
    Reg#(Bit#(1)) rg_meie <- mkReg(0);
    Bit#(1) heie = 0;
    Bit#(1) seie = 0;
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_ueie <- mkReg(0);
    `else
      Bit#(1) rg_ueie = 0;
    `endif
    Reg#(Bit#(1)) rg_mtie <- mkReg(0);
    Bit#(1) htie = 0;
    Bit#(1) stie = 0;
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_utie <- mkReg(0);
    `else
      Bit#(1) rg_utie = 0;
    `endif
    Reg#(Bit#(1)) rg_msie <- mkReg(0);
    Bit#(1) hsie = 0;
    Bit#(1) ssie = 0;
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_usie <-  mkReg(0);
    `else
      Bit#(1) rg_usie = 0;
    `endif
   
   `ifdef USERTRAPS
      Reg#(Bit#(12)) rg_mideleg <- mkReg(0);
      Reg#(Bit#(11)) rg_medeleg <- mkReg(0);
    `else
      Bit#(12) rg_mideleg = 0;
      Bit#(11) rg_medeleg = 0;
    `endif
    
	  // mip fields
    Reg#(Bit#(1)) rg_meip <- mkReg(0);
    Bit#(1) heip = 0;
    Bit#(1) seip = 0; 
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_ueip <- mkReg(0); //extInterruptReg(rg_ueips,rg_ueipe);
    `else
      Bit#(1) rg_ueip = 0;
    `endif
    Reg#(Bit#(1)) rg_mtip <- mkReg(0);
    Bit#(1) htip = 0;
    Bit#(1) stip = 0;
    `ifdef USERTRAPS
      Reg#(Bit#(1)) rg_utip <- mkReg(0);
    `else
      Bit#(1) rg_utip = 0;
    `endif
	  Reg#(Bit#(1)) rg_msip <- mkReg(0);
    Bit#(1) hsip = 0;
    Bit#(1) ssip = 0;
    `ifdef USERTRAPS 
      Reg#(Bit#(1)) rg_usip <- mkReg(0);
    `else
      Bit#(1) rg_usip = 0;
	  `endif 

    `ifdef RV64
	  	Reg#(Bit#(XLEN)) mcycle <- mkReg(0);
	  	Reg#(Bit#(XLEN)) minstret<-mkReg(0);
	  `else
	  	Reg#(Bit#(XLEN)) mcycle<-mkReg(0);
	  	Reg#(Bit#(XLEN)) minstret<-mkReg(0);
	  	Reg#(Bit#(XLEN)) mcycleh<-mkReg(0);
	  	Reg#(Bit#(XLEN)) minstreth<-mkReg(0);
	  `endif

	  // Machine Trap Handling
	  Reg#(Bit#(PADDR)) rg_mepc  		<- mkReg(0);
	  Reg#(Bit#(PADDR))rg_mtval  		<- mkReg(0);
	  Reg#(Bit#(XLEN)) rg_mscratch <- mkReg(0);
    
    Reg#(Bit#(1)) rg_minterrupt <-mkReg(0);
	  Reg#(Bit#(5)) rg_mcause   <- mkReg(0);
    
	  Reg#(Bit#(3)) rg_mcounteren<-mkReg(0);
	  Reg#(Bit#(XLEN)) rg_clint_mtime <-mkReg(0);
	  //////////////////////////////////////////////////////////////////////////////////////////
	  //////////////////////////////// USER LEVEL CSRs ////////////////////////////////////////
	  Reg#(Bit#(XLEN)) rg_uscratch <- mkReg(0);
    Reg#(Bit#(2)) rg_uxl = readOnlyReg(fromInteger(valueOf(XLEN)/32));

    `ifdef USERTRAPS
  	  Reg#(Bit#(PADDR)) rg_uepc  		<- mkReg(0);
	    Reg#(Bit#(PADDR))rg_utval  		<- mkReg(0);
      Reg#(Bit#(1)) rg_uinterrupt <-mkReg(0);
  	  Reg#(Bit#(5)) rg_ucause   <- mkReg(0);
	    Reg#(Bit#(2)) rg_umode <- mkReg(0); //0 if pc to base or 1 if pc to base + 4xcause
  	  Reg#(Bit#(TSub#(PADDR,2))) rg_utvec <- mkReg(0);
    `endif
    //MTVEC trap vector fields
	  //////////////////////////////////////////////////////////////////////////////////////////
    
    Bit#(12) csr_mip= {rg_meip, heip, seip, rg_ueip, rg_mtip, htie, stie, rg_utip, rg_msip,
                          hsip, ssip, rg_usip};
    Bit#(12) csr_mie= {rg_meie, heie, seie, rg_ueie, rg_mtie, htie, stie, rg_utie, rg_msie,
                          hsie, ssie, rg_usie};
    
    rule increment_cycle_counter;
	  	`ifdef RV64
      	mcycle<=mcycle+1;
	  	`else
	  		Bit#(64) new_cycle={mcycleh,mcycle};
	  		new_cycle=new_cycle+1;
	  		mcycle<=new_cycle[31:0];
	  		mcycleh<=new_cycle[63:32];
	  	`endif
    endrule
    
    method Bit#(XLEN) read_csr (Bit#(12) addr);
        Bit#(XLEN) data=0;
        if (addr == `MVENDORID) data= csr_mvendorid;
        if (addr == `MARCHID) data= csr_marchid;
        if (addr == `MIMPID) data= csr_mimpid;
        if (addr == `MHARTID) data= csr_mhartid;
        if (addr == `MISA) begin 
          data[25:0]= rg_misa; 
          `ifdef RV64
            data[63:62]= rg_mxl; 
          `else
            data[31:30]=rg_mxl;
          `endif
        end
        if (addr == `MTVEC) data= {'d0, rg_mtvec, rg_mode};
        if (addr == `MSTATUS)
          `ifdef RV64 
            data= {sd, 27'd0, rg_uxl, rg_uxl, 9'd0, tsr, tw, tvm, mxr, sum, rg_mprv, xs, fs, rg_mpp,
                    hpp, spp, rg_mpie, hpie, spie, rg_upie, rg_mie, hie, sie, rg_uie};
          `else
            data= {sd, 8'd0, tsr, tw, tvm, mxr, sum, rg_mprv, xs, fs, rg_mpp, hpp, spp, rg_mpie,
                    hpie, spie, rg_upie, rg_mie, hie, sie, rg_uie};
          `endif
        `ifdef USERTRAPS
          if (addr == `MIDELEG) data= {'d0, rg_mideleg};
          if (addr == `MEDELEG) data= {'d0, rg_medeleg};
        `endif
        if (addr == `MIE) data= {'d0, rg_meie, heie, seie, rg_ueie, rg_mtie, htie, stie, rg_utie, rg_msie,
                      hsie, ssie, rg_usie};
        if (addr == `MIP) data= {'d0, rg_meip, heip, seip, rg_ueip, rg_mtip, htie, stie, rg_utip, rg_msip,
                      hsip, ssip, rg_usip};
        if (addr == `MCYCLE) data= mcycle;
        if (addr == `MINSTRET) data= minstret;
        `ifndef RV64
          if (addr == `MCYCLEH) data= mcycleh;
          if (addr == `MINSTRETH) data= minstreth;
        `endif
        if (addr == `MEPC) data= zeroExtend(rg_mepc);
        if (addr == `MTVAL) data= signExtend(rg_mtval);//?
        if (addr == `MSCRATCH) data= rg_mscratch;
        if (addr == `MCAUSE) data= {rg_minterrupt, 'd0, rg_mcause};
        if (addr == `MCOUNTEREN) data= zeroExtend(rg_mcounteren);
        if (addr == `MTIME) data= rg_clint_mtime;
        // =============== User level CSRs ================//
        if (addr == `USTATUS)
          `ifdef RV64 
            data= {sd, 27'd0, 2'd0, rg_uxl, 9'd0, tsr, tw, tvm, mxr, sum, rg_mprv, xs, fs, rg_mpp,
                    hpp, spp, rg_mpie, hpie, spie, rg_upie, rg_mie, hie, sie, rg_uie};
          `else
            data= {sd, 8'd0, tsr, tw, tvm, mxr, sum, rg_mprv, xs, fs, rg_mpp, hpp, spp, rg_mpie,
                    hpie, spie, rg_upie, rg_mie, hie, sie, rg_uie};
          `endif
        `ifdef USERTRAPS
          if (addr == `UIE) data= {'d0, rg_meie, heie, seie, rg_mideleg[8]&rg_ueie, rg_mtie, htie, 
                         stie, rg_mideleg[4]&rg_utie, rg_msie, hsie, ssie, rg_mideleg[0]&rg_usie}; 
          if (addr == `UIP) data= {'d0, rg_meip, heip, seip, rg_mideleg[8]&rg_ueip, rg_mtip, htie, 
                         stie, rg_mideleg[4]&rg_utip, rg_msip, hsip, ssip, rg_mideleg[0]&rg_usip};
        `endif
        if (addr == `UCYCLE) data= mcycle;
        if (addr == `UINSTRET) data= minstret;
        `ifndef RV64
          if (addr == `UCYCLEH) data= mcycleh;
          if (addr == `UINSTRETH) data= minstreth;
        `endif
        `ifdef USERTRAPS
          if (addr == `UTVEC) data= {'d0, rg_utvec, rg_umode};
          if (addr == `UEPC) data= zeroExtend(rg_uepc);
          if (addr == `UTVAL) data= zeroExtend(rg_utval);
          if (addr == `UCAUSE) data= {rg_uinterrupt, 'd0, rg_ucause};
        `endif
        if (addr == `USCRATCH) data= rg_uscratch;
        if (addr == `UTIME) data= rg_clint_mtime;
        return data;
    endmethod

    method Action write_csr(Bit#(12) addr,  Bit#(XLEN) word);
      case(addr)
        `MISA: begin 
          rg_misa<= truncate(word);
        end
        `MTVEC: begin 
          rg_mtvec<= word[paddr- 1:2]; 
          rg_mode<=word[1:0];
        end
        `MSTATUS: begin 
          `ifdef USERTRAPS
            rg_uie<= word[0];
            rg_upie<= word[4];
          `endif
          rg_mie<= word[3];
          rg_mpie<= word[7];
          rg_mpp<= word[12:11];
          rg_mprv<= word[17];
        end
        `ifdef USERTRAPS
          `MIDELEG: begin
            rg_mideleg<= truncate(word);
          end
          `MEDELEG: begin
            rg_medeleg<= truncate(word);
          end
        `endif
        `MIE: begin
          rg_msie<= word[3];
          rg_mtie<= word[7];
          rg_meie<= word[11];
          `ifdef USERTRAPS
            rg_ueie<= word[8];
            rg_usie<= word[0];
            rg_utie<= word[4];
          `endif
        end
        `ifdef USERTRAPS
          `MIP: begin
            rg_usip<= word[0];
            rg_utip<= word[4];
            rg_ueip<= word[8];
          end
        `endif
        `MCYCLE: begin
          mcycle<= word;
        end
        `MINSTRET:begin
          minstret<= word;
        end
        `ifndef RV64
          `MCYCLEH: mcycleh<= word;
          `MINSTRETH: minstreth<= word;
        `endif
        `MEPC: rg_mepc<= truncate(word);
        `MTVAL: rg_mtval<= truncate(word);
        `MSCRATCH: rg_mscratch<= word;
        `MCAUSE: begin
          rg_minterrupt<= word[maxIndex-1];
          rg_mcause<= truncate(word);
        end
        `MCOUNTEREN: rg_mcounteren<= truncate(word);
        `ifdef USERTRAPS
          `USTATUS: begin 
            rg_uie<= word[0];
            rg_upie<= word[4];
          end
        `endif
        `USCRATCH: rg_uscratch<= word;

        `ifdef USERTRAPS
          `UIE: begin
            rg_usie<= word[0];
            rg_utie<= word[4];
            rg_ueie<= word[8];
          end
          `UIP: begin
            `ifdef USERTRAPS
              rg_usip<= word[0];
              rg_utip<= word[4];
            `endif
            //TODO what happens for rg_ueip?
          end
          `UTVEC: begin 
            rg_utvec<= word[paddr- 1:2]; 
            rg_umode<=word[1:0];
          end
          `UEPC: rg_uepc<= truncate(word);
          `UTVAL: rg_utval<= truncate(word);
          `UCAUSE: begin
            rg_uinterrupt<= word[maxIndex-1];
            rg_ucause<= truncate(word);
          end
        `endif
        default: noAction;
      endcase
    endmethod
    method csrs_to_decode = tuple7(rg_prv, csr_mip, csr_mie, rg_mideleg, rg_misa, rg_mcounteren, rg_mie);
  	method Action clint_msip(Bit#(1) intrpt);
  		rg_msip<=intrpt;
  	endmethod
  	method Action clint_mtip(Bit#(1) intrpt);
  		rg_mtip<=intrpt;
  	endmethod
  	method Action clint_mtime(Bit#(XLEN) c_mtime);
  		rg_clint_mtime<=c_mtime;
  	endmethod
    method Action externalinterrupt(Bit#(1) intrpt);
      rg_meip<= intrpt;
    endmethod
    
    method ActionValue#(Bit#(PADDR)) upd_on_ret `ifdef USERTRAPS (Privilege_mode prv) `endif ;
      `ifdef USERTRAPS
        if(prv==Machine)begin
      `endif
        rg_mpie <= 1;
        rg_mpp <= pack(User);
        rg_prv <= unpack(rg_mpp);
	  		rg_mie<=rg_mpie;
        return rg_mepc;
      `ifdef USERTRAPS
        end
        else begin
          rg_upie <= 1;
          rg_prv <= User;
	  	  	rg_uie<=rg_upie;
          return rg_uepc;
        end
      `endif
    endmethod
    method ActionValue#(Bit#(PADDR)) upd_on_trap(`ifdef USERTRAPS Privilege_mode prv, `endif 
                                                  Bit#(6) cause, Bit#(PADDR) pc, Bit#(PADDR) tval);
  	  `ifdef USERTRAPS 
        if(prv==Machine)begin
      `endif
          rg_mtval<=tval;
			    rg_mepc<=pc;
			    rg_mcause<=cause[4:0];
          rg_minterrupt<= cause[5];
			    rg_mie <= 0;
			    rg_mpp <= pack(rg_prv);
			    rg_mpie <= rg_mie;
			    rg_prv <= Machine;
          if(rg_mode==0)
            return {rg_mtvec, 2'b0}; // pc jumps to base
          else
            return ({(rg_mtvec+ zeroExtend(cause)),2'b0}); // pc jumps to base+(4*cause)
  	  `ifdef USERTRAPS 
        end
        else begin
          rg_utval<=tval;
			    rg_uepc<=pc;
			    rg_ucause<=cause[4:0];
          rg_uinterrupt<= cause[5];
			    rg_uie <= 0;
			    rg_upie <= rg_uie;
			    rg_prv <= User;
          if(rg_umode==0)
            return {rg_utvec, 2'b0}; // pc jumps to base
          else
            return ({(rg_utvec+ zeroExtend(cause)),2'b0}); // pc jumps to base+(4*cause)
        end
      `endif
    endmethod
    method Action incr_minstret;
      `ifdef RV64
        minstret<= minstret+1;
      `else
        Bit#(TMul#(2, XLEN)) instr ={minstreth, minstret};
        instr=instr+1;
        minstreth<= truncateLSB(instr); minstret <= truncate(instr);
      `endif
    endmethod
    method interrupt = unpack(|(csr_mie&csr_mip));
  endmodule
endpackage
