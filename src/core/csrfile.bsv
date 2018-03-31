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


 module mkcsrfile(Empty);
    let maxIndex=valueOf(XLEN);
    let paddr=valueOf(PADDR);

    function Bit#(XLEN) read_csr (Bit#(12) addr);
      case(addr)
        `MVENDORID: return csr_mvendorid;
        `MARCHID: return csr_marchid;
        `MIMPID: return csr_mimpid;
        `MHARTID: return csr_mhartid;
        'MISA: return {mxl, 4'd0, rg_misa};
        `MTVEC: return {'d0, rg_mtvec, rg_mode};
        `MSTATUS: 
          `ifdef RV64 
            return {sd, 27'd0, mxl, mxl, 9'd0, tsr, tw, tvm, mxr, sum, rg_mprv, xs, fs, rg_mpp,
                    hpp, spp, rg_mpie, hpie, spie, rg_upie, rg_mie, hie, sie, rg_uie}
          `else
            return {sd, 8'd0, tsr, tw, tvm, mxr, sum, rg_mprv, xs, fs, rg_mpp, hpp, spp, rg_mpie,
                    hpie, spie, rg_upie, rg_mie, hie, sie, rg_uie}
          `endif
        `MIDELEG: return {'d0, rg_mideleg};
        `MEDELEG: return {'d0, rg_medeleg};
        `MIE: return {'d0, rg_meie, heie, seie, rg_ueie, rg_mtie, htie, stie, rg_utie, rg_msie,
                      hsie, ssie, rg_usie};
        `MIP: return {'d0, rg_meip, heip, seip, rg_ueip, rg_mtip, htie, stie, rg_utip, rg_msip,
                      hsip, ssip, rg_usip};
        `MCYCLE: return mcycle[1];
        `MINSTRET: return minstret[1];
        `ifndef RV64
          `MCYCLEH: return mcycleh[1];
          `MINSTRETH: return minstreth[1];
        `endif
        `MEPC: return rg_mepc;
        `MTVAL: return zeroExtend(rg_mtval);
        `MSCRATCH: return rg_mscratch;
        `MCAUSE: return rg_mcause;
        `UIE: return {'d0, rg_meie, heie, seie, rg_mideleg[8]&rg_ueie, rg_mtie, htie, stie,
                      rg_mideleg[4]&rg_utie, rg_msie, hsie, ssie, rg_mideleg[0]&rg_usie}; 
        `UIP: return {'d0, rg_meip, heip, seip, rg_mideleg[8]&rg_ueip, rg_mtip, htie, stie,
                      rg_mideleg[4]&rg_utip, rg_msip, hsip, ssip, rg_mideleg[0]&rg_usip};
        `UCYCLE: return mcycle;
        `UINSTRET: return minstret;
        `ifndef RV64
          `UCYCLEH: return mcycleh[1];
          `UINSTRETH: return minstreth[1];
        `endif
      endcase
    endfunction

    function void write_csr(Bit#(12) addr,  Bit#(XLEN) word);
      case(addr)
        'MISA: begin 
          rg_misa<= truncate(word);
        end
        `MTVEC: begin 
          rg_mtvec<= word[paddr- 1:2]; 
          rg_mode<=word[1:0];
        end
        `MSTATUS: begin 
          rg_uie<= word[0];
          rg_mie<= word[3];
          rg_upie<= word[4];
          rg_mpie<= word[7];
          rg_mpp<= word[12:11];
          rg_mprv<= word[17];
        end
        `MIDELEG: begin
          rg_mideleg<= truncate(word);
        end
        `MEDELEG: begin
          rg_medeleg<= truncate(word);
        end
        `MIE: begin
          rg_usie<= word[0];
          rg_msie<= word[3];
          rg_utie<= word[4];
          rg_mtie<= word[7];
          rg_ueie<= word[8];
          rg_meie<= word[11];
        end
        `MIP: begin
          rg_usip<= word[0];
          rg_utip<= word[4];
          rg_ueip<= word[8];
        end
        `MCYCLE: begin
          mcycle[1]<= word;
        end
        `MINSTRET:begin
          minstret[1]<= word;
        end
        `ifndef RV64
          `MCYCLEH: mcycleh<= word;
          `MINSTRETH: minstreth<= word;
        `endif
        `MEPC: rg_mepc<= word;
        `MTVAL: rg_mtval<= truncate(word);
        `MSCRATCH: rg_mscratch<= word;
        `MCAUSE: rg_mcause<= word;
        `UIE: begin
          rg_usie<= word[0];
          rg_utie<= word[4];
          rg_ueie<= word[8];
        end
        `UIP: begin
          rg_usip<= word[0];
          rg_utip<= word[4];
          //TODO what happens for rg_ueip?
        end
      endcase
    endfunction
  
    /////////////////////////////// Machine level register /////////////////////////
    // Current Privilege Level
	  Reg#(Privilege_mode) rg_prv <- mkConfigReg(Machine); // resets to machine mode
	  
	  Bit#(XLEN) csr_mvendorid  = 0;
    Bit#(XLEN) csr_marchid    = 0;
    Bit#(XLEN) csr_mimpid     = 0;
    Bit#(XLEN) csr_mhartid    = 0;

	  //MISA fields
  	Bit#(2)  mxl	= `ifdef RV64 2 `else 1 `endif ;
    Bit#(26) temp_misa='d0;
	  temp_misa[8]=1;
	  temp_misa[20]=1;
	  `ifdef atomic	temp_misa[0]=1; `endif
	  `ifdef dpfpu	temp_misa[3]=1; `endif
	  `ifdef spfpu	temp_misa[5]=1; `endif
	  `ifdef muldiv	temp_misa[12]=1; `endif
	  Reg#(Bit#(26)) rg_misa	<- mkReg(temp_misa);
     
    //MTVEC trap vector fields
	  Bit#(2) rg_mode <- mkReg(0); //0 if pc to base or 1 if pc to base + 4xcause
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
    Reg#(Bit#(1)) rg_upie <- mkReg(0);
	  Reg#(Bit#(1)) rg_mie	<- mkReg(0);
    Bit#(1) hie = 0;
    Bit#(1) sie = 0;
    Reg#(Bit#(1)) rg_uie <- mkReg(0);

	  // mie fields
    Reg#(Bit#(1)) rg_meie <- mkReg(0);
    Bit#(1) heie = 0;
    Bit#(1) seie = 0;
    Reg#(Bit#(1)) rg_ueie <- mkReg(0);
    Reg#(Bit#(1)) rg_mtie <- mkReg(0);
    Bit#(1) htie = 0;
    Bit#(1) stie = 0;
    Reg#(Bit#(1)) rg_utie <- mkReg(0);
    Reg#(Bit#(1)) rg_msie <- mkReg(0);
    Bit#(1) hsie = 0;
    Bit#(1) ssie = 0;
    Reg#(Bit#(1)) rg_usie <-  mkReg(0);
    
    Reg#(Bit#(12)) rg_mideleg <- mkReg(0);
    Reg#(Bit#(11)) rg_medeleg <- mkReg(0);
    
	  // mip fields
    Reg#(Bit#(1)) rg_meip <- mkReg(0);
    Bit#(1) heip = 0;
    Bit#(1) seip = 0; 
    Reg#(Bit#(1)) rg_ueip =  readOnlyReg(0); //extInterruptReg(rg_ueips,rg_ueipe);
    Reg#(Bit#(1)) rg_mtip <- mkReg(0);
    Bit#(1) htip = 0;
    Bit#(1) stip = 0;
    Reg#(Bit#(1)) rg_utip =  readOnlyReg(0);
	  Reg#(Bit#(1)) rg_msip <- mkReg(0);
    Bit#(1) hsip = 0;
    Bit#(1) ssip = 0;
    Reg#(Bit#(1)) rg_usip =  readOnlyReg(0);
	  
    `ifdef RV64
	  	Reg#(Bit#(XLEN)) mcycle[2] <- mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) minstret[2]<-mkCReg(2,0);
	  `else
	  	Reg#(Bit#(XLEN)) mcycle[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) minstret[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) mcycleh[2]<-mkCReg(2,0);
	  	Reg#(Bit#(XLEN)) minstreth[2]<-mkCReg(2,0);
	  `endif

	  // Machine Trap Handling
	  Reg#(Bit#(XLEN)) rg_mepc  		<- mkReg(0);
	  Reg#(Bit#(PADDR))rg_mtval  		<- mkReg(0);
	  Reg#(Bit#(XLEN)) rg_mscratch <- mkReg(0);
	  Reg#(Bit#(XLEN)) rg_mcause   <- mkReg(0);
    
	  Reg#(Bit#(32)) reg_mcounteren<-mkReg(0);
  	Reg#(Bit#(XLEN)) csr_mcounteren=concatReg2(readOnlyReg(0),reg_mcounteren);
	  //////////////////////////////////////////////////////////////////////////////////////////
 endmodule
endpackage
