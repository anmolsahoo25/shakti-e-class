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
package BootRom;
	import common_types::*;
	`include "common_params.bsv"
  import BRAMCore :: *;
	import DReg::*;
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import BUtils::*;
	import axi_addr_generator::*;

interface BootRom_IFC;
	interface AXI4_Slave_IFC#(PADDR, XLEN, USERSPACE) axi_slave;
endinterface

typedef enum{Idle, HandleBurst} Mem_state deriving(Bits, Eq);

(*synthesize*)
module mkBootRom#(Bit#(PADDR) base_address)(BootRom_IFC);

  Integer verbosity = `VERBOSITY;
	// we create 2 32-bit BRAMs since the xilinx tool is easily able to map them to BRAM32BE cells
	// which makes it easy to use data2mem for updating the bit file.
	BRAM_PORT#(Bit#(13), Bit#(32)) dmemMSB <- mkBRAMCore1Load(valueOf(TExp#(13)), False, 
                                                                                "boot.MSB", False);
	BRAM_PORT#(Bit#(13), Bit#(32)) dmemLSB <- mkBRAMCore1Load(valueOf(TExp#(13)), False, 
                                                                                "boot.LSB", False);

	AXI4_Slave_Xactor_IFC #(PADDR,  XLEN, USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;

	Reg#(Mem_state) rd_state <-mkReg(Idle);
	Reg#(Mem_state) wr_state <-mkReg(Idle);
	Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	Reg#(AXI4_Rd_Addr	#(PADDR, USERSPACE)) rg_read_packet <-mkReg(?);
	Reg#(AXI4_Wr_Resp	#(USERSPACE)) rg_write_response <-mkReg(?);
	rule rl_wr_respond(wr_state==Idle);
    let aw <- pop_o (s_xactor.o_wr_addr);
    let w  <- pop_o (s_xactor.o_wr_data);
	  let b = AXI4_Wr_Resp {bresp: AXI4_SLVERR, buser: aw.awuser, bid:aw.awid};
		rg_write_response<=b;
		if(aw.awburst!=0)
			wr_state<=HandleBurst;
		else
	  	s_xactor.i_wr_resp.enq (b);
    if(verbosity!= 0)
  		$display($time, "\tBootROM: Illegal Write operation on BootROM");
	endrule

	rule rl_wr_burst_response(wr_state==HandleBurst);
    let w  <- pop_o (s_xactor.o_wr_data);
		if(w.wlast) begin
			wr_state<=Idle;
	    s_xactor.i_wr_resp.enq (rg_write_response);
		end
	endrule

	rule rl_rd_request(rd_state==Idle);
		let ar<- pop_o(s_xactor.o_rd_addr);
		rg_read_packet<=ar;
	  Bit#(13) index_address=(ar.araddr-(base_address))[15:3];
		dmemLSB.put(False, index_address, ?);
		dmemMSB.put(False, index_address, ?);
		rd_state<=HandleBurst;
		if(verbosity!= 0)
      $display($time, "\tBootROM: Recieved Read Request for Address: %h Index Address: %h",  
                                                                          ar.araddr, index_address);
	endrule

	rule rl_rd_response(rd_state==HandleBurst);
    Bit#(XLEN) data0 = {dmemMSB.read(), dmemLSB.read()};
    AXI4_Rd_Data#(XLEN, USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 , 
        rlast:rg_readburst_counter==rg_read_packet.arlen,  ruser: 0, rid:rg_read_packet.arid};
		let transfer_size=rg_read_packet.arsize;
		let address=rg_read_packet.araddr;
		if(transfer_size==2)begin // 32 bit
			if(address[2:0]==0)
				r.rdata=duplicate(data0[31:0]);
			else
				r.rdata=duplicate(data0[63:32]);
		end
    else if (transfer_size=='d1)begin // half_word
			if(address[2:0] ==0)
				r.rdata = duplicate(data0[15:0]);
			else if(address[2:0] ==2)
				r.rdata = duplicate(data0[31:16]);
			else if(address[2:0] ==4)
				r.rdata = duplicate(data0[47:32]);
			else if(address[2:0] ==6)
				r.rdata = duplicate(data0[63:48]);
      end
    else if (transfer_size=='d0) begin// one byte
			if(address[2:0] ==0)
        r.rdata = duplicate(data0[7:0]);
      else if(address[2:0] ==1)
        r.rdata = duplicate(data0[15:8]);
      else if(address[2:0] ==2)
        r.rdata = duplicate(data0[23:16]);
      else if(address[2:0] ==3)
        r.rdata = duplicate(data0[31:24]);
      else if(address[2:0] ==4)
				r.rdata = duplicate(data0[39:32]);
      else if(address[2:0] ==5)
				r.rdata = duplicate(data0[47:40]);
      else if(address[2:0] ==6)
				r.rdata = duplicate(data0[55:48]);
      else if(address[2:0] ==7)
				r.rdata = duplicate(data0[63:56]);
    end
    s_xactor.i_rd_data.enq(r);
		address=burst_address_generator(rg_read_packet.arlen, rg_read_packet.arsize, 
        rg_read_packet.arburst, rg_read_packet.araddr);
	 	Bit#(13) index_address=(address-(base_address))[15:3];
		if(rg_readburst_counter==rg_read_packet.arlen)begin
			rg_readburst_counter<=0;
			rd_state<=Idle;
		end
		else begin
			dmemLSB.put(False, index_address, ?);
			dmemMSB.put(False, index_address, ?);
			rg_readburst_counter<=rg_readburst_counter+1;
		end
		rg_read_packet.araddr<=address;
		Bit#(64) new_data=r.rdata;
		if(verbosity!=0) 
      $display($time, "\tBootROM : Responding Read Request with CurrAddr: %h Data: %8h \
          BurstCounter: %d BurstValue: %d NextAddress: %h", rg_read_packet.araddr, new_data, 
          rg_readburst_counter, rg_read_packet.arlen, address);
   endrule

   interface axi_slave= s_xactor.axi_side;
endmodule
endpackage
