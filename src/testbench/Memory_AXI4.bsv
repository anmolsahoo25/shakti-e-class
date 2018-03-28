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
package Memory_AXI4;
	/*====== Porject imports ====*/
	import common_types::*;
	`include "common_params.bsv"
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import axi_addr_generator::*;
	/*==== Package imports ======*/
 	import BRAMCore :: *;
	import BUtils::*;
	/*============================*/

	interface Memory_IFC#(numeric type base_address, numeric type mem_size);
		interface AXI4_Slave_IFC#(PADDR,XLEN,USERSPACE) axi_slave;
	endinterface
	typedef enum{Idle,HandleBurst} Mem_state deriving(Bits,Eq);
	module mkMemory#(parameter String mem_init_file1 `ifdef RV64 , parameter String 
        mem_init_file2 `endif  ,parameter String module_name) (Memory_IFC#(base_address,mem_size));
		
    let byte_offset = valueOf(Byte_offset);

    `ifdef RV64
		  BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemMSB <- 
                    mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file1,False);
		  BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemLSB <- 
                    mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file2,False);
    `else
  		BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemLSB <- 
                    mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file1,False);
    `endif
	
		AXI4_Slave_Xactor_IFC #(PADDR, XLEN, USERSPACE)  s_xactor <- mkAXI4_Slave_Xactor;
	
		Reg#(Mem_state) rd_state <-mkReg(Idle);
		Reg#(Mem_state) wr_state <-mkReg(Idle);
		Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
		Reg#(AXI4_Rd_Addr	#(PADDR,USERSPACE)) rg_read_packet <-mkReg(?);														   
		Reg#(AXI4_Wr_Addr	#(PADDR,USERSPACE)) rg_write_packet<-mkReg(?); 
	
		rule rl_wr_respond(wr_state==Idle);
	    let aw <- pop_o (s_xactor.o_wr_addr);
	    let w  <- pop_o (s_xactor.o_wr_data);
			Bit#(TSub#(mem_size,2)) index_address=(aw.awaddr - 
                            fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:byte_offset+1];
			dmemLSB.b.put(w.wstrb[3:0],index_address,truncate(w.wdata));
			`ifdef RV64 dmemMSB.b.put(w.wstrb[7:4],index_address,truncateLSB(w.wdata)); `endif
		  let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
			if(aw.awlen!=0) begin
				wr_state<=HandleBurst;
				let new_address=burst_address_generator(aw.awlen,aw.awsize,aw.awburst,aw.awaddr);
				aw.awaddr=new_address;
				rg_write_packet<=aw;
			end
			else
		   	s_xactor.i_wr_resp.enq (b);
			$display($time,"\t",module_name,":\t Recieved Write Request for Address: %h data: %h \
strb: %b awlen: %d awburst: %b",aw.awaddr,w.wdata,w.wstrb,aw.awlen, aw.awburst);
		endrule
	
		rule rl_wr_burst_response(wr_state==HandleBurst);
	    let w  <- pop_o (s_xactor.o_wr_data);
		  let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: rg_write_packet.awuser, 
                                                                        bid:rg_write_packet.awid};
			if(w.wlast)begin
				wr_state<=Idle;
				s_xactor.i_wr_resp.enq (b);
			end
			Bit#(TSub#(mem_size,2)) index_address=(rg_write_packet.awaddr - 
                            fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:byte_offset+1];
			dmemLSB.b.put(w.wstrb[3:0],index_address,truncate(w.wdata));
			`ifdef RV64 dmemMSB.b.put(w.wstrb[7:4],index_address,truncateLSB(w.wdata)); `endif
			let new_address=burst_address_generator(rg_write_packet.awlen, rg_write_packet.awsize,
                                                   rg_write_packet.awburst, rg_write_packet.awaddr);
			rg_write_packet.awaddr<=new_address;
        $display($time,"\t",module_name,":\t BURST Write Request for Address: %h data: %h strb: %b awlen: %d",rg_write_packet.awaddr,w.wdata,w.wstrb,rg_write_packet.awlen);  
		endrule
		
		rule rl_rd_request(rd_state==Idle);
			let ar<- pop_o(s_xactor.o_rd_addr);
			rg_read_packet<=ar;
			Bit#(TSub#(mem_size,2)) index_address=(ar.araddr - 
                            fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:byte_offset+1];
			dmemLSB.a.put(0,index_address,?);
			`ifdef RV64 dmemMSB.a.put(0,index_address,?); `endif
			rd_state<=HandleBurst;
        $display($time,"\t",module_name,"\t Recieved Read Request for Address: %h Index Address: %h"
                                                                          ,ar.araddr,index_address);  
		endrule
	
		rule rl_rd_response(rd_state==HandleBurst);
		  Bit#(XLEN) data0 = { `ifdef RV64 dmemMSB.a.read(), `endif dmemLSB.a.read()};
	    AXI4_Rd_Data#(XLEN,USERSPACE) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 ,
              rlast:rg_readburst_counter==rg_read_packet.arlen, ruser: 0, rid:rg_read_packet.arid};
			let transfer_size=rg_read_packet.arsize;
			let address=rg_read_packet.araddr;
      Bit#(TAdd#(Byte_offset, 1)) bytes = address[byte_offset:0];
      // TODO use shift logic instead of the below case statement.

			if(transfer_size==2)begin // 32 bit
				if(bytes==0)
					r.rdata=duplicate(data0[31:0]);
        `ifdef RV64
  				else
	  				r.rdata=duplicate(data0[63:32]);
        `endif
			end
	      else if (transfer_size=='d1)begin // half_word
				if(bytes ==0)
					r.rdata = duplicate(data0[15:0]);
				else if(bytes ==2)
					r.rdata = duplicate(data0[31:16]);
        `ifdef RV64
  				else if(bytes ==4)
	  				r.rdata = duplicate(data0[47:32]);
		  		else if(bytes ==6)
			  		r.rdata = duplicate(data0[63:48]);
        `endif
	      end
	      else if (transfer_size=='d0) begin// one byte
				if(bytes ==0)
	          r.rdata = duplicate(data0[7:0]);
	        else if(bytes ==1)
	          r.rdata = duplicate(data0[15:8]);
	        else if(bytes ==2)
	          r.rdata = duplicate(data0[23:16]);
	        else if(bytes ==3)
	          r.rdata = duplicate(data0[31:24]);
          `ifdef RV64
	          else if(bytes ==4)
					    r.rdata = duplicate(data0[39:32]);
	          else if(bytes ==5)
					    r.rdata = duplicate(data0[47:40]);
	          else if(bytes ==6)
					    r.rdata = duplicate(data0[55:48]);
	          else if(bytes ==7)
					    r.rdata = duplicate(data0[63:56]);
          `endif
	      end
	    s_xactor.i_rd_data.enq(r);
			address=burst_address_generator(rg_read_packet.arlen, rg_read_packet.arsize, 
                                                      rg_read_packet.arburst,rg_read_packet.araddr);
			Bit#(TSub#(mem_size,2)) index_address=(address - 
                            fromInteger(valueOf(base_address)))[valueOf(mem_size)-1:byte_offset+1];
			if(rg_readburst_counter==rg_read_packet.arlen)begin
				rg_readburst_counter<=0;
				rd_state<=Idle;
			end
			else begin
				dmemLSB.a.put(0,index_address,?);
				`ifdef RV64 dmemMSB.a.put(0,index_address,?); `endif
				rg_readburst_counter<=rg_readburst_counter+1;
			end
			rg_read_packet.araddr<=address;
			Bit#(XLEN) new_data=r.rdata;
        $display($time,"\t",module_name,"\t Responding Read Request with CurrAddr: %h Data: %8h \
          BurstCounter: %d BurstValue: %d NextAddress: %h", rg_read_packet.araddr, new_data,
            rg_readburst_counter, rg_read_packet.arlen, address);  
	   endrule
	
	   interface axi_slave= s_xactor.axi_side;
	endmodule
endpackage
