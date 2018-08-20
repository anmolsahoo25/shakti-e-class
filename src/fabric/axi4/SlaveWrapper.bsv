/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*/
package SlaveWrapper;
	/*======= Project Imports ====*/
	import AXI4_Types::*;
	import AXI4_Fabric  :: *;
	import AXI4_Lite_Types::*;
	import AXI4_Lite_Fabric  :: *;
	import Mem_Controller::*;
	/*==============================*/

	interface SlaveWrapper;
		interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) axi4slave;
		interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) axi4litemaster;
	endinterface

	module mkSlaveWrapper(SlaveWrapper);	
   let verbosity=`VERBOSITY;
   AXI4_Slave_Xactor_IFC #(`Addr_width, `Reg_width, 0)  s_axi4xactor <- mkAXI4_Slave_Xactor;
   AXI4_Lite_Master_Xactor_IFC #(`Addr_width, `Reg_width, 0)  m_axi4litexactor <- mkAXI4_Lite_Slave_Xactor;
	 
	 Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	 Reg#(Bit#(8)) rg_readburst_value<-mkReg(0);
	 Reg#(Bit#(8)) rg_writeburst_counter<-mkReg(0);
	 Reg#(Bit#(8)) rg_writeburst_value<-mkReg(0);
	 
   rule rl_wr_request;
      // Get the wr request
      let awreq <- pop_o (s_xactor.o_wr_addr);
      let wreq  <- pop_o (s_xactor.o_wr_data);
      let aw = AXI4_Lite_Wr_Addr {awaddr: awreq.awaddr, awprot:awreq.awprot, awuser:0}; 
      let w  = AXI4_Lite_Wr_Data {wdata:  wreq.wdata, wstrb: wreq.wstrb,};
      m_axi4litexactor.i_wr_addr.enq(aw);
      m_axi4litexactor.i_wr_data.enq(w);
			rg_writeburst_value<=awreq.awlen;
		endrule

		rule rl_wr_response;
			let axi4lite_wr_response<-pop_o(m_axi4litexactor.o_wr_resp);
	    let b = AXI4_Wr_Resp {bresp: axi4lite_wr_response.bresp, buser: axi4lite_wr_response.buser};
			if(rg_writeburst_counter==rg_writeburst_value)begin
				rg_writeburst_counter<=0;
      	s_axi4xactor.i_wr_resp.enq (b);
			end
			else
				rg_writeburst_counter<=rg_writeburst_counter+1;
			if(verbosity>1) $display($time,"\t",module_name,":\t Recieved Write Request for Address: %h data: %h strb: %b awlen: %d rg_writeburst_counter: %d",aw.awaddr,w.wdata,w.wstrb,aw.awlen,rg_writeburst_counter);  
   endrule

	endmodule

endpackage
