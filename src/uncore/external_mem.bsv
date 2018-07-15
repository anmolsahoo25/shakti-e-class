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
package external_mem;
	import AXI4_Types:: *;
	import AXI4_Fabric:: *;
  import Semi_FIFOF::*;
  import common_types::*;
  `include "common_params.bsv"
  import Clocks::*;
  import FIFO::*;
  import FIFOF::*;
  import SpecialFIFOs::*;

  interface Ifc_exteral_mem#(numeric type awidth, numeric type dwidth, numeric type uwidth) ;
    interface AXI4_Slave_IFC#(awidth, dwidth, uwidth) slave;
    interface AXI4_Master_IFC#(awidth, dwidth, uwidth) master;
  endinterface

  (*preempts="receive_write_request, receive_data_request"*)
  (*preempts="send_write_request_out, send_data_request_out"*)
  module mkexternal_mem#(Clock external_clk, Reset external_rst)(Ifc_exteral_mem#(awidth, dwidth, uwidth));
    let def_clk <- exposeCurrentClock;
    let def_rst <- exposeCurrentReset;
	  AXI4_Slave_Xactor_IFC #(awidth, dwidth, uwidth)  s_xactor <- mkAXI4_Slave_Xactor;
		AXI4_Master_Xactor_IFC #(awidth, dwidth, uwidth) m_xactor <- mkAXI4_Master_Xactor(clocked_by
        external_clk, reset_by external_rst);
    SyncFIFOIfc#(AXI4_Rd_Addr #(awidth, uwidth)) ff_read_request<-mkSyncFIFOFromCC(3, external_clk);
    SyncFIFOIfc#(AXI4_Wr_Addr #(awidth, uwidth)) ff_write_request<-
                                                                mkSyncFIFOFromCC(3, external_clk);
    SyncFIFOIfc#(AXI4_Wr_Data #(dwidth)) ff_data_request<-mkSyncFIFOFromCC(3, external_clk);
    SyncFIFOIfc#(AXI4_Rd_Data #(dwidth, uwidth)) ff_read_response<-
                                                    mkSyncFIFOToCC(3, external_clk, external_rst);
    SyncFIFOIfc#(AXI4_Wr_Resp #(uwidth)) ff_write_response<-
                                                    mkSyncFIFOToCC(3, external_clk, external_rst);
   
    rule receive_read_request;
      let ar<-pop_o(s_xactor.o_rd_addr);
      ff_read_request.enq(ar);
    endrule
    rule send_read_req_out;
      let req=ff_read_request.first;
      ff_read_request.deq;
   		m_xactor.i_rd_addr.enq(req);	
    endrule

    rule receive_write_request;
      let aw<-pop_o(s_xactor.o_wr_addr);
      let w<-pop_o(s_xactor.o_wr_data);
      ff_write_request.enq(aw);
      ff_data_request.enq(w);
    endrule
    rule send_write_request_out;
      let aw=ff_write_request.first;
      let w = ff_data_request.first;
      ff_write_request.deq;
      ff_data_request.deq;
	  	m_xactor.i_wr_addr.enq(aw);
		  m_xactor.i_wr_data.enq(w);
    endrule

    rule receive_data_request;
      let w<-pop_o(s_xactor.o_wr_data);
      ff_data_request.enq(w);
    endrule
    rule send_data_request_out;
      let w = ff_data_request.first;
      ff_data_request.deq;
		  m_xactor.i_wr_data.enq(w);
    endrule

    rule send_read_response;
      let r=ff_read_response.first;
      ff_read_response.deq;
      s_xactor.i_rd_data.enq(r);
    endrule
    rule receive_read_response;
      let resp<-pop_o(m_xactor.o_rd_data);
      ff_read_response.enq(resp);
    endrule

    rule send_write_response;
      let b=ff_write_response.first;
      ff_write_response.deq;
      s_xactor.i_wr_resp.enq(b);
    endrule
    rule receive_write_response;
      let resp<-pop_o(m_xactor.o_wr_resp);
      ff_write_response.enq(resp);
    endrule

    interface slave = s_xactor.axi_side;
    interface master = m_xactor.axi_side;

  endmodule
//
//  interface Ifc_syn ;
//    interface AXI4_Slave_IFC#(PADDR, XLEN, 0) slave;
//    interface AXI4_Master_IFC#(PADDR, XLEN, 0) master;
//  endinterface
//  (*synthesize*)
//  module mksyn(Ifc_syn);
//    let def_clk<-exposeCurrentClock;
//    let def_rst<-exposeCurrentReset;
//    Ifc_exteral_mem#(PADDR, XLEN, 0) external_mem <-mkexternal_mem(def_clk, def_rst);
//    interface slave=external_mem.slave;
//    interface master=external_mem.master;
//  endmodule
endpackage
