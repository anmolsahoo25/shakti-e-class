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
*/
package Tilelink_heavy_lite_bridge;
	/*=== Project imports ====*/
	import Tilelink_lite_Types::*;
	import Tilelink_Types::*;
	import Tilelink::*;
	import Tilelink_lite ::*;
	import tilelink_addr_generator ::*;
	import defined_types::*;
	`include "defined_parameters.bsv"
	/*======================*/
	/*=== Package imports ===*/
	import Clocks::*;
	/*=======================*/

	interface Ifc_Tilelink_Heavy_Lite_bridge;
		interface Ifc_fabric_side_slave_link slave_ifc_wr;
		interface Ifc_fabric_side_slave_link slave_ifc_rd;
		interface Ifc_fabric_side_master_link_lite rd_master_ifc;
		interface Ifc_fabric_side_master_link_lite wr_master_ifc;
	endinterface

	typedef enum {RegularReq,BurstReq} BridgeState deriving (Bits,Eq,FShow);

	(*synthesize*)
	module mkTilelink_heavy_lite_bridge#(Clock fast_clock, Reset fast_reset)(Ifc_Tilelink_Heavy_Lite_bridge);
    let verbosity=`VERBOSITY;
		Ifc_Slave_link  wr_s_xactor <- mkSlaveXactor(clocked_by fast_clock, reset_by fast_reset, True, True);
        Ifc_Slave_link  rd_s_xactor <- mkSlaveXactor(clocked_by fast_clock, reset_by fast_reset, True, True);
        Ifc_Master_link_lite rd_m_xactor <- mkMasterXactorLite(True, True);
        Ifc_Master_link_lite wr_m_xactor <- mkMasterXactorLite(True, True);

		Reg#(BridgeState) rd_state <-mkReg(RegularReq,clocked_by fast_clock, reset_by fast_reset);
		Reg#(BridgeState) wr_state <-mkReg(RegularReq,clocked_by fast_clock, reset_by fast_reset);
		Reg#(Bit#(4)) rd_id<-mkReg(0);
		Reg#(Bit#(4)) wr_id<-mkReg(0);
		Reg#(Bit#(8)) rd_request_counter<-mkReg(0,clocked_by fast_clock, reset_by fast_reset);
		Reg#(Bit#(8)) wr_request_counter<-mkReg(0,clocked_by fast_clock, reset_by fast_reset);
		Reg#(Bit#(8)) response_counter<-mkReg(0);
		Reg#(Bit#(8)) sync_rdburst_value <-mkSyncRegToCC(0,fast_clock,fast_reset);
		Reg#(A_channel) rg_read_packet <-mkReg(?,clocked_by fast_clock , reset_by fast_reset); //TODO
		Reg#(A_channel) rg_write_packet<-mkReg(?,clocked_by fast_clock , reset_by fast_reset);

		/*=== FIFOs to synchronize data between the two clock domains ====*/
		SyncFIFOIfc#(A_channel)		ff_rd_addr <-	mkSyncFIFOToCC(1,fast_clock,fast_reset);
		SyncFIFOIfc#(A_channel)		ff_wr_addr <-	mkSyncFIFOToCC(1,fast_clock,fast_reset);

		SyncFIFOIfc#(D_channel)	ff_rd_resp <-	mkSyncFIFOFromCC(1,fast_clock);
		SyncFIFOIfc#(D_channel) ff_wr_resp <-	mkSyncFIFOFromCC(1,fast_clock);
		/*=================================================================*/


		// These rule will receive the read request from the Tilelink-UH and pass it on to the Tilelink-UL fabric.
		// If the request is a burst then they are broken down to individual axi4lite read requests. These
		// are carried out in the next rule. 
		rule capture_read_requests_from_Axi4(rd_state==RegularReq);
			let request  <- rd_s_xactor.core_side.xactor_request.get;
			ff_rd_addr.enq(request);
			rg_read_packet<=request;
			Data_size beat_blocks<= request.a_size-3;
			Bit#(12) burst_counter = 1;
			burst_counter = burst_counter << beat_blocks;
			burst_counter = burst_counter-1;
			if(request.a_size>3) begin
				rd_state<=BurstReq;
				sync_rdburst_value<=burst_counter;
				rd_request_counter <= burst_counter;
			end
			else begin
				sync_rdburst_value<=0;
			end
		endrule
		// In case a read-burst request is received on the fast bus, then the bursts have to broken down into
		// individual slow-bus read requests. 
		// This is rule is fired after the first read-burst request is sent to the slow_bus. This rule will continue to 
		// fire as long as the slow bus has capacity to receive a new request and the burst is not complete.
		// the difference between the each individual requests on the slow bus is only the address. All other 
		// parameters remain the same.
		rule generate_bust_read_requests(rd_state==BurstReq);
			let request=rg_read_packet;
			let {transfer_size, address} = burst_address_generator(rg_read_packet.a_opcode, rg_read_packet.a_mask, 
																	 	rg_read_pack.a_addres, rg_read_packet.a_size);
			rg_read_packet.a_address = address;
			rg_read_packet<=request;
			ff_rd_addr.enq(request);
			if(rd_request_counter==0)begin
				rd_state<=RegularReq;
			end
			else
				rd_request_counter<=rd_request_counter-1;
		endrule
		rule send_read_request_on_slow_bus;
			let request=ff_rd_addr.first;
			ff_rd_addr.deq;
		 	let lite_request = A_channel_lite {a_opcode : request.a_opcode, a_size :request.a_size, a_source : 0, 
													a_address : request.a_address, a_mask : request.a_mask, a_data : ?}; // arburst: 00-FIXED 01-INCR 10-WRAP
			rd_m_xactor.core_side.master_request.put(lite_request);	
			rd_id<=request.a_source;
		endrule
		// This rule will capture the write request from the AXI4 fabric and pass it on to the AXI4Lite fabric.
		// In case of burst requests, they are broken down to individual requests of axi4lite writes. Care
		// needs to be taken when writes are of different sizes in settin the write-strobe correctly.
		rule capture_write_requests_from_Axi4(wr_state==RegularReq);
			let request  <- wr_s_xactor.core_side.xactor_request.get;
			ff_wr_addr.enq(request);
			rg_write_packet<=request;
			Data_size beat_blocks<= request.a_size-3;
			Bit#(12) burst_counter = 1;
			burst_counter = burst_counter << beat_blocks;
			burst_counter = burst_counter-1;
			if(request.a_size>3) begin
				wr_state<=BurstReq;
				//sync_rd_burst_value<=burst_counter; //TODO
				wr_request_counter <= burst_counter;
			end
			if(verbosity>1) $display($time,"\tAXIBRIDGE: Write Request"); 
			if(verbosity>1) $display($time,"\tAddress Channel :",fshow(wr_addr_req)); 
			if(verbosity>1) $display($time,"\tData Channel :",fshow(wr_data_req)); 
		endrule
		// In case a write-burst request is received on the fast bus, then the bursts have to broken down into
		// individual slow-bus write requests. 
		// This is rule is fired after the first write-burst request is sent to the slow_bus. This rule will continue to 
		// fire as long as the slow bus has capacity to receive a new request and the burst is not complete i.e.
		// fast bust xactor does not send wlast asserted.
		// The difference between the each individual requests on the slow bus is only the address. All other 
		// parameters remain the same.
		rule generate_bust_write_requests(wr_state==BurstReq);
			let request=rg_write_packet;
			let {transfer_size, address} = burst_address_generator(rg_read_packet.a_opcode, rg_read_packet.a_mask, 
																	 	rg_read_pack.a_addres, rg_read_packet.a_size);
			rg_write_packet.a_address = address;
			rg_write_packet<=request;
			ff_wr_addr.enq(request);
			if(rd_request_counter==0)begin
				wr_state<=RegularReq;
			end
			else
				wr_request_counter<=wr_request_counter-1;
			if(verbosity>1) $display($time,"\tAXIBRIDGE: Burst Write Request"); 
			if(verbosity>1) $display($time,"\tAddress Channel :",fshow(rg_write_packet)); 
			if(verbosity>1) $display($time,"\tData Channel :",fshow(wr_data_req)); 
		endrule
		rule send_write_request_on_slow_bus;
			let request  = ff_wr_addr.first;
		 	let request = A_channel_lite {a_opcode: request.a_opcode, a_size: request.a_size, a_source: 1,
                    a_address: request.a_address, a_mask: request.a_mask, a_data: request.a_data};
			wr_m_xactor.core_side.master_request.put(request);	
			ff_wr_addr.deq;
			wr_id<=request.a_source;
		endrule

		// This rule forwards the read response from the AXI4Lite to the AXI4 fabric.
		rule capture_read_responses;
			let response <- rd_m_xactor_rd.core_side.master_response.get;
			D_channel r = D_channel {d_opcode: response.d_opcode, d_param : ? , d_size : response.d_size,
														d_source : rd_id, d_sink : ?, d_data : response.d_data, d_error : response.d_error};
			//if(response_counter==sync_rdburst_value)
			//	response_counter<=0;
			//else
			//	response_counter<=response_counter+1;
			ff_rd_resp.enq(r);
		endrule
		rule send_read_response_on_fast_bus;
			ff_rd_resp.deq;
			rd_s_xactor.core_side.xactor_response.put(ff_rd_resp.first);
		endrule
		rule capture_write_responses;
			let response <- wr_m_xactor_rd.core_side.master_response.get;
			D_channel b = D_channel {d_opcode: response.d_opcode, d_param : ? , d_size : response.d_size,
														d_source : rd_id, d_sink : ?, d_data : response.d_data, d_error : response.d_error};
			if(response_counter==sync_burst_value) begin
				response_counter<=0;
				ff_wr_resp.enq(b);
			end
			else
				response_counter<=response_counter+1;
			ff_wr_resp.enq(b);
		endrule
		rule send_write_response_on_fast_bus;
			ff_wr_resp.deq;
			wr_s_xactor.i_wr_resp.enq(ff_wr_resp.first);
		endrule
		interface slave_ifc_wr=wr_s_xactor.fabric_side;
		interface slave_ifc_rd=rd_s_xactor.fabric_side;
		interface master_ifc_rd=rd_m_xactor.fabric_side;
		interface master_ifc_wr=rd_m_xactor.fabric_side;
	endmodule
endpackage
