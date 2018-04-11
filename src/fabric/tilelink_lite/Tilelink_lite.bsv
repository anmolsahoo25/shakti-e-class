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

Authors: Radhul Bodduna,  Neel Gala
Email: rahul.bodduna@gmail.com
Description: This module implements the fabric for the tile-link lite protocol.
*/
package Tilelink_lite;

import Tilelink_lite_Types ::*;
import common_types::*;
`include "common_params.bsv"
  `include "SoC.defines"
import Vector ::*;
import GetPut ::*;



interface Ifc_Master_tile#(numeric type a,  numeric type w,  numeric type z);
   interface Put#(A_channel_lite#(a, w, z)) xactor_request_d_master;
   interface Get#(D_channel_lite#(w, z)) xactor_response_master;
endinterface

interface Ifc_Slave_tile_lite#(numeric type a,  numeric type w,  numeric type z);
   interface Get#(A_channel_lite#(a, w, z)) xactor_request_to_slave;
   interface Put#(D_channel_lite#(w, z)) xactor_response_to_slave;
endinterface

// ================================================================
// The interface for the fabric module

interface Tilelink_Fabric_IFC_lite #(numeric type num_masters,
				                            numeric type num_slaves, 
                                    numeric type route, 
                                    numeric type a, 
                                    numeric type w,
                                    numeric type z);
				 
//   method Action reset; // TODO propagate resets to master to dequeue a stale ongoing request
   
   // From masters
   interface Vector #(num_masters, Ifc_master_tilelink_core_side_lite#(a, w, z)) v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  Ifc_slave_tilelink_core_side_lite#(a, w, z)) v_to_slaves;
endinterface

module mkTilelinkLite#(function Tuple2 #(Bool, Bit#(TLog#(num_slaves))) 
    fn_addr_to_slave_num(A_Opcode_lite cmd, Bit#(a) addr),  Vector#(num_slaves, Bit#(num_masters))
        master_route)(Tilelink_Fabric_IFC_lite#(num_masters, num_slaves, route, a, w, z));

  // The follow vectors and table below represent the possible master- slave connections. The
  // masters are encoded into a bit- vector. Currently there are 5 masters on the fabric: 
  // DMA (bit-4),  Debug (bit- 3),  IMEM (bit- 2),  DMEM Write (bit- 1),  DMEM Read (Bit- 0). 
  // We now create an array with depth = Number of slaves and width= Number of Masters (5). 
  // For a particular slave a 1 in the 5- bit vector indicates that there is a connection between
  // that slave interface and the respective master. 
  // This table is useful to avoid unnecessary connections such as: IMEM master may never need a
  // connection to I2C slave.
	// Transactors facing masters
	Vector #(num_masters,  Ifc_Master_tilelink_lite#(a, w, z))
	   xactors_masters <- replicateM (mkMasterFabricLite);

	// Transactors facing slaves
	Vector #(num_slaves,   Ifc_Slave_tilelink_lite#(a, w, z))
	    xactors_slaves    <- replicateM (mkSlaveFabricLite);

	function Bool fn_route_to_slave(Integer mj, Integer sj);
		Bool route_legal = False;
		let {legal, slave_num} = fn_addr_to_slave_num(
        xactors_masters[mj].fabric_side_request.fabric_a_channel.a_opcode,
        xactors_masters[mj].fabric_side_request.fabric_a_channel.a_address);
		if(legal && slave_num == fromInteger(sj))
			route_legal = True;
		return route_legal;
	endfunction

  Integer verbosity = `VERBOSITY;

	// These rules connect the masters and the slaves. If the sender is valid and the receiver is 
  // ready the the packet is exchanged. In addition the route must valid. 

	//The slave destination is determined by address map function
	for(Integer s = 0; s < valueOf(num_slaves); s = s+1) begin
		for(Integer m =0; m <valueOf(num_masters); m = m+1) begin
		  if(master_route[s][m]==1) begin
		  	rule rl_fabric_requests(fn_route_to_slave(m, s) &&
              xactors_masters[m].fabric_side_request.fabric_a_channel_valid && 
              xactors_slaves[s].fabric_side_request.fabric_a_channel_ready);
		  		let req = xactors_masters[m].fabric_side_request.fabric_a_channel; 
		  			xactors_masters[m].fabric_side_request.fabric_a_channel_ready(True);
		  			xactors_slaves[s].fabric_side_request.fabric_a_channel(req);
		  			if(verbosity > 1) 
              $display($time, "\tTILELINK : Beat exchanged master[%0d] -> slave[%0d]", m, s); 
		  	endrule
		  end
		  //else if() //TODO send the slave error
		end
	end

	//The master destination is determined by the signal in the D channel - d_source
	for(Integer m = 0; m < valueOf(num_masters); m = m+1) begin
		Rules rl_to_master = emptyRules();
		for(Integer s = 0; s < valueOf(num_slaves); s = s+1) begin
			Rules rs_to_master = (rules
			rule rl_fabric_responses(xactors_slaves[s].fabric_side_response.fabric_d_channel.d_source==
        fromInteger(m)	&&	xactors_slaves[s].fabric_side_response.fabric_d_channel_valid);
				let resp = xactors_slaves[s].fabric_side_response.fabric_d_channel; 
				if(	xactors_masters[m].fabric_side_response.fabric_d_channel_ready) begin
					xactors_slaves[s].fabric_side_response.fabric_d_channel_ready(True);
					xactors_masters[m].fabric_side_response.fabric_d_channel(resp);
					if(verbosity > 1) 
            $display($time, "\tTILELINK : Beat exchanged maser[%0d] <- slave[%0d]", m, s); 
				end
				//else if() //TODO send the slave error
			endrule
			endrules);
			rl_to_master = rJoinPreempts(rs_to_master, rl_to_master);
		end
		addRules(rl_to_master);
	end

  Vector #(num_masters, Ifc_master_tilelink_core_side_lite#(a, w, z))  temp_v_from_masters;

  Vector #(num_slaves,  Ifc_slave_tilelink_core_side_lite#(a, w, z)) temp_v_to_slaves;

	for(Integer m=0; m < valueOf(num_masters); m=m+1) begin 

		temp_v_from_masters[m] = xactors_masters[m].v_from_masters;

	end

	for(Integer s=0; s < valueOf(num_slaves); s=s+1) begin

		temp_v_to_slaves[s] = xactors_slaves[s].v_to_slaves;

	end

	interface v_from_masters = temp_v_from_masters;
	interface v_to_slaves = temp_v_to_slaves;

endmodule

endpackage
