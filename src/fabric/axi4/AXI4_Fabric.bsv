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
// Copyright (c) 2013-2017 Bluespec, Inc. All Rights Reserved

package AXI4_Fabric;

// ================================================================
// This package defines a fabric connecting CPUs, Memories and DMAs
// and other IP blocks.

// ================================================================
// Bluespec library imports

import Vector    :: *;
import FIFOF     :: *;
import ConfigReg :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;

// ================================================================
// Project imports

import Semi_FIFOF        :: *;
import AXI4_Types   :: *;

// ================================================================
// The interface for the fabric module

interface AXI4_Fabric_IFC #(numeric type num_masters,
				 numeric type num_slaves,
				 numeric type wd_addr,
				 numeric type wd_data,
				 numeric type wd_user);
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(num_masters, AXI4_Slave_IFC #(wd_addr, wd_data, wd_user))  v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  AXI4_Master_IFC #(wd_addr, wd_data, wd_user)) v_to_slaves;
endinterface

// ================================================================
// The Fabric module
// The function parameter is an address-decode function, which returns
// returns (True,  slave-port-num)  if address is mapped to slave-port-num
//         (False, ?)               if address is unmapped to any port

module mkAXI4_Fabric #(function Tuple2 #(Bool, Bit #(TLog #(num_slaves)))
			             fn_addr_to_slave_num (Bit #(wd_addr) addr))
                          (AXI4_Fabric_IFC #(num_masters, num_slaves, wd_addr, wd_data, wd_user))

   provisos (Log #(num_masters, log_nm),
	     Log #(num_slaves,  log_ns),
	     Log #(TAdd #(num_masters, 1), log_nm_plus_1),
	     Log #(TAdd #(num_slaves,  1), log_ns_plus_1),
	     Add #(_dummy, TLog #(num_slaves), log_ns_plus_1));


   // Transactors facing masters
   Vector #(num_masters, AXI4_Slave_Xactor_IFC  #(wd_addr, wd_data, wd_user))
      xactors_from_masters <- replicateM (mkAXI4_Slave_Xactor);

   // Transactors facing slaves
   Vector #(num_slaves,  AXI4_Master_Xactor_IFC #(wd_addr, wd_data, wd_user))
       xactors_to_slaves    <- replicateM (mkAXI4_Master_Xactor);

   // FIFOs to keep track of which master originated a transaction, in
   // order to route corresponding responses back to that master.
   // Legal masters are 0..(num_masters-1)
   // The value of 'num_masters' is used for decode errors (no such slave)

   Vector #(num_masters, FIFOF #(Bit #(log_ns_plus_1))) v_f_wr_sjs      <- replicateM (mkFIFOF);
   Vector #(num_masters, FIFOF #(Bit #(wd_user)))       v_f_wr_err_user <- replicateM (mkFIFOF);
   Vector #(num_slaves,  FIFOF #(Bit #(log_nm_plus_1))) v_f_wr_mis      <- replicateM (mkFIFOF);

   Vector #(num_masters, FIFOF #(Bit #(log_ns_plus_1))) v_f_rd_sjs      <- replicateM (mkFIFOF);
   Vector #(num_masters, FIFOF #(Bit #(wd_user)))       v_f_rd_err_user <- replicateM (mkFIFOF);
   Vector #(num_slaves,  FIFOF #(Bit #(log_nm_plus_1))) v_f_rd_mis      <- replicateM (mkFIFOF);

   // ----------------------------------------------------------------
   // BEHAVIOR

   function Bool wr_move_from_mi_to_sj (Integer mi, Integer sj);
      let addr = xactors_from_masters [mi].o_wr_addr.first.awaddr;
      match { .legal, .slave_num } = fn_addr_to_slave_num (addr);
      return (legal && (slave_num == fromInteger (sj)));
   endfunction

   function Bool wr_illegal_sj (Integer mi);
      let addr = xactors_from_masters [mi].o_wr_addr.first.awaddr;
      match { .legal, ._ } = fn_addr_to_slave_num (addr);
      return (! legal);
   endfunction

   function Bool rd_move_from_mi_to_sj (Integer mi, Integer sj);
      let addr = xactors_from_masters [mi].o_rd_addr.first.araddr;
      match { .legal, .slave_num } = fn_addr_to_slave_num (addr);
      return (legal && (slave_num == fromInteger (sj)));
   endfunction

   function Bool rd_illegal_sj (Integer mi);
      let addr = xactors_from_masters [mi].o_rd_addr.first.araddr;
      match { .legal, ._ } = fn_addr_to_slave_num (addr);
      return (! legal);
   endfunction

   // ----------------
   // Wr requests from masters to slaves

   // Legal destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_wr_xaction_master_to_slave (wr_move_from_mi_to_sj (mi, sj));
	    AXI4_Wr_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_wr_addr);
	    AXI4_Wr_Data #(wd_data)          d <- pop_o (xactors_from_masters [mi].o_wr_data);

	    xactors_to_slaves [sj].i_wr_addr.enq (a);
	    xactors_to_slaves [sj].i_wr_data.enq (d);

	    v_f_wr_mis        [sj].enq (fromInteger (mi));
	    v_f_wr_sjs        [mi].enq (fromInteger (sj));

		 `ifdef verbose
	       $display ($time,"\tAXI4_Fabric: wr master [%0d] -> slave [%0d]", mi, sj);
	       $display ($time,"\t        ", fshow (a));
	       $display ($time,"\t        ", fshow (d));
	    `endif
	 endrule
   
	for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_wr_xaction_master_to_slave_data ((v_f_wr_mis [sj].first == fromInteger (mi))
					  && (v_f_wr_sjs [mi].first == fromInteger (sj)));
	    AXI4_Wr_Data #(wd_data)          d <- pop_o (xactors_from_masters [mi].o_wr_data);
	    xactors_to_slaves [sj].i_wr_data.enq(d);
       `ifdef verbose $display ($time,"\tAXI4_Fabric: Write Data -> slave[%0d] \n",sj,$time,"\t", fshow (d)); `endif
	 endrule


   // Non-existent destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
	 rule rl_wr_xaction_no_such_slave (wr_illegal_sj (mi));
	    AXI4_Wr_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_wr_addr);
	    AXI4_Wr_Data #(wd_data)          d <- pop_o (xactors_from_masters [mi].o_wr_data);

	    v_f_wr_sjs        [mi].enq (fromInteger (valueOf (num_slaves)));
	    v_f_wr_err_user   [mi].enq (a.awuser);
		 `ifdef verbose
	       $display ($time,"\tAXI4_Fabric: wr master [%0d] -> illegal addr", mi);
	       $display ($time,"\t        ", fshow (a));
	    `endif
	 endrule

   // ----------------
   // Rd requests from masters to slaves

   // Legal destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_rd_xaction_master_to_slave (rd_move_from_mi_to_sj (mi, sj));
	    AXI4_Rd_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_rd_addr);

	    xactors_to_slaves [sj].i_rd_addr.enq (a);

	    v_f_rd_mis [sj].enq (fromInteger (mi));
	    v_f_rd_sjs [mi].enq (fromInteger (sj));

		 `ifdef verbose
	       $display ($time,"\tAXI4_Fabric: rd master [%0d] -> slave [%0d]", mi, sj);
	       $display ($time,"\t        ", fshow (a));
	    `endif
	 endrule

   // Non-existent destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
	 rule rl_rd_xaction_no_such_slave (rd_illegal_sj (mi));
	    AXI4_Rd_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_rd_addr);

	    v_f_rd_sjs      [mi].enq (fromInteger (valueOf (num_slaves)));
	    v_f_rd_err_user [mi].enq (a.aruser);

	    `ifdef verbose
	       $display ($time,"\tAXI4_Fabric: rd master [%0d] -> illegal addr", mi);
	       $display ($time,"\t        ", fshow (a));
	    `endif
	 endrule

   // ----------------
   // Wr responses from slaves to masters

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_wr_resp_slave_to_master (   (v_f_wr_mis [sj].first == fromInteger (mi))
					  && (v_f_wr_sjs [mi].first == fromInteger (sj)));
	    v_f_wr_mis [sj].deq;
	    v_f_wr_sjs [mi].deq;
	    AXI4_Wr_Resp #(wd_user) b <- pop_o (xactors_to_slaves [sj].o_wr_resp);

	    xactors_from_masters [mi].i_wr_resp.enq (b);

		 `ifdef verbose
	       $display ($time,"\tAXI4_Fabric: wr master [%0d] <- slave [%0d]", mi, sj);
	       $display ($time,"\t        ", fshow (b));
	    `endif
	 endrule

   // ----------------
   // Wr error responses to masters
   // v_f_wr_sjs [mi].first has value num_slaves (illegal value)
   // v_f_wr_err_user [mi].first contains the request's 'user' data

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)

      rule rl_wr_resp_err_to_master (v_f_wr_sjs [mi].first == fromInteger (valueOf (num_slaves)));
	 v_f_wr_sjs [mi].deq;
	 v_f_wr_err_user [mi].deq;

	 //let b = AXI4_Wr_Resp {bresp: AXI4_DECERR, buser: v_f_wr_err_user [mi].first};
	 let b = AXI4_Wr_Resp {bresp: AXI4_DECERR, buser: v_f_wr_err_user [mi].first, bid:fromInteger(mi)};

	 xactors_from_masters [mi].i_wr_resp.enq (b);

	 `ifdef verbose
	    $display ($time,"\tAXI4_Fabric: wr master [%0d] <- error", mi);
	    $display ($time,"\t        ", fshow (b));
	 `endif
      endrule

   // ----------------
   // Rd responses from slaves to masters

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_rd_resp_slave_to_master (   (v_f_rd_mis [sj].first == fromInteger (mi))
					  && (v_f_rd_sjs [mi].first == fromInteger (sj)));
	    AXI4_Rd_Data #(wd_data, wd_user) r <- pop_o (xactors_to_slaves [sj].o_rd_data);

	    xactors_from_masters [mi].i_rd_data.enq (r);
		 if(r.rlast)begin
		    v_f_rd_mis [sj].deq;
		    v_f_rd_sjs [mi].deq;
		 end

		 `ifdef verbose
	       $display ($time,"\tAXI4_Fabric: rd master [%0d] <- slave [%0d]", mi, sj);
	       $display ($time,"\t        ", fshow (r));
	    `endif
	 endrule

   // ----------------
   // Rd error responses to masters
   // v_f_rd_sjs [mi].first has value num_slaves (illegal value)
   // v_f_rd_err_user [mi].first contains the request's 'user' data

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)

      rule rl_rd_resp_err_to_master (v_f_rd_sjs [mi].first == fromInteger (valueOf (num_slaves)));
	 v_f_rd_sjs [mi].deq;
	 v_f_rd_err_user [mi].deq;

	 Bit #(wd_data) data = 0;
	 let r = AXI4_Rd_Data {rresp: AXI4_DECERR, ruser: v_f_rd_err_user [mi].first, rdata: data, rlast:True,rid:fromInteger(mi)};

	 xactors_from_masters [mi].i_rd_data.enq (r);

	 `ifdef verbose
		 $display ($time,"\tAXI4_Fabric: rd master [%0d] <- error", mi);
	    $display ($time,"\t        ", fshow (r));
	 `endif
      endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function AXI4_Slave_IFC  #(wd_addr, wd_data, wd_user) f1 (Integer j) = xactors_from_masters [j].axi_side;
   function AXI4_Master_IFC #(wd_addr, wd_data, wd_user) f2 (Integer j) = xactors_to_slaves    [j].axi_side;

   method Action reset;
      for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1) begin
	 xactors_from_masters [mi].reset;

	 v_f_wr_sjs [mi].clear;
	 v_f_wr_err_user [mi].clear;

	 v_f_rd_sjs [mi].clear;
	 v_f_rd_err_user [mi].clear;
      end

      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1) begin
	 xactors_to_slaves [sj].reset;
	 v_f_wr_mis [sj].clear;
	 v_f_rd_mis [sj].clear;
      end
   endmethod

   interface v_from_masters = genWith (f1);
   interface v_to_slaves    = genWith (f2);
endmodule

// ================================================================

endpackage: AXI4_Fabric
