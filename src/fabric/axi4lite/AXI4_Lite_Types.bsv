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
// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package AXI4_Lite_Types;

// ================================================================
// Facilities for ARM AXI4-Lite, consisting of 5 independent channels:
//   Write Address, Write Data, Write Response, Read Address and Read Data

// Ref: ARM document:
//    AMBA AXI and ACE Protocol Specification
//    AXI3, AXI4, and AXI4-Lite
//    ACE and ACE-Lite
//    ARM IHI 0022E (ID022613)
//    Issue E, 22 Feb 2013

// See export list below

// ================================================================
// Exports

export

// RTL-level interfaces (signals/buses)
AXI4_Lite_Master_IFC (..),
AXI4_Lite_Slave_IFC (..),

// Dummy slave that accepts no requests and generates no response
// Used for tying-off unused slave interfaces on fabrics.
dummy_AXI4_Lite_Slave_ifc,

// Higher-level enums and structs for the 5 AXI4 channel payloads
AXI4_Lite_Resp (..),

AXI4_Lite_Wr_Addr (..),
AXI4_Lite_Wr_Data (..),
AXI4_Lite_Wr_Resp (..),
AXI4_Lite_Rd_Addr (..),
AXI4_Lite_Rd_Data (..),

// Higher-level FIFO-like interfaces for the 5 AXI4 channels,
AXI4_Lite_Master_Xactor_IFC (..),
AXI4_Lite_Slave_Xactor_IFC (..),

// Transactors from RTL-level interfacecs to FIFO-like interfaces.
mkAXI4_Lite_Master_Xactor,
mkAXI4_Lite_Slave_Xactor;

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;

// ****************************************************************
// ****************************************************************
// Section: RTL-level interfaces
// ****************************************************************
// ****************************************************************

// ================================================================
// These are the signal-level interfaces for an AXI4-Lite master.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Lite_Master_IFC #(numeric type wd_addr,
				 numeric type wd_data,
				 numeric type wd_user);
   // Wr Addr channel
   (* always_ready, result="awvalid" *) method Bool           m_awvalid;                                // out
   (* always_ready, result="awaddr" *)  method Bit #(wd_addr) m_awaddr;                                 // out
   (* always_ready, result="awuser" *)  method Bit #(wd_user) m_awuser;                                 // out
   (* always_ready, result="awsize" *)  method Bit #(3)  m_awsize;			                                // out
   (* always_ready, always_enabled *)   method Action m_awready ((* port="awready" *) Bool awready);    // in

   // Wr Data channel
   (* always_ready, result="wvalid" *)  method Bool                      m_wvalid;                      // out
   (* always_ready, result="wdata" *)   method Bit #(wd_data)            m_wdata;                       // out
   (* always_ready, result="wstrb" *)   method Bit #(TDiv #(wd_data, 8)) m_wstrb;                       // out
   (* always_ready, always_enabled *)   method Action m_wready ((* port="wready" *)  Bool wready);      // in

   // Wr Response channel
   (* always_ready, always_enabled *)
   method Action m_bvalid ((* port="bvalid" *)  Bool           bvalid,    // in
			   (* port="bresp"  *)  Bit #(2)       bresp,     // in
			   (* port="buser"  *)  Bit #(wd_user) buser);    // in
   (* always_ready, result="bready" *)
   method Bool m_bready;                                            // out

   // Rd Addr channel
   (* always_ready, result="arvalid" *) method Bool            m_arvalid;                               // out
   (* always_ready, result="araddr" *)  method Bit #(wd_addr)  m_araddr;                                // out
   (* always_ready, result="aruser" *)  method Bit #(wd_user)  m_aruser;                                // out
   (* always_ready, result="arsize" *)  method Bit #(3)  m_arsize;			                                // out
   (* always_ready, always_enabled  *)  method Action m_arready ((* port="arready" *) Bool arready);    // in

   // Rd Data channel
   (* always_ready, always_enabled  *)
   method Action m_rvalid ((* port="rvalid" *) Bool           rvalid,    // in
			   (* port="rresp" *)  Bit #(2)       rresp,     // in
			   (* port="rdata" *)  Bit #(wd_data) rdata,     // in
			   (* port="ruser" *)  Bit #(wd_user) ruser);    // in
   (* always_ready, result="rready" *)
   method Bool m_rready;                                                 // out
endinterface: AXI4_Lite_Master_IFC

// ================================================================
// These are the signal-level interfaces for an AXI4-Lite slave.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Lite_Slave_IFC #(numeric type wd_addr,
				numeric type wd_data,
				numeric type wd_user);
   // Wr Addr channel
   (* always_ready, always_enabled *)
   method Action m_awvalid ((* port="awvalid" *) Bool           awvalid,    // in
			    (* port="awaddr" *)  Bit #(wd_addr) awaddr,     // in
			    (* port="awsize" *)   Bit #(3) awsize,						// in
			    (* port="awuser" *)  Bit #(wd_user) awuser);    // in
   (* always_ready, result="awready" *)
   method Bool m_awready;                                                   // out

   // Wr Data channel
   (* always_ready, always_enabled *)
   method Action m_wvalid ((* port="wvalid" *) Bool                     wvalid,    // in
			   (* port="wdata" *)  Bit #(wd_data)           wdata,     // in
			   (* port="wstrb" *)  Bit #(TDiv #(wd_data,8)) wstrb);    // in
   (* always_ready, result="wready" *)
   method Bool m_wready;                                                           // out

   // Wr Response channel
   (* always_ready, result="bvalid" *)  method Bool           m_bvalid;                                 // out
   (* always_ready, result="bresp" *)   method Bit #(2)       m_bresp;                                  // out
   (* always_ready, result="buser" *)   method Bit #(wd_user) m_buser;                                  // out
   (* always_ready, always_enabled *)   method Action m_bready  ((* port="bready" *)   Bool bready);    // in

   // Rd Addr channel
   (* always_ready, always_enabled *)
   method Action m_arvalid ((* port="arvalid" *) Bool           arvalid,    // in
			    (* port="araddr" *)  Bit #(wd_addr) araddr,     // in
				 (* port="arsize" *)	 Bit #(3)  arsize,        // in
			    (* port="aruser" *)  Bit #(wd_user) aruser);    // in
   (* always_ready, result="arready" *)
   method Bool m_arready;                                                   // out

   // Rd Data channel
   (* always_ready, result="rvalid" *)  method Bool           m_rvalid;                                 // out
   (* always_ready, result="rresp" *)   method Bit #(2)       m_rresp;                                  // out
   (* always_ready, result="rdata" *)   method Bit #(wd_data) m_rdata;                                  // out
   (* always_ready, result="ruser" *)   method Bit #(wd_user) m_ruser;                                  // out
   (* always_ready, always_enabled *)   method Action m_rready  ((* port="rready" *)   Bool rready);    // in
endinterface: AXI4_Lite_Slave_IFC

// ================================================================
// Connecting signal-level interfaces

instance Connectable #(AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user),
		       AXI4_Lite_Slave_IFC  #(wd_addr, wd_data, wd_user));

   module mkConnection #(AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user) axim,
			 AXI4_Lite_Slave_IFC  #(wd_addr, wd_data, wd_user) axis)
		       (Empty);

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_addr_channel;
	 axis.m_awvalid (axim.m_awvalid, axim.m_awaddr, axim.m_awsize, axim.m_awuser);
	 axim.m_awready (axis.m_awready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_data_channel;
	 axis.m_wvalid (axim.m_wvalid, axim.m_wdata, axim.m_wstrb);
	 axim.m_wready (axis.m_wready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_response_channel;
	 axim.m_bvalid (axis.m_bvalid, axis.m_bresp, axis.m_buser);
	 axis.m_bready (axim.m_bready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_rd_addr_channel;
	 axis.m_arvalid (axim.m_arvalid, axim.m_araddr, axim.m_arsize, axim.m_aruser);
	 axim.m_arready (axis.m_arready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_rd_data_channel;
	 axim.m_rvalid (axis.m_rvalid, axis.m_rresp, axis.m_rdata, axis.m_ruser);
	 axis.m_rready (axim.m_rready);
      endrule
   endmodule
endinstance

// ================================================================
// AXI4-Lite dummy slave: never accepts requests, never produces responses

AXI4_Lite_Slave_IFC #(wd_addr, wd_data, wd_user)
   dummy_AXI4_Lite_Slave_ifc = interface AXI4_Lite_Slave_IFC 
				  // Wr Addr channel
				  method Action m_awvalid (Bool           awvalid,
							   Bit #(wd_addr) awaddr,
								Bit #(3)			awsize,
							   Bit #(wd_user) awuser);
				     noAction;
				  endmethod

				  method Bool m_awready;
				     return False;
				  endmethod

				  // Wr Data channel
				  method Action m_wvalid (Bool                     wvalid,
							  Bit #(wd_data)           wdata,
							  Bit #(TDiv #(wd_data,8)) wstrb);
				     noAction;
				  endmethod

				  method Bool m_wready;
				     return False;
				  endmethod

				  // Wr Response channel
				  method Bool m_bvalid;
				     return False;
				  endmethod

				  method Bit #(2) m_bresp;
				     return 0;
				  endmethod

				  method Bit #(wd_user) m_buser;
				     return ?;
				  endmethod

				  method Action m_bready  (Bool bready);
				     noAction;
				  endmethod

				  // Rd Addr channel
				  method Action m_arvalid (Bool           arvalid,
							   Bit #(wd_addr) araddr,
								Bit#(3)				 arsize,
							   Bit #(wd_user) aruser);
				     noAction;
				  endmethod

				  method Bool m_arready;
				     return False;
				  endmethod

				  // Rd Data channel
				  method Bool m_rvalid;
				     return False;
				  endmethod

				  method Bit #(2) m_rresp;
				     return 0;
				  endmethod

				  method Bit #(wd_data) m_rdata;
				     return 0;
				  endmethod

				  method Bit #(wd_user) m_ruser;
				     return ?;
				  endmethod

				  method Action m_rready  (Bool rready);
				     noAction;
				  endmethod
			       endinterface;

// ****************************************************************
// ****************************************************************
// Section: Higher-level FIFO-like interfaces and transactors
// ****************************************************************
// ****************************************************************

// ================================================================
// Higher-level types for payloads (rather than just bits)

typedef enum { AXI4_LITE_OKAY, AXI4_LITE_EXOKAY, AXI4_LITE_SLVERR, AXI4_LITE_DECERR } AXI4_Lite_Resp
deriving (Bits, Eq, FShow);

// Write Address channel

typedef struct {
   Bit #(wd_addr)  awaddr;
   Bit #(wd_user)  awuser;
	Bit#(3) 				awsize;
   } AXI4_Lite_Wr_Addr #(numeric type wd_addr, numeric type wd_user)
deriving (Bits, FShow);

// Write Data channel

typedef struct {
   Bit #(wd_data)             wdata;
   Bit #(TDiv #(wd_data, 8))  wstrb;
   } AXI4_Lite_Wr_Data #(numeric type wd_data)
deriving (Bits, FShow);

// Write Response channel

typedef struct {
   AXI4_Lite_Resp  bresp;
   Bit #(wd_user)  buser;
   } AXI4_Lite_Wr_Resp #(numeric type wd_user)
deriving (Bits, FShow);

// Read Address channel

typedef struct {
   Bit #(wd_addr)  araddr;
   Bit #(wd_user)  aruser;
	Bit#(3)			 arsize;
   } AXI4_Lite_Rd_Addr #(numeric type wd_addr, numeric type wd_user)
deriving (Bits, FShow);

// Read Data channel

typedef struct {
   AXI4_Lite_Resp  rresp;
   Bit #(wd_data)  rdata;
   Bit #(wd_user)  ruser;
   } AXI4_Lite_Rd_Data #(numeric type wd_data, numeric type wd_user)
deriving (Bits, FShow);

// ================================================================
// Master transactor interface

interface AXI4_Lite_Master_Xactor_IFC #(numeric type wd_addr,
					numeric type wd_data,
					numeric type wd_user);
   method Action reset;

   // AXI side
   interface AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
   interface FIFOF_I #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user)) i_wr_addr;
   interface FIFOF_I #(AXI4_Lite_Wr_Data #(wd_data))          i_wr_data;
   interface FIFOF_O #(AXI4_Lite_Wr_Resp #(wd_user))          o_wr_resp;

   interface FIFOF_I #(AXI4_Lite_Rd_Addr #(wd_addr, wd_user)) i_rd_addr;
   interface FIFOF_O #(AXI4_Lite_Rd_Data #(wd_data, wd_user)) o_rd_data;
endinterface: AXI4_Lite_Master_Xactor_IFC

// ----------------------------------------------------------------
// Master transactor

module mkAXI4_Lite_Master_Xactor (AXI4_Lite_Master_Xactor_IFC #(wd_addr, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user)) f_wr_addr <- mkGFIFOF (guarded, unguarded);
   FIFOF #(AXI4_Lite_Wr_Data #(wd_data))          f_wr_data <- mkGFIFOF (guarded, unguarded);
   FIFOF #(AXI4_Lite_Wr_Resp #(wd_user))          f_wr_resp <- mkGFIFOF (unguarded, guarded);

   FIFOF #(AXI4_Lite_Rd_Addr #(wd_addr, wd_user))  f_rd_addr <- mkGFIFOF (guarded, unguarded);
   FIFOF #(AXI4_Lite_Rd_Data #(wd_data, wd_user)) f_rd_data  <- mkGFIFOF (unguarded, guarded);

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;
      f_rd_addr.clear;
      f_rd_data.clear;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Lite_Master_IFC;
			   // Wr Addr channel
			   method Bool           m_awvalid = f_wr_addr.notEmpty;
			   method Bit #(wd_addr) m_awaddr  = f_wr_addr.first.awaddr;
			   method Bit #(wd_user) m_awuser  = f_wr_addr.first.awuser;
				method Bit #(3)		 m_awsize  = f_wr_addr.first.awsize;
			   method Action m_awready (Bool awready);
			      if (f_wr_addr.notEmpty && awready) f_wr_addr.deq;
			   endmethod

			   // Wr Data channel
			   method Bool                       m_wvalid = f_wr_data.notEmpty;
			   method Bit #(wd_data)             m_wdata  = f_wr_data.first.wdata;
			   method Bit #(TDiv #(wd_data, 8))  m_wstrb  = f_wr_data.first.wstrb;
			   method Action m_wready (Bool wready);
			      if (f_wr_data.notEmpty && wready) f_wr_data.deq;
			   endmethod

			   // Wr Response channel
			   method Action m_bvalid (Bool bvalid, Bit #(2) bresp, Bit #(wd_user) buser);
			      if (bvalid && f_wr_resp.notFull)
				 f_wr_resp.enq (AXI4_Lite_Wr_Resp {bresp: unpack (bresp), buser: buser});
			   endmethod

			   method Bool m_bready;
			      return f_wr_resp.notFull;
			   endmethod

			   // Rd Addr channel
			   method Bool           m_arvalid = f_rd_addr.notEmpty;
			   method Bit #(wd_addr) m_araddr  = f_rd_addr.first.araddr;
			   method Bit #(wd_user) m_aruser  = f_rd_addr.first.aruser;
				method Bit #(3)		 m_arsize	 = f_rd_addr.first.arsize;
			   method Action m_arready (Bool arready);
			      if (f_rd_addr.notEmpty && arready) f_rd_addr.deq;
			   endmethod

			   // Rd Data channel
			   method Action m_rvalid (Bool           rvalid,
						   Bit #(2)       rresp,
						   Bit #(wd_data) rdata,
						   Bit #(wd_user) ruser);
			      if (rvalid && f_rd_data.notFull)
				 f_rd_data.enq (AXI4_Lite_Rd_Data {rresp: unpack (rresp),
								   rdata: rdata,
								   ruser: ruser});
			   endmethod

			   method Bool m_rready;
			      return f_rd_data.notFull;
			   endmethod

			endinterface;

   // FIFOF side
   interface i_wr_addr = to_FIFOF_I (f_wr_addr);
   interface i_wr_data = to_FIFOF_I (f_wr_data);
   interface o_wr_resp = to_FIFOF_O (f_wr_resp);

   interface i_rd_addr = to_FIFOF_I (f_rd_addr);
   interface o_rd_data = to_FIFOF_O (f_rd_data);
endmodule: mkAXI4_Lite_Master_Xactor

// ================================================================
// Slave transactor interface

interface AXI4_Lite_Slave_Xactor_IFC #(numeric type wd_addr,
				       numeric type wd_data,
				       numeric type wd_user);
   method Action reset;

   // AXI side
   interface AXI4_Lite_Slave_IFC #(wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
   interface FIFOF_O #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user)) o_wr_addr;
   interface FIFOF_O #(AXI4_Lite_Wr_Data #(wd_data))          o_wr_data;
   interface FIFOF_I #(AXI4_Lite_Wr_Resp #(wd_user))          i_wr_resp;

   interface FIFOF_O #(AXI4_Lite_Rd_Addr #(wd_addr, wd_user)) o_rd_addr;
   interface FIFOF_I #(AXI4_Lite_Rd_Data #(wd_data, wd_user)) i_rd_data;
endinterface: AXI4_Lite_Slave_Xactor_IFC

// ----------------------------------------------------------------
// Slave transactor

module mkAXI4_Lite_Slave_Xactor (AXI4_Lite_Slave_Xactor_IFC #(wd_addr, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user)) f_wr_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Lite_Wr_Data #(wd_data))          f_wr_data <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Lite_Wr_Resp #(wd_user))          f_wr_resp <- mkGFIFOF (guarded, unguarded);

   FIFOF #(AXI4_Lite_Rd_Addr #(wd_addr, wd_user)) f_rd_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Lite_Rd_Data #(wd_data, wd_user)) f_rd_data <- mkGFIFOF (guarded, unguarded);

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;
      f_rd_addr.clear;
      f_rd_data.clear;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Lite_Slave_IFC;
			   // Wr Addr channel
			   method Action m_awvalid (Bool           awvalid,
						    Bit #(wd_addr) awaddr,
							 Bit#(3) awsize,
						    Bit #(wd_user) awuser);
			      if (awvalid && f_wr_addr.notFull)
				 f_wr_addr.enq (AXI4_Lite_Wr_Addr {awaddr: awaddr,
									awsize:awsize,
								   awuser: awuser});
			   endmethod

			   method Bool m_awready;
			      return f_wr_addr.notFull;
			   endmethod

			   // Wr Data channel
			   method Action m_wvalid (Bool                      wvalid,
						   Bit #(wd_data)            wdata,
						   Bit #(TDiv #(wd_data, 8)) wstrb);
			      if (wvalid && f_wr_data.notFull)
				 f_wr_data.enq (AXI4_Lite_Wr_Data {wdata: wdata, wstrb: wstrb});
			   endmethod

			   method Bool m_wready;
			      return f_wr_data.notFull;
			   endmethod

			   // Wr Response channel
			   method Bool           m_bvalid = f_wr_resp.notEmpty;
			   method Bit #(2)       m_bresp  = pack (f_wr_resp.first.bresp);
			   method Bit #(wd_user) m_buser  = f_wr_resp.first.buser;
			   method Action m_bready (Bool bready);
			      if (bready && f_wr_resp.notEmpty)
				 f_wr_resp.deq;
			   endmethod

			   // Rd Addr channel
			   method Action m_arvalid (Bool           arvalid,
						    Bit #(wd_addr) araddr,
							 Bit#(3)				 arsize,
						    Bit #(wd_user) aruser);
			      if (arvalid && f_rd_addr.notFull)
				 f_rd_addr.enq (AXI4_Lite_Rd_Addr {araddr: araddr,
									arsize: arsize,
								   aruser: aruser});
			   endmethod

			   method Bool m_arready;
			      return f_rd_addr.notFull;
			   endmethod

			   // Rd Data channel
			   method Bool           m_rvalid = f_rd_data.notEmpty;
			   method Bit #(2)       m_rresp  = pack (f_rd_data.first.rresp);
			   method Bit #(wd_data) m_rdata  = f_rd_data.first.rdata;
			   method Bit #(wd_user) m_ruser  = f_rd_data.first.ruser;
			   method Action m_rready (Bool rready);
			      if (rready && f_rd_data.notEmpty)
				 f_rd_data.deq;
			   endmethod
			endinterface;

   // FIFOF side
   interface o_wr_addr = to_FIFOF_O (f_wr_addr);
   interface o_wr_data = to_FIFOF_O (f_wr_data);
   interface i_wr_resp = to_FIFOF_I (f_wr_resp);

   interface o_rd_addr = to_FIFOF_O (f_rd_addr);
   interface i_rd_data = to_FIFOF_I (f_rd_data);
endmodule: mkAXI4_Lite_Slave_Xactor

// ================================================================

endpackage
