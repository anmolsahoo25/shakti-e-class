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
// Copyright (c) 2013-2017 Bluespec, Inc.  All Rights Reserved

package TxRx;

// ================================================================
// This package allows a separately synthesized module to have direct
// access to an external communication channel via FIFOF-like interfaces
// (methods: notFull, enq, notEmpty, first, deq),
// without the overhead or latency of an internal FIFOs.

// THE PROBLEM: suppose we want module M1 to send messages to M2
// Inside M1, we can instantiate a FIFO f1, and enqueue into it,
// and return g1 = toGet (f1) as part of M1's interface.
// Inside M2, we can instantiate a FIFO f2, and dequeue from it,
// and return p2 = toPut (f2) as part of M2's interface.
// Outside, we connect them with mkConnection(g1,p2).
// Problem 1: there are 2 ticks of latency in the communication
// Problem 2: there are at least 2 state elements to hold messages
// Ideally, we'd like to instantiate a FIFO externally,
// and pass it as a parameter to M1 and M2
// so that M1 can enq to it, and M2 can dequeue from it.
// But then M1 and M2 cannot be separately synthesized.

// This package provides a solution.
// Terminology: the channel has a tail (enq side) and a head (deq side).
// Inside M1 (for outgoing data):
//     Instantiate:    TX #(t) tx <- mkTX;
//     Use tx.u to send data (methods enq, notFull).
//     Export tx.e as part of M1's interface.
// Inside M2 (for incoming data):
//     Instantiate:    RX #(t) rx <- mkRX;
//     Use rx.u to receive data (methods first, deq, notEmpty).
//     Export rx.e as part of M2's interface.
//
// Outside, use 'mkChan (buffer_fifof, m1.txe, m2.rxe)' to make an external
//        communication channel, passing in a module with a FIFOF
//        interface to instantiate the intermediate buffer.
//        The buffer could be mkFIFOF, mkPipelineFIFOF, mkSizedFIFOF, ...
//
// You can also connect each to a FIFOF:
//    mkConnection (m1.txe, fifof)
//    mkConnection (fifof,  m2.rxe)

// ================================================================
// BSV library imports

import FIFOF       :: *;
import GetPut      :: *;
import Connectable :: *;

// ================================================================
// TX (sender side)

// This interface is used by the sender

interface TXu #(type t);
   method Bool   notFull;
   method Action enq (t x);
endinterface

instance ToPut #(TXu #(t), t);
   function Put #(t) toPut (TXu #(t) f_in);
      return interface Put;
		method Action put (t x);
		   f_in.enq (x);
		endmethod
	     endinterface;
   endfunction
endinstance

// This interface is exported by the sender

(* always_enabled, always_ready *)
interface TXe #(type t);
   method Action notFull (Bool b);
   method Action enq_rdy (Bool b);
   method Bool   enq_ena;
   method t      enq_data;
endinterface

// ----------------------------------------------------------------
// Connecting a TXe to an ordinary FIFOF

instance Connectable #(FIFOF #(t), TXe #(t));
   module mkConnection #(FIFOF #(t) fifo, TXe #(t) txe)
                       (Empty);
      (* fire_when_enabled, no_implicit_conditions *)
      rule connect_notFull;
	 txe.notFull (fifo.notFull);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule connect_rdy;
	 txe.enq_rdy (fifo.notFull);
      endrule

      rule connect_ena_data (txe.enq_ena);
	 fifo.enq (txe.enq_data);
      endrule
   endmodule
endinstance

instance Connectable #(TXe #(t), FIFOF #(t));
   module mkConnection #(TXe #(t) txe, FIFOF #(t) fifo)
                       (Empty);
      mkConnection (fifo, txe);
   endmodule
endinstance

// ----------------------------------------------------------------
// Transactor from TXu to TXe interface

interface TX #(type t);
   interface TXu #(t) u;
   interface TXe #(t) e;
endinterface

module mkTX (TX #(t))
   provisos (Bits #(t, tsz));

   Wire #(Bool) w_notFull <- mkBypassWire;
   Wire #(Bool) w_rdy     <- mkBypassWire;
   Wire #(Bool) w_ena     <- mkDWire (False);
   Wire #(t)    w_data    <- mkDWire (?);

   interface TXu u;
      method Bool notFull;
	 return w_notFull;
      endmethod

      method Action enq (t x) if (w_rdy);
	 w_ena <= True;
	 w_data <= x;
      endmethod
   endinterface

   interface TXe e;
      method Action notFull (Bool b);
	 w_notFull <= b;
      endmethod

      method Action enq_rdy (Bool b);
	 w_rdy <= b;
      endmethod

      method Bool enq_ena;
	 return w_ena;
      endmethod

      method t enq_data;
	 return w_data;
      endmethod
   endinterface
endmodule: mkTX

// ================================================================
// RX (receiver side)

// This interface is used by the receiver

interface RXu #(type t);
   method Bool   notEmpty;
   method t      first;
   method Action deq;
endinterface

instance ToGet #(RXu #(t), t);
   function Get #(t) toGet (RXu #(t) f_out);
      return interface Get;
		method ActionValue #(t) get;
		   f_out.deq;
		   return f_out.first;
		endmethod
	     endinterface;
   endfunction
endinstance

// This interface is exported by the receiver

(* always_enabled, always_ready *)
interface RXe #(type t);
   method Action notEmpty (Bool b);
   method Action first_deq_rdy (Bool b);
   method Action first (t x);
   method Bool   deq_ena;
endinterface

// ----------------------------------------------------------------
// Connecting an ordinary FIFOF to an RXe

instance Connectable #(FIFOF #(t), RXe #(t));
   module mkConnection #(FIFOF #(t) fifo, RXe #(t) rxe)
                       (Empty);
      (* fire_when_enabled, no_implicit_conditions *)
      rule connect_notEmpty;
	 rxe.notEmpty (fifo.notEmpty);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule connect_rdy;
	 rxe.first_deq_rdy (fifo.notEmpty);
      endrule

      rule connect_first;
	 let data = (fifo.notEmpty ? fifo.first : ?);
	 rxe.first (data);
      endrule

      rule connect_ena (rxe.deq_ena);
	 fifo.deq;
      endrule
   endmodule
endinstance

instance Connectable #(RXe #(t), FIFOF #(t));
   module mkConnection #(RXe #(t) rxe, FIFOF #(t) fifo)
                       (Empty);
      mkConnection (fifo, rxe);
   endmodule
endinstance

// ----------------------------------------------------------------
// Transactor from RXe to RXu interface

interface RX #(type t);
   interface RXu #(t) u;
   interface RXe #(t) e;
endinterface

module mkRX (RX #(t))
   provisos (Bits #(t, tsz));

   Wire #(Bool) w_notEmpty <- mkBypassWire;
   Wire #(Bool) w_rdy      <- mkBypassWire;
   Wire #(Bool) w_ena      <- mkDWire (False);
   Wire #(t)    w_data     <- mkDWire (?);

   interface RXu u;
      method Bool notEmpty;
	 return w_notEmpty;
      endmethod

      method t first if (w_rdy);
	 return w_data;
      endmethod

      method Action deq () if (w_rdy);
	 w_ena <= True;
      endmethod
   endinterface

   interface RXe e;
      method Action notEmpty (Bool b);
	 w_notEmpty <= b;
      endmethod

      method Action first_deq_rdy (Bool b);
	 w_rdy <= b;
      endmethod

      method Action first (t x);
	 w_data <= x;
      endmethod

      method Bool deq_ena;
	 return w_ena;
      endmethod
   endinterface
endmodule: mkRX

// ================================================================
// Function to connect TXe to RXe, passing in the
// desired FIFOF constructor for the intermediate buffer

module mkChan #(module #(FIFOF #(t)) mkFIFOF,
		TXe #(t) txe,
		RXe #(t) rxe)
   (Empty);

   let fifof <- mkFIFOF;
   let empty_txe_to_fifof <- mkConnection (txe,   fifof);
   let empty_fifof_to_rxe <- mkConnection (fifof, rxe);
endmodule: mkChan

// ================================================================

endpackage
