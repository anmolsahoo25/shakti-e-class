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

package Semi_FIFOF;

// ================================================================
// Separate interfaces for input-side and output-side of FIFOF.
// Conversion functions to these, from FIFOF interfaces.

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ================================================================
// Semi-FIFOF interfaces

interface FIFOF_I #(type t);
   method Action  enq (t x);
   method Bool    notFull ();
endinterface

interface FIFOF_O #(type t);
   method t       first ();
   method Action  deq ();
   method Bool    notEmpty ();
endinterface

// ================================================================
// Converters from FIFOF

function FIFOF_I #(t) to_FIFOF_I (FIFOF #(t) f);
   return interface FIFOF_I;
	     method enq (x) = f.enq (x);
	     method notFull = f.notFull;
	  endinterface;
endfunction

function FIFOF_O #(t) to_FIFOF_O (FIFOF #(t) f);
   return interface FIFOF_O;
	     method first    = f.first;
	     method deq      = f.deq;
	     method notEmpty = f.notEmpty;
	  endinterface;
endfunction

// ================================================================
// Connections

// ----------------
// FIFOF_O to a FIFOF_I

instance Connectable #(FIFOF_O #(t), FIFOF_I #(t));
   module mkConnection #(FIFOF_O #(t) fo, FIFOF_I #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ----------------
// FIFOF_O to a FIFOF

instance Connectable #(FIFOF_O #(t), FIFOF #(t));
   module mkConnection #(FIFOF_O #(t) fo, FIFOF #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ----------------
// FIFOF to a FIFOF_I

instance Connectable #(FIFOF #(t), FIFOF_I #(t));
   module mkConnection #(FIFOF #(t) fo, FIFOF_I #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ================================================================
// Convenience function combining first/enq

function ActionValue #(t) pop_o (FIFOF_O #(t) f);
   actionvalue
      f.deq;
      return f.first;
   endactionvalue
endfunction

// ================================================================

endpackage
