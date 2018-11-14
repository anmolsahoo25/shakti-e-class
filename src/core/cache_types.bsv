/* 
Copyright (c) 2018, IIT Madras All rights reserved.

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
package cache_types;
  typedef 32 ICACHEADDR;
                  // addr, Fence, epoch, prefetch
  typedef Tuple4#(Bit#(addr), Bool, Bit#(1), Bool) ICore_request#(numeric type addr);
                 // word , err , epoch
  typedef Tuple3#(Bit#(data), Bool, Bit#(1)) ICore_response#(numeric type data);
                // addr ,  burst len, burst_size 
  typedef Tuple3#(Bit#(addr),  Bit#(8), Bit#(3)) IMem_request#(numeric type addr);
  typedef Tuple2#(Bit#(data), Bool) IMem_response#(numeric type data);

  typedef enum {Hit, Miss, None} RespState deriving(Eq,Bits,FShow);

  function String countName (Integer cntr);
    case (cntr)
      'd0: return "Total accesses";
      'd1: return "Total Hits in Cache";
      'd2: return "Total Hits in LB";
      'd3: return "Total IO requests";
      'd4: return "Misses which cause evictions";
      default: return "Null";
    endcase
  endfunction
  typedef struct {
  	Bool v;					//valid
  	Bool r;					//allow reads
  	Bool w;					//allow writes
  	Bool x;					//allow execute(instruction read)
  	Bool u;					//allow supervisor
  	Bool g;					//global page
  	Bool a;					//accessed already
  	Bool d;					//dirty
  } TLB_permissions deriving(Bits, Eq, FShow);
	
  function TLB_permissions bits_to_permission(Bit#(8) perms);
		return TLB_permissions { v : unpack(perms[0]),
														 r : unpack(perms[1]),
														 w : unpack(perms[2]),
														 x : unpack(perms[3]),
														 u : unpack(perms[4]),
														 g : unpack(perms[5]),
														 a : unpack(perms[6]),
														 d : unpack(perms[7])};
	endfunction
endpackage
