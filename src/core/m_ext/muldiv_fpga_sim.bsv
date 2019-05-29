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
package muldiv_fpga_sim;
  
  interface Ifc_multiplier#(numeric type width);
    (*always_ready, always_enabled*)
    method Action iA (Bit#(width) a);
    (*always_ready, always_enabled*)
    method Action iB (Bit#(width) b);
    (*always_enabled*)
    method Bit#(TMul#(width, 2)) oP();
  endinterface:Ifc_multiplier

  module mkmultiplier(Ifc_multiplier#(width))
    provisos (Add#(a__, width, TMul#(width, 2))) ;
    Reg#(Bit#(TMul#(width, 2))) reg_a <- mkReg(0);
    Reg#(Bit#(TMul#(width, 2))) reg_b <- mkReg(0);

    method Action iA (Bit#(width) a);
      reg_a<= zeroExtend(a);
    endmethod
    method Action iB (Bit#(width) b);
      reg_b<=zeroExtend(b);
    endmethod
    method Bit#(TMul#(width, 2)) oP();
    return reg_a*reg_b; 
    endmethod
  endmodule
endpackage
