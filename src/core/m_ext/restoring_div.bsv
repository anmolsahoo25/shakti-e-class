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
package restoring_div;

import common_types::*;
`include "common_params.bsv"
import Assert::*;

  (*noinline*)
  function Tuple2#(Bit#(TAdd#(1,XLEN)), Bit#(XLEN)) singlestep (Bit#(TAdd#(1,XLEN)) remainder, 
                                                      Bit#(XLEN) quotient, Bit#(XLEN) divisor);
    for(Integer i=0; i<(valueOf(XLEN)/`DIVSTAGES); i=i+ 1)begin
      let x={remainder, quotient}<<1;
      remainder=truncateLSB(x);
      quotient=truncate(x);
      Bit#(TAdd#(1, XLEN)) sub=remainder+signExtend(~divisor+1);
      if(truncate(remainder)>=divisor)begin // if subtraction is positive
        quotient[0]=1;
        remainder=sub;
      end
    end
    return tuple2(remainder,quotient);
  endfunction 

  interface Ifc_restoring_div;
    method Action get_inputs(Bit#(XLEN) op1, Bit#(XLEN) op2,  Bool qr);
    method Bit#(XLEN) quo_rem;
  endinterface
  (*synthesize*)
  module mkrestoring_div(Ifc_restoring_div);
    staticAssert(valueOf(TMul#(TDiv#(64, `DIVSTAGES), `DIVSTAGES))==64, "DIVSTAGES is not power of\
    2");
    staticAssert(`DIVSTAGES<= valueOf(XLEN), "DIVSTAGES cannot be larger than XLEN");

    Reg#(Bit#(TAdd#(1, TMul#(2, XLEN)))) partial<-mkReg(0);
    Reg#(Bit#(XLEN)) rg_op2 <-mkReg(0);
    Reg#(Bool) quotient_remainder <- mkReg(False);
    rule single_step_div;
      let {upper, lower}=singlestep(truncateLSB(partial),truncate(partial), rg_op2); 
      partial<= {upper, lower};
    endrule

    method Action get_inputs(Bit#(XLEN) op1, Bit#(XLEN) op2,  Bool qr);
      partial<= zeroExtend(op1);
      rg_op2<= op2;
      quotient_remainder<= qr;
    endmethod
    method quo_rem=quotient_remainder?partial[valueOf(TMul#(2, XLEN))-1:valueOf(XLEN)]:
                                                                                  truncate(partial);
  endmodule
endpackage
