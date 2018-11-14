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
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONRROBIN
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
package replacement;
  import Vector::*;
  import LFSR::*;
  import Assert::*;

  interface Ifc_replace#(numeric type sets, numeric type ways);
    method ActionValue#(Bit#(TLog#(ways))) line_replace (Bit#(TLog#(sets)) index, Bit#(ways) valid);
    method Action update_set (Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
    method Action reset_repl(Bit#(TLog#(sets)) index);
  endinterface

  module mkreplace#(String alg)(Ifc_replace#(sets,ways))
    provisos(Add#(a__, TLog#(ways), 4));
    let v_ways = valueOf(ways);
    let verbosity = `VERBOSITY;
    staticAssert(alg=="RANDOM" || alg=="RROBIN" || alg=="PLRU","Invalid replacement Algorithm");
    if(alg == "RANDOM")begin
      LFSR#(Bit#(4)) random <- mkLFSR_4();
      Reg#(Bool) rg_init <- mkReg(True);
      rule initialize_lfsr(rg_init);
        random.seed(1);
        rg_init<=False;
      endrule

      method ActionValue#(Bit#(TLog#(ways))) line_replace (Bit#(TLog#(sets)) index, Bit#(ways) valid);
        if (&(valid)==1)begin // if all lines are valid choose one to randomly replace
          return truncate(random.value());
        end
        else begin // if any line empty then send that
          Bit#(TLog#(ways)) temp=0;
          for(Bit#(TAdd#(1,TLog#(ways))) i=0;i<fromInteger(v_ways);i=i+1) begin
            if(valid[i]==0)begin
              temp=truncate(i);
            end
          end
          return temp;
        end
      endmethod
      method Action update_set (Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way)if(!rg_init);
        random.next();
      endmethod
      method Action reset_repl(Bit#(TLog#(sets)) index);
        random.seed(1);
      endmethod
    end
    else if(alg=="RROBIN")begin
      Vector#(sets,Reg#(Bit#(TLog#(ways)))) v_count <- replicateM(mkReg(fromInteger(v_ways-1)));
      method ActionValue#(Bit#(TLog#(ways))) line_replace (Bit#(TLog#(sets)) index, Bit#(ways) valid);
        if (verbosity>1)
          $display("valid: %b index: %d",valid,index);
        if (&(valid)==1)begin // if all lines are valid choose one to randomly replace
          if (verbosity>1)
            $display("replacing line :%d ",readVReg(v_count)[index]);
          return readVReg(v_count)[index];
        end
        else begin // if any line empty then send that
          Bit#(TLog#(ways)) temp=0;
          for(Bit#(TAdd#(1,TLog#(ways))) i=0;i<fromInteger(v_ways);i=i+1) begin
            if(valid[i]==0)begin
              temp=truncate(i);
            end
          end
          return temp;
        end
      endmethod
      method Action update_set (Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
        if (verbosity>1)
          $display("REPL: Updating index: %d",index);
        v_count[index]<=v_count[index]-1;
      endmethod
      method Action reset_repl(Bit#(TLog#(sets)) index);
        v_count[index]<=fromInteger(v_ways-1);
      endmethod
    end
    else if(alg=="PLRU")begin
      Vector#(sets,Reg#(Bit#(TSub#(ways,1)))) v_count <- replicateM(mkReg(5));
      method ActionValue#(Bit#(TLog#(ways))) line_replace (Bit#(TLog#(sets)) index, Bit#(ways) valid);
        if (&(valid)==1)begin // if all lines are valid choose one to randomly replace
          case (v_count[index]) matches
            'b?00: begin if(verbosity>1) $display($time,"\tREPL: Replacing line: 0"); return 0;end 
            'b?10: begin if(verbosity>1) $display($time,"\tREPL: Replacing line: 1"); return 1;end
            'b0?1: begin if(verbosity>1) $display($time,"\tREPL: Replacing line: 2"); return 2;end
            default: begin if(verbosity>1) $display($time,"\tREPL: Replacing line: 3"); return 3; end
          endcase
        end
        else begin // if any line empty then send that
          Bit#(TLog#(ways)) temp=0;
          for(Bit#(TAdd#(1,TLog#(ways))) i=0;i<fromInteger(v_ways);i=i+1) begin
            if(valid[i]==0)begin
              temp=truncate(i);
            end
          end
          return temp;
        end
      endmethod
      method Action update_set (Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
        Bit#(TSub#(ways,1)) mask='b000;
        Bit#(TSub#(ways,1)) val='b000;
        case (way) matches
          'd0:begin val='b011; mask='b011;end
          'd1:begin val='b001; mask='b011;end
          'd2:begin val='b100; mask='b101;end
          'd3:begin val='b000; mask='b101;end  
        endcase
        if(verbosity>1)
          $display($time,"\tREPL: old:%b mask:%b new: %b final: %b",v_count[index],mask,val,(v_count[index]&~mask)|(val&mask));
        v_count[index]<=(v_count[index]&~mask)|(val&mask);
      endmethod
      method Action reset_repl(Bit#(TLog#(sets)) index);
        v_count[index]<=5;
      endmethod
    end
    else begin
      method ActionValue#(Bit#(TLog#(ways))) line_replace (Bit#(TLog#(sets)) index, Bit#(ways) valid);
        return ?;
      endmethod
      method Action update_set (Bit#(TLog#(sets)) index, Bit#(TLog#(ways)) way);
        noAction;
      endmethod
      method Action reset_repl(Bit#(TLog#(sets)) index);
        noAction;
      endmethod
    end
  endmodule
endpackage

