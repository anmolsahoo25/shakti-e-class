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

Author: Neel Gala, Aditya Mathur
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package fetch_decode_stage;
  // packages to be imported
	import GetPut::*;
	import TxRx	::*;
  import FIFOF ::*;
  import SpecialFIFOs ::*;

  // project files to be imported/included
	import common_types::*;
  `include "common_params.bsv"
  import decode::*;


  // Interface for the fetch and decode unit
	interface Ifc_fetch_decode_stage;
    `ifdef compressed
  	  interface Get#(Tuple2#(Bit#(32),Bit#(1))) inst_request;//instruction whose addr is needed
      interface Put#(Tuple4#(Bit#(32),Bool,Bit#(32),Bit#(1))) inst_response;//addr of the given inst
    `else
      interface Get#(Bit#(32)) inst_request;//instruction whose addr is needed
 	    interface Put#(Tuple2#(Bit#(32),Bool)) inst_response;//addr of the given inst
    `endif
    // rs1,rs2,rd,fn,funct3,instruction_type will be passed on to opfetch and execute unit
    interface TXe#(PIPE1_DS) to_opfetch_unit;
    method Action flush_from_wb( Bit#(PADDR) newpc, Bool fl);
    method Action csrs (CSRtoDecode csr);
	endinterface:Ifc_fetch_decode_stage
	(*synthesize*)
	module mkfetch_decode_stage(Ifc_fetch_decode_stage);

    Reg#(Bit#(PADDR)) pc[2] <- mkCReg(2,'h1000);  //making program counter
    Reg#(Bit#(1)) rg_epoch[2] <- mkCReg(2,0);
		Reg#(Bit#(1)) shadow_epoch <- mkReg(0);  //shadow pc to preserve it
    Wire#(CSRtoDecode) wr_csr <-mkWire();
    Integer verbosity = valueOf(`VERBOSITY);
    `ifdef compressed
    	Reg#(Maybe#(Bit#(16)))buff<-mkReg(tagged Invalid);
    	Reg#(Bit#(1)) epoch_buff<-mkReg(0);
     	FIFOF#(Tuple4#(Bit#(32),Bool,Bit#(32),Bit#(1))) ff_response_from_memory <-mkSizedBypassFIFOF(1);
    `else 
    	Reg#(Bit#(PADDR)) shadow_pc <-mkRegU;  //shadow pc to preserve it
    `endif
    //instantiating the tx interface with name tx
		TX#(PIPE1_DS) tx<-mkTX;
    `ifdef compressed
      rule inst_response_to_decode;
        Bit#(32) inst_decode = 0;
        Bool compressed = False;
        let {inst,err,shadow_pc,shadow_epoch}=ff_response_from_memory.first;
         
        if(shadow_pc[1:0] == 2'b10 && inst[17:16]==2'b11) begin//upon a jump inst to pc+2
          ff_response_from_memory.deq;
          buff      <= tagged Valid inst[31:16];
          epoch_buff<= shadow_epoch;
        end
        else begin
          if(shadow_pc[1:0] == 2'b10 && inst[17:16]!=2'b11) begin
            inst_decode =  zeroExtend(inst[31:16]);
            ff_response_from_memory.deq;
            compressed=True;
            buff       <= tagged Invalid;
          end
          else if(!isValid(buff) && inst[1:0]==2'b11)begin
            inst_decode=inst;
            buff <= tagged Invalid;
            ff_response_from_memory.deq;
          end
          else if(buff matches tagged Valid .d) begin
            if(d[1:0]==2'b11 && (epoch_buff==shadow_epoch)) begin
              inst_decode={inst[15:0],d};
              buff <= tagged Valid inst[31:16];
              epoch_buff<=shadow_epoch;
              ff_response_from_memory.deq;
            end
            else begin
              inst_decode=zeroExtend(d);
              compressed = True;
              buff <= tagged Invalid;
            end
          end
          else if(!(isValid(buff)) && inst[1:0]!=2'b11) begin
            inst_decode=zeroExtend(inst[15:0]);
            buff <= tagged Valid inst[31:16];
            compressed = True;
            epoch_buff<=shadow_epoch;
            ff_response_from_memory.deq;
          end
//              TO DO WFI
          if(verbosity!=0)
            $display($time,"\tSTAGE1: PC: %h Inst: %h, Err: %b Epoch: %b", shadow_pc,
                inst_decode, err, shadow_epoch);

          let pc_inst =((!isValid(buff)||shadow_pc[1:0]==2'b10))?shadow_pc:shadow_pc-2;
          let epoch_inst=((!isValid(buff)||shadow_pc[1:0]==2'b10))?shadow_epoch:epoch_buff;
          PIPE1_DS x = decoder_func_16(inst_decode[15:0],pc_inst,epoch_inst,err,wr_csr);
          PIPE1_DS y = decoder_func(inst_decode,pc_inst,epoch_inst,err,wr_csr);
          if (compressed)
            tx.u.enq(x);
          else
            tx.u.enq(y);
        end
      endrule
    `endif
    //getting response from bus  
    //instruction whose addr is needed
		`ifdef compressed
		  interface inst_request = interface Get
			  method ActionValue#(Tuple2#(Bit#(PADDR),Bit#(1))) get;
				  if(pc[0][1:0]==2'b10)
				    pc[0]<=pc[0]+2;
				  else
            pc[0]<=pc[0]+4;
          if(verbosity!=0)
            $display($time, "\tSTAGE1: Sending Instruction Addr: %h", pc[0]);
		      return tuple2(pc[0],rg_epoch[0]);
        endmethod
      endinterface;
		  interface inst_response= interface Put
		  	method Action put (Tuple4#(Bit#(32),Bool,Bit#(32),Bit#(1)) resp);
          ff_response_from_memory.enq(resp);
            $display($time,"Enqueuing response on to the Stage1 for inst %h",resp);
		  	endmethod
		  endinterface;
    `else 
		  interface inst_request = interface Get
			  method ActionValue#(Bit#(PADDR)) get;
          pc[0]<=pc[0]+4;
          if(verbosity!=0)
            $display($time, "\tSTAGE1: Sending Instruction Addr: %h", pc[0]);
				  shadow_pc<=pc[0];
          shadow_epoch<=rg_epoch[0];
			    return pc[0];
		    endmethod
		  endinterface;
		  interface inst_response= interface Put
		  	method Action put (Tuple2#(Bit#(32),Bool) resp);
          let {inst,err}=resp;
		  	  PIPE1_DS x= decoder_func(inst,shadow_pc,shadow_epoch, err, wr_csr);
          // TODO WFI
          if(verbosity!=0)
            $display($time, "\tSTAGE1: PC: %h Inst: %h, Err: %b Epoch: %b", shadow_pc, inst, err, 
                shadow_epoch);
		  		tx.u.enq(x);  //enq the output of the decoder function in the tx interface
		  	endmethod
		  endinterface;
    `endif
    
    //providing the output of the decoder function to the opfetch unit via tx interface
		interface to_opfetch_unit=tx.e;
    method Action flush_from_wb( Bit#(PADDR) newpc, Bool fl);
      if(fl)begin
        rg_epoch[1]<=~rg_epoch[1];
        pc[1]<=newpc;
        if(verbosity>1)
          $display($time, "\tSTAGE1: Received Flush. PC: %h Flush: ",newpc, fshow(fl)); 
      end
    endmethod

    method Action csrs (CSRtoDecode csr);
      wr_csr <= csr;
    endmethod
	endmodule
endpackage
