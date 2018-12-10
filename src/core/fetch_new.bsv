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

Author: Neel Gala, Aditya Mathur, Deepa Sarma
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package fetch_new;
  // packages to be imported
	import GetPut::*;
	import TxRx	::*;
  import FIFOF ::*;
  import SpecialFIFOs ::*;

  // project files to be imported/included
	import common_types::*;
  `include "common_params.bsv"
  import decode::*;
  import cache_types::*;

  typedef enum {CheckPrev, None} ActionType deriving(Bits,Eq,FShow);

  // Interface for the fetch and decode unit
	interface Ifc_fetch_decode_stage;
	`ifdef icache 
	 interface Get#(Tuple4#(Bit#(PADDR),Bool,Bit#(1),Bool)) inst_request; //instruction whose addr is needed
    `else 
	interface Get#(Tuple2#(Bit#(PADDR),Bit#(1))) inst_request;
	`endif
	interface Put#(Tuple3#(Bit#(32),Bool,Bit#(1))) inst_response;//addr of the given inst
    // rs1,rs2,rd,fn,funct3,instruction_type will be passed on to opfetch and execute unit
    interface TXe#(PIPE1_DS) to_opfetch_unit;
    method Action flush_from_wb( Bit#(PADDR) newpc, Bool fence);
    method Action csrs (CSRtoDecode csr);
	endinterface:Ifc_fetch_decode_stage
	(*synthesize*)
	module mkfetch_decode_stage(Ifc_fetch_decode_stage);

    let verbosity = valueOf(`VERBOSITY);
    Wire#(CSRtoDecode) wr_csr <-mkWire();

    Reg#(Bit#(PADDR)) rg_icache_request <- mkReg('h1000);
	`ifdef icache
		Reg#(Bool) rg_fence <- mkReg(False); //fence integration
    `endif
    Reg#(Bit#(PADDR)) rg_pc <- mkReg('h1000);
    Reg#(Bit#(1)) rg_epoch <- mkReg(0);
    Reg#(ActionType) rg_action <-mkReg(None);
    Reg#(Bool) rg_discard_lower <-mkReg(False);
    Reg#(Bit#(16)) rg_instruction <- mkReg(0);

    FIFOF#(Tuple3#(Bit#(32),Bool,Bit#(1))) ff_memory_response<-mkSizedFIFOF(2);
		TX#(PIPE1_DS) tx<-mkTX;

    rule decode_instruction;
        let {prv, mip, csr_mie, mideleg, misa, counteren, mie}=wr_csr;
        let {cache_response,err,epoch}=ff_memory_response.first;
        Bit#(32) final_instruction=0;
        Bool compressed=False;
        Bool perform_decode=True;
        if(rg_epoch!=epoch)begin
          ff_memory_response.deq;
          rg_action<=None;
          perform_decode=False;
          $display($time,"\tSTAGE1: Dropping Instruction from Cache");
        end
        else if(rg_discard_lower && misa[2]==1)begin
          rg_discard_lower<=False;
          ff_memory_response.deq;
          if(cache_response[17:16]==2'b11)begin
            rg_instruction<=cache_response[31:16];
            rg_action<=CheckPrev;
            perform_decode=False;
          end
          else begin
            compressed=True;
            final_instruction=zeroExtend(cache_response[31:16]);
          end
        end
        else if(rg_action == None)begin
          ff_memory_response.deq;
          if(cache_response[1:0]=='b11)begin
            final_instruction=cache_response;
          end
          else if(misa[2]==1) begin
            compressed=True;
            final_instruction=zeroExtend(cache_response[15:0]);
            rg_instruction<=truncateLSB(cache_response);
            rg_action<=CheckPrev;
          end
        end
        else begin
          if(rg_instruction[1:0]==2'b11)begin
            final_instruction={cache_response[15:0],rg_instruction};
            rg_instruction<=truncateLSB(cache_response);
            ff_memory_response.deq;
          end
          else begin
            compressed=True;
            final_instruction=zeroExtend(rg_instruction);
            rg_action<=None;
          end
        end
        $display($time,"\tSTAGE1: rg_action: ",fshow(rg_action)," compressed: %b final_instruction:\
  %h rg_instruction: %h perform_decode: %b rg_epoch: %b",compressed,final_instruction,rg_instruction,
  perform_decode,rg_epoch);

        PIPE1_DS x = decoder_func_16(final_instruction[15:0],rg_pc,epoch,err,wr_csr);
        PIPE1_DS y = decoder_func(final_instruction,rg_pc,epoch,err,wr_csr);
        if(compressed  && perform_decode && misa[2]==1)begin
          rg_pc<=rg_pc+2;
          tx.u.enq(x);
        end
        else if(perform_decode)begin
          rg_pc<=rg_pc+4;
          tx.u.enq(y);
        end
        if(verbosity!=0)
          $display($time, "\tSTAGE1: PC: %h Inst: %h, Err: %b Epoch: %b", rg_pc, final_instruction,
                                                                                        err, epoch);
    endrule
    
	// when fence has to to initiated, we send fence=true along with the address of instruction following fence-instruction, but
	// this instr wont get fetched as it is tagged fence=true.
	// In this situation, so to actually fetch instruction following fence-instr, we should not increment icache_request by 4
    `ifdef icache
	interface inst_request=interface Get
      method ActionValue#(Tuple4#(Bit#(PADDR),Bool,Bit#(1),Bool)) get;
		   		if(rg_fence==True)
			    	rg_fence<=False; // reset fence once the command is sent
				else
				rg_icache_request<=rg_icache_request+4; 
        return tuple4(rg_icache_request,rg_fence,rg_epoch,False);
      endmethod
    endinterface;

    `else
    interface inst_request=interface Get
      method ActionValue#(Tuple2#(Bit#(PADDR),Bit#(1))) get;
				rg_icache_request<=rg_icache_request+4; 
        return tuple2(rg_icache_request,rg_epoch);
      endmethod
    endinterface;
    `endif


	interface inst_response= interface Put
	  method Action put (Tuple3#(Bit#(32),Bool,Bit#(1)) resp);
        ff_memory_response.enq(resp);
	  endmethod
    endinterface;
    

    //providing the output of the decoder function to the opfetch unit via tx interface
		interface to_opfetch_unit=tx.e;
    method Action flush_from_wb( Bit#(PADDR) newpc, Bool fence); 
		`ifdef icache
		  if(fence) //fence integration
		  	rg_fence<=True;
		`endif
      rg_pc<=newpc;
      rg_epoch<=~rg_epoch;
      rg_icache_request<={truncateLSB(newpc),2'b0};
      if(newpc[1:0]!=0)
        rg_discard_lower<=True;
      if(verbosity>1)
        $display($time, "\tSTAGE1: Received Flush. PC: %h Flush: ",newpc); 
      ff_memory_response.clear();
    endmethod

    method Action csrs (CSRtoDecode csr);
      wr_csr <= csr;
    endmethod
	endmodule:mkfetch_decode_stage
endpackage:fetch_new
