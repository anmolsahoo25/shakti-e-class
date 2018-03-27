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

  // project files to be imported/included
	import common_types::*;
  `include "common_params.bsv"
  import decode::*;


  // Interface for the fetch and decode unit
	interface Ifc_fetch_decode_stage;
  	interface Get#(Bit#(32)) inst_request;//instruction whose addr is needed
	  interface Put#(Tuple2#(Bit#(32),Bool)) inst_response;//addr of the given inst
    // rs1,rs2,rd,fn,funct3,instruction_type will be passed on to opfetch and execute unit
    interface TXe#(PIPE1_DS) to_opfetch_unit;
    method Action flush_from_wb( Bit#(PADDR) newpc, Bool fl);
    method Action csrs (CSRtoDecode csr);
	endinterface:Ifc_fetch_decode_stage
	(*synthesize*)
	module mkfetch_decode_stage(Ifc_fetch_decode_stage);

		Reg#(Bit#(PADDR)) pc[2] <- mkCReg(2,'h1000);  //making program counter
		Reg#(Bit#(PADDR)) shadow_pc <-mkRegU;  //shadow pc to preserve it
    Reg#(Bit#(1)) rg_epoch[2] <- mkCReg(2,0);
		Reg#(Bit#(1)) shadow_epoch <- mkReg(0);  //shadow pc to preserve it
    Wire#(CSRtoDecode) wr_csr <-mkWire();
    //instantiating the tx interface with name tx
		TX#(PIPE1_DS) tx<-mkTX;

    Integer verbosity = valueOf(`VERBOSITY);
    
    //instruction whose addr is needed
		interface inst_request = interface Get
			method ActionValue#(Bit#(PADDR)) get;
				shadow_pc<=pc[0];
        shadow_epoch<=rg_epoch[0];
        pc[0]<=pc[0]+4;
        if(verbosity!=0)
          $display($time, "\tSTAGE1: Sending Instruction Addr: %h", pc[0]);
				return pc[0];
			endmethod
		endinterface;
    
    //getting response from bus 
		interface inst_response= interface Put
			method Action put (Tuple2#(Bit#(32),Bool) resp);
        let {inst,err}=resp;
			  PIPE1_DS x= decoder_func(inst,shadow_pc,shadow_epoch, err, wr_csr);
        if(verbosity!=0)
          $display($time, "\tSTAGE1: Received from Bus: Inst: %h, PC: %h Err: %b Epoch: %b", inst,
            shadow_pc, err, shadow_epoch);
				tx.u.enq(x);  //enq the output of the decoder function in the tx interface
			endmethod
		endinterface;

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
	endmodule:mkfetch_decode_stage
endpackage:fetch_decode_stage
