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
package riscv;
  // project imports
  import common_types::*;
  import fetch_decode_stage::*;
  import opfetch_execute_stage::*;
  import mem_wb_stage::*;

  // package imports
  import GetPut::*;
  import Connectable::*;
  import FIFO::*;
  import FIFOF::*;

  interface Ifc_riscv;
  	interface Get#(Bit#(32)) inst_request;//instruction whose addr is needed
	  interface Put#(Tuple2#(Bit#(32),Bool)) inst_response;//addr of the given inst
    interface Get#(MemoryRequest) memory_request;
    interface Put#(Tuple3#(Bit#(XLEN), Bool, Access_type)) memory_response;
	  method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
  endinterface:Ifc_riscv

  (*synthesize*)
  module mkriscv(Ifc_riscv);
    // instantiate each stage here
    Ifc_fetch_decode_stage stage1 <- mkfetch_decode_stage;        // stage-1: fetch n decode
    Ifc_opfetch_execute_stage stage2 <- mkopfetch_execute_stage;  // stage-2: op-fetch n execute
    Ifc_mem_wb_stage stage3 <- mkmem_wb_stage;

    // instantiate the pipeline-buffers
    FIFOF#(PIPE1_DS) pipe1 <- mkSizedFIFOF(2);
    FIFOF#(PIPE2_DS) pipe2 <- mkSizedFIFOF(2);

		mkConnection(stage1.to_opfetch_unit, pipe1);  // connect stage-1 output to pipe-1
		mkConnection(pipe1, stage2.from_fetch_decode_unit);  // connect pipe-1 to inputs of stage-2
    mkConnection(stage2.to_mem_wb_unit, pipe2);  // connect stage-2 output to pipe-2
    mkConnection(pipe2,stage3.from_execute);

    mkConnection(stage3.commit_rd,stage2.commit_rd);
    mkConnection(stage3.operand_fwding, stage2.operand_fwding);
    `ifdef RV64
      rule send_inferred_xlen;
        stage2.inferred_xlen(stage3.inferred_xlen);
      endrule
    `endif

    rule flush_from_writeback;
      let {newpc, fl}=stage3.flush;
      stage1.flush_from_wb(newpc, fl);
      stage2.flush_from_wb(fl);
      if(fl)begin
        pipe1.clear;
        pipe2.clear;
      end
    endrule

    rule connect_csrs;
      stage1.csrs(stage3.csrs_to_decode);
    endrule

    interface inst_request = stage1.inst_request;
    interface inst_response = stage1.inst_response;
    interface memory_request = stage2.memory_request;
    interface memory_response = stage3.memory_response;

	 	method Action clint_msip(Bit#(1) intrpt);
      stage3.clint_msip(intrpt);
    endmethod
	  method Action clint_mtip(Bit#(1) intrpt);
      stage3.clint_mtip(intrpt);
    endmethod
		method Action clint_mtime(Bit#(XLEN) c_mtime);
      stage3.clint_mtime(c_mtime);
    endmethod
    method Action externalinterrupt(Bit#(1) intrpt);
      stage3.externalinterrupt(intrpt);
    endmethod
    `ifdef simulate
      interface dump=stage3.dump;
    `endif
  endmodule:mkriscv
endpackage: riscv
