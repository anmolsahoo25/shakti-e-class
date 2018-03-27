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
package core;
  // project related imports
	import Semi_FIFOF:: *;
	import AXI4_Types:: *;
	import AXI4_Fabric:: *;
  import riscv:: * ;
  import common_types:: * ;
  `include "common_params.bsv"

  // package imports
	import Connectable 				:: *;
  import GetPut:: *;
  
  typedef enum {Request, Response} TxnState deriving(Bits, Eq, FShow);
  interface Ifc_core;
		interface AXI4_Master_IFC#(PADDR, XLEN, USERSPACE) fetch_master;
		interface AXI4_Master_IFC#(PADDR, XLEN, USERSPACE) mem_master;
		`ifdef CLINT
			method Action clint_msip(Bit#(1) intrpt);
			method Action clint_mtip(Bit#(1) intrpt);
			method Action clint_mtime(Bit#(XLEN) c_mtime);
		`endif
  endinterface: Ifc_core

  (*synthesize*)
  module mkcore(Ifc_core);
    Ifc_riscv riscv <- mkriscv();
		AXI4_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) fetch_xactor <- mkAXI4_Master_Xactor;
		AXI4_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) memory_xactor <- mkAXI4_Master_Xactor;
    Reg#(TxnState) fetch_state<- mkReg(Request);
    Reg#(TxnState) memory_state<- mkReg(Request);
    Reg#(MemoryRequest) memory_request <- mkReg(unpack(0));
    rule handle_fetch_request(fetch_state == Request) ;
      let inst_addr<- riscv.inst_request.get;
			let read_request = AXI4_Rd_Addr {araddr: inst_addr, aruser: ?, arlen: 0, arsize: 2, arburst:
      'b01, arid:'d1}; // arburst: 00-FIXED 01-INCR 10-WRAP
			fetch_xactor.i_rd_addr.enq(read_request);	
      fetch_state<= Response;
    endrule
    rule handle_fetch_response(fetch_state == Response);
			let response <- pop_o (fetch_xactor.o_rd_data);	
			Bool bus_error = !(response.rresp==AXI4_OKAY);
      riscv.inst_response.put(tuple2(truncate(response.rdata), bus_error));
      fetch_state<= Request;
    endrule
    rule handle_memory_request(memory_state ==  Request);
      let {address, data, access, size, sign}<- riscv.memory_request.get;
      memory_request<= tuple5(address, data, access, size, sign);
			Bit#(TDiv#(XLEN, 8)) write_strobe=size==0?'b1:size==1?'b11:size==2?'hf:'1;
			if(size!=3)begin			// 8-bit write;
				write_strobe=write_strobe<<(address[2:0]);
			end
      if(access == Load) begin
        let read_request = AXI4_Rd_Addr {araddr: address, aruser: 0, arlen: 0, arsize: 
          zeroExtend(size), arburst:'b01, arid:'d0}; // arburst: 00-FIXED 01-INCR 10-WRAP
   	   		memory_xactor.i_rd_addr.enq(read_request);	
      end
      else begin
			  let aw = AXI4_Wr_Addr {awaddr: truncate(address), awuser:0, awlen: 0, awsize: 
          zeroExtend(size), awburst: 'b01, awid:'d0}; // arburst: 00-FIXED 01-INCR 10-WRAP
  			let w  = AXI4_Wr_Data {wdata: data, wstrb: write_strobe, wlast:True, wid:'d0};
	  		memory_xactor.i_wr_addr.enq(aw);
		  	memory_xactor.i_wr_data.enq(w);
      end
      memory_state<= Response;
    endrule
    rule handle_memoryRead_response(memory_state == Response && tpl_3(memory_request) == Load);
      let {address, data, access, size, sign}=  memory_request;
			let response <- pop_o (memory_xactor.o_rd_data);	
			let bus_error = !(response.rresp==AXI4_OKAY);
      // TODO shift, and perform signextension before sending to core.
			riscv.memory_response.put(tuple2(response.rdata, bus_error));
      memory_state<= Request;
    endrule
    rule handle_memoryWrite_response(memory_state == Response && tpl_3(memory_request) == Store);
      let {address, data, access, size, sign}=  memory_request;
			let response<-pop_o(memory_xactor.o_wr_resp);
			let bus_error = !(response.bresp==AXI4_OKAY);
			riscv.memory_response.put(tuple2(0, bus_error));
      memory_state<= Request;
    endrule
		`ifdef CLINT
	  	method Action clint_msip(Bit#(1) intrpt);
        riscv.clint_msip(intrpt);
      endmethod
			method Action clint_mtip(Bit#(1) intrpt);
        riscv.clint_mtip(intrpt);
      endmethod
			method Action clint_mtime(Bit#(XLEN) c_mtime);
        riscv.clint_mtime(c_mtime);
      endmethod
		`endif
		interface fetch_master= fetch_xactor.axi_side;
		interface mem_master= memory_xactor.axi_side;
  endmodule: mkcore
endpackage
