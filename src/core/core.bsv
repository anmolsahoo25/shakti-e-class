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

  //=================== Interface and module for a core- master on the AXI4 fabric ============= //
  // project related imports
	import Semi_FIFOF:: *;
  import AXI4_Lite_Types::*;
  import AXI4_Lite_Fabric::*;
	import AXI4_Types:: *;
	import AXI4_Fabric:: *;
  import Tilelink_lite_Types::*;
  import riscv:: * ;
  import common_types:: * ;
  import FIFOF::*;
  `include "common_params.bsv"
  `include "SoC.defines"

  // package imports
	import Connectable 				:: *;
  import GetPut:: *;
  import BUtils::*;
  
  typedef enum {Request, Response} TxnState deriving(Bits, Eq, FShow);
  interface Ifc_core_AXI4;
		interface AXI4_Master_IFC#(PADDR, XLEN, USERSPACE) fetch_master;
		interface AXI4_Master_IFC#(PADDR, XLEN, USERSPACE) mem_master;
		method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
  endinterface: Ifc_core_AXI4

  (*synthesize*)
  module mkcore_AXI4(Ifc_core_AXI4);
    Ifc_riscv riscv <- mkriscv();
		AXI4_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) fetch_xactor <- mkAXI4_Master_Xactor;
		AXI4_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) memory_xactor <- mkAXI4_Master_Xactor;
    Reg#(TxnState) fetch_state<- mkReg(Request);
    Reg#(TxnState) memory_state<- mkReg(Request);
    Reg#(CoreRequest) memory_request <- mkReg(unpack(0));

    `ifdef compressed
      FIFOF#(Bit#(32)) ff_inst <-mkFIFOF;
      FIFOF#(Bit#(1))  ff_epoch <-mkFIFOF;
    `endif
    Integer verbosity = `VERBOSITY;

    rule handle_fetch_request(fetch_state == Request) ;
      let {inst_addr `ifdef compressed ,epoch `endif }<- riscv.inst_request.get;
			AXI4_Rd_Addr#(PADDR, 0) read_request = AXI4_Rd_Addr {araddr: {inst_addr `ifdef compressed [31:2],2'b00 `endif }, aruser: ?, arlen: 0, 
          arsize: 2, arburst: 'b01, arid:`Fetch_master_num}; // arburst: 00-FIXED 01-INCR 10-WRAP
			fetch_xactor.i_rd_addr.enq(read_request);	
      `ifdef compressed
        ff_inst.enq(inst_addr);
        ff_epoch.enq(epoch);   	
      `endif
      fetch_state<= Response;
      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Request ", fshow(read_request));
    endrule
    rule handle_fetch_response(fetch_state == Response);
			let response <- pop_o (fetch_xactor.o_rd_data);	
			Bool bus_error = !(response.rresp==AXI4_OKAY);
      `ifdef compressed
        riscv.inst_response.put(tuple4(truncate(response.rdata), bus_error,ff_inst.first,ff_epoch.first));
        ff_inst.deq;
        ff_epoch.deq;
      `else
        riscv.inst_response.put(tuple2(truncate(response.rdata), bus_error));
      `endif
      fetch_state<= Request;

      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Response ", fshow(response));
    endrule
    rule handle_memory_request(memory_state ==  Request);
      let {address, data, access, size, sign}<- riscv.memory_request.get;
      memory_request<= tuple4(address, access, size, sign);
      if(size==0)
        data=duplicate(data[7:0]);
      else if(size==1)
        data=duplicate(data[15:0]);
      else if(size==2)
        data=duplicate(data[31:0]);
			Bit#(TDiv#(XLEN, 8)) write_strobe=size==0?'b1:size==1?'b11:size==2?'hf:'1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(address);
			if(size!=3)begin			// 8-bit write;
				write_strobe=write_strobe<<byte_offset;
			end
      if(access != Store) begin
        AXI4_Rd_Addr#(PADDR, 0) read_request = AXI4_Rd_Addr {araddr: address, aruser: 0, arlen: 0, 
            arsize: zeroExtend(size), arburst:'b01, arid:`Mem_master_num}; //arburst: 00-FIXED 01-INCR 10-WRAP
   	   		memory_xactor.i_rd_addr.enq(read_request);	
        if(verbosity!=0)
          $display($time, "\tCORE: Memory Read Request ", fshow(read_request));
      end
      else begin
			   AXI4_Wr_Addr#(PADDR, 0) aw = AXI4_Wr_Addr {awaddr: truncate(address), awuser:0, awlen: 0, 
            awsize: zeroExtend(size), awburst: 'b01, awid:`Mem_master_num}; //arburst: 00-FIXED 01-INCR 10-WRAP
  			let w  = AXI4_Wr_Data {wdata: data, wstrb: write_strobe, wlast:True, wid:`Mem_master_num};
        if(verbosity!=0)begin
          $display($time, "\tCORE: Memory write Request ", fshow(aw));
          $display($time, "\tCORE: Memory write Request ", fshow(w));
        end
	  		memory_xactor.i_wr_addr.enq(aw);
		  	memory_xactor.i_wr_data.enq(w);
      end
      memory_state<= Response;
    endrule
    rule handle_memoryRead_response(memory_state == Response && tpl_2(memory_request) != Store);
      let {address, access, size, sign}=  memory_request;
			let response <- pop_o (memory_xactor.o_rd_data);	
			let bus_error = !(response.rresp==AXI4_OKAY);
      let rdata=response.rdata;
      if(size==0)
          rdata=sign==1?signExtend(rdata[7:0]):zeroExtend(rdata[7:0]);
      else if(size==1)
          rdata=sign==1?signExtend(rdata[15:0]):zeroExtend(rdata[15:0]);
      else if(size==2)
          rdata=sign==1?signExtend(rdata[31:0]):zeroExtend(rdata[31:0]);
      // TODO shift, and perform signextension before sending to core.
      `ifdef atomic
        if(access==Atomic)
          riscv.atomic_response.put(tuple3(rdata, bus_error, access));
        else
      `endif
  			riscv.memory_response.put(tuple3(rdata, bus_error, access));
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Read Response ", fshow(response));
      memory_state<= Request;
    endrule
    rule handle_memoryWrite_response(memory_state == Response && tpl_2(memory_request) == Store);
      let {address, access, size, sign}=  memory_request;
			let response<-pop_o(memory_xactor.o_wr_resp);
			let bus_error = !(response.bresp==AXI4_OKAY);
			riscv.memory_response.put(tuple3(0, bus_error, access));
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Write Response ", fshow(response));
      memory_state<= Request;
    endrule
	  method Action clint_msip(Bit#(1) intrpt);
      riscv.clint_msip(intrpt);
    endmethod
		method Action clint_mtip(Bit#(1) intrpt);
      riscv.clint_mtip(intrpt);
    endmethod
		method Action clint_mtime(Bit#(XLEN) c_mtime);
      riscv.clint_mtime(c_mtime);
    endmethod
    method Action externalinterrupt(Bit#(1) intrpt);
      riscv.externalinterrupt(intrpt);
    endmethod
		interface fetch_master= fetch_xactor.axi_side;
		interface mem_master= memory_xactor.axi_side;
    `ifdef simulate
      interface dump=riscv.dump;
    `endif
  endmodule: mkcore_AXI4
  //=================== Interface and module for a core- master on the AXI4 fabric ============= //
  interface Ifc_core_AXI4Lite;
		interface AXI4_Lite_Master_IFC#(PADDR, XLEN, USERSPACE) fetch_master;
		interface AXI4_Lite_Master_IFC#(PADDR, XLEN, USERSPACE) mem_master;
		method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
  endinterface: Ifc_core_AXI4Lite

  (*synthesize*)
  module mkcore_AXI4Lite(Ifc_core_AXI4Lite);
    Ifc_riscv riscv <- mkriscv();
		AXI4_Lite_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) fetch_xactor<- mkAXI4_Lite_Master_Xactor;
		AXI4_Lite_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) memory_xactor<- mkAXI4_Lite_Master_Xactor;
    Reg#(TxnState) fetch_state<- mkReg(Request);
    Reg#(TxnState) memory_state<- mkReg(Request);
    Reg#(CoreRequest) memory_request <- mkReg(unpack(0));

    `ifdef compressed
      FIFOF#(Bit#(32)) ff_inst <-mkFIFOF;
      FIFOF#(Bit#(1))  ff_epoch <-mkFIFOF;
    `endif

    Integer verbosity = `VERBOSITY;

    rule handle_fetch_request(fetch_state == Request) ;
      let {inst_addr `ifdef compressed ,epoch `endif }<- riscv.inst_request.get;
			AXI4_Lite_Rd_Addr#(PADDR, 0) read_request = AXI4_Lite_Rd_Addr {araddr:{inst_addr `ifdef compressed [31:2],2'b00 `endif }, aruser: ?, 
          arsize: 2}; // arburst: 00-FIXED 01-INCR 10-WRAP
			fetch_xactor.i_rd_addr.enq(read_request);	
      `ifdef compressed
	      ff_inst.enq(inst_addr);
	      ff_epoch.enq(epoch);		
      `endif
      fetch_state<= Response;
      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Request ", fshow(read_request));
    endrule
    rule handle_fetch_response(fetch_state == Response);
			let response <- pop_o (fetch_xactor.o_rd_data);	
			Bool bus_error = !(response.rresp==AXI4_LITE_OKAY);
      `ifdef compressed
          riscv.inst_response.put(tuple4(truncate(response.rdata), bus_error,ff_inst.first,ff_epoch.first));
          ff_inst.deq;
          ff_epoch.deq;
       `else
         riscv.inst_response.put(tuple2(truncate(response.rdata), bus_error));
       `endif
      fetch_state<= Request;
      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Response ", fshow(response));
    endrule
    rule handle_memory_request(memory_state ==  Request);
      let {address, data, access, size, sign}<- riscv.memory_request.get;
      memory_request<= tuple4(address, access, size, sign);
      if(size==0)
        data=duplicate(data[7:0]);
      else if(size==1)
        data=duplicate(data[15:0]);
      else if(size==2)
        data=duplicate(data[31:0]);
			Bit#(TDiv#(XLEN, 8)) write_strobe=size==0?'b1:size==1?'b11:size==2?'hf:'1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(address);
			if(size!=3)begin			// 8-bit write;
				write_strobe=write_strobe<<byte_offset;
			end
      if(access == Load) begin
        AXI4_Lite_Rd_Addr#(PADDR, 0) read_request = AXI4_Lite_Rd_Addr {araddr: address, aruser:?, 
            arsize: zeroExtend(size)}; //arburst: 00-FIXED 01-INCR 10-WRAP
   	   	memory_xactor.i_rd_addr.enq(read_request);	
        if(verbosity!=0)
          $display($time, "\tCORE: Memory Read Request ", fshow(read_request));
      end
      else begin
			   AXI4_Lite_Wr_Addr#(PADDR, 0) aw = AXI4_Lite_Wr_Addr {awaddr: truncate(address), awuser:?, 
            awsize: zeroExtend(size)}; //arburst: 00-FIXED 01-INCR 10-WRAP
  			let w  = AXI4_Lite_Wr_Data {wdata: data, wstrb: write_strobe};
        if(verbosity!=0)begin
          $display($time, "\tCORE: Memory write Request ", fshow(aw));
          $display($time, "\tCORE: Memory write Request ", fshow(w));
        end
	  		memory_xactor.i_wr_addr.enq(aw);
		  	memory_xactor.i_wr_data.enq(w);
      end
      memory_state<= Response;
    endrule
    rule handle_memoryRead_response(memory_state == Response && tpl_2(memory_request) == Load);
      let {address, access, size, sign}=  memory_request;
			let response <- pop_o (memory_xactor.o_rd_data);	
			let bus_error = !(response.rresp==AXI4_LITE_OKAY);
      let rdata=response.rdata;
      if(size==0)
          rdata=sign==1?signExtend(rdata[7:0]):zeroExtend(rdata[7:0]);
      else if(size==1)
          rdata=sign==1?signExtend(rdata[15:0]):zeroExtend(rdata[15:0]);
      else if(size==2)
          rdata=sign==1?signExtend(rdata[31:0]):zeroExtend(rdata[31:0]);
      // TODO shift, and perform signextension before sending to core.
			riscv.memory_response.put(tuple3(rdata, bus_error, access));
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Read Response ", fshow(response));
      memory_state<= Request;
    endrule
    rule handle_memoryWrite_response(memory_state == Response && tpl_2(memory_request) == Store);
      let {address, access, size, sign}=  memory_request;
			let response<-pop_o(memory_xactor.o_wr_resp);
			let bus_error = !(response.bresp==AXI4_LITE_OKAY);
			riscv.memory_response.put(tuple3(0, bus_error,  access));
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Write Response ", fshow(response));
      memory_state<= Request;
    endrule
	  method Action clint_msip(Bit#(1) intrpt);
      riscv.clint_msip(intrpt);
    endmethod
		method Action clint_mtip(Bit#(1) intrpt);
      riscv.clint_mtip(intrpt);
    endmethod
		method Action clint_mtime(Bit#(XLEN) c_mtime);
      riscv.clint_mtime(c_mtime);
    endmethod
    method Action externalinterrupt(Bit#(1) intrpt);
      riscv.externalinterrupt(intrpt);
    endmethod
		interface fetch_master= fetch_xactor.axi_side;
		interface mem_master= memory_xactor.axi_side;
    `ifdef simulate
      interface dump=riscv.dump;
    `endif
  endmodule: mkcore_AXI4Lite

  interface Ifc_core_TLU;
		interface Ifc_fabric_side_master_link_lite#(PADDR, TDiv#(XLEN, 8), 2) fetch_master;
		interface Ifc_fabric_side_master_link_lite#(PADDR, TDiv#(XLEN, 8), 2) mem_master;
		method Action clint_msip(Bit#(1) intrpt);
		method Action clint_mtip(Bit#(1) intrpt);
		method Action clint_mtime(Bit#(XLEN) c_mtime);
    method Action externalinterrupt(Bit#(1) intrpt);
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
  endinterface: Ifc_core_TLU
  (*synthesize*)
  module mkcore_TLU(Ifc_core_TLU);
    Ifc_Master_link_lite#(PADDR, TDiv#(XLEN, 8), 2)  fetch_xactor <- mkMasterXactorLite(True, True);
    Ifc_Master_link_lite#(PADDR, TDiv#(XLEN, 8), 2)  dmem_xactor <- mkMasterXactorLite(True, True);
    Ifc_riscv riscv <- mkriscv();
    Reg#(TxnState) fetch_state<- mkReg(Request);
    Reg#(Bit#(1)) memory_request <- mkReg(0);

    `ifdef compressed
    	FIFOF#(Bit#(32)) ff_inst <-mkFIFOF;
    	FIFOF#(Bit#(1))  ff_epoch <-mkFIFOF;
    `endif

    Integer verbosity = `VERBOSITY;

    rule handle_fetch_request(fetch_state == Request) ;
      let {inst_addr `ifdef compressed ,epoch `endif }<- riscv.inst_request.get;
      A_channel_lite#(PADDR, TDiv#(XLEN, 8), 2) lite_request = 
            A_channel_lite { a_opcode : Get_data, a_size :2, a_source: `Fetch_master_num, a_address
                      :{inst_addr `ifdef compressed [31:2],2'b00 `endif }, a_mask : ?, a_data : ?};
	  	fetch_xactor.core_side.master_request.put(lite_request);	
      `ifdef compressed
      	ff_inst.enq(inst_addr);
       	ff_epoch.enq(epoch);	
      `endif
      fetch_state<= Response;
      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Request ", fshow(lite_request));
    endrule
    rule handle_fetch_response(fetch_state == Response);
	    let response <- fetch_xactor.core_side.master_response.get;	
      `ifdef compressed
        riscv.inst_response.put(tuple4(truncate(response.d_data), response.d_error,ff_inst.first,ff_epoch.first));
      	ff_inst.deq();
      	ff_epoch.deq();
      `else
	      riscv.inst_response.put(tuple2(truncate(response.d_data), response.d_error));
      `endif
      fetch_state<= Request;
      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Response ", fshow(response));
    endrule
    rule handle_memory_request;
      let {address, data, access, size, sign}<- riscv.memory_request.get;
      memory_request<= sign;
      if(size==0)
        data=duplicate(data[7:0]);
      else if(size==1)
        data=duplicate(data[15:0]);
      else if(size==2)
        data=duplicate(data[31:0]);
			Bit#(TDiv#(XLEN, 8)) write_strobe=size==0?'b1:size==1?'b11:size==2?'hf:'1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(address);
			if(size!=3)begin			// 8-bit write;
				write_strobe=write_strobe<<byte_offset;
			end
      A_channel_lite#(PADDR, TDiv#(XLEN, 8), 2) lite_request= A_channel_lite{a_opcode:
      unpack({1'b0,truncate(pack(access))}), 
          a_size: size,  a_source: `Mem_master_num, a_address : address, a_mask : write_strobe, a_data: data};
   	  dmem_xactor.core_side.master_request.put(lite_request);	
    endrule
    rule handle_memoryRead_response;
      let sign=  memory_request;
			let response <- dmem_xactor.core_side.master_response.get;
      let rdata=response.d_data;
      if(response.d_size==0)
          rdata=sign==1?signExtend(rdata[7:0]):zeroExtend(rdata[7:0]);
      else if(response.d_size==1)
          rdata=sign==1?signExtend(rdata[15:0]):zeroExtend(rdata[15:0]);
      else if(response.d_size==2)
          rdata=sign==1?signExtend(rdata[31:0]):zeroExtend(rdata[31:0]);
			let bus_error = (response.d_error);
      if(response.d_opcode==AccessAck) // store operation
			  riscv.memory_response.put(tuple3(0, bus_error, Load ));
      else
  			riscv.memory_response.put(tuple3(rdata, bus_error,  Store));
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Read Response ", fshow(response));
    endrule
    interface fetch_master = fetch_xactor.fabric_side;
    interface mem_master = dmem_xactor.fabric_side;
	  method Action clint_msip(Bit#(1) intrpt);
      riscv.clint_msip(intrpt);
    endmethod
		method Action clint_mtip(Bit#(1) intrpt);
      riscv.clint_mtip(intrpt);
    endmethod
		method Action clint_mtime(Bit#(XLEN) c_mtime);
      riscv.clint_mtime(c_mtime);
    endmethod
    method Action externalinterrupt(Bit#(1) intrpt);
      riscv.externalinterrupt(intrpt);
    endmethod
    `ifdef simulate
      interface dump=riscv.dump;
    `endif
  endmodule

endpackage
