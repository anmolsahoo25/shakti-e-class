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
package eclass;

  //=================== Interface and module for a eclass- master on the AXI4 fabric ============= //
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

  `define Mem_master_num 0
  `define Fetch_master_num 1
  `define IO_master_num 2

  // package imports
	import Connectable 				:: *;
  import GetPut:: *;
  import BUtils::*;
  
  export Ifc_eclass_axi4    (..);
  export mkeclass_axi4;
  //export Ifc_eclass_axi4lite  (..);
  //export mkeclass_axi4lite;
  export DumpType (..);

  typedef enum {Request, Response} TxnState deriving(Bits, Eq, FShow);
  interface Ifc_eclass_axi4;
		interface AXI4_Master_IFC#(`paddr, XLEN, USERSPACE) master_d;
		interface AXI4_Master_IFC#(`paddr, XLEN, USERSPACE) master_i;
    interface Put#(Bit#(1)) sb_clint_msip;
    interface Put#(Bit#(1)) sb_clint_mtip;
    interface Put#(Bit#(64)) sb_clint_mtime;
    interface Put#(Bit#(1)) sb_externalinterrupt;
    `ifdef rtldump
      interface Get#(DumpType) io_dump;
    `endif
  endinterface: Ifc_eclass_axi4

  (*synthesize*)
  module mkeclass_axi4(Ifc_eclass_axi4);
    Ifc_riscv riscv <- mkriscv();
		AXI4_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) fetch_xactor <- mkAXI4_Master_Xactor;
		AXI4_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) memory_xactor <- mkAXI4_Master_Xactor;
    // Reg#(TxnState) fetch_state<- mkReg(Request);
    //Reg#(TxnState) memory_state<- mkReg(Request);
    //Reg#(CoreRequest) memory_request <- mkReg(unpack(0));
	//Reg#(Bit#(1)) rg_epoch <- mkReg(0);

	// this fifo stores the epochs of instruction addresses latched onto fabric.
	// fifo size indicates no. of consecutive requests that can be made. The optimum size depends on internals of fabric.
	FIFOF#(Bit#(1)) ff_epoch <- mkSizedFIFOF(2);

	// fifo of size 1 effectively enables only one data request to be be latched & served at a time.
	FIFOF#(CoreRequest) ff_mem_request <- mkFIFOF1; 

    //`ifdef compressed
    // FIFOF#(Bit#(32)) ff_inst <-mkFIFOF;
    // FIFOF#(Bit#(1))  ff_epoch <-mkFIFOF;
    //`endif
    Integer verbosity = `VERBOSITY;

	  rule handle_fetch_request;
      //let {inst_addr `ifdef compressed ,epoch `endif }<- riscv.inst_request.get;
	    let {inst_addr ,epoch} <- riscv.inst_request.get; 
			AXI4_Rd_Addr#(`paddr, 0) read_request = AXI4_Rd_Addr {araddr: inst_addr, aruser: ?, arlen: 0, 
			arsize: 2, arburst: 'b01, arid:`Fetch_master_num}; // arburst: 00-FIXED 01-INCR 10-WRAP
  		fetch_xactor.i_rd_addr.enq(read_request);
	  	ff_epoch.enq(epoch);
      //fetch_state<= Response;
      if(verbosity!=0)
        $display($time, "\tCORE: Fetch Request ", fshow(read_request));
    endrule

    rule handle_fetch_response;
	  	let response <- pop_o (fetch_xactor.o_rd_data);	
  		Bool bus_error = !(response.rresp==AXI4_OKAY);
      riscv.inst_response.put(tuple3(truncate(response.rdata), bus_error, ff_epoch.first));
	  ff_epoch.deq;
      //fetch_state<= Request; 
		  if(verbosity!=0)
        $display($time, "\tCORE: Fetch Response ", fshow(response));
    endrule
    
    // if its a fence instruction, the request is simply stored in memory_request register and is not
	// latched on to the bus. This is done because we are only concerned about access_type being 
	// propagated to mem_wb stage.
    rule handle_memory_request;
      let {address, data, access, size, sign}<- riscv.memory_request.get;
      ff_mem_request.enq(tuple4(address, access, size, sign));
	 if(access != Fencei) begin
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
        AXI4_Rd_Addr#(`paddr, 0) read_request = AXI4_Rd_Addr {araddr: address, aruser: 0, arlen: 0, 
            arsize: zeroExtend(size), arburst:'b01, arid:`Mem_master_num}; //arburst: 00-FIXED 01-INCR 10-WRAP
   	   		memory_xactor.i_rd_addr.enq(read_request);	
        if(verbosity!=0)
          $display($time, "\tCORE: Memory Read Request ", fshow(read_request));
      end
      else begin
			   AXI4_Wr_Addr#(`paddr, 0) aw = AXI4_Wr_Addr {awaddr: truncate(address), awuser:0, awlen: 0, 
            awsize: zeroExtend(size), awburst: 'b01, awid:`Mem_master_num}; //arburst: 00-FIXED 01-INCR 10-WRAP
  			let w  = AXI4_Wr_Data {wdata: data, wstrb: write_strobe, wlast:True, wid:`Mem_master_num};
        if(verbosity!=0)begin
          $display($time, "\tCORE: Memory write Request ", fshow(aw));
          $display($time, "\tCORE: Memory write Request ", fshow(w));
        end
	  		memory_xactor.i_wr_addr.enq(aw);
		  	memory_xactor.i_wr_data.enq(w);
      end
	 end
    endrule
    
	// Rule to handle memory response of Load and Atomic type instr 
    rule handle_memoryRead_response( tpl_2(ff_mem_request.first) == Load || tpl_2(ff_mem_request.first) ==Atomic);
      let {address, access, size, sign}=  ff_mem_request.first;
			let response <- pop_o (memory_xactor.o_rd_data);	
			let bus_error = !(response.rresp==AXI4_OKAY);
      let rdata=response.rdata;
      if(size==0)
          rdata=sign==1?signExtend(rdata[7:0]):zeroExtend(rdata[7:0]);
      else if(size==1)
          rdata=sign==1?signExtend(rdata[15:0]):zeroExtend(rdata[15:0]);
      else if(size==2)
          rdata=sign==1?signExtend(rdata[31:0]):zeroExtend(rdata[31:0]);
      // TODO shift, and perform signextension before sending to eclass.
      `ifdef atomic
        if(access==Atomic)
          riscv.atomic_response.put(tuple3(rdata, bus_error, access));
        else
      `endif
  			riscv.memory_response.put(tuple3(rdata, bus_error, access));
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Read Response ", fshow(response));
		ff_mem_request.deq;
	endrule


	// Rule to hande memory response of Store type instr
	rule handle_memoryWrite_response(tpl_2(ff_mem_request.first) == Store);
      let {address, access, size, sign}=  ff_mem_request.first;
			let response<-pop_o(memory_xactor.o_wr_resp);
			let bus_error = !(response.bresp==AXI4_OKAY);
			riscv.memory_response.put(tuple3(0, bus_error, access));
			ff_mem_request.deq;
      if(verbosity!=0)
        $display($time, "\tCORE: Memory Write Response ", fshow(response));
    endrule

    
	// rule to handle fence reponse.Contents of memory_request register are sent back.
	  rule handle_fence_response(tpl_2(ff_mem_request.first) == Fencei);
		  let {address, access, size, sign}=  ff_mem_request.first;
  		riscv.memory_response.put(tuple3(0, False, access)); // data is dont care, bus error is false.
		ff_mem_request.deq;
		  if(verbosity!=0)
			  $display($time, "\tCORE: Data memory serviced fence request");
  	endrule

    interface sb_clint_msip = interface Put
  	  method Action put(Bit#(1) intrpt);
        riscv.clint_msip(intrpt);
      endmethod
    endinterface;
    interface sb_clint_mtip= interface Put
      method Action put(Bit#(1) intrpt);
        riscv.clint_mtip(intrpt);
      endmethod
    endinterface;
    interface sb_clint_mtime= interface Put
  		method Action put (Bit#(64) c_mtime);
        riscv.clint_mtime(c_mtime);
      endmethod
    endinterface;
    interface sb_externalinterrupt = interface Put
      method Action put(Bit#(1) intrpt);
        riscv.externalinterrupt(intrpt);
      endmethod
    endinterface;
	interface master_i= fetch_xactor.axi_side;
	interface master_d= memory_xactor.axi_side;
  `ifdef rtldump
    interface io_dump=riscv.dump;
  `endif
  endmodule: mkeclass_axi4
  //=================== Interface and module for a eclass- master on the AXI4 fabric ============= //
//  interface Ifc_eclass_axi4lite;
//		interface AXI4_Lite_Master_IFC#(`paddr, XLEN, USERSPACE) master_i;
//		interface AXI4_Lite_Master_IFC#(`paddr, XLEN, USERSPACE) master_d;
//    interface Put#(Bit#(1)) sb_clint_msip;
//    interface Put#(Bit#(1)) sb_clint_mtip;
//    interface Put#(Bit#(64)) sb_clint_mtime;
//    interface Put#(Bit#(1)) sb_externalinterrupt;
//    `ifdef rtldump
//      interface Get#(DumpType) io_dump;
//    `endif
//  endinterface: Ifc_eclass_axi4lite
//
//  (*synthesize*)
//  module mkeclass_axi4lite(Ifc_eclass_axi4lite);
//    Ifc_riscv riscv <- mkriscv();
//		AXI4_Lite_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) fetch_xactor<- mkAXI4_Lite_Master_Xactor;
//		AXI4_Lite_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) memory_xactor<- mkAXI4_Lite_Master_Xactor;
//    Reg#(TxnState) fetch_state<- mkReg(Request);
//    Reg#(TxnState) memory_state<- mkReg(Request);
//    Reg#(CoreRequest) memory_request <- mkReg(unpack(0));
//
//    `ifdef compressed
//      FIFOF#(Bit#(32)) ff_inst <-mkFIFOF;
//      FIFOF#(Bit#(1))  ff_epoch <-mkFIFOF;
//    `endif
//
//    Integer verbosity = `VERBOSITY;
//
//
//    rule handle_fetch_request(fetch_state == Request) ;
//      let {inst_addr `ifdef compressed ,epoch `endif }<- riscv.inst_request.get;
//			AXI4_Lite_Rd_Addr#(`paddr, 0) read_request = AXI4_Lite_Rd_Addr {araddr:{inst_addr `ifdef compressed [31:2],2'b00 `endif }, aruser: ?, 
//          arsize: 2}; // arburst: 00-FIXED 01-INCR 10-WRAP
//			fetch_xactor.i_rd_addr.enq(read_request);	
//      `ifdef compressed
//	      ff_inst.enq(inst_addr);
//	      ff_epoch.enq(epoch);		
//      `endif
//      fetch_state<= Response;
//      if(verbosity!=0)
//        $display($time, "\tCORE: Fetch Request ", fshow(read_request));
//    endrule
//    rule handle_fetch_response(fetch_state == Response);
//			let response <- pop_o (fetch_xactor.o_rd_data);	
//			Bool bus_error = !(response.rresp==AXI4_LITE_OKAY);
//      `ifdef compressed
//          riscv.inst_response.put(tuple4(truncate(response.rdata), bus_error,ff_inst.first,ff_epoch.first));
//          ff_inst.deq;
//          ff_epoch.deq;
//       `else
//         riscv.inst_response.put(tuple2(truncate(response.rdata), bus_error));
//       `endif
//      fetch_state<= Request;
//      if(verbosity!=0)
//        $display($time, "\tCORE: Fetch Response ", fshow(response));
//    endrule
//    rule handle_memory_request(memory_state ==  Request);
//      let {address, data, access, size, sign}<- riscv.memory_request.get;
//      memory_request<= tuple4(address, access, size, sign);
//      if(size==0)
//        data=duplicate(data[7:0]);
//      else if(size==1)
//        data=duplicate(data[15:0]);
//      else if(size==2)
//        data=duplicate(data[31:0]);
//			Bit#(TDiv#(XLEN, 8)) write_strobe=size==0?'b1:size==1?'b11:size==2?'hf:'1;
//      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(address);
//			if(size!=3)begin			// 8-bit write;
//				write_strobe=write_strobe<<byte_offset;
//			end
//      if(access == Load) begin
//        AXI4_Lite_Rd_Addr#(`paddr, 0) read_request = AXI4_Lite_Rd_Addr {araddr: address, aruser:?, 
//            arsize: zeroExtend(size)}; //arburst: 00-FIXED 01-INCR 10-WRAP
//   	   	memory_xactor.i_rd_addr.enq(read_request);	
//        if(verbosity!=0)
//          $display($time, "\tCORE: Memory Read Request ", fshow(read_request));
//      end
//      else begin
//			   AXI4_Lite_Wr_Addr#(`paddr, 0) aw = AXI4_Lite_Wr_Addr {awaddr: truncate(address), awuser:?, 
//            awsize: zeroExtend(size)}; //arburst: 00-FIXED 01-INCR 10-WRAP
//  			let w  = AXI4_Lite_Wr_Data {wdata: data, wstrb: write_strobe};
//        if(verbosity!=0)begin
//          $display($time, "\tCORE: Memory write Request ", fshow(aw));
//          $display($time, "\tCORE: Memory write Request ", fshow(w));
//        end
//	  		memory_xactor.i_wr_addr.enq(aw);
//		  	memory_xactor.i_wr_data.enq(w);
//      end
//      memory_state<= Response;
//    endrule
//    rule handle_memoryRead_response(memory_state == Response && tpl_2(memory_request) == Load);
//      let {address, access, size, sign}=  memory_request;
//			let response <- pop_o (memory_xactor.o_rd_data);	
//			let bus_error = !(response.rresp==AXI4_LITE_OKAY);
//      let rdata=response.rdata;
//      if(size==0)
//          rdata=sign==1?signExtend(rdata[7:0]):zeroExtend(rdata[7:0]);
//      else if(size==1)
//          rdata=sign==1?signExtend(rdata[15:0]):zeroExtend(rdata[15:0]);
//      else if(size==2)
//          rdata=sign==1?signExtend(rdata[31:0]):zeroExtend(rdata[31:0]);
//      // TODO shift, and perform signextension before sending to eclass.
//			riscv.memory_response.put(tuple3(rdata, bus_error, access));
//      if(verbosity!=0)
//        $display($time, "\tCORE: Memory Read Response ", fshow(response));
//      memory_state<= Request;
//    endrule
//    rule handle_memoryWrite_response(memory_state == Response && tpl_2(memory_request) == Store);
//      let {address, access, size, sign}=  memory_request;
//			let response<-pop_o(memory_xactor.o_wr_resp);
//			let bus_error = !(response.bresp==AXI4_LITE_OKAY);
//			riscv.memory_response.put(tuple3(0, bus_error,  access));
//      if(verbosity!=0)
//        $display($time, "\tCORE: Memory Write Response ", fshow(response));
//      memory_state<= Request;
//    endrule
//    interface sb_clint_msip = interface Put
//  	  method Action put(Bit#(1) intrpt);
//        riscv.clint_msip(intrpt);
//      endmethod
//    endinterface;
//    interface sb_clint_mtip= interface Put
//      method Action put(Bit#(1) intrpt);
//        riscv.clint_mtip(intrpt);
//      endmethod
//    endinterface;
//    interface sb_clint_mtime= interface Put
//  		method Action put (Bit#(64) c_mtime);
//        riscv.clint_mtime(c_mtime);
//      endmethod
//    endinterface;
//    interface sb_externalinterrupt = interface Put
//      method Action put (Bit#(1) intrpt);
//        riscv.externalinterrupt(intrpt);
//      endmethod
//    endinterface;
//		interface master_i= fetch_xactor.axi_side;
//		interface master_d= memory_xactor.axi_side;
//    `ifdef rtldump
//      interface io_dump=riscv.dump;
//    `endif
//  endmodule: mkeclass_axi4lite
//
//  interface Ifc_eclass_TLU;
//		interface Ifc_fabric_side_master_link_lite#(`paddr, TDiv#(XLEN, 8), 2) fetch_master;
//		interface Ifc_fabric_side_master_link_lite#(`paddr, TDiv#(XLEN, 8), 2) mem_master;
//    interface Put#(Bit#(1)) sb_clint_msip;
//    interface Put#(Bit#(1)) sb_clint_mtip;
//    interface Put#(Bit#(64)) sb_clint_mtime;
//    interface Put#(Bit#(1)) sb_externalinterrupt;
//    `ifdef rtldump
//      interface Get#(DumpType) io_dump;
//    `endif
//  endinterface: Ifc_eclass_TLU
//  (*synthesize*)
//  module mkeclass_TLU(Ifc_eclass_TLU);
//    Ifc_Master_link_lite#(`paddr, TDiv#(XLEN, 8), 2)  fetch_xactor <- mkMasterXactorLite(True, True);
//    Ifc_Master_link_lite#(`paddr, TDiv#(XLEN, 8), 2)  dmem_xactor <- mkMasterXactorLite(True, True);
//    Ifc_riscv riscv <- mkriscv();
//    Reg#(TxnState) fetch_state<- mkReg(Request);
//    Reg#(Bit#(1)) memory_request <- mkReg(0);
//
//    `ifdef compressed
//    	FIFOF#(Bit#(32)) ff_inst <-mkFIFOF;
//    	FIFOF#(Bit#(1))  ff_epoch <-mkFIFOF;
//    `endif
//
//    Integer verbosity = `VERBOSITY;
//
//    rule handle_fetch_request(fetch_state == Request) ;
//      let {inst_addr `ifdef compressed ,epoch `endif }<- riscv.inst_request.get;
//      A_channel_lite#(`paddr, TDiv#(XLEN, 8), 2) lite_request = 
//            A_channel_lite { a_opcode : Get_data, a_size :2, a_source: `Fetch_master_num, a_address
//                      :{inst_addr `ifdef compressed [31:2],2'b00 `endif }, a_mask : ?, a_data : ?};
//	  	fetch_xactor.core_side.master_request.put(lite_request);	
//      `ifdef compressed
//      	ff_inst.enq(inst_addr);
//       	ff_epoch.enq(epoch);	
//      `endif
//      fetch_state<= Response;
//      if(verbosity!=0)
//        $display($time, "\tCORE: Fetch Request ", fshow(lite_request));
//    endrule
//    rule handle_fetch_response(fetch_state == Response);
//	    let response <- fetch_xactor.core_side.master_response.get;	
//      `ifdef compressed
//        riscv.inst_response.put(tuple4(truncate(response.d_data), response.d_error,ff_inst.first,ff_epoch.first));
//      	ff_inst.deq();
//      	ff_epoch.deq();
//      `else
//	      riscv.inst_response.put(tuple2(truncate(response.d_data), response.d_error));
//      `endif
//      fetch_state<= Request;
//      if(verbosity!=0)
//        $display($time, "\tCORE: Fetch Response ", fshow(response));
//    endrule
//    rule handle_memory_request;
//      let {address, data, access, size, sign}<- riscv.memory_request.get;
//      memory_request<= sign;
//      if(size==0)
//        data=duplicate(data[7:0]);
//      else if(size==1)
//        data=duplicate(data[15:0]);
//      else if(size==2)
//        data=duplicate(data[31:0]);
//			Bit#(TDiv#(XLEN, 8)) write_strobe=size==0?'b1:size==1?'b11:size==2?'hf:'1;
//      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(address);
//			if(size!=3)begin			// 8-bit write;
//				write_strobe=write_strobe<<byte_offset;
//			end
//      A_channel_lite#(`paddr, TDiv#(XLEN, 8), 2) lite_request= A_channel_lite{a_opcode:
//      unpack({1'b0,truncate(pack(access))}), 
//          a_size: size,  a_source: `Mem_master_num, a_address : address, a_mask : write_strobe, a_data: data};
//   	  dmem_xactor.core_side.master_request.put(lite_request);	
//    endrule
//    rule handle_memoryRead_response;
//      let sign=  memory_request;
//			let response <- dmem_xactor.core_side.master_response.get;
//      let rdata=response.d_data;
//      if(response.d_size==0)
//          rdata=sign==1?signExtend(rdata[7:0]):zeroExtend(rdata[7:0]);
//      else if(response.d_size==1)
//          rdata=sign==1?signExtend(rdata[15:0]):zeroExtend(rdata[15:0]);
//      else if(response.d_size==2)
//          rdata=sign==1?signExtend(rdata[31:0]):zeroExtend(rdata[31:0]);
//			let bus_error = (response.d_error);
//      if(response.d_opcode==AccessAck) // store operation
//			  riscv.memory_response.put(tuple3(0, bus_error, Load ));
//      else
//  			riscv.memory_response.put(tuple3(rdata, bus_error,  Store));
//      if(verbosity!=0)
//        $display($time, "\tCORE: Memory Read Response ", fshow(response));
//    endrule
//    interface fetch_master = fetch_xactor.fabric_side;
//    interface mem_master = dmem_xactor.fabric_side;
//    interface sb_clint_msip = interface Put
//  	  method Action put(Bit#(1) intrpt);
//        riscv.clint_msip(intrpt);
//      endmethod
//    endinterface;
//    interface sb_clint_mtip= interface Put
//      method Action put(Bit#(1) intrpt);
//        riscv.clint_mtip(intrpt);
//      endmethod
//    endinterface;
//    interface sb_clint_mtime= interface Put
//  		method Action put (Bit#(64) c_mtime);
//        riscv.clint_mtime(c_mtime);
//      endmethod
//    endinterface;
//    interface sb_externalinterrupt = interface Put
//      method Action put(Bit#(1) intrpt);
//        riscv.externalinterrupt(intrpt);
//      endmethod
//    endinterface;
//    `ifdef rtldump
//      interface io_dump=riscv.dump;
//    `endif
//  endmodule

endpackage
