/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
 * Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and / or other materials provided 
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

Author : Neel Gala
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
 */
package eclass;

  // library imports
  import FIFOF::*;
  import Connectable 				:: *;
  import GetPut:: *;
  import BUtils::*;

  // project imports
  import Semi_FIFOF:: *;
  import AXI4_Types:: *;
  import AXI4_Fabric:: *;
  import riscv:: * ;
  import common_types:: * ;
  `include "common_params.bsv"
  `include "Logger.bsv"

  export Ifc_eclass_axi4    (..);
  export mkeclass_axi4;
  export DumpType (..);

  typedef enum {Request, Response} TxnState deriving(Bits, Eq, FShow);

  interface Ifc_eclass_axi4;
    interface AXI4_Master_IFC#(`paddr, XLEN, USERSPACE) master_d;
    interface AXI4_Master_IFC#(`paddr, XLEN, USERSPACE) master_i;
    interface Put#(Bit#(1)) sb_clint_msip;
    interface Put#(Bit#(1)) sb_clint_mtip;
    interface Put#(Bit#(64)) sb_clint_mtime;
    interface Put#(Bit#(1)) sb_ext_interrupt;
    `ifdef rtldump
      interface Get#(DumpType) io_dump;
    `endif
  endinterface : Ifc_eclass_axi4

  (*synthesize*)
  module mkeclass_axi4#(parameter Bit#(`vaddr) resetpc) (Ifc_eclass_axi4);

    String eclass = ""; // for logger

    Ifc_riscv riscv <- mkriscv(resetpc);
    AXI4_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) fetch_xactor <- mkAXI4_Master_Xactor;
    AXI4_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) memory_xactor <- mkAXI4_Master_Xactor;

    // this fifo stores the epochs of instruction addresses latched onto fabric.
    // fifo size indicates no. of consecutive requests that can be made. The optimum size depends 
    // on internals of fabric.
    FIFOF#(Bit#(1)) ff_epoch <- mkSizedFIFOF(2);

    // fifo of size 1 effectively enables only one data request to be be latched & served at a time.
    FIFOF#(MemoryRequest) ff_mem_request <- mkFIFOF1; 

    rule handle_fetch_request;
      let req <- riscv.inst_request.get; 
      AXI4_Rd_Addr#(`paddr, 0) read_request = AXI4_Rd_Addr {araddr : truncate(req.addr), aruser: ?, 
                                              arlen : 0, arsize : 2, arburst : 'b01, arid : 0};
      fetch_xactor.i_rd_addr.enq(read_request);
      ff_epoch.enq(req.epoch);
      `logLevel( eclass, 0, $format("CORE : Fetch Request ", fshow(read_request)))
    endrule

    rule handle_fetch_response;
      let response <- pop_o (fetch_xactor.o_rd_data);	
      Bool bus_error = !(response.rresp == AXI4_OKAY);
      riscv.inst_response.put(InstResponse{inst : truncate(response.rdata), err : bus_error, 
                                            epoch : ff_epoch.first});
      ff_epoch.deq;
      `logLevel( eclass, 0, $format("CORE : Fetch Response ", fshow(response)))
    endrule
    
    // if its a fence instruction, the request is simply stored in memory_request register and is 
    // not latched on to the bus. This is done because we are only concerned about access_type 
    // being propagated to mem_wb stage.
    rule handle_memory_request;
      let req <- riscv.memory_request.get;
      ff_mem_request.enq(req);
      if(req.size[1:0] == 0)
        req.data = duplicate(req.data[7 : 0]);
      else if(req.size[1:0] == 1)
        req.data = duplicate(req.data[15 : 0]);
      else if(req.size[1:0] == 2)
        req.data = duplicate(req.data[31 : 0]);
      Bit#(TDiv#(XLEN, 8)) write_strobe = req.size == 0?'b1 : req.size == 1?'b11 : 
                                                              req.size == 2?'hf : '1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(req.addr);
      if(req.size != 3)begin			// 8 - bit write;
        write_strobe = write_strobe<<byte_offset;
      end
      if(req.memaccess != Store) begin
        AXI4_Rd_Addr#(`paddr, 0) read_request = AXI4_Rd_Addr {araddr : truncate(req.addr), 
              aruser : 0, arlen : 0, arsize : zeroExtend(req.size[1:0]), arburst : 'b01, arid : 0};
        memory_xactor.i_rd_addr.enq(read_request);	
        `logLevel( eclass, 0, $format("CORE : Memory Read Request ", fshow(read_request)))
      end
      else begin
        AXI4_Wr_Addr#(`paddr, 0) aw = AXI4_Wr_Addr {awaddr : truncate(req.addr), awuser : 0, 
              awlen : 0, awsize : zeroExtend(req.size[1:0]), awburst : 'b01, awid : 0}; 
        let w  = AXI4_Wr_Data {wdata : req.data, wstrb : write_strobe, wlast : True, wid : 0};
        memory_xactor.i_wr_addr.enq(aw);
        memory_xactor.i_wr_data.enq(w);
        `logLevel( eclass, 0 , $format("CORE : Memory write Request ", fshow(aw)))
        `logLevel( eclass, 0 , $format("CORE : Memory write Request ", fshow(w)))
      end
    endrule
    
  // Rule to handle memory response of Load and Atomic type instr 
    rule handle_memoryRead_response( ff_mem_request.first.memaccess == Load );
      let req =  ff_mem_request.first;
      let response <- pop_o (memory_xactor.o_rd_data);	
      let bus_error = !(response.rresp == AXI4_OKAY);
      let rdata = response.rdata;

      if(req.size[1:0] == 0)
          rdata = req.size[2] == 0?signExtend(rdata[7 : 0]) : zeroExtend(rdata[7 : 0]);
      else if(req.size[1:0] == 1)
          rdata = req.size[2] == 0?signExtend(rdata[15 : 0]) : zeroExtend(rdata[15 : 0]);
      else if(req.size[1:0] == 2)
          rdata = req.size[2] == 0?signExtend(rdata[31 : 0]) : zeroExtend(rdata[31 : 0]);
      // TODO send address in case of error
      riscv.memory_response.put(MemoryResponse{data: rdata, err: bus_error});
      `logLevel( eclass, 0, $format("CORE : Memory Read Response ", fshow(response)))
      ff_mem_request.deq;
    endrule


    // Rule to hande memory response of Store type instr
    rule handle_memoryWrite_response( ff_mem_request.first.memaccess == Store );
      let req =  ff_mem_request.first;
      let response <- pop_o(memory_xactor.o_wr_resp);
      let bus_error = !(response.bresp == AXI4_OKAY);
      riscv.memory_response.put(MemoryResponse{data:0, err: bus_error});
      ff_mem_request.deq;
      `logLevel( eclass, 0, $format("CORE : Memory Write Response ", fshow(response)))
    endrule

    interface sb_clint_msip = interface Put
      method Action put(Bit#(1) intrpt);
        riscv.clint_msip(intrpt);
      endmethod
    endinterface;
    interface sb_clint_mtip = interface Put
      method Action put(Bit#(1) intrpt);
        riscv.clint_mtip(intrpt);
      endmethod
    endinterface;
    interface sb_clint_mtime = interface Put
      method Action put (Bit#(64) c_mtime);
        riscv.clint_mtime(c_mtime);
      endmethod
    endinterface;
    interface sb_ext_interrupt = interface Put
      method Action put(Bit#(1) intrpt);
        riscv.ext_interrupt(intrpt);
      endmethod
    endinterface;
  interface master_i = fetch_xactor.axi_side;
  interface master_d = memory_xactor.axi_side;
  `ifdef rtldump
    interface io_dump = riscv.dump;
  `endif
  endmodule : mkeclass_axi4
endpackage
