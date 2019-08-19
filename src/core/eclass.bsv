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
  import Vector :: *;

  // project imports
  import Semi_FIFOF:: *;
  import AXI4_Types:: *;
  import AXI4_Fabric:: *;
  import AXI4_Lite_Types:: *;
  import AXI4_Lite_Fabric:: *;
  import riscv:: * ;
  import common_types:: * ;
  `include "common_params.bsv"
  `include "Logger.bsv"
`ifdef debug
  import debug_types::*;
`endif

`ifdef pmp
  import pmp_func :: *;
`endif

  export Ifc_eclass_axi4    (..);
  export mkeclass_axi4;
  export Ifc_eclass_axi4lite    (..);
  export mkeclass_axi4lite;
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
  `ifdef debug
    interface Hart_Debug_Ifc debug_server;
  `endif
`ifdef perfmonitors
  `ifdef simulate
    method Tuple2#(Vector#(`counters, Bit#(XLEN)), Vector#(`counters, Bit#(64))) counter_values;
  `endif
`endif
  endinterface : Ifc_eclass_axi4

`ifdef atomic
  function Bit#(XLEN) fn_atomic_op (Bit#(5) op,  Bit#(XLEN) rs2, Bit#(XLEN) loaded);
      Bit#(XLEN) op1 = loaded;
      Bit#(XLEN) op2 = rs2;
      if(op[4] == 0)begin
        op1 = signExtend(loaded[31 : 0]);
        op2 = signExtend(rs2[31 : 0]);
      end
      Int#(XLEN) s_op1 = unpack(op1);
      Int#(XLEN) s_op2 = unpack(op2);
      
      case (op[3 : 0])
          'b0011 : return op2;
          'b0000 : return (op1 + op2);
          'b0010 : return (op1 ^ op2);
          'b0110 : return (op1 & op2);
          'b0100 : return (op1 | op2);
          'b1100 : return min(op1, op2);
          'b1110 : return max(op1, op2);
          'b1000 : return pack(min(s_op1, s_op2));
          'b1010 : return pack(max(s_op1, s_op2));
          default : return op1;
        endcase
    endfunction
`endif

  module mkeclass_axi4#(Bit#(`vaddr) resetpc) (Ifc_eclass_axi4);

    String eclass = ""; // for logger


    Ifc_riscv riscv <- mkriscv(resetpc);
    AXI4_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) fetch_xactor <- mkAXI4_Master_Xactor;
    AXI4_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) memory_xactor <- mkAXI4_Master_Xactor;

    Reg#(Bit#(1)) rg_wEpoch[2] <- mkCReg(2,0);

    let curr_priv = riscv.mv_curr_priv;

    // this fifo stores the epochs of instruction addresses latched onto fabric.
    // fifo size indicates no. of consecutive requests that can be made. The optimum size depends 
    // on internals of fabric.
    FIFOF#(InstRequest) ff_inst_request <- mkSizedFIFOF(2);
    FIFOF#(Bool) ff_inst_access_fault <- mkSizedFIFOF(2);

    // fifo of size 1 effectively enables only one data request to be be latched & served at a time.
    FIFOF#(MemoryRequest) ff_mem_request <- mkSizedFIFOF(2);
    FIFOF#(Bool) ff_mem_access_fault <- mkSizedFIFOF(2);
  `ifdef atomic
    FIFOF#(Bit#(XLEN)) ff_atomic_state <- mkFIFOF1();
  `endif
  
  `ifdef debug
    // registered container for responses
    Reg#(Maybe#(Bit#(DXLEN))) rg_abst_response <- mkReg(tagged Invalid); 
  `endif

    rule update_epochs(riscv.mv_trap);
      rg_wEpoch[0] <= ~rg_wEpoch[0];
    endrule

    rule handle_fetch_request;
      let req <- riscv.inst_request.get; 
      Bool err = False;
      if(`paddr < `vaddr) begin
        Bit#(TSub#(`vaddr, `paddr )) upper_bits = truncateLSB(req.addr);
        err = (|upper_bits == 1);
      end
    `ifdef pmp
      let pmpreq = PMPReq{ address: truncate(req.addr), num_bytes: 4, access_type:2};
      let {pmp_error, pmp_cause} = fn_pmp_lookup(pmpreq, unpack(riscv.mv_curr_priv), 
                                                riscv.mv_pmp_cfg, riscv.mv_pmp_addr);
      if(!err)
        err = pmp_error;
    `endif
      if (!err) begin
        AXI4_Rd_Addr#(`paddr, 0) read_request = AXI4_Rd_Addr {araddr : truncate(req.addr), 
                                                aruser: ?, arlen : 0, arsize : 2, arburst : 'b01, 
                                                arid : 0, arprot: {1'b0, 1'b0, curr_priv[1]}};
        fetch_xactor.i_rd_addr.enq(read_request);
        `logLevel( eclass, 0, $format("CORE : Fetch Request ", fshow(read_request)))
      end
      else begin
        `logLevel( eclass, 0, $format("CORE : Fetch Request is Faulty: ", fshow(req)))
      end
      ff_inst_access_fault.enq(err);
      ff_inst_request.enq(req);
    endrule

    rule handle_fetch_response(!ff_inst_access_fault.first);
      let response <- pop_o (fetch_xactor.o_rd_data);	
			
      Bit#(TLog#(TDiv#(XLEN, 8))) lower_addr_bits = truncate(ff_inst_request.first.addr); 
      Bit#(TAdd#(TLog#(TDiv#(XLEN, 8)), 3)) lv_shift = {lower_addr_bits, 3'd0};
      let lv_data = response.rdata >> lv_shift;
      Bool bus_error = !(response.rresp == AXI4_OKAY);
      if (bus_error)
        lv_data = zeroExtend(ff_inst_request.first.addr);
      riscv.inst_response.put(InstResponse{inst : truncate(lv_data), err : bus_error, 
                                            epoch : ff_inst_request.first.epoch});
      ff_inst_request.deq;
      ff_inst_access_fault.deq;
      `logLevel( eclass, 0, $format("CORE : Fetch Response ", fshow(response)))
    endrule

    rule handle_inst_access_fault(ff_inst_access_fault.first);
      riscv.inst_response.put(InstResponse{inst : truncate(ff_inst_request.first.addr), err : True, 
                                            epoch : ff_inst_request.first.epoch});
      ff_inst_request.deq;
      ff_inst_access_fault.deq;
      `logLevel( eclass, 0, $format("CORE : Fetch Access Fault "))
    endrule
    
    // if its a fence instruction, the request is simply stored in memory_request register and is 
    // not latched on to the bus. This is done because we are only concerned about access_type 
    // being propagated to mem_wb stage.
    rule handle_memory_request;
      let req <- riscv.memory_request.get;
      if(req.size[1 : 0] == 0)
        req.data = duplicate(req.data[7 : 0]);
      else if(req.size[1 : 0] == 1)
        req.data = duplicate(req.data[15 : 0]);
      else if(req.size[1 : 0] == 2)
        req.data = duplicate(req.data[31 : 0]);
      Bit#(TDiv#(XLEN, 8)) write_strobe = req.size == 0?'b1 : req.size == 1?'b11 : 
                                                              req.size == 2?'hf : '1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(req.addr);
      if(req.size != 3)begin			// 8 - bit write;
        write_strobe = write_strobe<<byte_offset;
      end
    `ifdef pmp
      let pmpreq = PMPReq{ address: truncate(req.addr), num_bytes: zeroExtend(req.size[1:0]), 
                          access_type:req.memaccess == Load?0:1};
      let {pmp_error, pmp_cause} = fn_pmp_lookup(pmpreq, unpack(riscv.mv_curr_priv), 
                                                riscv.mv_pmp_cfg, riscv.mv_pmp_addr);
      if(pmp_error)
        req.addr = 0;
    `endif
      if (rg_wEpoch[1] == req.epoch) begin
        if(req.memaccess != Store) begin
          `ifdef perfmonitors
            riscv.ma_event_loads(1);
          `endif
          AXI4_Rd_Addr#(`paddr, 0) read_request = AXI4_Rd_Addr {araddr : truncate(req.addr), 
                aruser : 0, arlen : 0, arsize : zeroExtend(req.size[1 : 0]), arburst : 'b01, 
                arid : 0, arprot: {1'b0, 1'b0,1'b1}};
          memory_xactor.i_rd_addr.enq(read_request);	
          `logLevel( eclass, 0, $format("CORE : Memory Read Request ", fshow(read_request)))
        end
        else begin
          `ifdef perfmonitors
            riscv.ma_event_stores(1);
          `endif
          AXI4_Wr_Addr#(`paddr, 0) aw = AXI4_Wr_Addr {awaddr : truncate(req.addr), awuser : 0, 
                awlen : 0, awsize : zeroExtend(req.size[1 : 0]), awburst : 'b01, awid : 0,
                awprot: {1'b0, 1'b0,1'b1} };
          let w  = AXI4_Wr_Data {wdata : req.data, wstrb : write_strobe, wlast : True, wid : 0};
          memory_xactor.i_wr_addr.enq(aw);
          memory_xactor.i_wr_data.enq(w);
          `logLevel( eclass, 0 , $format("CORE : Memory write Request ", fshow(aw)))
          `logLevel( eclass, 0 , $format("CORE : Memory write Request ", fshow(w)))
        end 
        ff_mem_request.enq(req);
      end
    endrule
    
  // Rule to handle memory response of Load and Atomic type instr 
    rule handle_memoryRead_response( ff_mem_request.first.memaccess == Load );
      let req =  ff_mem_request.first;
      let response <- pop_o (memory_xactor.o_rd_data);	
      let bus_error = !(response.rresp == AXI4_OKAY);
      Bit#(TLog#(TDiv#(XLEN, 8))) lower_addr_bits = truncate(ff_mem_request.first.addr); 
      Bit#(TAdd#(TLog#(TDiv#(XLEN, 8)), 3)) lv_shift = {lower_addr_bits, 3'd0};
      let rdata = response.rdata >> lv_shift;

      if(req.size[1 : 0] == 0)
          rdata = req.size[2] == 0?signExtend(rdata[7 : 0]) : zeroExtend(rdata[7 : 0]);
      else if(req.size[1 : 0] == 1)
          rdata = req.size[2] == 0?signExtend(rdata[15 : 0]) : zeroExtend(rdata[15 : 0]);
      else if(req.size[1 : 0] == 2)
          rdata = req.size[2] == 0?signExtend(rdata[31 : 0]) : zeroExtend(rdata[31 : 0]);
      if(bus_error)
        rdata = zeroExtend(ff_mem_request.first.addr);
      riscv.memory_response.put(MemoryResponse{data : rdata, err : bus_error, epoch : req.epoch});
      `logLevel( eclass, 0, $format("CORE : Memory Read Response ", fshow(response)))
      ff_mem_request.deq;
    endrule


    // Rule to hande memory response of Store type instr
    rule handle_memoryWrite_response( ff_mem_request.first.memaccess == Store );
      let req =  ff_mem_request.first;
      let response <- pop_o(memory_xactor.o_wr_resp);
      let bus_error = !(response.bresp == AXI4_OKAY);
      Bit#(XLEN) data = 0;
      if(bus_error)
        data = zeroExtend(ff_mem_request.first.addr);
      riscv.memory_response.put(MemoryResponse{data : data, err : bus_error, epoch : req.epoch});
      ff_mem_request.deq;
      `logLevel( eclass, 0, $format("CORE : Memory Write Response ", fshow(response)))
    endrule
  `ifdef atomic
    rule handle_atomic_readresponse( ff_mem_request.first.memaccess == Atomic && 
                                    !ff_atomic_state.notEmpty );
      let req =  ff_mem_request.first;
      let response <- pop_o (memory_xactor.o_rd_data);	
      let bus_error = !(response.rresp == AXI4_OKAY);
      Bit#(TLog#(TDiv#(XLEN, 8))) lower_addr_bits = truncate(ff_mem_request.first.addr); 
      Bit#(TAdd#(TLog#(TDiv#(XLEN, 8)), 3)) lv_shift = {lower_addr_bits, 3'd0};
      let rdata = response.rdata >> lv_shift;

      if(req.size[1 : 0] == 2)
          rdata = req.size[2] == 0?signExtend(rdata[31 : 0]) : zeroExtend(rdata[31 : 0]);
      Bit#(TDiv#(XLEN, 8)) write_strobe = req.size == 2?'hf : '1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(req.addr);
      `logLevel( eclass, 0, $format("CORE : Atomic Read Response ", fshow(response)))
      if(bus_error) begin
        rdata = zeroExtend(ff_mem_request.first.addr);
        riscv.memory_response.put(MemoryResponse{data : rdata, err : bus_error, epoch : req.epoch});
        ff_mem_request.deq;
      end
      else begin
        ff_atomic_state.enq(rdata);
        Bit#(XLEN) wdata = fn_atomic_op(req.atomic_op, req.data, rdata);
        if(req.size != 3)begin			// 8 - bit write;
          write_strobe = write_strobe<<byte_offset;
          wdata = duplicate(wdata[31:0]);
        end
        AXI4_Wr_Addr#(`paddr, 0) aw = AXI4_Wr_Addr {awaddr : truncate(req.addr), awuser : 0, 
              awlen : 0, awsize : zeroExtend(req.size[1 : 0]), awburst : 'b01, awid : 0,
              awprot: {1'b0, 1'b0, curr_priv[1]} };
        let w  = AXI4_Wr_Data {wdata : wdata, wstrb : write_strobe, wlast : True, wid : 0};
        memory_xactor.i_wr_addr.enq(aw);
        memory_xactor.i_wr_data.enq(w);
      end
    endrule

    rule handle_atomic_writeresponse( ff_mem_request.first.memaccess == Atomic);
      let req =  ff_mem_request.first;
      let response <- pop_o(memory_xactor.o_wr_resp);
      let bus_error = !(response.bresp == AXI4_OKAY);
      Bit#(XLEN) data = ff_atomic_state.first;
      if(bus_error)
        data = zeroExtend(ff_mem_request.first.addr);
      riscv.memory_response.put(MemoryResponse{data : data, err : bus_error, epoch : req.epoch});
      ff_mem_request.deq;
      ff_atomic_state.deq;
      `logLevel( eclass, 0, $format("CORE : Atomic Write Response ", fshow(response)))
    endrule
  `endif
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
  `ifdef debug
    interface debug_server = interface Hart_Debug_Ifc
      method Action   abstractOperation(AbstractRegOp cmd)if (!(isValid(rg_abst_response)));
        if(cmd.address < zeroExtend(14'h1000))begin // Explot address bits to optimize this filter
          let lv_resp <- riscv.mav_debug_access_csrs(cmd);
          rg_abst_response <= tagged Valid zeroExtend(lv_resp);
        end
        else if(cmd.address < 'h1020 )begin
          let lv_resp <- riscv.mav_debug_access_gprs(cmd);
          rg_abst_response <= tagged Valid zeroExtend(lv_resp);
        end
        else begin
          rg_abst_response <= tagged Valid zeroExtend(32'h00000000);
        end
      endmethod

      method ActionValue#(Bit#(DXLEN)) abstractReadResponse if (isValid(rg_abst_response));
        rg_abst_response <= tagged Invalid;
        return validValue(rg_abst_response);
      endmethod

      method haltRequest    = riscv.ma_debug_halt_request;
      method resumeRequest  = riscv.ma_debug_resume_request;
      method dm_active      = riscv.ma_debugger_available;
      method is_halted      = riscv.mv_core_is_halted();
      method is_unavailable = ~riscv.mv_core_debugenable;
      method Action hartReset(Bit#(1) hart_reset_v);
        noAction;
      endmethod
      method Bit#(1) has_reset;
        return 1;
      endmethod
    endinterface;
  `endif
`ifdef perfmonitors
  `ifdef simulate
    method counter_values = riscv.counter_values;
  `endif
`endif
  endmodule : mkeclass_axi4
  
  interface Ifc_eclass_axi4lite;
    interface AXI4_Lite_Master_IFC#(`paddr, XLEN, USERSPACE) master_d;
    interface AXI4_Lite_Master_IFC#(`paddr, XLEN, USERSPACE) master_i;
    interface Put#(Bit#(1)) sb_clint_msip;
    interface Put#(Bit#(1)) sb_clint_mtip;
    interface Put#(Bit#(64)) sb_clint_mtime;
    interface Put#(Bit#(1)) sb_ext_interrupt;
  `ifdef rtldump
    interface Get#(DumpType) io_dump;
  `endif
  `ifdef debug
    interface Hart_Debug_Ifc debug_server;
  `endif
`ifdef perfmonitors
  `ifdef simulate
    method Tuple2#(Vector#(`counters, Bit#(XLEN)), Vector#(`counters, Bit#(64))) counter_values;
  `endif
`endif
  endinterface : Ifc_eclass_axi4lite
  
  (*synthesize*)
  module mkeclass_axi4lite#(Bit#(`vaddr) resetpc) (Ifc_eclass_axi4lite);

    String eclass = ""; // for logger


    Ifc_riscv riscv <- mkriscv(resetpc);
    AXI4_Lite_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) fetch_xactor <- 
                                                                        mkAXI4_Lite_Master_Xactor;
    AXI4_Lite_Master_Xactor_IFC #(`paddr, XLEN, USERSPACE) memory_xactor <- 
                                                                        mkAXI4_Lite_Master_Xactor;

    Reg#(Bit#(1)) rg_wEpoch[2] <- mkCReg(2,0);

    let curr_priv = riscv.mv_curr_priv;

    // this fifo stores the epochs of instruction addresses latched onto fabric.
    // fifo size indicates no. of consecutive requests that can be made. The optimum size depends 
    // on internals of fabric.
    FIFOF#(InstRequest) ff_inst_request <- mkSizedFIFOF(2);
    FIFOF#(Bool) ff_inst_access_fault <- mkSizedFIFOF(2);

    // fifo of size 1 effectively enables only one data request to be be latched & served at a time.
    FIFOF#(MemoryRequest) ff_mem_request <- mkSizedFIFOF(2);
    FIFOF#(Bool) ff_mem_access_fault <- mkSizedFIFOF(2);
  `ifdef atomic
    FIFOF#(Bit#(XLEN)) ff_atomic_state <- mkFIFOF1();
  `endif
  
  `ifdef debug
    // registered container for responses
    Reg#(Maybe#(Bit#(DXLEN))) rg_abst_response <- mkReg(tagged Invalid); 
  `endif

    rule update_epochs(riscv.mv_trap);
      rg_wEpoch[0] <= ~rg_wEpoch[0];
    endrule

    rule handle_fetch_request;
      let req <- riscv.inst_request.get; 
      Bool err = False;
      if(`paddr < `vaddr) begin
        Bit#(TSub#(`vaddr, `paddr )) upper_bits = truncateLSB(req.addr);
        err = (|upper_bits == 1);
      end
    `ifdef pmp
      let pmpreq = PMPReq{ address: truncate(req.addr), num_bytes: 4, access_type:2};
      let {pmp_error, pmp_cause} = fn_pmp_lookup(pmpreq, unpack(riscv.mv_curr_priv), 
                                                riscv.mv_pmp_cfg, riscv.mv_pmp_addr);
      if(!err)
        err = pmp_error;
    `endif
      if (!err) begin
        AXI4_Lite_Rd_Addr#(`paddr, 0) read_request = AXI4_Lite_Rd_Addr {araddr : truncate(req.addr),
                                                aruser: ?, arsize : 2, 
                                                arprot: {1'b0, 1'b0, curr_priv[1]}};
        fetch_xactor.i_rd_addr.enq(read_request);
        `logLevel( eclass, 0, $format("CORE : Fetch Request ", fshow(read_request)))
      end
      else begin
        `logLevel( eclass, 0, $format("CORE : Fetch Request is Faulty: ", fshow(req)))
      end
      ff_inst_access_fault.enq(err);
      ff_inst_request.enq(req);
    endrule

    rule handle_fetch_response(!ff_inst_access_fault.first);
      let response <- pop_o (fetch_xactor.o_rd_data);	
			
      Bit#(TLog#(TDiv#(XLEN, 8))) lower_addr_bits = truncate(ff_inst_request.first.addr); 
      Bit#(TAdd#(TLog#(TDiv#(XLEN, 8)), 3)) lv_shift = {lower_addr_bits, 3'd0};
      let lv_data = response.rdata >> lv_shift;
      Bool bus_error = !(response.rresp == AXI4_LITE_OKAY);
      if (bus_error)
        lv_data = zeroExtend(ff_inst_request.first.addr);
      riscv.inst_response.put(InstResponse{inst : truncate(lv_data), err : bus_error, 
                                            epoch : ff_inst_request.first.epoch});
      ff_inst_request.deq;
      ff_inst_access_fault.deq;
      `logLevel( eclass, 0, $format("CORE : Fetch Response ", fshow(response)))
    endrule

    rule handle_inst_access_fault(ff_inst_access_fault.first);
      riscv.inst_response.put(InstResponse{inst : truncate(ff_inst_request.first.addr), err : True, 
                                            epoch : ff_inst_request.first.epoch});
      ff_inst_request.deq;
      ff_inst_access_fault.deq;
      `logLevel( eclass, 0, $format("CORE : Fetch Access Fault "))
    endrule
    
    // if its a fence instruction, the request is simply stored in memory_request register and is 
    // not latched on to the bus. This is done because we are only concerned about access_type 
    // being propagated to mem_wb stage.
    rule handle_memory_request;
      let req <- riscv.memory_request.get;
      if(req.size[1 : 0] == 0)
        req.data = duplicate(req.data[7 : 0]);
      else if(req.size[1 : 0] == 1)
        req.data = duplicate(req.data[15 : 0]);
      else if(req.size[1 : 0] == 2)
        req.data = duplicate(req.data[31 : 0]);
      Bit#(TDiv#(XLEN, 8)) write_strobe = req.size == 0?'b1 : req.size == 1?'b11 : 
                                                              req.size == 2?'hf : '1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(req.addr);
      if(req.size != 3)begin			// 8 - bit write;
        write_strobe = write_strobe<<byte_offset;
      end
    `ifdef pmp
      let pmpreq = PMPReq{ address: truncate(req.addr), num_bytes: zeroExtend(req.size[1:0]), 
                          access_type:req.memaccess == Load?0:1};
      let {pmp_error, pmp_cause} = fn_pmp_lookup(pmpreq, unpack(riscv.mv_curr_priv), 
                                                riscv.mv_pmp_cfg, riscv.mv_pmp_addr);
      if(pmp_error)
        req.addr = 0;
    `endif
      if (rg_wEpoch[1] == req.epoch) begin
        if(req.memaccess != Store) begin
          `ifdef perfmonitors
            riscv.ma_event_loads(1);
          `endif
          AXI4_Lite_Rd_Addr#(`paddr, 0) read_request = AXI4_Lite_Rd_Addr {
                araddr : truncate(req.addr), aruser : 0, arsize : zeroExtend(req.size[1 : 0]), 
                arprot: {1'b0, 1'b0,1'b1}};
          memory_xactor.i_rd_addr.enq(read_request);	
          `logLevel( eclass, 0, $format("CORE : Memory Read Request ", fshow(read_request)))
        end
        else begin
          `ifdef perfmonitors
            riscv.ma_event_stores(1);
          `endif
          AXI4_Lite_Wr_Addr#(`paddr, 0) aw = AXI4_Lite_Wr_Addr {awaddr : truncate(req.addr), 
                awuser : 0, awsize : zeroExtend(req.size[1 : 0]), 
                awprot: {1'b0, 1'b0,1'b1} }; // TODO
          let w  = AXI4_Lite_Wr_Data {wdata : req.data, wstrb : write_strobe};
          memory_xactor.i_wr_addr.enq(aw);
          memory_xactor.i_wr_data.enq(w);
          `logLevel( eclass, 0 , $format("CORE : Memory write Request ", fshow(aw)))
          `logLevel( eclass, 0 , $format("CORE : Memory write Request ", fshow(w)))
        end 
        ff_mem_request.enq(req);
      end
    endrule
    
  // Rule to handle memory response of Load and Atomic type instr 
    rule handle_memoryRead_response( ff_mem_request.first.memaccess == Load );
      let req =  ff_mem_request.first;
      let response <- pop_o (memory_xactor.o_rd_data);	
      let bus_error = !(response.rresp == AXI4_LITE_OKAY);
      Bit#(TLog#(TDiv#(XLEN, 8))) lower_addr_bits = truncate(ff_mem_request.first.addr); 
      Bit#(TAdd#(TLog#(TDiv#(XLEN, 8)), 3)) lv_shift = {lower_addr_bits, 3'd0};
      let rdata = response.rdata >> lv_shift;

      if(req.size[1 : 0] == 0)
          rdata = req.size[2] == 0?signExtend(rdata[7 : 0]) : zeroExtend(rdata[7 : 0]);
      else if(req.size[1 : 0] == 1)
          rdata = req.size[2] == 0?signExtend(rdata[15 : 0]) : zeroExtend(rdata[15 : 0]);
      else if(req.size[1 : 0] == 2)
          rdata = req.size[2] == 0?signExtend(rdata[31 : 0]) : zeroExtend(rdata[31 : 0]);
      if(bus_error)
        rdata = zeroExtend(ff_mem_request.first.addr);
      riscv.memory_response.put(MemoryResponse{data : rdata, err : bus_error, epoch : req.epoch});
      `logLevel( eclass, 0, $format("CORE : Memory Read Response ", fshow(response)))
      ff_mem_request.deq;
    endrule


    // Rule to hande memory response of Store type instr
    rule handle_memoryWrite_response( ff_mem_request.first.memaccess == Store );
      let req =  ff_mem_request.first;
      let response <- pop_o(memory_xactor.o_wr_resp);
      let bus_error = !(response.bresp == AXI4_LITE_OKAY);
      Bit#(XLEN) data = 0;
      if(bus_error)
        data = zeroExtend(ff_mem_request.first.addr);
      riscv.memory_response.put(MemoryResponse{data : data, err : bus_error, epoch : req.epoch});
      ff_mem_request.deq;
      `logLevel( eclass, 0, $format("CORE : Memory Write Response ", fshow(response)))
    endrule
  `ifdef atomic
    rule handle_atomic_readresponse( ff_mem_request.first.memaccess == Atomic && 
                                    !ff_atomic_state.notEmpty );
      let req =  ff_mem_request.first;
      let response <- pop_o (memory_xactor.o_rd_data);	
      let bus_error = !(response.rresp == AXI4_LITE_OKAY);
      Bit#(TLog#(TDiv#(XLEN, 8))) lower_addr_bits = truncate(ff_mem_request.first.addr); 
      Bit#(TAdd#(TLog#(TDiv#(XLEN, 8)), 3)) lv_shift = {lower_addr_bits, 3'd0};
      let rdata = response.rdata >> lv_shift;

      if(req.size[1 : 0] == 2)
          rdata = req.size[2] == 0?signExtend(rdata[31 : 0]) : zeroExtend(rdata[31 : 0]);
      Bit#(TDiv#(XLEN, 8)) write_strobe = req.size == 2?'hf : '1;
      Bit#(TAdd#(1, TDiv#(XLEN, 32))) byte_offset = truncate(req.addr);
      `logLevel( eclass, 0, $format("CORE : Atomic Read Response ", fshow(response)))
      if(bus_error) begin
        rdata = zeroExtend(ff_mem_request.first.addr);
        riscv.memory_response.put(MemoryResponse{data : rdata, err : bus_error, epoch : req.epoch});
        ff_mem_request.deq;
      end
      else begin
        ff_atomic_state.enq(rdata);
        Bit#(XLEN) wdata = fn_atomic_op(req.atomic_op, req.data, rdata);
        if(req.size != 3)begin			// 8 - bit write;
          write_strobe = write_strobe<<byte_offset;
          wdata = duplicate(wdata[31:0]);
        end
        AXI4_Lite_Wr_Addr#(`paddr, 0) aw = AXI4_Lite_Wr_Addr {awaddr : truncate(req.addr), 
              awuser : 0, awsize : zeroExtend(req.size[1 : 0]), 
              awprot: {1'b0, 1'b0, curr_priv[1]} };
        let w  = AXI4_Lite_Wr_Data {wdata : wdata, wstrb : write_strobe};
        memory_xactor.i_wr_addr.enq(aw);
        memory_xactor.i_wr_data.enq(w);
      end
    endrule

    rule handle_atomic_writeresponse( ff_mem_request.first.memaccess == Atomic);
      let req =  ff_mem_request.first;
      let response <- pop_o(memory_xactor.o_wr_resp);
      let bus_error = !(response.bresp == AXI4_LITE_OKAY);
      Bit#(XLEN) data = ff_atomic_state.first;
      if(bus_error)
        data = zeroExtend(ff_mem_request.first.addr);
      riscv.memory_response.put(MemoryResponse{data : data, err : bus_error, epoch : req.epoch});
      ff_mem_request.deq;
      ff_atomic_state.deq;
      `logLevel( eclass, 0, $format("CORE : Atomic Write Response ", fshow(response)))
    endrule
  `endif
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
  `ifdef debug
    interface debug_server = interface Hart_Debug_Ifc
      method Action   abstractOperation(AbstractRegOp cmd)if (!(isValid(rg_abst_response)));
        if(cmd.address < zeroExtend(14'h1000))begin // Explot address bits to optimize this filter
          let lv_resp <- riscv.mav_debug_access_csrs(cmd);
          rg_abst_response <= tagged Valid zeroExtend(lv_resp);
        end
        else if(cmd.address < 'h1020 )begin
          let lv_resp <- riscv.mav_debug_access_gprs(cmd);
          rg_abst_response <= tagged Valid zeroExtend(lv_resp);
        end
        else begin
          rg_abst_response <= tagged Valid zeroExtend(32'h00000000);
        end
      endmethod

      method ActionValue#(Bit#(DXLEN)) abstractReadResponse if (isValid(rg_abst_response));
        rg_abst_response <= tagged Invalid;
        return validValue(rg_abst_response);
      endmethod

      method haltRequest    = riscv.ma_debug_halt_request;
      method resumeRequest  = riscv.ma_debug_resume_request;
      method dm_active      = riscv.ma_debugger_available;
      method is_halted      = riscv.mv_core_is_halted();
      method is_unavailable = ~riscv.mv_core_debugenable;
      method Action hartReset(Bit#(1) hart_reset_v);
        noAction;
      endmethod
      method Bit#(1) has_reset;
        return 1;
      endmethod
    endinterface;
  `endif
`ifdef perfmonitors
  `ifdef simulate
    method counter_values = riscv.counter_values;
  `endif
`endif
  endmodule : mkeclass_axi4lite
endpackage
