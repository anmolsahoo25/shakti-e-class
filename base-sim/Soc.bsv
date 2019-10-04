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
package Soc;
  // project related imports
  import Semi_FIFOF:: *;
`ifdef CORE_AXI4
  import AXI4_Types:: *;
  import AXI4_Fabric:: *;
`elsif CORE_AXI4Lite
  import AXI4_Lite_Types:: *;
  import AXI4_Lite_Fabric:: *;
`endif
  import common_types:: * ;
  import Clocks :: *;
  `include "common_params.bsv"
  `include "Soc.defines"

  // peripheral imports
  import eclass:: * ;
  import uart::*;
  import clint::*;
  import err_slave::*;

  // package imports
  import Connectable:: *;
  import GetPut:: *;
  import Vector::*;

`ifdef debug
  import debug_types::*;                                                                          
  import jtagdtm::*;                                                                              
  import riscvDebug013::*;                                                                        
  import debug_halt_loop::*;
`endif

  typedef 0 Sign_master_num;
  typedef (TAdd#(Sign_master_num, 1)) Mem_master_num;
  typedef (TAdd#(Mem_master_num, 1)) Fetch_master_num;
  typedef (TAdd#(Fetch_master_num, `ifdef debug 1 `else 0 `endif )) Debug_master_num;
  typedef (TAdd#(Debug_master_num, 1)) Num_Masters;
 
    function Bit#(TLog#(`Num_Slaves)) fn_slave_map (Bit#(`paddr) addr);
      Bit#(TLog#(`Num_Slaves)) slave_num = 0;
      if(addr >= `MemoryBase && addr<= `MemoryEnd)
        slave_num = `Memory_slave_num;
      else if(addr>= `BootRomBase && addr<= `BootRomEnd)
        slave_num =  `BootRom_slave_num;
      else if(addr>= `UartBase && addr<= `UartEnd)
        slave_num = `Uart_slave_num;
      else if(addr>= `ClintBase && addr<= `ClintEnd)
        slave_num = `Clint_slave_num;
      else if(addr>= `SignBase && addr<= `SignEnd)
        slave_num = `Sign_slave_num;
    `ifdef debug
      else if(addr>= `DebugBase && addr<= `DebugEnd)
        slave_num = `Debug_slave_num;
    `endif
      else
        slave_num = `Err_slave_num;
        
      return slave_num;
    endfunction:fn_slave_map

  interface Ifc_Soc;
  `ifdef rtldump
    interface Get#(DumpType) io_dump;
  `endif
  `ifdef CORE_AXI4
    interface AXI4_Master_IFC#(`paddr, XLEN, USERSPACE) main_mem_master;
    interface AXI4_Master_IFC#(`paddr, XLEN, USERSPACE) boot_mem_master;
  `elsif CORE_AXI4Lite
    interface AXI4_Lite_Master_IFC#(`paddr, XLEN, USERSPACE) main_mem_master;
    interface AXI4_Lite_Master_IFC#(`paddr, XLEN, USERSPACE) boot_mem_master;
  `endif
    interface RS232 uart_io;
  `ifdef debug
      // ------------- JTAG IOs ----------------------//
    (*always_enabled,always_ready*)                                                               
    method Action wire_tms(Bit#(1)tms_in);                                                        

    (*always_enabled,always_ready*)                                                               
    method Action wire_tdi(Bit#(1)tdi_in);                                                        

    (*always_enabled,always_ready*)                                                               
    method Bit#(1)wire_tdo;                                                                       
      // ---------------------------------------------//
  `endif
`ifdef simulate
  `ifdef perfmonitors
    method Tuple2#(Vector#(`counters, Bit#(XLEN)), Vector#(`counters, Bit#(64))) counter_values;
  `endif
    method Bool mv_end_simulation ;
`endif

  `ifdef formal
    method Bit#(1) rvfi_valid;
    method Bit#(64) rvfi_order;
    method Bit#(32) rvfi_insn;

    method Bit#(5)  rvfi_rs1_addr;
    method Bit#(5)  rvfi_rs2_addr;
    method Bit#(32) rvfi_rs1_rdata;
    method Bit#(32) rvfi_rs2_rdata;
    method Bit#(5)  rvfi_rd_addr;
    method Bit#(32) rvfi_rd_wdata;

    // pc
    method Bit#(32) rvfi_pc_rdata;
 
    // mem
    method Bit#(32) rvfi_mem_addr;
    method Bit#(4)  rvfi_mem_rmask;
    method Bit#(4)  rvfi_mem_wmask;
    method Bit#(32) rvfi_mem_rdata;
    method Bit#(32) rvfi_mem_wdata;
  `endif
  endinterface

  `ifdef CORE_AXI4
    module mkeclass(Ifc_eclass_axi4);
      let ifc();
      mkeclass_axi4#(`resetpc) _temp(ifc);
      return ifc;
    endmodule
  `elsif CORE_AXI4Lite
    module mkeclass(Ifc_eclass_axi4lite);
      let ifc();
      mkeclass_axi4lite#(`resetpc) _temp(ifc);
      return ifc;
    endmodule
  `endif

  module mkSoc#(Clock tck_clk, Reset trst)(Ifc_Soc);
    let curr_clk<-exposeCurrentClock;
    let curr_reset<-exposeCurrentReset;
`ifdef CORE_AXI4
    AXI4_Fabric_IFC #(Num_Masters, `Num_Slaves, `paddr, XLEN, USERSPACE) 
                                                    fabric <- mkAXI4_Fabric(fn_slave_map);
    let eclass <- mkeclass();
  `ifdef debug
    Ifc_debug_halt_loop_axi4#(`paddr, XLEN, USERSPACE) debug_memory <- mkdebug_halt_loop_axi4;
  `endif
    Ifc_uart_axi4#(`paddr,XLEN,0, 16) uart <- mkuart_axi4(curr_clk,curr_reset, 5);
    Ifc_clint_axi4#(`paddr, XLEN, 0, 1, 16) clint <- mkclint_axi4();
    Ifc_err_slave_axi4#(`paddr,XLEN,0) err_slave <- mkerr_slave_axi4;

`elsif CORE_AXI4Lite

    AXI4_Lite_Fabric_IFC #(Num_Masters, `Num_Slaves, `paddr, XLEN, USERSPACE) 
                                                    fabric <- mkAXI4_Lite_Fabric(fn_slave_map);

    Ifc_eclass_axi4lite eclass <- mkeclass_axi4lite(`resetpc);
  `ifdef debug
    Ifc_debug_halt_loop_axi4lite#(`paddr, XLEN, USERSPACE) debug_memory <-  
                                                                        mkdebug_halt_loop_axi4lite;
  `endif
    Ifc_uart_axi4lite#(`paddr,XLEN,0, 16) uart <- mkuart_axi4lite(curr_clk,curr_reset, 5);
    Ifc_clint_axi4lite#(`paddr, XLEN, 0, 1, 16) clint <- mkclint_axi4lite();
    Ifc_err_slave_axi4lite#(`paddr,XLEN,0) err_slave <- mkerr_slave_axi4lite;
`endif

    // -------------------------------- JTAG + Debugger Setup ---------------------------------- //
`ifdef debug
    // null crossing registers to transfer input signals from current_domain to tck domain
    CrossingReg#(Bit#(1)) tdi<-mkNullCrossingReg(tck_clk,0);                                        
    CrossingReg#(Bit#(1)) tms<-mkNullCrossingReg(tck_clk,0);                                        
    // null crossing registers to transfer signals from tck to curr_clock domain.
    CrossingReg#(Bit#(1)) tdo<-mkNullCrossingReg(curr_clk,0,clocked_by tck_clk, reset_by trst);     
                                                                                                    
    Ifc_jtagdtm jtag_tap <- mkjtagdtm(clocked_by tck_clk, reset_by trst);                           
    Ifc_riscvDebug013 debug_module <- mkriscvDebug013();                                           

    // synFIFOs to transact data between JTAG and debug module 
    SyncFIFOIfc#(Bit#(41)) sync_request_to_dm <-mkSyncFIFOToCC(1,tck_clk,trst);                     
    SyncFIFOIfc#(Bit#(34)) sync_response_from_dm <-mkSyncFIFOFromCC(1,tck_clk);                     
                           
            // ----------- Connect JTAG IOs through null-crossing registers ------ //
    rule assign_jtag_inputs;
      jtag_tap.tms_i(tms.crossed);                                                                  
      jtag_tap.tdi_i(tdi.crossed);                                                                  
      jtag_tap.bs_chain_i(0);                                                                       
      jtag_tap.debug_tdi_i(0);                                                                      
    endrule                                                                                         
                                                                                                    
    rule assign_jtag_output;
      tdo <= jtag_tap.tdo(); //  Launched by a register clocked by inverted tck                     
    endrule                                                                                        
            // ------------------------------------------------------------------- //

    // capture jtag tap request into a syncfifo first.
    rule connect_tap_request_to_syncfifo;                                                           
      let x<-jtag_tap.request_to_dm;                                                                
      sync_request_to_dm.enq(zeroExtend(x));          

    // send captured synced jtag tap request to the debug module
    endrule                                                                                         
    rule read_synced_request_to_dm;                                                                 
      sync_request_to_dm.deq;                                                                       
      debug_module.dtm.putCommand.put(sync_request_to_dm.first);                                    
    endrule                                                                                         

    // collect debug response into a syncfifo
    rule connect_debug_response_to_syncfifo;                                                        
      let x <- debug_module.dtm.getResponse.get;                                                    
      sync_response_from_dm.enq(x);          
    endrule                                  

    // send synced debug response back to the JTAG
    rule read_synced_response_from_dm;                                                              
      sync_response_from_dm.deq;                                                                    
      jtag_tap.response_from_dm(sync_response_from_dm.first);                                       
    endrule                                                                                         
                                                                                                    
    mkConnection (eclass.debug_server ,debug_module.hart);
  `endif
      
    // ------------------------------------------------------------------------------------------//
  `ifdef debug
    mkConnection (debug_module.debug_master,fabric.v_from_masters[valueOf(Debug_master_num)]);
  `endif
    mkConnection(eclass.master_d,	fabric.v_from_masters[valueOf(Mem_master_num)]);
    mkConnection(eclass.master_i, fabric.v_from_masters[valueOf(Fetch_master_num)]);
  `ifdef cache_control
    mkConnection(eclass.master_io, fabric.v_from_masters[valueOf(IO_master_num)]);
  `endif
    mkConnection (fabric.v_to_slaves [`Uart_slave_num ],uart.slave);
    mkConnection (fabric.v_to_slaves [`Clint_slave_num ],clint.slave);
    mkConnection (fabric.v_to_slaves [`Err_slave_num ] , err_slave.slave);
  `ifdef debug
    mkConnection (fabric.v_to_slaves [`Debug_slave_num ] , debug_memory.slave);
  `endif

    // sideband connection
    mkConnection(eclass.sb_clint_msip,clint.sb_clint_msip);
    mkConnection(eclass.sb_clint_mtip,clint.sb_clint_mtip);
    mkConnection(eclass.sb_clint_mtime,clint.sb_clint_mtime);

    `ifdef rtldump
      interface io_dump= eclass.io_dump;
    `endif
    interface uart_io=uart.io;
    interface main_mem_master = fabric.v_to_slaves[`Memory_slave_num] ;
    interface boot_mem_master = fabric.v_to_slaves[`BootRom_slave_num] ;

      // ------------- JTAG IOs ----------------------//
  `ifdef debug
    method Action wire_tms(Bit#(1)tms_in);                                                        
      tms <= tms_in;                                                                              
    endmethod                                                                                     
    method Action wire_tdi(Bit#(1)tdi_in);                                                        
      tdi <= tdi_in;                                                                              
    endmethod                                                                                     
    method Bit#(1)wire_tdo;                                                                       
      return tdo.crossed();                                                                       
    endmethod
  `endif
`ifdef simulate
  `ifdef perfmonitors
    method counter_values = eclass.counter_values;
  `endif
`endif
  `ifdef formal
    method Bit#(1) rvfi_valid = eclass.rvfi_valid;
    method Bit#(64) rvfi_order = eclass.rvfi_order;
    method Bit#(32) rvfi_insn = eclass.rvfi_insn;
    method Bit#(5)  rvfi_rs1_addr  = eclass.rvfi_rs1_addr;
    method Bit#(5)  rvfi_rs2_addr  = eclass.rvfi_rs2_addr;
    method Bit#(32) rvfi_rs1_rdata = eclass.rvfi_rs1_rdata;
    method Bit#(32) rvfi_rs2_rdata = eclass.rvfi_rs2_rdata;
    method Bit#(5)  rvfi_rd_addr   = eclass.rvfi_rd_addr;
    method Bit#(32) rvfi_rd_wdata  = eclass.rvfi_rd_wdata;
    method Bit#(32) rvfi_pc_rdata  = eclass.rvfi_pc_rdata;
    method Bit#(32) rvfi_mem_addr  = eclass.rvfi_mem_addr;
    method Bit#(4)  rvfi_mem_rmask = eclass.rvfi_mem_rmask;
    method Bit#(4)  rvfi_mem_wmask = eclass.rvfi_mem_wmask;
    method Bit#(32) rvfi_mem_rdata = eclass.rvfi_mem_rdata;
    method Bit#(32) rvfi_mem_wdata = eclass.rvfi_mem_wdata;
  `endif
      // -------------------------------------------- //
  endmodule: mkSoc
endpackage: Soc
