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
package SoC;
  // project related imports
	import Semi_FIFOF:: *;
	import AXI4_Types:: *;
	import AXI4_Fabric:: *;
	import AXI4_Lite_Types:: *;
	import AXI4_Lite_Fabric:: *;
  import Tilelink_lite_Types::*;
  import Tilelink_lite::*;
  import core:: * ;
  import common_types:: * ;
  `include "common_params.bsv"
  `include "SoC.defines"

  // peripheral imports
  import memory::*;
  `ifdef BOOTROM
    import bootrom:: *;
  `endif
  `ifdef EXTERNAL
    import external_mem::*;
  `endif
  // package imports
  import Connectable:: *;
  import GetPut:: *;
  import Vector::*;
 
 `ifdef CORE_TLU
    function Tuple2 #(Bool, Bit#(TLog#(`Num_Slaves))) fn_slave_map (A_Opcode_lite cmd, 
                                                                                 Bit#(PADDR) addr);
      Bool slave_exist = True;
      Bit#(TLog#(`Num_Slaves)) slave_num = 0;
      if(addr >= `MemoryBase && addr<= `MemoryEnd)
        if(cmd==Get_data)
          slave_num = `Memory_slave_num;
        else
          slave_num = `Memory_wr_slave_num;
      else
      `ifdef BOOTROM
        if(addr>= `BootRomBase && addr<= `BootRomEnd)
          slave_num =  `BootRom_slave_num;
        else
      `endif
        slave_exist = False;
        
      return tuple2(slave_exist, slave_num);
    endfunction:fn_slave_map
 `else
    function Tuple2 #(Bool, Bit#(TLog#(`Num_Slaves))) fn_slave_map (Bit#(PADDR) addr);
      Bool slave_exist = True;
      Bit#(TLog#(`Num_Slaves)) slave_num = 0;
      if(addr >= `MemoryBase && addr<= `MemoryEnd)
        slave_num = `Memory_slave_num;
      else
      `ifdef BOOTROM
        if(addr>= `BootRomBase && addr<= `BootRomEnd)
          slave_num =  `BootRom_slave_num;
        else
      `endif
        slave_exist = False;
        
      return tuple2(slave_exist, slave_num);
    endfunction:fn_slave_map
  `endif

  interface Ifc_SoC;
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
    `ifdef EXTERNAL
      interface AXI4_Master_IFC#(PADDR, XLEN, USERSPACE) mem_master;
    `endif
  endinterface

`ifdef CORE_AXI4
  (*synthesize*)
  module mkSoC `ifdef EXTERNAL #(Clock exmem_clk, Reset exmem_rst) `endif (Ifc_SoC);
    Ifc_core_AXI4 core <- mkcore_AXI4();
    AXI4_Fabric_IFC #(`Num_Masters, `Num_Slaves, PADDR, XLEN, USERSPACE) 
                                                    fabric <- mkAXI4_Fabric(fn_slave_map);
    `ifdef BRAM
  		Ifc_memory_AXI4#(PADDR, XLEN, USERSPACE, `Addr_space) main_memory <- mkmemory_AXI4(`MemoryBase, 
                                                "code.mem.MSB", "code.mem.LSB");
    `endif
    `ifdef EXTERNAL
      Ifc_exteral_mem#(PADDR, XLEN, USERSPACE) external_memory <- mkexternal_mem(exmem_clk,
                                                                                        exmem_rst);
    `endif
		`ifdef BOOTROM
			Ifc_bootrom_AXI4#(PADDR, XLEN, USERSPACE) bootrom <-mkbootrom_AXI4(`BootRomBase);
		`endif

   	mkConnection(core.mem_master,	fabric.v_from_masters[`Mem_master_num]);
   	mkConnection(core.fetch_master, fabric.v_from_masters[`Fetch_master_num]);

    `ifdef EXTERNAL
  		mkConnection(fabric.v_to_slaves[`Memory_slave_num],external_memory.slave);
    `endif
    `ifdef BRAM
  		mkConnection(fabric.v_to_slaves[`Memory_slave_num],main_memory.slave);
    `endif
		`ifdef BOOTROM
			mkConnection (fabric.v_to_slaves [`BootRom_slave_num],bootrom.slave);
		`endif

    `ifdef simulate
      interface dump= core.dump;
    `endif
    `ifdef EXTERNAL
      interface mem_master=external_memory.master;
    `endif
  endmodule: mkSoC
`endif
`ifdef CORE_AXI4Lite
  (*synthesize*)
  module mkSoC(Ifc_SoC);
    Ifc_core_AXI4Lite core <- mkcore_AXI4Lite();
    AXI4_Lite_Fabric_IFC #(`Num_Masters, `Num_Slaves, PADDR, XLEN, USERSPACE) 
                                                    fabric <- mkAXI4_Lite_Fabric(fn_slave_map);
		Ifc_memory_AXI4Lite#(PADDR, XLEN, USERSPACE, `Addr_space) main_memory <- mkmemory_AXI4Lite(
                                                      `MemoryBase, "code.mem.MSB", "code.mem.LSB");
		`ifdef BOOTROM
			Ifc_bootrom_AXI4Lite#(PADDR, XLEN, USERSPACE) bootrom <-mkbootrom_AXI4Lite(`BootRomBase);
		`endif

   	mkConnection(core.mem_master,	fabric.v_from_masters[`Mem_master_num]);
   	mkConnection(core.fetch_master, fabric.v_from_masters[`Fetch_master_num]);

		mkConnection(fabric.v_to_slaves[`Memory_slave_num],main_memory.slave);
		`ifdef BOOTROM
			mkConnection (fabric.v_to_slaves [`BootRom_slave_num],bootrom.slave);
		`endif

    `ifdef simulate
      interface dump= core.dump;
    `endif
  endmodule: mkSoC
`endif
`ifdef CORE_TLU
  (*synthesize*)
  module mkSoC(Ifc_SoC);
    Ifc_core_TLU core <- mkcore_TLU();
    // TODO do this somewhere better
    Vector#(`Num_Slaves, Bit#(`Num_Masters)) master_route; //encoding: IMEM, DMEM  
	  master_route[valueOf(`Memory_slave_num)]                    = truncate(2'b11);
	  master_route[valueOf(`Memory_wr_slave_num)]                 = truncate(2'b01);
	  `ifdef BOOTROM master_route[valueOf(`BootRom_slave_num)]     = truncate(2'b11); `endif

    Tilelink_Fabric_IFC_lite#(`Num_Masters, `Num_Slaves, 1, PADDR, TDiv#(XLEN, 8), 2) fabric <- 
                                                                      mkTilelinkLite(fn_slave_map,
                                                                      master_route);
	  Ifc_memory_TLU#(PADDR, TDiv#(XLEN, 8), 2, `Addr_space) main_memory <- mkmemory_TLU(`MemoryBase,
                                                                    "code.mem.MSB", "code.mem.LSB");	
		`ifdef BOOTROM
			Ifc_bootrom_TLU#(PADDR, TDiv#(XLEN, 8), 2) bootrom <-mkbootrom_TLU(`BootRomBase);
		`endif

   	mkConnection(core.mem_master,	fabric.v_from_masters[`Mem_master_num]);
   	mkConnection(core.fetch_master, fabric.v_from_masters[`Fetch_master_num]);

		mkConnection(fabric.v_to_slaves[`Memory_slave_num],main_memory.read_slave);
		mkConnection(fabric.v_to_slaves[`Memory_wr_slave_num],main_memory.write_slave);
		`ifdef BOOTROM
			mkConnection (fabric.v_to_slaves [`BootRom_slave_num],bootrom.slave);
		`endif

    `ifdef simulate
      interface dump= core.dump;
    `endif
  endmodule: mkSoC
`endif
endpackage: SoC
