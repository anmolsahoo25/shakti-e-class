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
  import core:: * ;
  import common_types:: * ;
  `include "common_params.bsv"
  `include "SoC.defines"

  // peripheral imports
  import Memory_AXI4::*;
  `ifdef BOOTROM
    import BootRom:: *;
  `endif
  // package imports
  import Connectable:: *;
  import GetPut:: *;
  
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

  interface Ifc_SoC;
    `ifdef simulate
      interface Get#(DumpType) dump;
    `endif
  endinterface

  (*synthesize*)
  module mkSoC(Ifc_SoC);
    Ifc_core core <- mkcore();
    AXI4_Fabric_IFC #(`Num_Masters, `Num_Slaves, PADDR, XLEN, USERSPACE) 
                                                    fabric <- mkAXI4_Fabric(fn_slave_map);
		Memory_IFC#(`MemoryBase,`Addr_space) main_memory <- mkMemory(`ifdef RV64 "code.mem.MSB", `endif
                                                                "code.mem.LSB",
                                                                "MainMEM");
		`ifdef BOOTROM
			BootRom_IFC bootrom <-mkBootRom(`BootRomBase);
		`endif

   	mkConnection(core.mem_master,	fabric.v_from_masters[`Mem_master_num]);
   	mkConnection(core.fetch_master, fabric.v_from_masters[`Fetch_master_num]);

		mkConnection(fabric.v_to_slaves[`Memory_slave_num],main_memory.axi_slave);
		`ifdef BOOTROM
			mkConnection (fabric.v_to_slaves [`BootRom_slave_num],bootrom.axi_slave);
		`endif

    `ifdef simulate
      interface dump= core.dump;
    `endif
  endmodule: mkSoC
endpackage: SoC
