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

*/
package TLMemoryMap;
	/*=== Project imports ==== */
	import defined_types::*;
	import Tilelink_Types::*;
	`include "defined_parameters.bsv"
	/*========================= */


function Tuple2 #(Bool, Bit#(TLog#(Num_Slaves))) fn_addr_to_slave_num  (Opcode command, Bit#(`PADDR) addr);

		if(addr>=`SDRAMMemBase && addr<=`SDRAMMemEnd && (command==Get_data || command==GetWrap)) 
				return tuple2(True,fromInteger(valueOf(Sdram_slave_num)));
		else if(addr>=`SDRAMMemBase && addr<=`SDRAMMemEnd && (command==PutPartialData || command==PutFullData))
				return tuple2(True,fromInteger(valueOf(Sdram_slave_num_wr)));
		`ifdef DEBUG
		else if(addr>=`DebugBase && addr<=`DebugEnd)
			return tuple2(True,fromInteger(valueOf(Debug_slave_num)));
		`endif
		`ifdef SDRAM
			else if(addr>=`SDRAMCfgBase && addr<=`SDRAMCfgEnd )
				return tuple2(True,fromInteger(valueOf(Sdram_cfg_slave_num)));
		`endif
		`ifdef BOOTROM
			else if(addr>=`BootRomBase && addr<=`BootRomEnd)
				return tuple2(True,fromInteger(valueOf(BootRom_slave_num)));
		`endif
			else if( (addr>=`UART0Base && addr<=`UART0End) || (addr>=`UART1Base && addr<=`UART1End) || (addr>=`ClintBase && addr<=`ClintEnd) || (addr>=`PLICBase && addr<=`PLICEnd) || (addr>=`GPIOBase && addr<=`GPIOEnd) || (addr>=`I2C1Base && addr<=`I2C1End)|| (addr>=`I2C0Base && addr<=`I2C0End) || (addr>=`QSPI1CfgBase && addr<=`QSPI1CfgEnd) || (addr>=`QSPI1MemBase && addr<=`QSPI1MemEnd)|| (addr>=`QSPI0CfgBase && addr<=`QSPI0CfgEnd) || (addr>=`QSPI0MemBase && addr<=`QSPI0MemEnd) || (addr>=`AxiExp1Base && addr<=`AxiExp1End) )
				return tuple2(True,fromInteger(valueOf(SlowPeripheral_slave_num)));
		`ifdef DMA
			else if(addr>=`DMABase && addr<=`DMAEnd)
				return tuple2(True,fromInteger(valueOf(Dma_slave_num)));
		`endif
		`ifdef TCMemory
			else if(addr>=`TCMBase && addr<=`TCMEnd)
				return tuple2(True,fromInteger(valueOf(TCM_slave_num)));
		`endif
	else
		return tuple2(False,?);
endfunction

function Bool is_IO_Addr(Bit#(`PADDR) addr); // TODO Shuold be PADDR
		if(addr>=`DebugBase && addr<=`DebugEnd)
			return (True);
		else if(addr>=`SDRAMMemBase && addr<=`SDRAMMemEnd)
			return (False);
		`ifdef BOOTROM
			else if(addr>=`BootRomBase && addr<=`BootRomEnd)
				return (False);
		`endif
		`ifdef TCMemory
			else if(addr>=`TCMBase && addr<=`TCMEnd)
				return (False);
		`endif
		else
			return True;
endfunction

	
endpackage
