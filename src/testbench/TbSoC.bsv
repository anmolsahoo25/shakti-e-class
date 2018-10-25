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
package TbSoC;
  import SoC:: *;
  import Clocks::*;
  import GetPut:: *;
	import Semi_FIFOF:: *;
	import AXI4_Types:: *;
	import AXI4_Fabric:: *;
  import uart::*;
  import common_types::*;
  `include "common_params.bsv"
  import device_common::*;
  import DReg::*;

  module mkTbSoC(Empty);

    let def_clk <- exposeCurrentClock;
    let def_rst <- exposeCurrentReset;
    `ifdef EXTERNAL
      Ifc_SoC soc <- mkSoC(def_clk, def_rst);
    `else
      Ifc_SoC soc <- mkSoC();
    `endif
    
    UserInterface#(PADDR,XLEN,16) uart <- mkuart_user(5);
    Reg#(Bool) rg_read_rx<- mkDReg(False);

    let verbosity=`VERBOSITY;
    Reg#(Bit#(5)) rg_cnt <-mkReg(0);
 	  let dump <- mkReg(InvalidFile) ;
 	  let dump1 <- mkReg(InvalidFile) ;
    rule open_file(rg_cnt<5);
      String dumpFile = "rtl.dump" ;
      String dumpFile1 = "app_log" ;
    	File lfh <- $fopen( dumpFile, "w" ) ;
    	File lfh1 <- $fopen( dumpFile1, "w" ) ;
    	if ( lfh == InvalidFile || lfh1==InvalidFile )begin
    	  if(verbosity>1) $display("cannot open %s", dumpFile); 
    	  $finish(0);
    	end
    	dump <= lfh ;
      dump1 <= lfh1;
    	rg_cnt <= rg_cnt+1 ;
    endrule

    rule connect_uart_out;
      soc.uart_io.sin(uart.io.sout);
    endrule
    rule connect_uart_in;
      uart.io.sin(soc.uart_io.sout);
    endrule

    rule check_if_character_present(!rg_read_rx);
      let {data,err}<- uart.read_req('hc,Byte);
      if (data[3]==1) // character present
        rg_read_rx<=True;
    endrule

    rule write_received_character(rg_cnt>=5 && rg_read_rx);
      let data<-uart.read_req('h8,Byte);
      $fwrite(dump1,"%c",data);
    endrule

    `ifdef simulate
      rule write_dump_file(rg_cnt>=5);
        let {prv, pc, instruction, rd, data}<- soc.io_dump.get;
        if(instruction=='h00006f||instruction =='h00a001)
          $finish(0);
        else begin
  		  	$fwrite(dump, prv, " 0x%16h", pc, " (0x%8h", instruction, ")"); 
	  	  	$fwrite(dump, " x%d", rd, " 0x%16h", data, "\n"); 
        end
      endrule
    `endif

    `ifdef simulate
      rule display_eol;
        if(verbosity!=0)
          $display("\n");
      endrule
    `endif
  endmodule
endpackage: TbSoC
