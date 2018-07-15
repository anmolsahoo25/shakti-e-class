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
  module mkTbSoC(Empty);

    let def_clk <- exposeCurrentClock;
    let def_rst <- exposeCurrentReset;
    `ifdef EXTERNAL
      Ifc_SoC soc <- mkSoC(def_clk, def_rst);
    `else
      Ifc_SoC soc <- mkSoC();
    `endif

    Reg#(Bit#(1)) rg_cnt <-mkReg(0);
 	  let dump <- mkReg(InvalidFile) ;
    rule open_file(rg_cnt==0);
      String dumpFile = "rtl.dump" ;
    	File lfh <- $fopen( dumpFile, "w" ) ;
    	if ( lfh == InvalidFile )begin
    	  `ifdef verbose $display("cannot open %s", dumpFile); `endif
    	  $finish(0);
    	end
    	dump <= lfh ;
    	rg_cnt <= 1 ;
    endrule

    `ifdef simulate
      rule write_dump_file(rg_cnt!=0);
        let {prv, pc, instruction, rd, data}<- soc.dump.get;
        if(instruction=='h00006f)
          $finish(0);
		  	$fwrite(dump, prv, " 0x%16h", pc, " (0x%8h", instruction, ")"); 
		  	$fwrite(dump, " x%d", rd, " 0x%16h", data, "\n"); 
      endrule
    `endif

    `ifdef simulate
      rule display_eol;
        $display("\n");
      endrule
    `endif
  endmodule
endpackage: TbSoC
