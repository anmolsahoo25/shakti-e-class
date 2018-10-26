/* 
Copyright (c) 2018, IIT Madras All rights reserved.

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
package sign_dump;
  import Vector::*;
  import FIFOF::*;
  import DReg::*;
  import SpecialFIFOs::*;
  import BRAMCore::*;
  import FIFO::*;

  import common_types::*;
  `include "common_params.bsv"
	import AXI4_Types:: *;
	import AXI4_Fabric:: *;
	import Semi_FIFOF:: *;

  interface Ifc_sign_dump;
		interface AXI4_Master_IFC#(PADDR, XLEN, USERSPACE) master;
    method Action start();
  endinterface

  (*synthesize*)
  module mksign_dump(Ifc_sign_dump);
    let word_count = 128/valueOf(XLEN);

    Reg#(Bool) rg_start<- mkReg(False);
    Reg#(Bool) rg_end<- mkReg(False);
    Reg#(Bit#(PADDR)) rg_address<- mkReg(`signstart ); // TODO argument should be macro
    Reg#(Bit#(TLog#(TDiv#(128,XLEN)))) rg_word_count <- mkReg(fromInteger(word_count-1));
		AXI4_Master_Xactor_IFC #(PADDR, XLEN, USERSPACE) m_xactor <- mkAXI4_Master_Xactor;

    Reg#(Bit#(XLEN)) dataarray[word_count];
    for(Integer i=0;i<word_count;i=i+1)
      dataarray[i]<-mkReg(0);
   `ifdef signature 
      Reg#(Bit#(5)) rg_cnt <-mkReg(0);
 	    let dump <- mkReg(InvalidFile) ;
      rule open_file(rg_cnt<5);
        String dumpFile = "signature" ;
      	File lfh <- $fopen( dumpFile, "w" ) ;
      	if ( lfh == InvalidFile )begin
      	  $display("cannot open %s", dumpFile); 
      	  $finish(0);
      	end
      	dump <= lfh ;
      	rg_cnt <= rg_cnt+1 ;
      endrule
      
      rule send_request(rg_start);
        // TODO parameterize the arsize here
		  	AXI4_Rd_Addr#(PADDR, 0) read_request = AXI4_Rd_Addr {araddr: rg_address, aruser: ?, 
            arlen:'hFF, arsize: 2, arburst: 'b01, arid:2}; // arburst: 00-FIXED 01-INCR 10-WRAP
		  	m_xactor.i_rd_addr.enq(read_request);	
        rg_start<=False;
      endrule

      rule receive_response(rg_cnt>=5);
		  	let response <- pop_o (m_xactor.o_rd_data);	
        Bit#(XLEN) lv_dataarray[word_count];
        for(Integer i=0;i<word_count;i=i+1)
          lv_dataarray[i]=dataarray[i];
        if (response.rresp!=AXI4_OKAY)begin
          $display($time,"\tSIGNATURE: Memory responded with Error");
          $finish(0);
        end
        rg_word_count<=rg_word_count-1;
        lv_dataarray[rg_word_count]=response.rdata;
        dataarray[rg_word_count]<=response.rdata;

        if(rg_word_count==0)begin 
          for(Integer i=0;i<word_count;i=i+1)
        		$fwrite(dump,"%8h", lv_dataarray[i]); 
          $fwrite(dump,"\n");
        end
        if(response.rlast)
          $finish(0);
      endrule
    `endif

    method Action start()if(!rg_start);
      rg_start<=True;
    endmethod
    interface master=m_xactor.axi_side;
  endmodule
endpackage

