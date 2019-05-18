/* 
Copyright (c) 2018, IIT Madras All rights reserved.

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
Details: This is a Register File module where the reads will bypass the new value being written to 
the same address in the same cycle
--------------------------------------------------------------------------------------------------
*/
package registerfile;
  // library package
  import RegFile :: *;
  import common_types :: *;
`ifdef debug
  import debug_types :: *;
`endif

  // -------------------------- interface definitions ------------------------------------------ //
  interface Ifc_registerfile;
    method Action upd(Bit#(5) addr, Bit#(XLEN) d);
    method Bit#(XLEN) sub(Bit#(5) addr);
  `ifdef debug
    method ActionValue#(Bit#(XLEN)) mav_debug_access_gprs(AbstractRegOp cmd);
  `endif
  endinterface: Ifc_registerfile
  // ----------------------------end interface definitions ------------------------------------- //

  // ------------------------- module definitions ---------------------------------------------- //
  module mkregisterfile(Ifc_registerfile);
    // -------------------- Instantiations ------------------------------------------------------ //

    // instantiate a CF regile file module. CF is required since you will be reading and writing in
    // the same cycle.
    RegFile#(Bit#(5), Bit#(XLEN)) rf <- mkRegFileWCF(0,31);

    // capture the curren address and value being written into the regfile
    Wire#(Bit#(5)) wr_write_address <- mkDWire(0);
    Wire#(Bit#(XLEN)) wr_write_data <- mkDWire(0);

    // update the regfile with new data
    method Action upd(Bit#(5) addr, Bit#(XLEN) d);
      rf.upd(addr, d);
      wr_write_address <= addr;
      wr_write_data <= d;
    endmethod

    // read the latest content of the regfile at a particular address
    method Bit#(XLEN) sub(Bit#(5) addr);
      if( addr == wr_write_address )
        return wr_write_data;
      else
        return rf.sub(addr);
    endmethod

  `ifdef debug
    method ActionValue#(Bit#(XLEN)) mav_debug_access_gprs(AbstractRegOp cmd);
      Bit#(XLEN) read = ?;
      Bit#(5) index = truncate(cmd.address);
      if (cmd.read_write) // write_op
        rf.upd(index, cmd.writedata);
      else
        read = rf.sub(index);
      return read;
    endmethod
  `endif
  endmodule: mkregisterfile
  // ----------------------------end module definitions ------------------------------------- //
endpackage

