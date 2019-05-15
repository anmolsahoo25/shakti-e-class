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

  // -------------------------- interface definitions ------------------------------------------ //
  interface Ifc_registerfile#(type index_t, type data_t);
    method Action upd(index_t addr, data_t d);
    method data_t sub(index_t addr);
  endinterface: Ifc_registerfile
  // ----------------------------end interface definitions ------------------------------------- //

  // ------------------------- module definitions ---------------------------------------------- //
  module mkregisterfile(Ifc_registerfile#(index_t, data_t))
    provisos(Bits#(index_t, size_index), 
              Bits#(data_t, size_data),
              Bounded#(index_t),
              Eq#(index_t),
              Literal#(data_t),
              Literal#(index_t));
    // -------------------- Instantiations ------------------------------------------------------ //

    // instantiate a CF regile file module. CF is required since you will be reading and writing in
    // the same cycle.
    RegFile#(index_t, data_t) rf <- mkRegFileWCF(0,fromInteger(valueOf(TExp#(size_index))-1));

    // capture the curren address and value being written into the regfile
    Wire#(index_t) wr_write_address <- mkDWire(0);
    Wire#(data_t) wr_write_data <- mkDWire(0);

    // update the regfile with new data
    method Action upd(index_t addr, data_t d);
      rf.upd(addr, d);
      wr_write_address <= addr;
      wr_write_data <= d;
    endmethod

    // read the latest content of the regfile at a particular address
    method data_t sub(index_t addr);
      if( addr == wr_write_address )
        return wr_write_data;
      else
        return rf.sub(addr);
    endmethod
  endmodule: mkregisterfile
  // ----------------------------end module definitions ------------------------------------- //
endpackage

