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

This module allows you to create the cache data tag arrays in various configurations. You
can define the number of banks and type of concatenation you would like using the various modules
described in this file. These are implemented as Dual-BRAM modules such that 1-port is a dedicated
read and the other is a dedicated write. This allows easy mapping to SRAM libraries having 1r+1w
configuration.

Three primary parameters are available: 
  n_entries: The total number of BRAM entries for the entire instance.
  datawidth: The total number of bits on a read-response.
  banks: the total number of banks present in an instance.

Four different types of modules are available:

  1. mkmem_config_h: This module banks the BRAM instances horizontally. That is the bit-width of
  each bank is now datawidth/banks. The provisos of the module require that datawidth%banks=0. Each
  bank however has the same number of entries. This module does not support byte-enables.

  2. mkmem_config_hbe: This module banks the BRAM instances horizontally. That is the bit-width of
  each bank is now datawidth/banks. The provisos of the module require that datawidth%banks=0. Each
  bank however has the same number of entries. This module supports byte-enables and the provisos
  require that (datawidth/8)%banks=0.

  3. mkmem_config_v: this module banks the BRAM instances vertically,  i.e. the number of entries
  per bank = n_entries/banks. The bit-width of each bank instances is however the same: datawidth.
  The provisos of this module require that: n_entries%banks=0. This module does not support
  byte-enables.

  4. mkmem_config_v: this module banks the BRAM instances vertically,  i.e. the number of entries
  per bank = n_entries/banks. The bit-width of each bank instances is however the same: datawidth.
  The provisos of this module require that: n_entries%banks=0. This module supports byte-enables and
  has an additional proviso of (datawidth%8)=0
--------------------------------------------------------------------------------------------------
*/
package mem_config;
 
  import BRAMCore::*;
  import DReg::*;
  import FIFOF::*;
  import SpecialFIFOs::*;
  import Assert::*;

  interface Ifc_mem_config#( numeric type n_entries, numeric type datawidth, numeric type banks);
    method Action read_request(Bit#(TLog#(n_entries)) index);
    method ActionValue#(Bit#(datawidth)) read_response;
    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data);
  endinterface
  
  interface Ifc_mem_config_be#( numeric type n_entries, numeric type datawidth, numeric type banks);
    method Action read_request(Bit#(TLog#(n_entries)) index);
    method Bit#(datawidth) read_response;
    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data,
        Bit#(TDiv#(datawidth, 8)) we);
  endinterface
  // TODO check if single-port BRAMs can be instantiated through a parameter.
  module mkmem_config_h#(parameter Bool ramreg, parameter String porttype)(Ifc_mem_config#(n_entries, datawidth,  banks))
    provisos(
             Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth),
             Add#(a__, bpb, datawidth)
    );
    Integer bits_per_bank=valueOf(bpb);
    
    staticAssert(porttype=="single" || porttype=="dual","Only supported porttypes are: single, dual");

    BRAM_DUAL_PORT#(Bit#(TLog#(n_entries)), Bit#(bpb)) ram_double [valueOf(banks)];
    BRAM_PORT#(Bit#(TLog#(n_entries)), Bit#(bpb)) ram_single [valueOf(banks)];
    Reg#(Bit#(bpb)) rg_output[valueOf(banks)][2];
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      if(porttype=="single")
        ram_single[i]<-mkBRAMCore1(valueOf(n_entries), False);
      else
        ram_double[i]<-mkBRAMCore2(valueOf(n_entries), False);
      rg_output[i] <- mkCReg(2,0);
    end

    Reg#(Bool) rg_read_req_made <- mkDReg(False);
    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(rg_read_req_made && !ramreg);
        if(porttype=="single")
          rg_output[i][0]<=ram_single[i].read;
        else
          rg_output[i][0]<=ram_double[i].a.read;
      endrule
      rule capture_output_reg(ramreg);
        if(porttype=="single")
          rg_output[i][1]<=ram_single[i].read;
        else
          rg_output[i][1]<=ram_double[i].a.read;
      endrule
    end

    method Action read_request(Bit#(TLog#(n_entries)) index);
      for(Integer i=0;i<valueOf(banks);i=i+1) begin
        if(porttype=="single")
          ram_single[i].put(False, index,  ?);
        else
          ram_double[i].a.put(False, index,  ?);
      end
      rg_read_req_made<=True;
    endmethod
    method ActionValue#(Bit#(datawidth)) read_response;
      Bit#(datawidth) data_resp=0;
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        data_resp[i*bits_per_bank+bits_per_bank-1 : i*bits_per_bank]=rg_output[i][1];
      end
      return data_resp;
    endmethod
    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data);
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        if (porttype=="single")
          ram_single[i].put(True, address, data[i*bits_per_bank+bits_per_bank-1:i*bits_per_bank]);
        else
          ram_double[i].b.put(True, address, data[i*bits_per_bank+bits_per_bank-1:i*bits_per_bank]);
      end
    endmethod
  endmodule
  
  module mkmem_config_hbe#(parameter Bool ramreg)(Ifc_mem_config_be#(n_entries, datawidth,  banks))
    provisos(Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth), 
             Div#(bpb, 8, bytes), 
             Div#(datawidth, 8, totalbytes), 
             Div#(totalbytes, banks, bytes_pbank), 
             Add#(a__, TDiv#(datawidth, banks), datawidth),
             // compiler required provisos
             Add#(b__, bpb, datawidth), // datawidth is atleast bpb wide
             Mul#(TDiv#(bpb, bytes_pbank), bytes_pbank, bpb)
    );

    Integer bits_per_bank=valueOf(bpb);
    let bytes_per_bank=valueOf(bytes_pbank);
    
    BRAM_DUAL_PORT_BE#(Bit#(TLog#(n_entries)), Bit#(bpb), bytes_pbank) ram [valueOf(banks)];
    Reg#(Bit#(bpb)) rg_output[valueOf(banks)][2];
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram[i]<-mkBRAMCore2BE(valueOf(n_entries), False);
      rg_output[i] <- mkCReg(2,0);
    end
    Reg#(Bool) rg_read_req_made <- mkDReg(False);
    
    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(rg_read_req_made && !ramreg);
        rg_output[i][0]<=ram[i].a.read;
      endrule
      rule capture_output_reg(ramreg);
        rg_output[i][1]<=ram[i].a.read;
      endrule
    end
    
    method Action read_request(Bit#(TLog#(n_entries)) index);
      for(Integer i=0;i<valueOf(banks);i=i+1)
        ram[i].a.put(0, index,  ?);
      rg_read_req_made<=True;
    endmethod
    method Bit#(datawidth) read_response;
      Bit#(datawidth) data_resp=0;
      for(Integer i=0;i<valueOf(banks);i=i+1)
        data_resp[i*bits_per_bank+bits_per_bank-1 : i*bits_per_bank]=rg_output[i][1];
      return data_resp;
    endmethod
    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data,
        Bit#(TDiv#(datawidth, 8)) we);
      for(Integer i=0;i<valueOf(banks);i=i+1)
        ram[i].b.put(we[i*bytes_per_bank+bytes_per_bank-1:i*bytes_per_bank], address, 
                                            data[i*bits_per_bank+bits_per_bank-1:i*bits_per_bank]);
    endmethod
  endmodule:mkmem_config_hbe
  
  module mkmem_config_v#(parameter Bool ramreg)(Ifc_mem_config#(n_entries, datawidth,  banks))
    provisos( Log#(n_entries, log_entries), 
              Div#(n_entries, banks, epb), 
              Mul#(epb, banks, n_entries), 
              Log#(epb, log_epb), 
              Add#(a__, TDiv#(datawidth, banks), datawidth), 
              Add#(b__, TLog#(TDiv#(n_entries, banks)), TLog#(n_entries)), 
              Add#(TSub#(log_entries, log_epb), c__, TLog#(n_entries)));
    Integer entries_per_bank=valueOf(epb);
    Reg#(Bit#(TLog#(n_entries))) rg_address <- mkReg(0);
    
    BRAM_DUAL_PORT#(Bit#(TLog#(TDiv#(n_entries, banks))), Bit#(datawidth)) ram [valueOf(banks)];
    Reg#(Bit#(datawidth)) rg_output[valueOf(banks)][2];
    Reg#(Bool) rg_read_req_made <- mkDReg(False);
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram[i]<-mkBRAMCore2(valueOf(epb), False);
      rg_output[i]<- mkCReg(2,0);
    end
    
    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(rg_read_req_made && !ramreg);
        rg_output[i][0]<=ram[i].a.read;
      endrule
      rule capture_output_reg(ramreg);
        rg_output[i][1]<=ram[i].a.read;
      endrule
    end
    
    method Action read_request(Bit#(TLog#(n_entries)) index);
      for(Integer i=0;i<valueOf(banks);i=i+1)
        ram[i].a.put(False, truncate(index),  ?);
      rg_address<= index;
      rg_read_req_made<=True;
    endmethod
    method ActionValue#(Bit#(datawidth)) read_response;
      Bit#(datawidth) data_resp [valueOf(banks)];
      for(Integer i=0;i<valueOf(banks);i=i+1)
        data_resp[i]=rg_output[i][1];
      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
      return data_resp[selection_index];
    endmethod
    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data);
      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
      ram[selection_index].b.put(True, truncate(address), data);
    endmethod
  endmodule:mkmem_config_v
  
  module mkmem_config_vbe#(parameter Bool ramreg)(Ifc_mem_config_be#(n_entries, datawidth,  banks))
    provisos( Log#(n_entries, log_entries), 
              Div#(n_entries, banks, epb), 
              Div#(datawidth, 8, we_line), 
              Mul#(epb, banks, n_entries), 
              Log#(epb, log_epb), 
              Add#(a__, TDiv#(datawidth, banks), datawidth), 
              Add#(b__, TLog#(TDiv#(n_entries, banks)), TLog#(n_entries)), 
              Add#(TSub#(log_entries, log_epb), c__, TLog#(n_entries)), 
              Mul#(TDiv#(datawidth, we_line), we_line, datawidth));
    Integer entries_per_bank=valueOf(epb);
    Reg#(Bit#(TLog#(n_entries))) rg_address <- mkReg(0);
    
    BRAM_DUAL_PORT_BE#(Bit#(TLog#(TDiv#(n_entries, banks))), Bit#(datawidth),  we_line) ram [valueOf(banks)];
    Reg#(Bit#(datawidth)) rg_output[valueOf(banks)][2];
    Reg#(Bool) rg_read_req_made <- mkDReg(False);
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram[i]<-mkBRAMCore2BE(valueOf(epb), False);
      rg_output[i]<- mkCReg(2,0);
    end
    
    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(rg_read_req_made && !ramreg);
        rg_output[i][0]<=ram[i].a.read;
      endrule
      rule capture_output_reg(ramreg);
        rg_output[i][1]<=ram[i].a.read;
      endrule
    end
    
    method Action read_request(Bit#(TLog#(n_entries)) index);
      for(Integer i=0;i<valueOf(banks);i=i+1)
        ram[i].a.put(0, truncate(index),  ?);
      rg_address<= index;
      rg_read_req_made<=True;
    endmethod
    method Bit#(datawidth) read_response;
      Bit#(datawidth) data_resp [valueOf(banks)];
      for(Integer i=0;i<valueOf(banks);i=i+1)
        data_resp[i]=rg_output[i][1];
      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
      return data_resp[selection_index];
    endmethod
    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data,
        Bit#(TDiv#(datawidth, 8)) we);
      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
      ram[selection_index].b.put(we, truncate(address), data);
    endmethod
  endmodule:mkmem_config_vbe

//  (*synthesize*)
//  module mkTb(Empty);
//    Ifc_mem_config#(64, 256, 4) myram <- mkmem_config_h;
//    Ifc_mem_config#(64, 256, 4) myram1 <- mkmem_config_v;
//    Ifc_mem_config_be#(64, 256, 4) myram2 <- mkmem_config_hbe;
//    Ifc_mem_config_be#(64, 256, 4) myram3 <- mkmem_config_vbe;
//  endmodule
endpackage
