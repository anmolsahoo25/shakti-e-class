/* 
Copyright (c) 2013, IIT Madras All rights reserved.

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
Details:

--------------------------------------------------------------------------------------------------
 */
package riscv;

  // library imports
  import GetPut::*;
  import Connectable::*;
  import FIFO::*;
  import FIFOF::*;
  import SpecialFIFOs :: *;
  import TxRx :: *;

  // project imports
  import common_types :: *;
  import stage1 :: *;
  import stage2 :: *;
  import stage3 :: *;
  `include "common_params.bsv"
  `include "Logger.bsv"

  interface Ifc_riscv;
    // interface between fetch and fabric
    interface Get#(InstRequest) inst_request;
    interface Put#(InstResponse) inst_response;

    // interface between memory - stage and fabric
    interface Get#(MemoryRequest) memory_request;
    interface Put#(MemoryResponse) memory_response;

    // side - band connections to the csr
    method Action clint_msip(Bit#(1) intrpt);
    method Action clint_mtip(Bit#(1) intrpt);
    method Action clint_mtime(Bit#(64) c_mtime);
    method Action ext_interrupt(Bit#(1) intrpt);

    `ifdef rtldump
      interface Get#(DumpType) dump;
    `endif

    method Bit#(2) mv_curr_priv;
  endinterface : Ifc_riscv

  (*synthesize*)
  module mkriscv#(parameter Bit#(`vaddr) resetpc) (Ifc_riscv);
    // instantiate each stage here
    Ifc_stage1              stage1          <- mkstage1(resetpc);
    Ifc_stage2              stage2          <- mkstage2();
    Ifc_stage3              stage3          <- mkstage3();

    mkChan(mkLFIFOF()   , stage1.tx_stage1_operands , stage2.rx_stage1_operands);
    mkChan(mkLFIFOF()   , stage1.tx_stage1_control  , stage2.rx_stage1_control);
    mkChan(mkLFIFOF()   , stage1.tx_stage1_meta     , stage2.rx_stage1_meta);

    mkChan(mkLFIFOF()   , stage2.tx_stage3_common   , stage3.rx_stage3_common);
    mkChan(mkLFIFOF()   , stage2.tx_stage3_type     , stage3.rx_stage3_type);

  `ifdef rtldump
    mkChan(mkLFIFOF()   , stage1.tx_stage1_dump     , stage2.rx_stage1_dump);
    mkChan(mkLFIFOF()   , stage2.tx_stage3_dump     , stage3.rx_stage3_dump);
  `endif
    
    mkConnection(stage3.commit_rd         , stage1.commit_rd);
    mkConnection(stage3.operand_fwding    , stage2.operand_fwding);

    mkConnection(stage1.ma_csr_decode     , stage3.mv_csr_decode);
    mkConnection(stage1.ma_csr_misa_c     , stage3.mv_csr_misa_c);
    mkConnection(stage2.ma_csr_misa_c     , stage3.mv_csr_misa_c);
  `ifdef triggers
    mkConnection(stage1.ma_trigger_data1  , stage3.mv_trigger_data1);
    mkConnection(stage1.ma_trigger_data2  , stage3.mv_trigger_data2);
    mkConnection(stage1.ma_trigger_enable , stage3.mv_trigger_enable);
    mkConnection(stage2.ma_trigger_data1  , stage3.mv_trigger_data1);
    mkConnection(stage2.ma_trigger_data2  , stage3.mv_trigger_data2);
    mkConnection(stage2.ma_trigger_enable , stage3.mv_trigger_enable);
  `endif

    let {newpc, fl}=stage3.flush; 

    // TODO ma_interrupt to stage1 to be connected to interrupt from stage3
    rule connect_interrupt;
      stage1.ma_interrupt(False);
    endrule

    rule flush_from_writeback(fl); 
      stage1.ma_flush(newpc);
      stage2.ma_flush();
    endrule

    interface inst_request = stage1.inst_request;
    interface inst_response = stage1.inst_response;

    interface memory_request = stage2.memory_request;
    interface memory_response = stage3.memory_response;

    method Action clint_msip(Bit#(1) intrpt);
      stage3.clint_msip(intrpt);
    endmethod
    method Action clint_mtip(Bit#(1) intrpt);
      stage3.clint_mtip(intrpt);
    endmethod
    method Action clint_mtime(Bit#(64) c_mtime);
      stage3.clint_mtime(c_mtime);
    endmethod
    method Action ext_interrupt(Bit#(1) intrpt);
      stage3.ext_interrupt(intrpt);
    endmethod
    `ifdef rtldump
      interface dump = stage3.dump;
    `endif
    method mv_curr_priv = stage3.mv_curr_priv;
  endmodule : mkriscv
endpackage : riscv
