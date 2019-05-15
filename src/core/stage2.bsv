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

Author: Neel Gala, Aditya Mathur
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
 */
package stage2;
  // library packages 
  import GetPut::*;
  import RegFile::*;
  import FIFOF::*;
  import DReg::*;
  import UniqueWrappers ::*;
  import TxRx ::*;

  // project packages
  import common_types::*;
  import alu::*;
  `include "common_params.bsv"

  interface Ifc_stage2;
    // interface to send decoded instruction to the next stage
    interface RXe#(STAGE1_operands) rx_stage1_operands;
  
    interface RXe#(STAGE1_meta) rx_stage1_meta;

    interface RXe#(STAGE1_control) rx_stage1_control;

  `ifdef rtldump
    interface RXe#(STAGE1_dump) rx_stage1_dump ;
  `endif
    
    //result being transfered to the memory and write back
    interface TXe#(PIPE2_DS) to_mem_wb_unit;
  
    //rd,valid and value given back by the mem and wb unit for eliminating congestion
    interface Put#(OpFwding) operand_fwding;
    
    // memory request interface in case of Load/Store instruction
    interface Get#(MemoryRequest) memory_request;
  
    method Action flush_from_wb;
    method Action csr_updated (Bool upd);
    method Action misa_c_from_csr (Bit#(1) c);
    `ifdef atomic
      interface Put#(Tuple3#(Bit#(XLEN), Bool, Access_type)) atomic_response;
    `endif
  endinterface:Ifc_stage2
  
  (*synthesize*)
  module mkstage2(Ifc_stage2);

    Wire#(Bit#(1)) wr_misa_c <- mkWire();
     
    // generating the register file
    Reg#(Bit#(1)) rg_epoch[2] <- mkCReg(2,0);
    Reg#(OpFwding) wr_opfwding <- mkDWire(unpack(0));
    FIFOF#(MemoryRequest) ff_memory_request <- mkSizedFIFOF(2);

  `ifdef RV64
    Wrapper3#(ALU_Inputs,Bool,Bit#(1),ALU_OUT) alu_wrapper <- mkUniqueWrapper3(fn_alu);
  `else
    Wrapper2#(ALU_Inputs,Bit#(1),ALU_OUT) alu_wrapper <- mkUniqueWrapper2(fn_alu);
  `endif

  `ifdef atomic
    FIFOF#(Tuple3#(Bit#(XLEN), Bool, Access_type)) ff_atomic_response <- mkSizedFIFOF(2);
    Reg#(Bit#(`paddr)) rg_atomic_address<- mkReg(0);
    Reg#(Bit#(XLEN)) rg_op2 <- mkReg(0);
    Reg#(Maybe#(Bit#(`paddr))) rg_loadreserved_addr <- mkReg(tagged Invalid);
  `endif

    // If a CSR operation is detected then you need to stall fetching operands from the regfile
    // untill the csr instruction has been committed. the forwarding path from the csr operation to
    // the ALU is huge. This way we break the path and neither flush the entire pipe.
    // Flushing the entire pipe will lead to fetching the same instruction again.
    // However,  if we do add csrs which affect how an instruction is fetched (protection,  etc)
    // then the entire pipe will have to flushed. 
    // There does exist mechanism in the last stage to flush pipe on a trap. in case a full flush is
    // required,  that particular method should be excited.
    Reg#(Bool) rg_csr_stall[2] <- mkCReg(2,False);
  `ifdef muldiv
    Ifc_alu alu <-mkalu;
    Reg#(Bool) rg_stall <- mkReg(False);
  `elsif atomic
    Reg#(Bool) rg_stall <- mkReg(False);
  `endif


  `ifdef muldiv
    `ifdef atomic
      Reg#(Bool) rg_muldiv_atomic <- mkReg(False); // False=muldiv,  True=atomic
    `endif
  `endif
  
    // the fifo to communicate with the previous stage.
    RX#(STAGE1_operands) ff_stage1_operands <- mkRX;
    RX#(STAGE1_meta) ff_stage1_meta <- mkRX;
    RX#(STAGE1_control) ff_stage1_control <- mkrX;
  `ifdef rtldump
    RX#(STAGE1_dump) ff_stage1_dump <- mkRX;
  `endif

    TX#(PIPE2_DS) tx<-mkTX;

    rule fetch_execute_pass(!initialize `ifdef muldiv && !rg_stall `elsif atomic && !rg_stall 
        `endif && !rg_csr_stall[0] );
      // receiving the decoded data from the previous stage
      let {fn, rs1, rs2, rd, imm `ifdef RV64, word32 `endif , funct3, rs1_type, rs2_type, 
          insttype, mem_access, pc, trap, epoch_atomicop `ifdef rtldump , inst `endif }=rx.u.first;
      `ifdef atomic
        Bit#(1) epoch=epoch_atomicop[0];
        Bit#(4) atomic_op=epoch_atomicop[5:2];
      `else
        Bit#(1) epoch=epoch_atomicop;
      `endif
      // rs1,rs2 will be passed to the register file and the recieve value along with the other 
      // parameters reqiured by the alu function will be passed
      let {op1, op2, op3, available}=operand_provider(rs1, rs1_type, rs2, rs2_type, pc, insttype, 
                                                      imm, mem_access);
      ALU_Inputs inp1=tuple8(fn, op1, op2, imm, op3, insttype, funct3,mem_access);

      `ifndef muldiv
        let {committype, op1_reslt, effaddr_csrdata, trap1} <- alu_wrapper.func(inp1 
            `ifdef RV64, word32 `endif , wr_misa_c );
      `endif
      if(epoch==rg_epoch[0])begin
        //passing the result to next stage via fifo
        if(available)begin
          
          `ifdef muldiv
            let {done, committype, op1_reslt, effaddr_csrdata, trap1} <- alu.get_inputs(inp1
              `ifdef RV64 ,word32 `endif , wr_misa_c);
          `endif
          
          `ifdef atomic
            if(mem_access==Atomic)begin
              rg_op2<= op2;
              rg_atomic_address<= truncate(effaddr_csrdata);
            end
            if(committype==MEMORY) begin 
              if(mem_access==Atomic && epoch_atomicop[5:1]=='b00010) // LR
                rg_loadreserved_addr<=tagged Valid truncate(effaddr_csrdata);
              else
                rg_loadreserved_addr<=tagged Invalid;
            end
            Bool perform_memory=True;
            
            if(committype==MEMORY && mem_access==Atomic && epoch_atomicop[5:1]=='b00011)begin // SC
              if(rg_loadreserved_addr matches tagged Valid .a &&& truncate(effaddr_csrdata)!=a)begin
                $display($time,"\tSTAGE2: StoreConditional Failed");
                perform_memory=False;
                op1_reslt=1;
                committype=REGULAR;
              end
              else if(rg_loadreserved_addr matches tagged Invalid)begin
                perform_memory=False;
                op1_reslt=1;
                committype=REGULAR;
              end
              else
                op1_reslt=0;
            end
            if(insttype==MEMORY && mem_access==Atomic)begin
              if(epoch_atomicop[5:1]=='b00010)
                mem_access=Load;
              else if(epoch_atomicop[5:1]=='b00011)
                mem_access=Store;
            end
          `endif

          Trap_type final_trap=trap matches tagged None?trap1:trap;
          if(committype == MEMORY &&& final_trap matches tagged None `ifdef atomic &&&
                                                                            perform_memory `endif )
            ff_memory_request.enq(tuple5(truncate(effaddr_csrdata), op2, mem_access,
                                                                        funct3[1:0], ~funct3[2]));
          `ifdef atomic
            `ifdef muldiv
              if(committype==MEMORY && mem_access==Atomic)begin
                done=False;
              end
            `endif
          `endif

          if(insttype==SYSTEM_INSTR)begin
            rg_csr_stall[0]<= True;
          end
        `ifdef muldiv 
          if(done) begin 
            rx.u.deq;
          `ifdef rtldump
            tx.u.enq(tuple8(committype,op1_reslt, effaddr_csrdata, pc, rd, rg_epoch[0], 
                final_trap, inst));
          `else
            tx.u.enq(tuple7(committype,op1_reslt, effaddr_csrdata, pc, rd, rg_epoch[0], 
                final_trap));
          `endif
          end
          else begin
            rg_stall<= True;
            `ifdef atomic
              if(committype==MEMORY)
                rg_muldiv_atomic<= True;
              else
                rg_muldiv_atomic<= False;
            `endif
          end
        `else
          `ifdef atomic
            if(committype==MEMORY && mem_access==Atomic)begin
              rg_stall<= True;
            end
            else begin
          `endif
              rx.u.deq;
            `ifdef rtldump
              tx.u.enq(tuple8(committype,op1_reslt, effaddr_csrdata, pc, rd, rg_epoch[0], 
                  final_trap, inst));
            `else
              tx.u.enq(tuple7(committype,op1_reslt, effaddr_csrdata, pc, rd, rg_epoch[0], 
                  final_trap));
            `endif
          `ifdef atomic
            end
          `endif
        `endif
        end
      end
      else begin
        rx.u.deq;
      end
    endrule
 
    `ifdef muldiv
      rule capture_stalled_output(rg_stall `ifdef atomic && !rg_muldiv_atomic `endif );
        let {fn, rs1, rs2, rd, imm `ifdef RV64 , word32 `endif , funct3, rs1_type, 
        rs2_type, insttype, mem_access, pc, trap, `ifdef atomic epoch_atomicop `else epoch `endif 
                `ifdef rtldump , inst `endif }=rx.u.first;
        `ifdef atomic
          Bit#(1) epoch=epoch_atomicop[0];
        `endif
        let {committype, op1_reslt, effaddr_csrdata, trap1} <- alu.delayed_output;
        if(epoch==rg_epoch[0])begin
          `ifdef rtldump
            tx.u.enq(tuple8(committype,op1_reslt, effaddr_csrdata, pc, rd, rg_epoch[0], trap, inst));
          `else
            tx.u.enq(tuple7(committype,op1_reslt, effaddr_csrdata, pc, rd, rg_epoch[0], trap));
          `endif
        end
        rg_stall<= False;
        rx.u.deq;
      endrule
    `endif
    `ifdef atomic
      rule atomic_second_phase(rg_stall `ifdef muldiv && rg_muldiv_atomic `endif );
        rg_stall<= False;
        let {data, err, access}=ff_atomic_response.first;
        ff_atomic_response.deq;
        let {fn, rs1, rs2, rd, imm `ifdef RV64 , word32 `endif ,funct3, rs1_type, rs2_type, 
            insttype, mem_access, pc, trap, `ifdef atomic epoch_atomicop `else epoch `endif 
                `ifdef rtldump , inst `endif }=rx.u.first;
        `ifdef atomic
          Bit#(4) atomic_op=epoch_atomicop[5:2];
          Bit#(1) maxop=epoch_atomicop[1];
          Bit#(1) epoch=epoch_atomicop[0];
        `endif
        rx.u.deq;
        if(epoch==rg_epoch[0])begin
          `ifdef muldiv
            let {done, committype, op1_reslt, effaddr_csrdata, trap1} <- 
                  alu.get_inputs(tuple8(atomic_op, data, rg_op2, 0, 0, ALU, funct3, mem_access) 
                      `ifdef RV64 , word32 `endif , wr_misa_c);
          `else
            let {committype, op1_reslt, effaddr_csrdata, trap1} <-
                alu_wrapper.func(tuple8(atomic_op, data, rg_op2, 0, 0, ALU, funct3, mem_access)
                  `ifdef RV64 , word32 `endif  , wr_misa_c);
          `endif
          if(&atomic_op==1)begin // AMOSWAP
            op1_reslt=rg_op2;
          end
          if(atomic_op == 'b1100 || atomic_op == 'b1110)begin // AMOMAX[U], AMOMIN[U]
            op1_reslt=(op1_reslt[0]^maxop)==1?data:rg_op2;
          end
          if(err)
            trap = tagged Exception Store_access_fault;
          else
            ff_memory_request.enq(tuple5(rg_atomic_address, op1_reslt, Store, funct3[1:0], ~funct3[2]));
          Commit_type committype1=MEMORY;                                         
          `ifdef rtldump
            tx.u.enq(tuple8(committype1, data, {1'b0, rg_atomic_address}, pc, rd, rg_epoch[0], trap, inst));
          `else
            tx.u.enq(tuple7(committype1, data, {1'b0, rg_atomic_address}, pc, rd, rg_epoch[0], trap));
          `endif
        end
      endrule
    `endif
    // interface definition

    // interface to receive decoded instruction to the previous stage
    interface rx_stage1_operands = ff_stage1_operands.e;
  
    interface rx_stage1_meta = ff_stage1_meta.e;

    interface rx_stage1_control = ff_stage1_control.e;

  `ifdef rtldump
    interface rx_stage1_dump = ff_stage1_dump.e;
  `endif
    
    interface to_mem_wb_unit=tx.e;
   
    // the memory_wb stage has to ensure that it sends only 0 when there is no data
    // to be forwarded
    interface operand_fwding=interface Put
      method Action put (Tuple3#(Bit#(5),Bool,Bit#(XLEN)) from_mem_to_opfetch );
        let {rd, valid, data} =  from_mem_to_opfetch;
        wr_opfwding <= from_mem_to_opfetch;
      endmethod 
    endinterface;
    
    interface commit_rd=interface Put
      method Action put (Tuple2#(Bit#(5),Bit#(XLEN)) from_mem_to_rf ) if(!initialize);
        let {rd,value} = from_mem_to_rf;
          integer_rf.upd(rd,value);
      endmethod
    endinterface;
    
    interface memory_request = interface Get
      method ActionValue#(MemoryRequest) get ;
        ff_memory_request.deq;
        return ff_memory_request.first;
      endmethod
    endinterface;
    `ifdef atomic
      interface atomic_response=interface Put
        method Action put(Tuple3#(Bit#(XLEN), Bool, Access_type) resp);
          ff_atomic_response.enq(resp);
        endmethod
      endinterface;
    `endif

    method Action flush_from_wb; //fence integration
        rg_epoch[1]<=~rg_epoch[1];
        ff_memory_request.clear();
    endmethod
    method Action csr_updated (Bool upd);
      if(upd) begin
        rg_csr_stall[1]<= False;
      end
    endmethod
    method Action misa_c_from_csr (Bit#(1) c);
      wr_misa_c<=c;
    endmethod
  endmodule:mkstage2
endpackage:stage2
