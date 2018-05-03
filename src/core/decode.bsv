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
package decode;
  
  // pacakge imports from project
  import common_types::*;
  `include "common_params.bsv"
	  
  function Bool address_valid(Bit#(12) csr_address);
		case(csr_address[11:8])
			'h3: begin // machine read-write registers
				if((csr_address[7:0]>'h6 && csr_address[7:0]<'h23) || 
				  (csr_address[7:0]>'h26 && csr_address[7:0]<'h40) ||
				  (csr_address[7:0]>'h44 && csr_address[7:0]<='hA0) ||
				  (csr_address[7:0]>'hA3 && csr_address[7:0]<'hB8) ||
				  (csr_address[7:0]>'hbf))
					return False;
				else
					return True;
			end
			'hB:begin
				if((csr_address[7:0]>'h6 && csr_address[7:0]<'h80 && csr_address[7:0]!='h20)||
				 (csr_address[7:0]>'h86 && csr_address[7:0]<'hA0)||
				 (csr_address[7:0]>'hA6))
					return False;
				else
					return True;
			end
			'hC:begin
				if((csr_address[7:0]>'h6 && csr_address[7:0]<'h83)|| 
				 (csr_address[7:0]>'h86))
					return False; 
				else
					return True;
			end
			'hF:begin
				if(csr_address[7:0]<'h11 || csr_address[7:0]>'h15)
					return False;
				else
					return True;
			end
			default:return False;
		endcase
	endfunction
	
  function Bool hasCSRPermission(Bit#(12) address, Bool write,  Privilege_mode prv);
    Bit#(12) csr_index = pack(address);
    return ((pack(prv) >= csr_index[9:8]) && !(write && csr_index[11:10]==2'b11) );
  endfunction
   
  // if the operand is not 0 then the instruction will perform a write on the CSR.
	function Bool valid_csr_access(Bit#(12) csr_addr, Bit#(5) operand, Bit#(2) operation,
                                                                              Privilege_mode prv);
		Bool ret = hasCSRPermission(unpack(csr_addr), (operand != 0 || operation=='b01) ? True:False,
                                                                                              prv);
		return ret;
	endfunction
	
	function Trap_type chk_interrupt(Privilege_mode prv, Bit#(12) mip, Bit#(12) csr_mie, 
                                                        Bit#(12) mideleg,  Bit#(1) mie);
		Bit#(12) pending_interrupts = (truncate(mip)) & truncate(csr_mie) ;
		let pending_machine_interrupts = pending_interrupts & ~truncate(mideleg);
		let machine_interrupts_enabled = (mie == 1) || (prv != Machine);
		pending_interrupts =	(machine_interrupts_enabled ? pending_machine_interrupts : 0);

		// format pendingInterrupt value to return
		Trap_type ret = tagged None;
		if (pending_interrupts != 0) begin
			ret = tagged Interrupt unpack(zeroExtend(pack(countZerosLSB(pending_interrupts))));
		end
		return ret;
	endfunction

  (*noinline*)
  function PIPE1_DS decoder_func(Bit#(32) inst,Bit#(PADDR) shadow_pc, Bit#(1) epoch, Bool err, 
                                                                               CSRtoDecode csrs);
    let {prv, mip, csr_mie, mideleg, misa, counteren, mie}=csrs;

    Trap_type exception = tagged None;
    Trap_type interrupt = chk_interrupt(prv, mip, csr_mie, mideleg, mie);

		Bit#(5) rs1=inst[19:15];
		Bit#(5) rs2=inst[24:20];
		Bit#(5) rd =inst[11:7] ;
		Bit#(5) opcode= inst[6:2];
		Bit#(3) funct3= inst[14:12];
    Bit#(7) funct7 = inst[31:25]; 
		Bool word32 =False;
		Bit#(PADDR) pc=shadow_pc;
    
		//operand types
		Operand1_type rs1type=IntegerRF;
		Operand2_type rs2type=IntegerRF;

		//memory access type
		Access_type mem_access=Load;
		if(opcode[3]=='b1 && opcode[1]==0)
			mem_access=Store;

		//immediate value 
		Bit#(32) immediate_value=signExtend(inst[31:20]);
    if(opcode==`LUI_op|| opcode==`AUIPC_op) 
      immediate_value=signExtend({inst[31:12],12'd0}); 
   else if(opcode==`JAL_op) 
      immediate_value=signExtend({inst[31], inst[19:12], inst[20], inst[30:21],1'b0}); 
   else if(opcode==`JALR_op) 
      immediate_value=signExtend({inst[31:21],1'b0}); 
		else if(opcode==`BRANCH_op)
			immediate_value=signExtend({inst[31],inst[7],inst[30:25],inst[11:8],1'b0}); 
		else if	(opcode==`STORE_op)
			immediate_value=signExtend({inst[31:25],inst[11:7]});
		else if(opcode==`SYSTEM_INSTR_op)//what should be done for systems instruction		
			immediate_value[16:12]=inst[19:15];

    // Following table describes what the ALU will need for some critical operations. Based on this
    // the next set of logic is implemented. rs1+ rs2 is a XLEN bit adder. rs3+ rs4 is PADDR bit
    // adder.
    //
    //          rs1   rs2   rs3   rs4
    // Branch   OP1   OP2   PC    Imm
    // JAL      PC    'd4   PC    Imm   (rs1=0, rs2=0 since neither required)
    // JALR     PC    'd4   op1   Imm   (rs2=0 since not required)
    // LOAD                 op1   Imm   (rs2=0 since not required)
    // STORE                op1   Imm   (both required. op2 is the data)
    // AUIPC    PC    Imm               (rs1=0, rs2=0 since neither required)
    // LUI      0 Imm                   (rs1=0, rs2=0 since neither required)
    /////////////////////////////////////////////////////////////////////////////////

		//instruction following U OR UJ TYPE INSTRUCTION FORMAT	
		//funct3[2]==1 might not be required as division is not included till now
		if (opcode==`JAL_op  || opcode==`LUI_op || opcode==`AUIPC_op || 
        (opcode==`SYSTEM_INSTR_op && funct3[2]==1))	
			rs1=0;
		//instruction following I,U OR UJ INSTRUCTION FORMAT	
		if (opcode==`SYSTEM_INSTR_op || opcode[4:2]=='b000 || opcode==`LUI_op // CSR or (Load) or LUI 
  			 ||opcode == `AUIPC_op || opcode==`JAL_op || opcode==`JALR_op)	// AUIPC or JAL or JALR
			rs2=0;
		//insturction following S OR SB TYPE INSTRUCTION FORMAT
		if (opcode==`BRANCH_op || opcode[4:1]=='b0100)	
			rd=0;

		if(opcode==`JAL_op || opcode==`JALR_op|| opcode==`AUIPC_op)	
			rs1type=PC;

		if(opcode==`JALR_op || opcode==`JAL_op)
      rs2type=Constant4;
    else if(opcode[4:2] == 'b001 || opcode==`LUI_op )
			rs2type=Immediate;
		
		//instructions which support word lenght operation in RV64 are to be added in Alu
		//need to be edited according to the supported instruction

    `ifdef RV64
  		if(opcode==`IMM_ARITHW_op `ifdef muldiv || opcode==`MULDIVW_op `endif || opcode==`ARITHW_op ||
          (opcode[4:1]=='b0101 && funct3[0]==0)) 
      	word32=True;
    `endif
    			

    Instruction_type inst_type=ALU;
    if(opcode[4:3]=='b11)begin
    	case(opcode[2:0])
    		'b001:inst_type=JALR; 
        'b011:inst_type=JAL;
    		'b000:inst_type=BRANCH;
    		'b100:inst_type=SYSTEM_INSTR;
    	endcase
    end
    else if(opcode[4:3]=='b01)begin 
      case (opcode[2:0])  
         'b000:inst_type=MEMORY; // STORE
         'b101:inst_type=ALU;      // LUI 
         'b100,'b110: begin 
            if(funct7[0]==0)
              inst_type=ALU;
            `ifdef muldiv 
              else
                inst_type=MULDIV; 
            `endif
          end
      endcase 
    end 
    else if(opcode[4:3]=='b00)begin
    	case(opcode[2:0])
    		'b000,'b001:inst_type=MEMORY;
    		'b101,'b100,'b110:inst_type=ALU;
    	endcase
    end
    else
      exception = tagged Exception Illegal_inst;
		Bit#(4) fn=0;
		if(opcode==`BRANCH_op)begin
			if(funct3[2]==0)
				fn={2'b0,1,funct3[0]};
			else
				fn={1'b1,funct3};
		end
//		else if(opcode==`JAL_op || opcode==`JALR_op || opcode==`LOAD_op	|| opcode==`STORE_op ||
//                                                              opcode==`AUIPC_op || opcode==`LUI_op)
//			fn=0;
		else if(`ifdef RV64 opcode==`IMM_ARITHW_op || `endif opcode==`IMM_ARITH_op)begin
			fn=case(funct3)
				'b010: 'b1100;
				'b011: 'b1110;
				'b101: if(funct7[5]==1) 'b1011; else 'b0101;
				default:{1'b0,funct3};
			endcase;
		end
		else if(`ifdef RV64 opcode==`ARITHW_op || `endif opcode==`ARITH_op)begin
			fn=case(funct3)
				'b000:if(funct7[5]==1) 'b1010; else 'b0000;
				'b010:'b1100;
				'b011:'b1110;
				'b101:if (funct7[5]==1) 'b1011;else 'b0101;
				default:{1'b0,funct3};
			endcase;
		end

		Bool address_is_valid=address_valid(inst[31:20]);
		Bool access_is_valid=valid_csr_access(inst[31:20],inst[19:15], inst[13:12], prv);
    if(pc[1:0]!=0)
      exception = tagged Exception Inst_addr_misaligned;
    else if(err)
      exception = tagged Exception Inst_access_fault;
    else if( `ifdef atomic (inst_type==MEMORY && mem_access==Atomic && misa[0]==0) || `endif 
             `ifdef muldiv (inst_type==MULDIV && misa[12]==0) || `endif
             (inst_type==ALU && misa[8]==0) )
      exception=tagged Exception Illegal_inst; 
    else if(inst_type == SYSTEM_INSTR)begin
      if(funct3 == 0)
        case(inst[31:20])
          'h000: exception = tagged Exception ((prv==User)?Ecall_from_user:Ecall_from_machine);
          'h001: exception = tagged Exception Breakpoint;
          'h302: exception = (prv!=Machine)?tagged Exception Illegal_inst:tagged None;
          default: exception = tagged None;
        endcase
      else begin // CSR read write operation
  		  if(!(address_is_valid && access_is_valid))
          exception = tagged Exception Illegal_inst;
      end
    end
    if(interrupt matches tagged None)
      interrupt =  exception;

    `ifdef simulate 
      Tuple8#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), Trap_type, 
        Bit#(1) `ifdef simulate , Bit#(32) `endif ) type_tuple = tuple8(rs1type, rs2type, inst_type, 
          mem_access, pc, interrupt, epoch, inst);
    `else
      Tuple7#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), Trap_type, 
      Bit#(1)) type_tuple = tuple7(rs1type, rs2type, inst_type, mem_access, pc, interrupt, epoch);
    `endif
    return tuple8(fn, rs1, rs2, rd, signExtend(immediate_value), word32, funct3, type_tuple);            
  endfunction
endpackage
