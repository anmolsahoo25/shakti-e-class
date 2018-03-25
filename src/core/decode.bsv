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

  (*noinline*)
  function PIPE1_DS decoder_func(Bit#(32) inst,Bit#(PADDR) shadow_pc, Bit#(1) epoch, Bool err, 
                                                                               CSRtoDecode csrs);
    let {prv, mip, mie, mideleg}=csrs;
		Bit#(5) rs1=inst[19:15];
		Bit#(5) rs2=inst[24:20];
		Bit#(5) rd =inst[11:7] ;
		Bit#(5) opcode= inst[6:2];
		Bit#(3) funct3= inst[14:12];
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
		Bit#(XLEN) immediate_value=signExtend(inst[31:20]);
		if(opcode==`JAL_R_op)
			immediate_value=signExtend({inst[31:20],1'b0});
		else if(opcode==`BRANCH_op)
			immediate_value=signExtend({inst[31],inst[7],inst[30:25],inst[11:8],1'b0}); 
		else if	(opcode==`STORE_op)
			immediate_value=signExtend({inst[31:25],inst[11:7]});
		else if(opcode==`SYSTEM_INSTR_op)//what should be done for systems instruction		
			immediate_value[16:12]=inst[19:15];

		//instruction following U OR UJ TYPE INSTRUCTION FORMAT	
		//funct3[2]==1 might not be required as division is not included till now
		if (opcode==`JAL_R_op || (opcode==`SYSTEM_INSTR_op && funct3[2]==1))	
			rs1=0;
		//instruction following I,U OR UJ INSTRUCTION FORMAT	
		if (opcode==`SYSTEM_INSTR_op || opcode[4:2]=='b000// CSR or ( (F)Load or FENCE ) 
  			||opcode[4:2]=='b001 || opcode==`JAL_R_op)	//LUI or JAL 
			rs2=0;
		//insturction following S OR SB TYPE INSTRUCTION FORMAT
		if (opcode==`BRANCH_op || opcode[4:1]=='b0100)	
			rd=0;

		if(opcode==`JAL_R_op)	
			rs1type=PC;
		if(opcode==`JAL_R_op || opcode=='b001)	
			rs2type=Immediate;
		
		//instructions which support word lenght operation in RV64 are to be added in Alu
		//need to be edited according to the supported instruction

		//if(opcode==`IMM_ARITHW_op || opcode==`MULDIVW_op ||
		 //opcode==`ARITHW_op ||(opcode[4:3]=='b10 && funct7[0]==0)||
		  //(opcode[4:1]=='b0101 && funct3[0]==0)) 
    		//word32=True;
    			

    Instruction_type inst_type=ILLEGAL;
    if(opcode[4:3]=='b11)begin
    	case(opcode[2:0])
    		'b001:inst_type=JAL_R;
    		'b000:inst_type=BRANCH;
    		'b100:inst_type=SYSTEM_INSTR;
    	endcase
    end
    else if(opcode[4:3]=='b00)begin
    	case(opcode[2:0])
    		'b000,'b001:inst_type=MEMORY;
    		'b101,'b100,'b110:inst_type=ALU;
    	endcase
    end
    Bit#(4) fn=0;
    if(opcode==`BRANCH_op)begin
    	if(funct3[2]==0)
    		fn={2'b0,1,funct3[0]};
    	else
    		fn={1'b1,funct3}	;
    end
    else if(opcode==`JAL_R_op || opcode==`LOAD_op || opcode==`STORE_op)
    	fn=0;
    else if(opcode==`IMM_ARITH_op)begin
		  fn=case(funct3)
		  	'b010: 'b1100;
		  	'b011: 'b1110;
		  	'b101: 'b0101;
		  	default:{1'b0,funct3};
		  endcase;
		end
		else if(opcode==`ARITH_op)begin
			fn=case(funct3)
				'b000:'b0000;
				'b010:'b1100;
				'b011:'b1110;
				'b101:'b0101;
				default:{1'b0,funct3};
		endcase;
		end		
  	else if(opcode[4:3]=='b10)	
 		  fn=opcode[3:0];

    ExcpStage1 exception = None;
		Bool address_is_valid=address_valid(inst[31:20]);
		Bool access_is_valid=valid_csr_access(inst[31:20],inst[19:15], inst[13:12], prv);
    if(pc[1:0]!=0)
      exception = Inst_addr_misaligned;
    else if(err)
      exception = Inst_access_fault;
    else if(inst_type==ILLEGAL)
	    exception = Illegal_inst;
    else if(inst_type == SYSTEM_INSTR)begin
      if(funct3 == 0)
        case(inst[31:20])
          'h000: exception = (prv==User)?Ecall_from_user:Ecall_from_machine; // ECALL
          'h001: exception = Breakpoint;
          'h302: exception = (prv!=Machine)?Illegal_inst:None;
          default: exception = None;
        endcase
      else begin // CSR read write operation
  		  if(!(address_is_valid && access_is_valid))
          exception = Illegal_inst;
      end
    end

    Tuple7#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), ExcpStage1, 
    Bit#(1)) type_tuple = tuple7(rs1type, rs2type, inst_type, mem_access, pc, exception, epoch);

    return tuple8(fn, rs1, rs2, rd, immediate_value, word32, funct3, type_tuple);            
  endfunction
endpackage