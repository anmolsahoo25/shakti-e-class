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

Author: Neel Gala, Aditya Mathur,Deepa N Sarma
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package decode;
  
  // pacakge imports from project
  import common_types::*;
  import BUtils::*;
  `include "common_params.bsv"
	  
  function Bool address_valid(Bit#(12) csr_address);
		case(csr_address[11:8])
      `ifdef user
        'h0: begin
          if(csr_address[7:0]>'h00 && csr_address[7:0]<'h4)begin 
            return False;
          end
          else if (csr_address[7:0]=='h00 || csr_address[7:0]=='h4 || csr_address[7:0]=='h5 ||
          (csr_address[7:0]>='h40 && csr_address[7:0]<= 'h44)) begin
            `ifndef usertraps  
              return False;
            `else
              return True;
            `endif
          end
          else if(csr_address[7:0]>'h5 && csr_address[7:0]<'h40)
            return False;
          else
            return True;
        end
      `endif
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
      `ifdef Debug
        'h7:begin
          if(csr_address[7:0]<'hA0 || (csr_address[7:0]>'hA3 && csr_address[7:0]<'hb0) ||
              csr_address[7:0]>'hB2)
            return False;
          else
            return True;
        end
      `endif
			'hB:begin
				if( (csr_address[7:0]>('h2+ `Counters) `ifndef RV64 && csr_address[7:0]<'h80) ||
             csr_address[7:0]>('h82+ `Counters)) `else )) `endif 
					return False; 
				else
					return True;
			end
			'hC:begin
				if( (csr_address[7:0]>('h2+ `Counters) `ifndef RV64 && csr_address[7:0]<'h80) ||
             csr_address[7:0]>('h82+ `Counters)) `else )) `endif 
					return False; 
				else
					return True;
			end
			'hF:begin // MAchine MRO registers
				if(csr_address[7:0]<'h11 || csr_address[7:0]>'h14)
					return False;
				else
					return True;
			end
			default:return False;
		endcase
	endfunction
`ifdef compressed
  function Bit#(3) gen_funct3(Bit#(5) opcode,Bit #(16) inst);
    Bit #(3) funct3 =3'b000;
    
    
    case (opcode)
    
    5'b00000:funct3=3'b000;
    5'b01000:funct3=3'b000;
    5'b10000:funct3=3'b001;
    5'b01001:funct3=3'b000;
    5'b00010:funct3=3'b010;
    5'b01010:funct3=3'b000;
    5'b10010:funct3=3'b010;
    5'b00011:funct3=3'b011;
    5'b01011:funct3=3'b000;
    5'b10011:funct3=3'b011;
    5'b01100:
            if((inst[11:10]==2'b00)||(inst[11:10]==2'b01))
                funct3=3'b101;//SRLI,SRAI
            else if(inst[11:10]==2'b10)
                funct3=3'b111;//ANDI
            else if(inst[11:10]==2'b11)
            case({inst[6:5]})
                2'b00:funct3=3'b000;
                2'b01:if(inst[12]==1'b1)
                          funct3=3'b000;
                      else
                          funct3=3'b100;
                2'b10:funct3=3'b110;
                2'b11:funct3=3'b111;
              endcase
    5'b10100:funct3=3'b000;
    5'b00110:funct3=3'b010;
    5'b01110:funct3=3'b000;
    5'b10110:funct3=3'b010;
    5'b10111:funct3=3'b011;
    5'b00111:funct3=3'b011;
    5'b01111:funct3=3'b001;
    default:funct3=3'b000;
    endcase
    
    return funct3;

 endfunction
`endif
	
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

  `ifdef atomic
  function Bit#(5) fn_atomic(Bit#(5) op);
    Bit#(5) fn=0;
      fn= case(op)
        'b00001:'b11110;// AMOSWAP
        'b00000:'b00000;// AMOADD
        'b00100:'b01000; //AMOXOR
        'b01100:'b01110;// AMOAND
        'b01000:'b01100;// AMOOR
        'b10000:'b11000;// AMOMIN
        'b10100:'b11001;// AMOMAX
        'b11000:'b11100;// AMOMINU
        'b11100:'b11101;// AMOMAXU
        default:0;
      endcase;
    return fn;
  endfunction
  `endif

  (*noinline*)
  function PIPE1_DS decoder_func(Bit#(32) inst, Bit#(PADDR) pc, Bit#(1) epoch, Bool err, 
                                                                               CSRtoDecode csrs);
    let {prv, mip, csr_mie, mideleg, misa, counteren, mie}=csrs;

    Trap_type exception = tagged None;
    let interrupt = chk_interrupt(prv, mip, csr_mie, mideleg, mie);

		Bit#(5) rs1=inst[19:15];
		Bit#(5) rs2=inst[24:20];
		Bit#(5) rd =inst[11:7] ;
		Bit#(5) opcode= inst[6:2];
		Bit#(3) funct3= inst[14:12];
    Bit#(7) funct7 = inst[31:25]; 
		Bool word32 =False;
    
		//operand types
		Operand1_type rs1type=IntegerRF;
		Operand2_type rs2type=IntegerRF;

		//memory access type
		Access_type mem_access=Load;
		if(opcode[3]=='b1 && opcode[1]==0)
			mem_access=Store;
    `ifdef atomic
      else if(opcode=='b01011)
        mem_access=Atomic;
    `endif

    // Decoding the immediate values
    Bool stype= (opcode=='b01000); 
    Bool btype= (opcode=='b11000);
    Bool utype= (opcode=='b01101 || opcode=='b00101);
    Bool jtype= (opcode=='b11011);
    Bool atomictype=(opcode=='b01011);

    Bit#(1) bit0 = inst[20]; // because of I-type instructions
    `ifdef atomic
      if(atomictype)
        bit0=0;
      else
    `endif
    if(stype)
      bit0=inst[7];
    else if(btype || utype || jtype) 
      bit0=0;

    Bit#(4) bit1_4=inst[24:21]; // I/J-type instructions
    `ifdef atomic
      if(atomictype)
        bit1_4=0;
      else
    `endif
    if(stype || btype) // S/B-Type
      bit1_4=inst[11:8];
    else if(utype) // U type
      bit1_4=0;

    Bit#(6) bit5_10=inst[30:25];
    `ifdef atomic
      if(atomictype)
        bit5_10=0;
      else
    `endif
    if(utype)
      bit5_10=0;
    
    Bit#(1) bit11 = inst[31]; // I/S type
    `ifdef atomic
      if(atomictype)
        bit11=0;
      else
    `endif
    if(btype)
      bit11=inst[7];
    else if(utype)
      bit11=0;
    else if(jtype)
      bit11=inst[20];

    Bit#(8) bit12_19=duplicate(inst[31]); // I/S/B type
    `ifdef atomic
      if(atomictype)
        bit12_19=0;
      else
    `endif
    if(utype || jtype)
      bit12_19=inst[19:12];

    Bit#(11) bit20_30=duplicate(inst[31]); // I/B/S/J type
    `ifdef atomic
      if(atomictype)
        bit20_30=0;
      else
    `endif
    if(utype)
      bit20_30=inst[30:20];
    Bit#(1) bit31= `ifdef atomic (atomictype)?0: `endif inst[31];
    Bit#(32) immediate_value={bit31, bit20_30, bit12_19, bit11, bit5_10, bit1_4, bit0};

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
		if (opcode==`SYSTEM_INSTR_op || opcode[4:2]=='b000 || opcode==`LUI_op // CSR or (Load) or LUI 
  			 ||opcode == `AUIPC_op || opcode==`JAL_op || opcode==`JALR_op)	// AUIPC or JAL or JALR
			rs2=0;
		if (opcode==`BRANCH_op || opcode[4:1]=='b0100)	
			rd=0;

		if(opcode==`JAL_op || opcode==`JALR_op|| opcode==`AUIPC_op 
        `ifdef atomic || opcode=='b01011 `endif )	
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
    		'b100:inst_type=(funct7=='b001000)?WFI:SYSTEM_INSTR;
    	endcase
    end
    else if(opcode[4:3]=='b01)begin 
      case (opcode[2:0])  
        'b000 `ifdef atomic ,'b011 `endif : `ifdef RV32 if(funct3!='b011) `endif 
            inst_type=MEMORY; // STORE + Atomic
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
    		'b000: `ifdef RV32 if(funct3!='b011) `endif inst_type=MEMORY;
    		'b101,'b100,'b110:inst_type=ALU;
    	endcase
    end
    else
      exception = tagged Exception Illegal_inst;
		Bit#(4) fn=0;
		if(opcode==`BRANCH_op )begin
			if(funct3[2]==0)
				fn={2'b0,1,funct3[0]};
			else
				fn={1'b1,funct3};
		end
		else if(`ifdef RV64 opcode==`IMM_ARITHW_op || `endif opcode==`IMM_ARITH_op )begin
			fn=case(funct3)
				'b010: 'b1100;
				'b011: 'b1110;
				'b101: if(funct7[5]==1) 'b1011; else 'b0101;
				default:{1'b0,funct3};
			endcase;
		end
		else if(`ifdef RV64 opcode==`ARITHW_op || `endif opcode==`ARITH_op )begin
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
    if(err)
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
    if(inst_type==SYSTEM_INSTR)
      immediate_value={'d0,inst[19:15],immediate_value[11:0]};// TODO fix this

    if(interrupt matches tagged None)
      interrupt =  exception;

    `ifdef atomic
      Bit#(5) atomic_op = fn_atomic(inst[31:27]);
    `endif
    `ifdef simulate 
      Tuple8#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), Trap_type, 
        `ifdef atomic Bit#(6) `else Bit#(1) `endif `ifdef simulate , Bit#(32) `endif ) 
        type_tuple = tuple8(rs1type, rs2type, inst_type, mem_access, pc, interrupt, 
          `ifdef atomic {atomic_op,epoch} `else epoch `endif , inst);
    `else
      Tuple7#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), Trap_type, 
          `ifdef atomic Bit#(6) `else Bit#(1) `endif ) type_tuple = 
          tuple7(rs1type, rs2type, inst_type, mem_access, pc, interrupt,  
          `ifdef atomic {atomic_op,epoch} `else epoch `endif );
    `endif
    return tuple8(fn, rs1, rs2, rd, signExtend(immediate_value), word32, funct3, type_tuple);            
  endfunction

`ifdef compressed
 function PIPE1_DS decoder_func_16(Bit#(16) inst,Bit#(PADDR) shadow_pc, Bit#(1) epoch, Bool err, 
                                                                               CSRtoDecode csrs );
    let {prv, mip, csr_mie, mideleg, misa, counteren, mie}=csrs;

    Trap_type exception = tagged None;
    Trap_type interrupt = chk_interrupt(prv, mip, csr_mie, mideleg, mie);
    
		Bit#(2) op_comp=inst[1:0];
		Bit#(3) funct3_comp=inst[15:13];
    let opcode = {op_comp,funct3_comp};
  

    Bool t_CL_LOAD = (opcode=='b00010||opcode =='b00001||opcode=='b00011);
    Bool t_CL_STORE = (opcode=='b00101||opcode=='b00110||opcode=='b00111);
    Bool t_CL=t_CL_LOAD ||t_CL_STORE;
    Bool t_ADDI_LUI=(opcode=='b01011);
    Bool t_ADDI16SP= t_ADDI_LUI && inst[11:7]==2;
    Bool t_CS =(opcode=='b01100&&inst[11:10]==2'b11);
    Bool t_ADDI_EQ=(opcode=='b00000||opcode=='b01010||opcode=='b01000||t_ADDI16SP);
    Bool t_ADDI = (opcode == 'b01000);
    Bool t_ADDIW=((opcode=='b01001)&&(inst[11:7]!=0));
    Bool t_SLLI=(opcode=='b10000);
    Bool t_J_R =((opcode =='b10100)&&inst[6:2]==0);
    Bool t_ADD =((opcode =='b10100)&&inst[6:2]!=0);
    Bool t_BR  =((opcode=='b01110)||opcode =='b01111);
    Bool t_ARITH_W=(t_ADDIW||(t_CS &&inst[12]==1'b1));
    Bool t_SP_OP =
    (opcode=='b10001||opcode=='b10010||opcode=='b10011||opcode=='b10101||opcode=='b10110||opcode=='b10111||opcode=='b00000||t_ADDI16SP);
    Bool t_CJ=(`ifdef RV32 (opcode =='b01001) || `endif opcode=='b01101);
    Bool t_LUI = t_ADDI_LUI && inst[11:7]!=2;
    Bool t_LI = (opcode =='b01010);
    Bool t_LWSP = (opcode =='b10010); 
    Bool t_LDSP = (opcode =='b10011);
    Bool t_SWSP = (opcode =='b10110);
    Bool t_SDSP = (opcode =='b10111);
    Bool t_CI =(t_ADDI||t_ADDIW||t_LUI||t_LI||t_LWSP||t_LDSP||t_ADDI16SP);
    Bool t_CB =(t_BR);
    Bool t_CIW = (opcode =='b00000);
    Bool t_ANDI=(opcode=='b01100 && inst[9:7]==3);
    Bool t_IMM=((funct3_comp=='b000)||(op_comp=='b01 && inst[15]==1'b0)||(opcode=='b01100 &&
    inst[11:10]!='b11)); 

    Bit#(5) rs1={2'b01,inst[9:7]};
    Bit#(5) rs2={2'b01,inst[4:2]};
    Bit#(5) rd={2'b01,inst[4:2]};

    if((t_CL)||(t_CB)||(t_CS)||(opcode=='b01100 && inst[11:10]!='b11))//Memory,branch and logical
           rs1={2'b01,inst[9:7]};
    else if((opcode==5'b01000)||t_ADDIW||(t_ADD&&inst[12]==1)||t_J_R||t_SLLI)//SLLI,JUMP,ADDI and ADDIW
           rs1=inst[11:7];
    else if (t_SP_OP)//SP operations
           rs1 =2;
    else 
           rs1=0;

	  if((t_CL_STORE)||(t_CS))//Store and logical inst 
           rs2={2'b01,inst[4:2]};
    else if(t_ADD||t_SWSP||t_SDSP)//SP operations,mov and add
           rs2=inst[6:2];
    else 
           rs2=0;

    if((t_CI||t_SLLI||t_ADD)&&(!t_ADDI16SP))//ADDI,ADDIW,LUI,Stack load
     rd =inst[11:7];
    else if (t_CL_LOAD||t_CIW)//Load operations,ADDI14SP
     rd ={2'b01,inst[4:2]};
    else if ((opcode=='b01100))//ALU operations 
     rd ={2'b01,inst[9:7]};
    else if (t_CJ)//Jump
     rd =((inst[15]==1'b1)?5'b0:5'b1);//JR x0,JALR x1
    else if (t_ADDI16SP)
     rd =2;
    else if( t_J_R)
     rd =inst[12]==1?'d1:'d0;
    else
    rd =0;

  //  Bit#(7) funct7=inst[31:25]; 
		Bool word32 =False;
		Bit#(PADDR) pc=shadow_pc;
    
		//operand types
		Operand1_type rs1type=IntegerRF;
		Operand2_type rs2type=IntegerRF;

		//memory access type
		Access_type mem_access=Load;
		if(t_CL_STORE||t_SWSP||t_SDSP)
			mem_access=Store;

    // immediate value 
    Bit#(32)imm_value=0;

    if(t_LWSP) 
      imm_value=zeroExtend({inst[3:2],inst[12],inst[6:4],2'b00});//word 
    else if(t_LDSP)
      imm_value=zeroExtend({inst[4:2],inst[12],inst[6:5],3'b000});//double 
    else if(t_SWSP)
      imm_value=zeroExtend({inst[8:7],inst[12:10],inst[9],2'b00});
    else if(t_SDSP)
      imm_value=zeroExtend({inst[9:7],inst[12:10],3'b000});
    else if(t_CIW)
      imm_value=zeroExtend({inst[10],inst[9],inst[8],inst[7],inst[12],inst[11],inst[5],inst[6],2'b00}); 
    else if(t_ADDI16SP)
      imm_value =signExtend({inst[12],inst[4],inst[3],inst[5],inst[2],inst[6],4'b0000});
 	  else if(t_CL)
      if(inst[14:13]!=2'b11)
 		    imm_value=zeroExtend({inst[5],inst[12],inst[11],inst[10],inst[6],2'b00});
      else
        imm_value=zeroExtend({inst[6],inst[5],inst[12],inst[11],inst[10],3'b000});
    else if(t_CJ)
      imm_value=signExtend({inst[12],inst[8],inst[10:9],inst[6],inst[7],inst[2],inst[11],inst[5:3],1'b0});
    else if(t_CB)
      imm_value=signExtend({inst[12],inst[6],inst[5],inst[2],inst[11],inst[10],inst[4],inst[3],1'b0}); 
    else if( t_ADDI||t_ADDIW||t_LI||(opcode=='b01100&&inst[11:10]!=2'b11||t_SLLI))//imm_arith
      imm_value=signExtend({inst[12],inst[6],inst[5],inst[4],inst[3],inst[2]});
    else if (t_LUI)
      imm_value=signExtend({inst[12],inst[6],inst[5],inst[4],inst[3],inst[2],12'b0});
    else
      imm_value=0;

		Bit#(32) immediate_value=signExtend(imm_value);
		if(t_CJ || t_J_R)	
			rs1type=PC;

		if(t_CJ || t_J_R)
      rs2type=Constant2;

    else if(t_IMM)//instruction LUI
			rs2type=Immediate;
		
		//instructions which support word lenght operation in RV64 are to be added in Alu
		//need to be edit/rded according to the supported instruction

    `ifdef RV64
  		if(t_ADDIW || t_ARITH_W)//ADDW,SUBW and ADDIW
      	word32=True;
    `endif

    Instruction_type inst_type=ALU;
    if((t_CL)||(t_LWSP)||t_LDSP||t_SWSP||t_SDSP)
      inst_type=MEMORY;
    else if(t_CJ)
      inst_type=JAL;
    else if(t_J_R)
      inst_type=JALR;
    else if(t_BR)
      inst_type=BRANCH;
    else
      inst_type=ALU;

    Bit#(3) funct3=gen_funct3(opcode,inst);
    if((op_comp==2'b11)||(opcode==5'b00100)||(opcode==5'b00101)||
        (opcode==5'b10001)||(opcode==5'b10101))
      exception = tagged Exception Illegal_inst;

    if (inst==0)
		  exception = tagged Exception Illegal_inst;

    if(t_CIW||t_ADDI16SP||t_LUI||t_SLLI)
      if(immediate_value==0)
        exception = tagged Exception Illegal_inst;
    Bit#(4) fn=0;
		if(t_BR) begin
		  fn={2'b0,1,funct3[0]};
		end
    else if(`ifdef RV64 t_ADDIW||`endif t_SLLI||(t_ADDI)||(opcode=='b01100 
        && inst[11:10]!='b11))begin//SLLI,SRLI,SRAI,ANDI,ADDI 
		  fn=case(funct3)
				'b010: 'b1100;
				'b011: 'b1110;
				'b101: if(inst[10]==1'b1)'b1011; else 'b0101 ;
				default:{1'b0,funct3};
			endcase;
		end
    //Arithmetic instructions
		else if(`ifdef RV64 t_ARITH_W ||`endif t_CS||t_ADD  )begin
			fn=case(funct3)
				'b000:if (t_ADD)
                if(inst[1]==1'b0)'b1010;else 'b0000;
               else 
                if(inst[5]==1'b0)'b1010;else 'b0000; 				
        'b011:'b1110;
				'b101:'b1011;
				default:{1'b0,funct3};
			endcase;
		end
    if(pc[0]!=0)
      exception = tagged Exception Inst_addr_misaligned;
    else if(err)
      exception = tagged Exception Inst_access_fault;
    if(interrupt matches tagged None)
      interrupt =  exception;
   
   `ifdef simulate 
      Tuple8#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), Trap_type, 
        Bit#(1) `ifdef simulate , Bit#(32) `endif ) type_tuple = tuple8(rs1type, rs2type, inst_type,
        mem_access, pc, interrupt, epoch,zeroExtend(inst));
    `else
      Tuple7#(Operand1_type,Operand2_type,Instruction_type,Access_type,Bit#(PADDR), Trap_type, 
      Bit#(1)) type_tuple = tuple7(rs1type, rs2type, inst_type, mem_access, pc, interrupt, epoch);
    `endif
    return tuple8(fn, rs1, rs2, rd, signExtend(immediate_value), word32, funct3, type_tuple);            
  endfunction
`endif
endpackage
