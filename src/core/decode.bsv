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

Author : Neel Gala, Aditya Mathur, Deepa N Sarma
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package decode;
  
  // pacakge imports from project
  import common_types::*;
  import BUtils::*;
  `include "common_params.bsv"
	  
  typedef enum {Q0 = 'b00, Q1 = 'b01, Q2 = 'b10} Quadrant deriving(Bits, Eq, FShow);

  (*noinline*)
  function Bool address_valid(Bit#(12) addr, Bit#(26) misa);
    Bool valid = False;
    case(addr[9 : 8])
      // user level CSRS
      'b00 : case(addr[11 : 10]) 
              'b00 : case (addr[7 : 0])
                `ifdef user
                  // User Trap setup and user trap handling registers
                  'h0, 'h4, 'h5, 'h40, 'h41, 'h42, 'h43, 'h44 : valid = unpack(misa[13] & misa[20]);
                `endif
                  // user floating point csrs
                  'h1, 'h2, 'h3 : valid = True;
              endcase
              // User Counters / Timers
            `ifdef user
              'b11 : case(addr[7 : 5])
                  'b000 : valid = True;
                `ifdef RV32
                  'b100 : valid = True;
                `endif
              endcase
            `endif
             'b10 : begin
                   valid = (addr[7 : 0] == 0);
                   end
            endcase
    // machine level CSRS
    'b11 : case(addr[11 : 10]) 
            // machine info registers
            'b11 : if(addr[7 : 4] == 1)
                    case(addr[3 : 0])
                      'h1, 'h2, 'h3, 'h4 : valid = True;
                    endcase
            'b00 : case(addr[7 : 4])
                    // Machine Trap Setup
                  'h0 : case(addr[3 : 0]) 
                        'h0, 'h1, 'h4, 'h5, 'h6 : valid = True;
                      `ifdef non_m_traps
                        'h2, 'h3 : if( ((misa[13] & misa[20]) == 1) || misa[18] == 1) valid = True;
                      `endif
                      endcase
                  // Machine counter Setup
                  'h2 : if(addr[3 : 0]>2) valid = True;
                  'h3 : valid = True;
                    // Machine Trap Handling
                  'h4 : case(addr[3 : 0])
                        'h0, 'h1, 'h2, 'h3, 'h4 : valid = True;
                      endcase
                    // Maching Protection and Translation
                `ifdef pmp
                  'hA : case(addr[3 : 0])
                        'h0, 'h2 `ifdef RV32 ,'h1,'h3 `endif : valid = True;
                      endcase
                    // PMP ADDR registers
                  'hB : if((`PMPSIZE != 0 ) && addr[3 : 0] <= fromInteger(valueOf(TSub#(`PMPSIZE, 1) ))) valid = True;
                `endif
                  endcase
              // Machine Counter / Timers
            'b10 : `ifdef RV32 if(addr[6 : 5] == 0 `else if(addr[7 : 5] == 0 `endif && addr[3 : 0] != 1) valid = True;
           // TODO B01 and 801 should be invalid
              // DTVEC and DEnable
            'b01 : begin 
                `ifdef debug 
                  if( addr[7 : 0] == 'hC0 || addr[7 : 0] == 'hC1 ) valid = True; 
                `endif
                `ifdef triggers
                  if( addr[7 : 4] == 'hA && addr[3 : 0] < 4) valid = True;
                `endif
            end
          endcase
    endcase
    return valid;
  endfunction

  function Bit#(3) gen_funct3(Bit#(5) opcode, Bit #(16) inst);
    Bit #(3) funct3 = 3'b000;
    
    
    case (opcode)
    
    5'b00000 : funct3 = 3'b000;
    5'b01000 : funct3 = 3'b000;
    5'b10000 : funct3 = 3'b001;
    5'b01001 : funct3 = 3'b000;
    5'b00010 : funct3 = 3'b010;
    5'b01010 : funct3 = 3'b000;
    5'b10010 : funct3 = 3'b010;
    5'b00011 : funct3 = 3'b011;
    5'b01011 : funct3 = 3'b000;
    5'b10011 : funct3 = 3'b011;
    5'b01100:
            if((inst[11 : 10] == 2'b00)||(inst[11 : 10] == 2'b01))
                funct3 = 3'b101;//SRLI, SRAI
            else if(inst[11 : 10] == 2'b10)
                funct3 = 3'b111;//ANDI
            else if(inst[11 : 10] == 2'b11)
            case({inst[6 : 5]})
                2'b00 : funct3 = 3'b000;
                2'b01 : if(inst[12] == 1'b1)
                          funct3 = 3'b000;
                      else
                          funct3 = 3'b100;
                2'b10 : funct3 = 3'b110;
                2'b11 : funct3 = 3'b111;
              endcase
    5'b10100 : funct3 = 3'b000;
    5'b00110 : funct3 = 3'b010;
    5'b01110 : funct3 = 3'b000;
    5'b10110 : funct3 = 3'b010;
    5'b10111 : funct3 = 3'b011;
    5'b00111 : funct3 = 3'b011;
    5'b01111 : funct3 = 3'b001;
    default : funct3 = 3'b000;
    endcase
    
    return funct3;

 endfunction
	
  function Bool hasCSRPermission(Bit#(12) address, Bool write,  Privilege_mode prv);
    Bit#(12) csr_index = pack(address);
    return ((pack(prv) >= csr_index[9 : 8]) && !(write && csr_index[11 : 10] == 2'b11) );
  endfunction
   
  // if the operand is not 0 then the instruction will perform a write on the CSR.
	function Bool valid_csr_access(Bit#(12) csr_addr, Bit#(5) operand, Bit#(2) operation,
                                                                              Privilege_mode prv);
		Bool ret = hasCSRPermission(unpack(csr_addr), (operand != 0 || operation == 'b01) ? True : False,
                                                                                              prv);
		return ret;
	endfunction

  (*noinline*)
	function Tuple3#(Bit#(`causesize), Bool, Bool) chk_interrupt(Privilege_mode prv, Bit#(XLEN) mstatus,
        Bit#(14) mip, Bit#(12) mie `ifdef non_m_traps, Bit#(12) mideleg `endif
      `ifdef usertraps
        ,Bit#(12) uip, Bit#(12) uie
      `endif  
      `ifdef debug 
        ,DebugStatus debug, Bool step_done
      `endif );
    Bool m_enabled = (prv != Machine) || (mstatus[3] == 1);
  `ifdef usertraps
    Bool u_enabled = (mstatus[0] == 1 && prv == User);
  `endif
    Bool resume_wfi = unpack(|( mie & truncate(mip))); // should halt interrupt on wfi cause

  `ifdef debug
    Bit#(14) debug_interrupts = { mip[13], mip[12], 12'd0};
    Bool d_enabled = debug.debugger_available && debug.core_debugenable;
  `endif

    // truncating because in debug mode mie and mip are 14 bits. 12 - halt - req 13 - resume - req
    Bit#(12) m_interrupts = mie & truncate(mip) & signExtend(pack(m_enabled)) 
             `ifdef non_m_traps & ~mideleg `endif 
             `ifdef debug       & signExtend(pack(!debug.core_is_halted)) `endif ;
  `ifdef usertraps
    Bit#(12) u_interrupts = uie & uip & mideleg & signExtend(pack(u_enabled)) 
              `ifdef debug      & signExtend(pack(!debug.core_is_halted)) `endif ;
  `endif

  Bit#(14) pending_interrupts = `ifdef debug d_enabled ? debug_interrupts : 0 | `endif
                           (m_enabled ? zeroExtend(m_interrupts) : 0) 
      `ifdef usertraps  |  (u_enabled ? zeroExtend(u_interrupts) : 0) `endif ;
		// format pendingInterrupt value to return
    Bool taketrap = unpack(|pending_interrupts) `ifdef debug ||  (step_done && !debug.core_is_halted) `endif ;
    Bit#(TSub#(`causesize, 1)) int_cause = '1;
  `ifdef debug
    if(step_done && !debug.core_is_halted) begin
      int_cause = `HaltStep;
    end
    else if(pending_interrupts[12] == 1)
      int_cause = `HaltDebugger;
    else if(pending_interrupts[13] == 1)
      int_cause = `Resume_int;
    else
  `endif
    if(pending_interrupts[11] == 1)
      int_cause = `Machine_external_int;
    else if(pending_interrupts[3] == 1)
      int_cause = `Machine_soft_int;
    else if(pending_interrupts[7] == 1)
      int_cause = `Machine_timer_int;
  `ifdef user
    else if(pending_interrupts[8] == 1)
      int_cause = `User_external_int;
    else if(pending_interrupts[0] == 1)
      int_cause = `User_soft_int;
    else if(pending_interrupts[4] == 1)
      int_cause = `User_timer_int;
  `endif

		return tuple3({1'b1, int_cause}, taketrap, resume_wfi);
	endfunction
  
  (*noinline*)
  function DecodeOut decoder_func_32(Bit#(32) inst, CSRtoDecode csrs);

    Bit#(1) fs = |csrs.csr_mstatus[14 : 13];
    Bit#(3) frm = csrs.frm;

  `ifdef debug
    Bool ebreakm = unpack(csrs.csr_dcsr[15]);
    Bool ebreaku = unpack(`ifdef user csrs.csr_dcsr[13] `else 0 `endif );
  `endif

    // ------- Default declarations of all local variables -----------//

		Bit#(5) rs1 = inst[19 : 15];
		Bit#(5) rs2 = inst[24 : 20];
		Bit#(5) rd = inst[11 : 7] ;
		Bit#(5) opcode = inst[6 : 2];
		Bit#(3) funct3 = inst[14 : 12];
    Bit#(7) funct7 = inst[31 : 25]; 
		Bool word32 = False;
    
		//operand types
		Op1type rs1type = IntegerRF;
		Op2type rs2type = IntegerRF;
    
    // ------------------------------------------------------------------

    //---------------- Decoding the immediate values-------------------------------------

    // Identify the type of intruction first
    Bool stype = (opcode == 'b01000);
    Bool btype = (opcode == 'b11000);
    Bool utype = (opcode == 'b01101 || opcode == 'b00101);
    Bool jtype = (opcode == 'b11011);
    Bool r4type = (opcode[4 : 2] == 'b100);
    Bool atomictype = (opcode == 'b01011);
    Bool csrtype = (opcode == `SYSTEM_INSTR_op) ;
    Bool itype = (opcode == 'b00100 || opcode == 'b000000 || opcode == 'b00011
                  `ifdef RV64 || opcode == 'b00110 `endif  );

    // refer to section 2.3 (Immediate Encoding Variants) of the risc - v iser spec for more details
    // on the following logic.
    // The default values are chosen such that in case of FPU,  the immediate encoding will hold the
    // upper 7 - bit for further decoding.
    // The default values also enable capturing the encoding for atomic operations as well.
    Bit#(1) bit0 = inst[20]; // because of I - type instructions
    `ifdef atomic
      if(atomictype)
        bit0 = 0;
      else
    `endif
    if(stype)
      bit0 = inst[7];
    else if(btype || utype || jtype) 
      bit0 = 0;

    Bit#(4) bit1_4 = inst[24 : 21]; // I / J-type instructions
    `ifdef atomic
      if(atomictype)
        bit1_4 = 0;
      else
    `endif
    if(stype || btype) // S / B-Type
      bit1_4 = inst[11 : 8];
    else if(utype) // U type
      bit1_4 = 0;

    Bit#(6) bit5_10 = inst[30 : 25];
    `ifdef atomic
      if(atomictype)
        bit5_10 = 0;
      else
    `endif
    if(utype)
      bit5_10 = 0;
    
    Bit#(1) bit11 = inst[31]; // I / S type
    `ifdef atomic
      if(atomictype)
        bit11 = 0;
      else
    `endif
    if(btype)
      bit11 = inst[7];
    else if(utype)
      bit11 = 0;
    else if(jtype)
      bit11 = inst[20];

    Bit#(8) bit12_19 = duplicate(inst[31]); // I / S/B type
    `ifdef atomic
      if(atomictype)
        bit12_19 = 0;
      else
    `endif
    if(utype || jtype)
      bit12_19 = inst[19 : 12];

    Bit#(11) bit20_30 = duplicate(inst[31]); // I / B/S / J type
    `ifdef atomic
      if(atomictype)
        bit20_30 = 0;
      else
    `endif
    if(utype)
      bit20_30 = inst[30 : 20];
    Bit#(1) bit31 = `ifdef atomic (atomictype) ? 0: `endif inst[31];
    Bit#(32) immediate_value={bit31, bit20_30, bit12_19, bit11, bit5_10, bit1_4, bit0};
    // ----------------------------------------------------------------------------------
		
    //memory access type
		Access_type mem_access = Load;
		if(stype)
		mem_access = Store;
    if(opcode == 'b00011)
      mem_access = Fence;
  `ifdef atomic
    if(opcode == 'b01011)
      mem_access = Atomic;
  `endif
    

    // Following table describes what the ALU will need for some critical operations. Based on this
    // the next set of logic is implemented. rs1 + rs2 is a XLEN bit adder. rs3 + rs4 is `paddr bit
    // adder.
    // Now PC can be present either in rs1 or rs3. This has been done to reduce the mux to the ALU
    // in the next stage. There will only be a mux in the next stage to identify the PC and send it
    // to the next stage.
    //
    //          rs1   rs2   rs3   rs4
    // Branch   OP1   OP2   PC    Imm
    // JAL      PC    'd4   PC    Imm   (rs1 = 0, rs2 = 0 since neither required)
    // JALR     PC    'd4   op1   Imm   (rs2 = 0 since not required)
    // LOAD     PC    op2   op1   Imm   (rs2 = 0 since not required) // PC needs to be sent as well
    // STORE    PC    op2   op1   Imm   (both required. op2 is the data)
    // AUIPC    PC    Imm   PC    Imm   (rs1 = 0, rs2 = 0 since neither required)
    // Atomic   PC    op1   op1    0
    /////////////////////////////////////////////////////////////////////////////////

		// assign rs1 to x0 when not required
		if ( utype || jtype || (csrtype && funct3[2] == 1) )
			rs1 = 0;

    // assign rs2 to x0 when not required.
		if ( itype || csrtype || utype || jtype || opcode == `JALR_op || opcode[4 : 2] == 'b000)
			rs2 = 0;

    // assign rd to x0 when not required.
		if ( btype || stype )	
			rd = 0;

    // rs1type is IRF by default. Based on the table assign it the PC value.
		if(opcode == `JAL_op || opcode == `JALR_op|| opcode == `AUIPC_op || opcode == `STORE_op 
        || opcode == `FENCE_op  ||  opcode == `LOAD_op `ifdef atomic || opcode == `ATOMIC_op `endif )
			rs1type = PC;

    // rs2type is IRF by default. Assign Immediate value based on the instructions
		if(opcode == `JALR_op || opcode == `JAL_op || opcode == `FENCE_op)
      rs2type = Constant4;
    else if(itype || utype || jtype )
			rs2type = Immediate;

// ------------------------------------------------------------------------------------------- //
  Bit#(`causesize) trapcause = `Illegal_inst;
  Bool validload = `ifdef RV32 funct3 != 3 && funct3 != 7 `else funct3 != 7 `endif ;
`ifdef RV32
  Bool validImm = (funct3 == 1) ? (funct7 == 0) : (funct3 == 5) ? 
                                  (funct7 == 'b0000000 || funct7 == 'b0100000) : True;
  Bool validImm32 = False;
`else
  Bool validImm = (funct3 == 1) ? (funct7[6 : 1] == 0) : (funct3 == 5) ? 
                                  (funct7[6 : 1] == 'b000000 || funct7[6 : 1] == 'b010000) : True;
  Bool validImm32 = (funct3 == 0) ? True : (funct3 == 1) ? (funct7 == 0) : (funct3 == 5) ? 
                                            (funct7 == 'b0000000 || funct7 == 'b0100000) : False;
`endif
  Bool validStore = `ifdef RV32 funct3<3 `else funct3<4 `endif ;
  Bool validAtomicOp = case(inst[31 : 27])
      'd0, 'd1, 'd3, 'd4, 'd8, 'd12, 'd16, 'd20, 'd24, 'd28 : True;
      'd2 : if (inst[24 : 20] == 0) True; else False;
      default : False;
    endcase;
  Bool validAtomic = (csrs.csr_misa[0] == 1 && (funct3 == 2 `ifdef RV64 || funct3 == 3 `endif ) && validAtomicOp);
  Bool validMul = (csrs.csr_misa[12] == 1 && funct7 == 1) ? True : False; 
  Bool validOp = (funct3 == 0 || funct3 == 5) ? (funct7 == 'b0000000 || funct7 == 'b0100000) : (funct7 == 0);
  Bool validMul32 = (csrs.csr_misa[12] == 1 && funct7 == 1 && (funct3 == 0 || funct3>3));
  Bool validOp32  = (funct3 == 1) ? (funct7 == 0) : (funct3 == 0 || funct3 == 5) ? (funct7 == 'b0000000||funct7 == 'b0100000) : False;
	Bool address_is_valid = address_valid(inst[31 : 20], csrs.csr_misa);
	Bool access_is_valid = valid_csr_access(inst[31 : 20], inst[19 : 15], inst[13 : 12], csrs.prv);
  Instruction_type inst_type = TRAP;
  case (opcode[4 : 3])
    'b00 : case(opcode[2 : 0])
        'b000 : if(validload) inst_type = MEMORY;      // Load
        'b011 : if(funct3 == 0 || funct3 == 1) inst_type = MEMORY;    // Fence, FenceI
        'b100 : if(validImm) inst_type = ALU;        // OP - Imm
        'b101 : inst_type = ALU;                       // AUIPC
      `ifdef RV64
        'b110 : if(validImm32) inst_type = ALU;      // Op - IMM32
      `endif
      endcase
    'b01 : case(opcode[2 : 0])
        'b000 : if(validStore) inst_type = MEMORY;     // Store
      `ifdef atomic
        'b011 : if(validAtomic) inst_type = MEMORY;    // Atomic 
      `endif
        'b100 : `ifdef muldiv  if(validMul) inst_type = MULDIV; else `endif  // MULDIV
                if(validOp) inst_type = ALU; // OP
        'b101 : inst_type = ALU;
      `ifdef RV64
        'b110 : `ifdef muldiv if(validMul32) inst_type = MULDIV; else `endif // MULDIV - 32
              if(validOp32) inst_type = ALU; // OP
      `endif
      endcase
    'b11 : case(opcode[2 : 0])
      'b000 : if(funct3 != 2 && funct3 != 3) inst_type = BRANCH; // BRANCH
      'b001 : if(funct3 == 0) inst_type = JALR; // JALR
      'b011 : inst_type = JAL; // jal
      'b100 : case(funct3)
          'b000:  if(inst[31 : 7] == 0) trapcause = (csrs.csr_misa[20] == 1 && csrs.prv == User) ? `Ecall_from_user: 
                                              `Ecall_from_machine;
                  else if(inst[31 : 7] == 'h2000) begin
                  `ifdef debug
                    if( (ebreakm && csrs.prv == Machine) ||
                        (ebreaku && csrs.prv == User)) begin
                      trapcause = `HaltEbreak;
                      trapcause[`causesize - 1] = 1;
                    end
                    else
                  `endif
                    trapcause = `Breakpoint;
                 end
                 else if(inst[31 : 20] == 'h002 && inst[19 : 15] == 0 && inst[11 : 7] == 0 && csrs.csr_misa[13] == 1) inst_type = SYSTEM_INSTR;
                 else if(inst[31 : 20] == 'h302 && inst[19 : 15] == 0 && inst[11 : 7] == 0 && csrs.prv == Machine)
                        inst_type = SYSTEM_INSTR;
                 else if(inst[31 : 20] == 'h105 && inst[19 : 15] == 0 && inst[11 : 7] == 0 && csrs.prv == Machine)
                        inst_type = WFI;
          default : if(funct3 != 0 && funct3 != 4 && access_is_valid && address_is_valid) 
                    inst_type = SYSTEM_INSTR;
      endcase
    endcase
  endcase
  if(inst[1 : 0] != 'b11 && inst_type != TRAP)begin
    inst_type = TRAP;
    trapcause = `Illegal_inst;
  end

  // checks : TVM = 1 TW = 1 TSR = 0
// --------------------------------------------------------------------------------------------//

    // --------- Function for ALU -------------
    // In case of Atomic operations as well,  the immediate portion will ensure the right opcode is
    // sent to the cache for operations.
		Bit#(4) fn = 0;
    `ifdef atomic
    if( opcode == `ATOMIC_op )begin
      if((inst[27]|inst[28]) == 1)
        fn={inst[29 : 27], 1'b1};
      else
        fn={inst[31 : 29], inst[27]};
    end
    `endif
		if(opcode == `BRANCH_op )begin
			if(funct3[2] == 0)
				fn={2'b0, 1,funct3[0]};
			else
				fn={1'b1, funct3};
		end
		else if(`ifdef RV64 opcode == `IMM_ARITHW_op || `endif opcode == `IMM_ARITH_op )begin
			fn = case(funct3)
				'b010 : 'b1100;
				'b011 : 'b1110;
				'b101 : if(funct7[5] == 1) 'b1011; else 'b0101;
				default:{1'b0, funct3};
			endcase;
		end
		else if(`ifdef RV64 opcode == `ARITHW_op || `endif opcode == `ARITH_op )begin
			fn = case(funct3)
				'b000 : if(funct7[5] == 1) 'b1010; else 'b0000;
				'b010 : 'b1100;
				'b011 : 'b1110;
				'b101 : if (funct7[5] == 1) 'b1011;else 'b0101;
				default:{1'b0, funct3};
			endcase;
		end
    else if(opcode[4 : 3] == 'b10 && (csrs.csr_misa[5]|csrs.csr_misa[3]) == 1) // floating point instructions
	  		fn = opcode[3 : 0];
    // ---------------------------------------

    if(inst_type == SYSTEM_INSTR)
      immediate_value={'d0, inst[19 : 15], immediate_value[11 : 0]};// TODO fix this
    Bit#(7) temp1 = {fn, funct3};
    if(inst_type == TRAP)
      temp1 = zeroExtend(trapcause);

    let op_addr = OpAddr{rs1addr : rs1, rs2addr : rs2, rd : rd };
    let op_type = OpType{rs1type : rs1type, rs2type : rs2type };
    let instr_meta = InstrMeta{inst_type : inst_type, memaccess : mem_access, funct : temp1,
                              immediate : immediate_value `ifdef RV64, word32: ? `endif };
    return DecodeOut{op_addr : op_addr, op_type : op_type, meta : instr_meta
                    `ifdef compressed, compressed : False `endif };
  endfunction
  
  (*noinline*)
  function Bool decode_word32 (Bit#(32) inst, Bit#(1) misa_c);
    Bool word32 = False;
    `ifdef RV64
		  Bit#(5) opcode = inst[6 : 2];
      Bit#(7) funct7 = inst[31 : 25]; 
      if(misa_c == 1 && inst[1 : 0] != 'b11)begin
        Quadrant quad = unpack(inst[1 : 0]);
        Bit#(3) funct3 = inst[15 : 13];
        if( quad == Q1 && (funct3 == 'b001 || (funct3 == 'b100 && inst[12 : 10] == 'b111 && inst[6] == 'b0)))
          word32 = True;
      end
      else begin
		    Bit#(3) funct3 = inst[14 : 12];
  		  if(opcode == `IMM_ARITHW_op || opcode == `MULDIVW_op ||  opcode == `ARITHW_op ||
            (opcode[4 : 1] == 'b0101 && funct3[0] == 0)) 
      	word32 = True;
      end
    `endif
    return word32;
  endfunction

  function ActionValue#(DecodeOut) decoder_func (Bit#(32) inst, Bool trap, CSRtoDecode csrs
                `ifdef debug, DebugStatus debug, Bool step_done `endif ) =  actionvalue

    DecodeOut result_decode = decoder_func_32(inst, csrs);
    let {icause, takeinterrupt, resume_wfi} = 
            chk_interrupt( csrs.prv, csrs.csr_mstatus, zeroExtend(csrs.csr_mip), csrs.csr_mie 
                          `ifdef non_m_traps, csrs.csr_mideleg `endif
                          `ifdef usertraps  ,csrs.csr_uip, csrs.csr_uie `endif 
                          `ifdef debug, debug, step_done `endif );

    Bit#(7) func_cause = result_decode.meta.funct;
    Instruction_type x_inst_type = result_decode.meta.inst_type;
      
    if(takeinterrupt)begin
      func_cause = zeroExtend(icause);
      x_inst_type = TRAP;
    end
    else if(trap) begin
      x_inst_type = TRAP;
      func_cause = `Inst_access_fault ;
    end

    result_decode.meta.inst_type = x_inst_type;
    result_decode.meta.funct = func_cause;
  `ifdef RV64
    result_decode.meta.word32 = decode_word32(inst, csrs.csr_misa[2]);
  `endif

    return result_decode;

  endactionvalue;
/*
 function PIPE1_DS decoder_func_16(Bit#(16) inst, Bit#(`vaddr) shadow_pc, Bit#(1) epoch, Bool err, 
                                                                               CSRtoDecode csrs );
    let {prv, mip, csr_mie, mideleg, misa, counteren, mie}=csrs;
    Trap_type exception = tagged None;
    Trap_type interrupt = chk_interrupt(prv, mip, csr_mie, mideleg, mie);
    
		Bit#(2) op_comp = inst[1 : 0];
		Bit#(3) funct3_comp = inst[15 : 13];
    let opcode = {op_comp, funct3_comp};
    Bool t_CL_LOAD = (opcode == 'b00010);
    
    `ifndef RV32
      t_CL_LOAD = (opcode == 'b00010||opcode == 'b00011);
    `endif

    Bool t_CL_STORE = (opcode == 'b00110);
    
    `ifndef RV32
      t_CL_STORE = (opcode == 'b00110||opcode == 'b00111);
    `endif

    Bool t_CL = t_CL_LOAD ||t_CL_STORE;
    Bool t_ADDI_LUI = (opcode == 'b01011);
    Bool t_ADDI16SP = t_ADDI_LUI && inst[11 : 7] == 2;
    Bool t_CS = (opcode == 'b01100&&inst[11 : 10] == 2'b11);
    Bool t_ADDI_EQ = (opcode == 'b00000||opcode == 'b01010||opcode == 'b01000||t_ADDI16SP);
    Bool t_ADDI = (opcode == 'b01000);
    Bool t_ADDIW = False;

    `ifndef RV32
      t_ADDIW = ((opcode == 'b01001)&&(inst[11 : 7] != 0));
    `endif


    Bool t_SLLI = (opcode == 'b10000);
    Bool t_J_R = ((opcode == 'b10100)&&inst[6 : 2] == 0);
    Bool t_ADD = ((opcode == 'b10100)&&inst[6 : 2] != 0);
    Bool t_BR  =((opcode == 'b01110)||opcode == 'b01111);
    Bool t_ARITH_W = (t_ADDIW||(t_CS &&inst[12] == 1'b1));
    Bool t_SP_OP = (opcode == 'b10001||opcode == 'b10010||opcode == 'b10011||opcode == 'b10101
    ||opcode == 'b10110||opcode == 'b10111||opcode == 'b00000||t_ADDI16SP);
    Bool t_CJ = (`ifdef RV32 (opcode == 'b01001) ||`endif opcode == 'b01101);
    Bool t_LUI = t_ADDI_LUI && inst[11 : 7] != 2;
    Bool t_LI = (opcode == 'b01010);
    Bool t_LWSP = (opcode == 'b10010);
    Bool t_LDSP = False;
   
    `ifndef RV32
     t_LDSP = (opcode == 'b10011); 
    `endif 


    Bool t_SWSP = (opcode == 'b10110);
    Bool t_SDSP = False;
   
     `ifndef RV32
       t_SDSP = (opcode == 'b10111); 
     `endif

    Bool t_CI = (t_ADDI||t_ADDIW||t_LUI||t_LI||t_LWSP||t_LDSP||t_ADDI16SP);
    Bool t_CB = (t_BR);
    Bool t_CIW = (opcode == 'b00000);
    Bool t_ANDI = (opcode == 'b01100 && inst[9 : 7] == 3);
    Bool t_IMM = ((funct3_comp == 'b000)||(op_comp == 'b01 && inst[15] == 1'b0)||(opcode == 'b01100 &&
    inst[11 : 10] != 'b11)); 
    Bool t_BREAK = ((opcode == 'b10100) && inst[11 : 2] == 0);
    Bit#(5) rs1={2'b01, inst[9 : 7]};
    Bit#(5) rs2={2'b01, inst[4 : 2]};
    Bit#(5) rd={2'b01, inst[4 : 2]};
    
    if((t_CL)||(t_CB)||(t_CS)||(opcode == 'b01100 && inst[11 : 10] != 'b11))//Memory, branch and logical
           rs1={2'b01, inst[9 : 7]};
    else if((opcode == 5'b01000)||t_ADDIW||(t_ADD&&inst[12] == 1)||t_J_R||t_SLLI)//SLLI, JUMP, ADDI and ADDIW
           rs1 = inst[11 : 7];
    else if (t_SP_OP)//SP operations
           rs1 = 2;
    else 
           rs1 = 0;

	  if((t_CL_STORE)||(t_CS))//Store and logical inst 
           rs2={2'b01, inst[4 : 2]};
    else if(t_ADD||t_SWSP||t_SDSP)//SP operations, mov and add
           rs2 = inst[6 : 2];
    else 
           rs2 = 0;

    if((t_CI||t_SLLI||t_ADD)&&(!t_ADDI16SP))//ADDI, ADDIW, LUI, Stack load
     rd = inst[11 : 7];
    else if (t_CL_LOAD||t_CIW)//Load operations, ADDI14SP
     rd ={2'b01, inst[4 : 2]};
    else if ((opcode == 'b01100))//ALU operations 
     rd ={2'b01, inst[9 : 7]};
    else if (t_CJ)//Jump
     rd = ((inst[15] == 1'b1) ? 5'b0 : 5'b1);//JR x0, JALR x1
    else if (t_ADDI16SP)
     rd = 2;
    else if( t_J_R)
     rd = inst[12] == 1?'d1 : 'd0;
    else
    rd = 0;

  //  Bit#(7) funct7 = inst[31 : 25]; 
		`ifdef RV64 Bool word32 = False; `endif
		Bit#(`vaddr) pc = shadow_pc;
    
		//operand types
		Operand1_type rs1type = IntegerRF;
		Operand2_type rs2type = IntegerRF;

		//memory access type
		Access_type mem_access = Load;
		if(t_CL_STORE||t_SWSP||t_SDSP)
			mem_access = Store;

    // immediate value 
    Bit#(32)imm_value = 0;
    
    if(t_LWSP) 
      imm_value = zeroExtend({inst[3 : 2], inst[12], inst[6 : 4], 2'b00});//word 
    else if(t_LDSP)
      imm_value = zeroExtend({inst[4 : 2], inst[12], inst[6 : 5], 3'b000});//double 
    else if(t_SWSP)
      imm_value = zeroExtend({inst[8 : 7], inst[12 : 10], inst[9], 2'b00});
    else if(t_SDSP)
      imm_value = zeroExtend({inst[9 : 7], inst[12 : 10], 3'b000});
    else if(t_CIW)
      imm_value = zeroExtend({inst[10], inst[9], inst[8], inst[7], inst[12], inst[11], inst[5], inst[6], 2'b00}); 
    else if(t_ADDI16SP)
      imm_value = signExtend({inst[12], inst[4], inst[3], inst[5], inst[2], inst[6], 4'b0000});
 	  else if(t_CL)
      if(inst[14 : 13] != 2'b11)
 		    imm_value = zeroExtend({inst[5], inst[12], inst[11], inst[10], inst[6], 2'b00});
      else
        imm_value = zeroExtend({inst[6], inst[5], inst[12], inst[11], inst[10], 3'b000});
    else if(t_CJ)
      imm_value = signExtend({inst[12], inst[8], inst[10 : 9], inst[6], inst[7], inst[2], inst[11], inst[5 : 3], 1'b0});
    else if(t_CB)
      imm_value = signExtend({inst[12], inst[6], inst[5], inst[2], inst[11], inst[10], inst[4], inst[3], 1'b0}); 
    else if( t_ADDI||t_ADDIW||t_LI||(opcode == 'b01100&&inst[11 : 10] != 2'b11||t_SLLI))//imm_arith
      imm_value = signExtend({inst[12], inst[6], inst[5], inst[4], inst[3], inst[2]});
    else if (t_LUI)
      imm_value = signExtend({inst[12], inst[6], inst[5], inst[4], inst[3], inst[2], 12'b0});
    else
      imm_value = 0;

		Bit#(32) immediate_value = signExtend(imm_value);
		if(t_CJ || t_J_R)	
			rs1type = PC;

		if(t_CJ || t_J_R)
      rs2type = Constant2;

    else if(t_IMM)//instruction LUI
			rs2type = Immediate;
		
		//instructions which support word lenght operation in RV64 are to be added in Alu
		//need to be edit / rded according to the supported instruction

    `ifdef RV64
  		if(t_ADDIW || t_ARITH_W)//ADDW, SUBW and ADDIW
      	word32 = True;
    `endif

    Instruction_type inst_type = ALU;
    if((t_CL)||(t_LWSP)||t_LDSP||t_SWSP||t_SDSP)
      inst_type = MEMORY;
    else if(t_CJ)
      inst_type = JAL;
    else if(t_J_R)
      inst_type = JALR;
    else if(t_BR)
      inst_type = BRANCH;
    else
      inst_type = ALU;
    
    Bit#(3) funct3 = gen_funct3(opcode, inst);
    //Illegal opcodes:-FLD||LQ, FSD||SQ, Reserved, FLDSP||LQ, FSDSP||SQ
    if((op_comp == 2'b11)||(opcode == 5'b00001)||(opcode == 5'b00100)||(opcode == 5'b00101)||
        (opcode == 5'b10001)||(opcode == 5'b10101))
      exception = tagged Exception Illegal_inst;
    //Illegal instruction
    if (inst == 0)
		  exception = tagged Exception Illegal_inst;  
    //Generate exceptions on nzimm in case of ADDI, ADDI14SP, ADDI16SP, SLLI, SRLI, SRAI, LUI
    if(t_CIW||t_ADDI16SP||t_LUI||t_SLLI||((opcode == 'b01100) && (inst[11] == 0)))
      if(immediate_value == 0)
        exception = tagged Exception Illegal_inst;
      if(t_BREAK)
        exception = tagged Exception Breakpoint;
    Bit#(4) fn = 0;
		if(t_BR) begin
		  fn={2'b0, 1,funct3[0]};
		end
    else if(`ifdef RV64 t_ADDIW||`endif t_SLLI||(t_ADDI)||(opcode == 'b01100 
        && inst[11 : 10] != 'b11))begin//SLLI, SRLI, SRAI, ANDI, ADDI 
		  fn = case(funct3)
				'b010 : 'b1100;
				'b011 : 'b1110;
				'b101 : if(inst[10] == 1'b1)'b1011; else 'b0101 ;
				default:{1'b0, funct3};
			endcase;
		end
    //Arithmetic instructions
		else if(`ifdef RV64 t_ARITH_W ||`endif t_CS||t_ADD  )begin
			fn = case(funct3)
				'b000 : if (t_ADD)
                if(inst[1] == 1'b0)'b1010;else 'b0000;
               else 
                if(inst[5] == 1'b0)'b1010;else 'b0000; 				
        'b011 : 'b1110;
				'b101 : 'b1011;
				default:{1'b0, funct3};
			endcase;
		end

    if(err)
      exception = tagged Exception Inst_access_fault;
    
    if(interrupt matches tagged None)
      interrupt =  exception;
    
    `ifdef rtldump
	 Tuple8#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(`vaddr), Trap_type, 
        `ifdef atomic Bit#(6) `else Bit#(1) `endif, Bit#(32) ) 
        type_tuple = tuple8(rs1type, rs2type, inst_type, mem_access, pc, interrupt, 
          `ifdef atomic {0, epoch} `else epoch `endif, zeroExtend(inst));
	  
    `else
      Tuple7#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(`vaddr), Trap_type, 
          `ifdef atomic Bit#(6) `else Bit#(1) `endif ) type_tuple = 
          tuple7(rs1type, rs2type, inst_type, mem_access, pc, interrupt,  
          `ifdef atomic {0, epoch} `else epoch `endif );
    `endif

    `ifdef RV64 
      return tuple8(fn, rs1, rs2, rd, signExtend(immediate_value), word32, funct3, type_tuple);            
    `else
      return tuple7(fn, rs1, rs2, rd, signExtend(immediate_value), funct3, type_tuple);            
    `endif

  endfunction
*/

endpackage
