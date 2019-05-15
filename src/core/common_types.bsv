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

Author : Neel Gala, Aditya Mathur
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
 */
package common_types;
  `include "common_params.bsv"

  `ifdef RV64
    typedef 64 XLEN;
  `else
    typedef 32 XLEN;
  `endif
//	typedef 32 `paddr ;

  typedef Bit #(3)  Funct3;

  // Define all enums here 
  typedef enum {ALU, MEMORY, BRANCH, JAL, JALR, SYSTEM_INSTR, TRAP, WFI
                `ifdef muldiv, MULDIV `endif 
  } Instruction_type deriving(Bits, Eq, FShow);

  typedef enum {Load = 0, Store = 1, Fence = 3 
                `ifdef atomic, Atomic = 2 `endif 
  } Access_type deriving (Bits, Eq, FShow);

  typedef enum {Flush = 1, None = 0} Flush_type deriving (Bits, Eq, FShow);

  typedef enum {SYSTEM_INSTR, MEMORY, REGULAR} Commit_type deriving(Eq, Bits, FShow);
  typedef enum {Machine = 3, User = 0} Privilege_mode deriving(Eq, Bits, FShow);
  typedef enum {NOFENCE = 0, FENCE = 1} Set_fence deriving(Eq, Bits, FShow);

  typedef Tuple8#(Bit#(4), Bit#(XLEN), Bit#(XLEN), Bit#(`paddr), Bit#(XLEN), Instruction_type, Funct3,
        Access_type)  ALU_Inputs;
  // ----------------------------- Trigger based structures -------------------------------------//
`ifdef triggers
  typedef struct{
    Bit#(1) load;
    Bit#(1) store;
    Bit#(1) execute;
  `ifdef user
    Bit#(1) user;
  `endif
    Bit#(1) machine;
    Bit#(4) matched;
    Bit#(1) chain;
    Bit#(4) action_;
  `ifdef RV64
    Bit#(4) size;
  `else
    Bit#(2) size;
  `endif
    Bit#(1) select;
    Bit#(1) dmode;
  } MControl deriving(Bits, Eq, FShow);

  typedef struct {
    Bit#(6) action_;
  `ifdef user
    Bit#(1) user;
  `endif
    Bit#(1) machine;
    Bit#(14) count;
    Bit#(1) dmode;
  } ICount deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(6) action_;
  `ifdef user
    Bit#(1) user;
  `endif
    Bit#(1) machine;
    Bit#(1) dmode;
  } ITrigger deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(6) action_;
  `ifdef user
    Bit#(1) user;
  `endif
    Bit#(1) machine;
    Bit#(1) dmode;
  } ETrigger deriving(Bits, Eq, FShow);

  typedef union tagged {
    MControl MCONTROL;
    ICount   ICOUNT;
    ITrigger ITRIGGER;
    ETrigger ETRIGGER;
    void NONE;
  } TriggerData deriving(Bits, Eq, FShow);
    
  typedef struct{
      Bool trap;
      Bit#(`causesize) cause;
    } TriggerStatus deriving(Bits, Eq, FShow);

`endif

  // ----------------------------- Outputs from the Decode Stage ------------------------------- //
  typedef enum {IntegerRF, PC} Op1type deriving(Bits, Eq, FShow);
  typedef enum {IntegerRF, Immediate, Constant4, Constant2} Op2type deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(5) rs1addr;
    Bit#(5) rs2addr;
    Bit#(5) rd;
  } OpAddr deriving(Bits, Eq, FShow);

  // this struct captures the type of the operands based on the instruction being decoded.
  // Max width : 2+3 + 1+1 = 7 bits
  typedef struct{
    Op1type rs1type;
    Op2type rs2type;
  } OpType deriving(Bits, Eq, FShow);
  
  // this struct holds the meta decoded information of an instruction
  typedef struct{
    Instruction_type inst_type; // instruction type
    Access_type memaccess;      // memory access type
    Bit#(32) immediate;         // immediate fields
    Bit#(7) funct;              // concatenation of f3 and fn fields
  `ifdef RV64
    Bool word32;
  `endif
  } InstrMeta deriving(Bits, Eq, FShow); 
  
  // the final structure of the response from the decoder
  typedef struct{
    OpAddr    op_addr;
    OpType    op_type;
    InstrMeta meta;
  `ifdef compressed
    Bool compressed;
  `endif
  } DecodeOut deriving(Bits, Eq, FShow);
  // ------------------------------------------------------------------------------------------- //

  typedef struct{
    Bit#(XLEN) op1;
    Bit#(XLEN) op2;
  } STAGE1_operands deriving(Bits, FShow, Eq);

  typedef DecodeOut STAGE1_meta;

  typedef struct{
    Bit#(1) epoch;
    Bit#(`vaddr) pc;
  } STAGE1_control deriving(Bits, FShow, Eq);

  typedef struct{
    Bit#(`vaddr) pc;
    Bit#(32) instruction;
  } STAGE1_dump deriving(Bits, FShow, Eq);

  //// define all tuples here
  //`ifdef rtldump
  //  `ifdef RV64
  //  typedef Tuple8#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bool, Bit#(3),
  //            Tuple8#(Op1type, Op2type, Instruction_type, Access_type, Bit#(`paddr),
  //              Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif, Bit#(32))) PIPE1_DS;
  //  `else
  //  typedef Tuple7#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bit#(3),
  //            Tuple8#(Op1type, Op2type, Instruction_type, Access_type, Bit#(`paddr),
  //              Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif, Bit#(32))) PIPE1_DS;
  //  `endif
  //  typedef Tuple8#(Commit_type, Bit#(XLEN), Bit#(TAdd#(`paddr, 1)), Bit#(`paddr), Bit#(5), Bit#(1), 
  //                  Trap_type, Bit#(32)) PIPE2_DS;
  //`else
  //  `ifdef RV64
  //  typedef Tuple8#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bool, Bit#(3),
  //            Tuple7#(Op1type, Op2type, Instruction_type, Access_type, Bit#(`paddr),
  //              Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif )) PIPE1_DS;
  //  `else
  //  typedef Tuple7#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bit#(3),
  //            Tuple7#(Op1type, Op2type, Instruction_type, Access_type, Bit#(`paddr),
  //              Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif )) PIPE1_DS;
  //  `endif
  //  typedef Tuple7#(Commit_type, Bit#(XLEN), Bit#(TAdd#(`paddr, 1)), Bit#(`paddr), Bit#(5), Bit#(1), 
  //                  Trap_type) PIPE2_DS;
  //`endif
  typedef Tuple4#(Commit_type, Bit#(XLEN), Bit#(TAdd#(`paddr, 1)), Trap_type) ALU_OUT;
  
  typedef Tuple5#(Bit#(`paddr), Bit#(XLEN), Access_type, Bit#(2), Bit#(1)) MemoryRequest;
  typedef Tuple4#(Bit#(`paddr), Access_type, Bit#(2), Bit#(1)) CoreRequest;

  typedef Tuple3#(Bit#(5), Bool, Bit#(XLEN)) OpFwding;
                  // rg_prv,      csr_mip,   csr_mie, mideleg,  misa,   counteren, rg_mie
  typedef struct{
    Privilege_mode prv;
    Bit#(`ifdef debug 14 `else 12 `endif ) csr_mip;
    Bit#(12) csr_mie;
  `ifdef non_m_traps
    Bit#(12) csr_mideleg;
  `endif
  `ifdef usertraps
    Bit#(12) csr_uip;
    Bit#(12) csr_uie;
  `endif
    Bit#(26) csr_misa;
    Bit#(XLEN) csr_mstatus;
    Bit#(3) frm;
  `ifdef debug
    Bit#(32)  csr_dcsr;
  `endif } CSRtoDecode deriving(Bits, Eq, FShow);

  typedef Tuple5#(Privilege_mode, Bit#(XLEN), Bit#(32), Bit#(5), Bit#(XLEN)) DumpType;

  typedef enum {
    Inst_addr_misaligned = 0,
    Inst_access_fault = 1,
    Illegal_inst = 2,
    Breakpoint = 3,
    Load_addr_misaligned = 4,
    Load_access_fault = 5,
    Store_addr_misaligned = 6,
    Store_access_fault = 7,
    Ecall_from_user = 8,
    Ecall_from_machine = 11
  } Exception_cause deriving (Bits, Eq, FShow);

  typedef enum{
    User_soft_int = 0,
    Machine_soft_int = 3,
    User_timer_int = 4,
    Machine_timer_int = 7,
    User_external_int = 8,
    Machine_external_int = 11
  } Interrupt_cause deriving (Bits, Eq, FShow);

  typedef union tagged{
    Exception_cause Exception;
    Interrupt_cause Interrupt;
    void None;
  } Trap_type deriving(Bits, Eq, FShow);

  // fabrics related definitions.
  typedef 0 USERSPACE;
endpackage
