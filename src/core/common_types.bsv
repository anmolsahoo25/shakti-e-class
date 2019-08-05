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

  typedef Tuple8#(Bit#(4), Bit#(XLEN), Bit#(XLEN), Bit#(`paddr), Bit#(XLEN), Instruction_type, 
                  Funct3, Access_type)  ALU_Inputs;
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
    //ICount   ICOUNT;
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
    Bit#(2) epoch;
    Bit#(`vaddr) pc;
  } STAGE1_control deriving(Bits, FShow, Eq);

  typedef struct{
    Bit#(`vaddr) pc;
    Bit#(32) instruction;
  } TraceDump deriving(Bits, FShow, Eq);

  // ------------------ Structs used in the stage2 --------------------------------------------- //

  typedef struct{
    Bool done; 
    PreCommit_type cmtype;
    Bit#(XLEN) aluresult ;
    Bit#(`vaddr) effective_addr;
    Bit#(`causesize) cause;
    Bool redirect;
  `ifdef bpu
    Bool branch_taken;
    Bit#(`vaddr) redirect_pc;
  `endif
  } ALU_OUT deriving (Bits,  Eq,  FShow);

  typedef struct{
    Bit#(XLEN) aluresult;
    Bool       valid;
  } DelayedOut deriving(Bits, Eq, FShow);

  // ---------- Tuples for the third Pipeline Stage -----------//
  typedef enum {MEMORY, SYSTEM_INSTR, REGULAR, TRAP} PreCommit_type deriving(Eq, Bits, FShow);

  typedef struct{
    Bit#(`vaddr) pc;
    Bit#(5)      rd;
    Bit#(1)      epoch;
  } Stage3Common deriving(Bits, Eq, FShow);

  typedef struct{
    Access_type   memaccess;
  `ifdef triggers
    Bit#(`vaddr)  address;
    Bit#(2)       size;
  `endif
  } Stage3Memory deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(`causesize)    cause;
    Bit#(`vaddr)        badaddr;
  } Stage3Trap deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(XLEN)    rdvalue;
  `ifdef muldiv
    Bool          delayed;
  `endif
  } Stage3Regular deriving(Bits, Eq, FShow);

  typedef struct{
    Bit#(XLEN)    rs1_imm;
    Bit#(2)       lpc;
    Bit#(12)      csr_address;
    Bit#(3)       funct3;
  } Stage3System deriving(Bits, Eq, FShow);

  typedef union tagged{
    Stage3Memory  Memory;
    Stage3Trap    Trap;
    Stage3Regular Regular;
    Stage3System  System;
  } Stage3Type deriving (Bits, Eq, FShow);

                
  // ----------------------------------------------------------//

  typedef struct{
    Bit#(`vaddr) addr;
    Bit#(2) epoch ;
  } InstRequest deriving(Bits, FShow, Eq);

  typedef struct{
    Bit#(32) inst;
    Bit#(2) epoch;
    Bool    err;
  } InstResponse deriving(Bits, FShow, Eq);

  typedef struct{
    Bit#(`vaddr) addr;
    Bit#(XLEN) data;
    Access_type memaccess;
    Bit#(3) size;
    Bit#(1) epoch;
  `ifdef atomic
    Bit#(5) atomic_op;
  `endif
  } MemoryRequest deriving (Eq, FShow, Bits);

  typedef struct{
    Bit#(XLEN) data;
    Bool err;
    Bit#(1) epoch;
  } MemoryResponse deriving(Eq, FShow, Bits);

  typedef struct{
    Bit#(5) rdaddr;
    Bit#(XLEN) rdvalue;
  } CommitPacket deriving(Bits, FShow, Eq);

//  typedef Tuple3#(Bit#(5), Bool, Bit#(XLEN)) OpFwding;
  typedef struct{
    Bit#(5) rdaddr;
    Bit#(XLEN) rdvalue;
    Bool    valid;
  } OpFwding deriving(Bits, FShow, Eq);

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
  `ifdef debug
    Bit#(32)  csr_dcsr;
  `endif } CSRtoDecode deriving(Bits, Eq, FShow);

  typedef struct {
      Bool debugger_available;
      Bool core_is_halted;
      Bool step_set;
      Bool step_ie;
      Bool core_debugenable;
  } DebugStatus deriving(Bits, Eq, FShow);

  typedef Tuple5#(Privilege_mode, Bit#(XLEN), Bit#(32), Bit#(5), Bit#(XLEN)) DumpType;

  // fabrics related definitions.
  typedef 0 USERSPACE;

  // types for events
  typedef struct{
    Bit#(1) exceptions;
    Bit#(1) interrupts;
    Bit#(1) branches_taken;
    Bit#(1) branches_notken;
    Bit#(1) muldiv;
    Bit#(1) csr_ops;
    Bit#(1) jumps;
    Bit#(1) loads;
    Bit#(1) stores;
    Bit#(1) redirections;
    Bit#(1) raw_stalls; 
  } Events deriving(Bits, Eq, FShow);

  function String event_to_string(Bit#(XLEN) event_count);
    case (event_count)
      'd1:  return "Exceptions";
      'd2:  return "Interrupts";
      'd3:  return "Branches Taken";
      'd4:  return "Branches Not Taken";
      'd5:  return "MulDiv Inst";
      'd6:  return "CSR Inst";
      'd7:  return "Jumps";
      'd8:  return "Loads";
      'd9:  return "Stores";
      'd10: return "Control Redirections";
      'd11: return "RAW Stalls";
      default: return "Unknown Event";
    endcase
  endfunction

  // ------------ PMP related types ----------------//

  typedef enum { OFF=0, TOR=1, NA4=2, NAPOT=3} PMPAddrMode deriving(Bits, Eq, FShow);

  typedef struct{
    Bool read;
    Bool write;
    Bool exec;
    PMPAddrMode access;
    Bool lock;
  } PMPCfg deriving(Bits, FShow, Eq);

  typedef struct{
    Bit#(`paddr) address;
    Bit#(6)      num_bytes;
    Bit#(2)      access_type; // 0-load 1-store 2-fetch
  } PMPReq deriving(Bits, FShow, Eq);

  function PMPCfg fn_unpack_cfg(Bit#(8) cfg);
    return PMPCfg{  read  : unpack(cfg[0]),
                    write : unpack(cfg[1]),
                    exec  : unpack(cfg[2]),
                    access: unpack(cfg[4:3]),
                    lock  : unpack(cfg[7])};
  endfunction
endpackage
