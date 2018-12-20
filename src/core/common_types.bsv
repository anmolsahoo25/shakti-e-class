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
package common_types;
	`include "common_params.bsv"

	`ifdef RV64
		typedef 64 XLEN;
	`else
		typedef 32 XLEN;
	`endif
	typedef 32 PADDR ;

	typedef Bit #(3)  Funct3;

  // Define all enums here 
	typedef enum {ALU, MEMORY, BRANCH, JAL, JALR, SYSTEM_INSTR `ifdef muldiv , MULDIV `endif }
      Instruction_type deriving(Bits, Eq, FShow); // the type of the decoded instruction.// Adding fence
	  typedef enum {Load=0, Store=1,Fencei=3 `ifdef atomic ,Atomic=2 `endif } Access_type deriving (Bits, Eq, FShow);
	typedef enum {Flush= 1, None= 0} Flush_type deriving (Bits, Eq, FShow);
	typedef enum {IntegerRF, PC} Operand1_type deriving(Bits, Eq, FShow);
	typedef enum {IntegerRF, Immediate, Constant4, Constant2} Operand2_type deriving(Bits, Eq, FShow);
  typedef enum {SYSTEM_INSTR, MEMORY, REGULAR} Commit_type deriving(Eq, Bits, FShow);
  typedef enum {Machine=3, User=0} Privilege_mode deriving(Eq, Bits, FShow);
  typedef enum {NOFENCE=0, FENCE=1} Set_fence deriving(Eq, Bits, FShow);

  typedef Tuple8#(Bit#(4), Bit#(XLEN), Bit#(XLEN), Bit#(PADDR), Bit#(XLEN), Instruction_type, Funct3,
        Access_type)  ALU_Inputs;
  // define all tuples here
  `ifdef rtldump
    `ifdef RV64
    typedef Tuple8#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bool, Bit#(3),
              Tuple8#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(PADDR),
                Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif , Bit#(32))) PIPE1_DS;
    `else
    typedef Tuple7#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bit#(3),
              Tuple8#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(PADDR),
                Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif , Bit#(32))) PIPE1_DS;
    `endif
    typedef Tuple8#(Commit_type, Bit#(XLEN), Bit#(TAdd#(PADDR, 1)), Bit#(PADDR), Bit#(5), Bit#(1), 
                    Trap_type, Bit#(32)) PIPE2_DS;
  `else
    `ifdef RV64
    typedef Tuple8#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bool, Bit#(3),
              Tuple7#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(PADDR),
                Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif )) PIPE1_DS;
    `else
    typedef Tuple7#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(32), Bit#(3),
              Tuple7#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(PADDR),
                Trap_type, `ifdef atomic Bit#(6) `else Bit#(1) `endif )) PIPE1_DS;
    `endif
    typedef Tuple7#(Commit_type, Bit#(XLEN), Bit#(TAdd#(PADDR, 1)), Bit#(PADDR), Bit#(5), Bit#(1), 
                    Trap_type) PIPE2_DS;
  `endif
  typedef Tuple4#(Commit_type, Bit#(XLEN), Bit#(TAdd#(PADDR, 1)), Trap_type) ALU_OUT;
  
  typedef Tuple5#(Bit#(PADDR), Bit#(XLEN), Access_type, Bit#(2), Bit#(1)) MemoryRequest;
  typedef Tuple4#(Bit#(PADDR), Access_type, Bit#(2), Bit#(1)) CoreRequest;

  typedef Tuple3#(Bit#(5), Bool, Bit#(XLEN)) OpFwding;
                  // rg_prv,      csr_mip,   csr_mie, mideleg,  misa,   counteren, rg_mie
  typedef Tuple7#(Privilege_mode, Bit#(12), Bit#(12), Bit#(12), Bit#(26), Bit#(3), Bit#(1)) CSRtoDecode;

  typedef Tuple5#(Privilege_mode, Bit#(XLEN), Bit#(32), Bit#(5), Bit#(XLEN)) DumpType;

	typedef enum {
		Inst_addr_misaligned=0,
		Inst_access_fault=1,
		Illegal_inst=2,
		Breakpoint=3,
		Load_addr_misaligned=4,
		Load_access_fault=5,
		Store_addr_misaligned=6,
		Store_access_fault=7,
		Ecall_from_user=8,
		Ecall_from_machine=11
	} Exception_cause deriving (Bits,Eq,FShow);

	typedef enum{
		User_soft_int=0,
		Machine_soft_int=3,
		User_timer_int=4,
		Machine_timer_int=7,
		User_external_int=8,
		Machine_external_int=11
	} Interrupt_cause deriving (Bits,Eq,FShow);

	typedef union tagged{
	  Exception_cause Exception;
	  Interrupt_cause Interrupt;
	  void None;
	} Trap_type deriving(Bits,Eq,FShow);

  // fabrics related definitions.
  typedef 0 USERSPACE;
endpackage
