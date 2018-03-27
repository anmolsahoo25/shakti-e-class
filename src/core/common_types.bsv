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
	typedef 128 XLEN_2;
	typedef Bit#(XLEN) Data;
	typedef Bit#(XLEN_2) Data_2;
	typedef Int #(XLEN) Data_S;   // Signed register data
	typedef UInt#(XLEN) Data_U;
	typedef 32 PADDR ;

  `ifdef RV64
    typedef 2 Byte_offset;
  `else
    typedef 1 Byte_offset;
  `endif

	typedef Bit #(3)  Funct3;
	Funct3 f3_MUL     = 3'b000;     // 0
	Funct3 f3_MULH    = 3'b001;     // 1
	Funct3 f3_MULHSU  = 3'b010;     // 2
	Funct3 f3_MULHU   = 3'b011;     // 3
	Funct3 f3_DIV     = 3'b100;     // 4
	Funct3 f3_DIVU    = 3'b101;     // 5

  // Define all enums here 
	typedef enum {ALU, MUL, DIV, MEMORY, BRANCH, JAL_R, SYSTEM_INSTR, ILLEGAL} Instruction_type 
                                  deriving(Bits, Eq, FShow); // the type of the decoded instruction.
	typedef enum {Load, Store} Access_type deriving (Bits, Eq, FShow);
	typedef enum {Flush, None} Flush_type deriving (Bits, Eq, FShow);
	typedef enum {IntegerRF, PC} Operand1_type deriving(Bits, Eq, FShow);
	typedef enum {IntegerRF, Immediate} Operand2_type deriving(Bits, Eq, FShow);
  typedef enum {SYSTEM_INSTR, MEMORY, REGULAR} Commit_type deriving(Eq, Bits, FShow);
  typedef enum {Machine=3, User=0} Privilege_mode deriving(Eq, Bits, FShow);

  // define all tuples here
  `ifdef simulate
    typedef Tuple8#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(XLEN), Bool, Bit#(3),
              Tuple8#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(PADDR),
                Trap_type, Bit#(1) `ifdef simulate , Bit#(32) `endif )) PIPE1_DS;

    typedef Tuple8#(Commit_type, Bit#(XLEN), Bit#(21), Bit#(PADDR), Bit#(5), Bit#(1), 
                    Trap_type `ifdef simulate , Bit#(32) `endif ) PIPE2_DS;
  `else
    typedef Tuple8#(Bit#(4), Bit#(5), Bit#(5), Bit#(5), Bit#(XLEN), Bool, Bit#(3),
              Tuple7#(Operand1_type, Operand2_type, Instruction_type, Access_type, Bit#(PADDR),
                Trap_type, Bit#(1))) PIPE1_DS;

    typedef Tuple7#(Commit_type, Bit#(XLEN), Bit#(21), Bit#(PADDR), Bit#(5), Bit#(1), 
                    Trap_type) PIPE2_DS;
  `endif
  typedef Tuple3#(Commit_type, Bit#(XLEN), Bit#(21)) ALU_OUT;
  
  typedef Tuple5#(Bit#(PADDR), Bit#(XLEN), Access_type, Bit#(2), Bit#(1)) MemoryRequest;

  typedef Tuple3#(Bit#(5), Bool, Bit#(XLEN)) OpFwding;
  // rg_prv,  csr_mip, csr_mie, csr_mideleg, rg_mie 
  typedef Tuple5#(Privilege_mode, Bit#(XLEN), Bit#(XLEN), Bit#(XLEN), Bit#(1)) CSRtoDecode;

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
