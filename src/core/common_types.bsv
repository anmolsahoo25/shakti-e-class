package common_types;
	import isa_defs::*;
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
	typedef enum {ALU,MULDIV,MEMORY,BRANCH,JAL_R,SYSTEM_INSTR}Instruction_type deriving(Bits, Eq,FShow); // the type of the decoded instruction.
	typedef enum {Load, Store} Access_type deriving (Bits,Eq,FShow);
	typedef enum {Flush,None} Flush_type deriving (Bits,Eq,FShow);
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
		Ecall_from_supervisor=9,
		Ecall_from_machine=11,
		Inst_pagefault=12,
		Load_pagefault=13,
		Store_pagefault=15
		`ifdef simulate ,Endsimulation =16 `endif
	} Exception_cause deriving (Bits,Eq,FShow);
	typedef enum{
		/*==== Standard Interrupts =============== */
		User_soft_int=0,
		Supervisor_soft_int=1,
		Machine_soft_int=3,
		User_timer_int=4,
		Supervisor_timer_int=5,
		Machine_timer_int=7,
		User_external_int=8,
		Supervisor_external_int=9,
		Machine_external_int=11,
		/*=============================*/
		/*===== Non Standard Interrupts ========= */
		DebugInterrupt =12,
		DebugResume=13,
		DebugReset=14
	} Interrupt_cause deriving (Bits,Eq,FShow);

	typedef union tagged{
	  Exception_cause Exception;
	  Interrupt_cause Interrupt;
	  void None;
	} Trap_type deriving(Bits,Eq,FShow);

	typedef struct{
		Bit#(PADDR) address;
		Bit#(XLEN) memory_data; // data to be written in the memory
		Bit#(TLog#(TDiv#(PADDR, 8))) transfer_size; // 0 -8 bits, 1- 16 bits, 2 -32 bits 3 - 64-bits;
		Bit#(1) signextend; // whether the loaded value has to be signextended
		Access_type mem_type; // STORE or AMO or LOAD or FENCE
		`ifdef atomic Bit#(5) atomic_op;`endif
	}Memout deriving(Bits,Eq,FShow);

	typedef struct{
		Bit#(XLEN) aluresult;
		Bit#(5) fflags;
	} Arithout deriving(Bits,Eq,FShow); // output struct from the alu.

	typedef struct{
		Bit#(XLEN) rs1;
		Bit#(XLEN) rs2;
		Bit#(5) rs1_addr;
		Bit#(12) csr_address;
		Bit#(3) funct3;
	}CSRInputs deriving(Bits,Eq,FShow);
	
	typedef union tagged{
		Arithout RESULT;	// 64+5
		Memout MEMORY;		// 64+64+3+1+3+5 = 140
		CSRInputs SYSTEM; // 64+64+5+12+3 = 148
		void Busy;
	} Execution_output deriving(Bits,Eq,FShow);
endpackage
