`ifdef usertraps
  `define non_m_traps True
`endif
`ifdef supervisor
  `define non_m_traps True
`endif
`define FNADD   0   //'b0000
`define FNSL	  1	  //'b0001  	
`define FNSEQ	  2	  //'b0010  	
`define FNSNE	  3	  //'b0011  	
`define FNXOR	  4	  //'b0100  	
`define FNSR	  5	  //'b0101  	
`define FNOR	  6	  //'b0110	
`define FNAND	  7	  //'b0111
`define FNSUB	  10	//'b1010
`define FNSRA	  11	//'b1011
`define FNSLT	  12	//'b1100
`define FNSGE	  13	//'b1101
`define FNSLTU	14	//'b1110
`define FNSGEU	15	//'b1111

`define FNLR	2	
`define FNSC    3	
`define FNSWAP 1 
`define FMINU	10
`define FMIN	11
`define FMAXU  12
`define FMAX	13

`define FNRAND	8

/////////////////////////////////////////////////////////////////////////
////////////////////// opcode definitions of ISA ////////////////////////
`define AUIPC_op			    'b00101
`define LUI_op				    'b01101
`define JAL_op  			    'b11011
`define JALR_op  			    'b11001
`define BRANCH_op			    'b11000
`define LOAD_op				    'b00000
`define STORE_op			    'b01000
`define IMM_ARITH_op	    'b00100
`define	ARITH_op			    'b01100
`ifdef RV64
  `define IMM_ARITHW_op	  'b00110
  `define	ARITHW_op		    'b01110
  `define MULDIVW_op		  'b01110
`endif
`define	CSR_op				    'b11100
`define	SYSTEM_INSTR_op		'b11100
`define	MULDIV_op			    'b01100
`define	ATOMIC_op			'b01011
`define FENCE_op          'b00011

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// funct3 defintions for ISA ////////////////////

`define MUL_f3		'b000
`define MULH_f3		'b001
`define MULHSU_f3	'b010
`define MULHU_f3	'b011
`define DIV_f3		'b100
`define DIVU_f3		'b101

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////// Register Mapping for Machine Mode Regs /////////////////
`define MSTATUS	'h300 // Machine Status register                                
`define MISA		'h301 // ISA and extensions                                     
`define MEDELEG	'h302 // Machine exception delegation                               
`define MIDELEG	'h303 // Machine interrupt delegation                               
`define MIE			'h304 // Machine interrupt enable                                   
`define MTVEC		'h305 // Machine trap-handler base address                          
`define MCOUNTEREN  'h306 // Machine counter setup register                                  
`define MSCRATCH	'h340 // Scratch rgister for machine trap hanglers                  
`define MEPC			'h341 // Machine exception program counter                          
`define MCAUSE		'h342 // Machine trap cause                                         
`define MTVAL		  'h343 // Machine bad address                                        
`define MIP			  'h344 // Machine interrupt pending
`define MCYCLE		'hB00 // Machine cycle counter                                      
`define MTIME		  'hB01	// mtime register (Non-standard r/w)
`define MINSTRET	'hB02 // Machine instructions retired.                              
`define MTIMECMP	'hB20 //  time compare register (Non-standard r/w)
`define MCYCLEH	  'hB80 // Upper 32 bits of mcycle                                   
`define MTIMEH		'hB81	// mtime hi-register (Non-standard r/w)
`define MINSTRETH 'hB82 // Upper 32 bits of minstret.                                 
`define MTIMECMPH 'hBA0 //  time compare hi-register (Non-standard r/w)
`define MVENDORID 'hF11 // Vendor ID                                                  
`define MARCHID	  'hF12 // Architecture ID                                           
`define MIMPID		'hF13 // Implementation ID                                        
`define MHARTID		'hF14 // Hardware Thread ID                                      
////// Reister Mapping for User Mode Regs /////////////////
`define USTATUS	  'h000 // User status register
`define FFLAGS		'h001 // FP Accrued exceptions
`define FRM			  'h002 // FP Dynamic rounding mode
`define FCSR			'h003 // FP Control and status register
`define UIE			  'h004 // User interrupt enable register
`define UTVEC		  'h005 // User trap handler base address
`define USCRATCH	'h040 // Scratch register for user trap handlers
`define UEPC			'h041 // User exception program counter
`define UCAUSE		'h042 // User trap cause
`define UTVAL		  'h043 // User bad address or illegal instruction
`define UIP			  'h044 // User interrupt pending
`define UMEMSE		'h045 // Machine Memory Structures enable
`define UCYCLE		'hC00 // cycle counter for RDCYCLE instruction.
`define UTIME		  'hC01 // Tiemr for RDTIME instruction
`define UINSTRET	'hC02 // Instruction retired counter for RDINSTRET
`define UCYCLEH	  'hC80 // Upper 32bits of UCYCLE
`define UTIMEH		'hC81 // Upper 32bits of UTIME
`define UINSTRETH 'hC82 // Upper 32bits of UINSTRET
////////////////////////////////////////////////////////////////////////////////////
`define CACHECNTRL 'h800 // cache control register

`define Inst_addr_misaligned  0 
`define Inst_access_fault     1 
`define Illegal_inst          2 
`define Breakpoint            3 
`define Load_addr_misaligned  4 
`define Load_access_fault     5 
`define Store_addr_misaligned 6 
`define Store_access_fault    7 
`define Ecall_from_user       8 
`define Ecall_from_machine    11
`define Int_divide_by_zero    17
`define FP_invalid            18
`define FP_divide_by_zero     19
`define FP_overflow           20
`define FP_underflow          21
`define FP_inexact            22

`define User_soft_int         0
`define Machine_soft_int      3
`define User_timer_int        4
`define Machine_timer_int     7
`define User_external_int     8
`define Machine_external_int  11
`define HaltEbreak            17
`define HaltTrigger           18
`define HaltDebugger          19
`define HaltStep              20
`define HaltReset             21
`define Resume_int            22
