`define FNADD   0	
`define FNSL	1	
`define FNLR	2	
`define FNSEQ	2	
`define FNSC    3	
`define FNSNE	3	
`define FNXOR	4	
`define FNSR	5	
`define FNOR	6	
`define FNAND	7	
`define FNSUB	10	
`define FNSRA	11	
`define FNSLT	12	
`define FNSGE	13	
`define FNSLTU	14	
`define FNSGEU	15	

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

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// funct3 defintions for ISA ////////////////////

`define MUL_f3		'b000
`define MULH_f3		'b001
`define MULHSU_f3	'b010
`define MULHU_f3	'b011
`define DIV_f3		'b100
`define DIVU_f3		'b101

/////////////////////////// Register Mapping for Machine Mode Regs /////////////////
`define MSTATUS	'h00 		//'h300 // Machine Status register                                
`define MISA			'h01 	//'h301 // ISA and extensions                                     
`define MEDELEG	'h02 		//'h302 // Machine exception delegation                               
`define MIDELEG	'h03 		//'h303 // Machine interrupt delegation                               
`define MIE			'h04 		//'h304 // Machine interrupt enable                                   
`define MTVEC		'h05 		//'h305 // Machine trap-handler base address                          
`define MCOUNTEREN		'h06 //'h306 // Machine counter setup register                                  
`define MSCRATCH	'h40 	//'h340 // Scratch rgister for machine trap hanglers                  
`define MEPC			'h41 	//'h341 // Machine exception program counter                          
`define MCAUSE		'h42 	//'h342 // Machine trap cause                                         
`define MTVAL		'h43 		//'h343 // Machine bad address                                        
`define MIP			'h44 		//'h344 // Machine interrupt pending
`define MCYCLE		'h00 		//'hB00 // Machine cycle counter                                      
`define MTIME		'h01 		  //'hB01	// mtime register (Non-standard r/w)
`define MINSTRET	'h02 		//'hB02 // Machine instructions retired.                              
`define MTIMECMP	'h20 		//'hB20 //  time compare register (Non-standard r/w)
`define MCYCLEH	'h80 		  //'hB80 // Upper 32 bits of mcycle                                   
`define MTIMEH		'h81 		//'hB81	// mtime hi-register (Non-standard r/w)
`define MINSTRETH'h82 			//'hB82 // Upper 32 bits of minstret.                                 
`define MTIMECMPH'hA0 			//'hBA0 //  time compare hi-register (Non-standard r/w)
`define MVENDORID'h11 			//'hF11 // Vendor ID                                                  
`define MARCHID	'h12 		  //'hF12 // Architecture ID                                           
`define MIMPID		'h13 		//'hF13 // Implementation ID                                        
`define MHARTID	'h14 		  //'hF14 // Hardware Thread ID                                      
////// Reister Mapping for User Mode Regs /////////////////
`define USTATUS	'h00 	//'h000 // User status register
`define FFLAGS		'h01 //'h001 // FP Accrued exceptions
`define FRM			'h02 	//'h002 // FP Dynamic rounding mode
`define FCSR			'h03 //'h003 // FP Control and status register
`define UIE			'h04 	//'h004 // User interrupt enable register
`define UTVEC		'h05 	//'h005 // User trap handler base address
`define USCRATCH	'h40 //'h040 // Scratch register for user trap handlers
`define UEPC			'h41 //'h041 // User exception program counter
`define UCAUSE		'h42 //'h042 // User trap cause
`define UTVAL		'h43 	//'h043 // User bad address or illegal instruction
`define UIP			'h44 	//'h044 // User interrupt pending
`define UMEMSE		'h45 //'h045 // Machine Memory Structures enable
`define UCYCLE		'h00 //'hC00 // cycle counter for RDCYCLE instruction.
`define UTIME		'h01 	//'hC01 // Tiemr for RDTIME instruction
`define UINSTRET	'h02 //'hC02 // Instruction retired counter for RDINSTRET
`define UCYCLEH	'h80 	//'hC80 // Upper 32bits of UCYCLE
`define UTIMEH		'h81 //'hC81 // Upper 32bits of UTIME
`define UINSTRETH'h82 	//'hC82 // Upper 32bits of UINSTRET
////////////////////////////////////////////////////////////////////////////////////
