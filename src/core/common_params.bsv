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
`define RV64

/////////////////////////////////////////////////////////////////////////
////////////////////// opcode definitions of ISA ////////////////////////
`define JAL_R_op			'b11001
`define BRANCH_op			'b11000
`define LOAD_op				'b00000
`define STORE_op			'b01000
`define IMM_ARITH_op	    'b00100
`define	ARITH_op			'b01100
`ifdef RV64
	`define IMM_ARITHW_op	'b00110
	`define	ARITHW_op		'b01110
	`define MULDIVW_op		'b01110
`endif
`define	CSR_op				'b11100
`define	SYSTEM_INSTR_op		'b11100
`define	MULDIV_op			'b01100

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// funct3 defintions for ISA ////////////////////

`define MUL_f3		'b000
`define MULH_f3		'b001
`define MULHSU_f3	'b010
`define MULHU_f3	'b011
`define DIV_f3		'b100
`define DIVU_f3		'b101