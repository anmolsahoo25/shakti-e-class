package isa_defs;

	typedef Bit #(3)  Funct3;
	typedef Bit #(7)  Funct7;
	Funct3 f3_MUL     = 3'b000;     // 0
	Funct7 f7_MUL     = 7'b0000001; // 1
	Funct3 f3_MULH    = 3'b001;     // 1
	Funct7 f7_MULH    = 7'b0000001; // 1
	Funct3 f3_MULHSU  = 3'b010;     // 2
	Funct7 f7_MULHSU  = 7'b0000001; // 1
	Funct3 f3_MULHU   = 3'b011;     // 3
	Funct7 f7_MULHU   = 7'b0000001; // 1
	Funct3 f3_DIV     = 3'b100;     // 4
	Funct7 f7_DIV     = 7'b0000001; // 1
	Funct3 f3_DIVU    = 3'b101;     // 5
	Funct7 f7_DIVU    = 7'b0000001; // 1
endpackage
