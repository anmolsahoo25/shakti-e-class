package isa_defs;

	typedef Bit #(3)  Funct3;
	Funct3 f3_MUL     = 3'b000;     // 0
	Funct3 f3_MULH    = 3'b001;     // 1
	Funct3 f3_MULHSU  = 3'b010;     // 2
	Funct3 f3_MULHU   = 3'b011;     // 3
	Funct3 f3_DIV     = 3'b100;     // 4
	Funct3 f3_DIVU    = 3'b101;     // 5
endpackage
