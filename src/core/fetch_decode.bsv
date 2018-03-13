package fetch_decode;
	
	import GetPut::*;
	import common_types::*;
    import common_params::*;


	interface Ifc_fetch_decode;
		method Put#(Bit#(32)) inst_in(Bit#(32) instruction);//instruction whose addr is needed
		method Get#(Bit#(32)) inst_addr();//addr of the given inst
	endinterface:Ifc_fetch_decode


	module mkFetch_decode(Ifc_fetch_decode);

		Reg#(Bit#(TDiv#(XLEN,2))) pc <- mkRegU;//making program counter

		rule decode;
			
			let inst = inst_addr();//getting the instruction address 
			Bit#(5) rs1=inst[19:15];
			Bit#(5) rs2=inst[24:20];
			Bit#(5) rd =inst[11:7] ;
			Bit#(5) opcode= inst[6:0];//should it be [6:2]
			Bit#(3) funct3= inst[14:12];
			Bool word32 =False;

			//operand types
			Operand_type rs1type=IntegerRF;
			Operand_type rs2type=IntegerRF;
			Operand_type rdtype=IntegerRF;

			//memory access type
			Access_type mem_access=Load;
			if(opcode[3]=='b1 && opcode[1]==0)
				mem_access=Store;


			Bit#(XLEN) immediate_value=signExtend(inst[31:20]);
			if(opcode==JAL_R)
				immediate_value=signExtend({inst[31:20],1'b0});
			else if(opcode==BRANCH)
				immediate_value=signExtend({inst[31],inst[7],inst[30:25],inst[11:8],1'b0}); 
			else if	(opcode==Store)
				immediate_value=signExtend({inst[31:25],inst[11:7]});
			else if(opcode==SYSTEM_INSTR)//what should be done for systems instruction		
				immediate_value[16:12]=instruction[19:15];

			/*instruction following U OR UJ TYPE INSTRUCTION FORMAT*/	
			if (opcode==JAL_R || (opcode==SYSTEM_INSTR && funct3[2]==1))	//funct3[2]==1 might not be required as division is not included till now
				rs1=0;
			/*instruction following I,U OR UJ INSTRUCTION FORMAT*/	
			if (opcode==SYSTEM_INSTR || opcode[4:2]=='b000                           // CSR or ( (F)Load or FENCE ) 
  				||opcode[4:2]=='b001 || opcode==JAL_R)	//LUI or JAL 
				rs2=0;
			/*insturction following S OR SB TYPE INSTRUCTION FORMAT*/	
			if (opcode==BRANCH || opcode[4:1]=='b0100)	
				rd=0;

			if(opcode==JAL_R)	
				rs1type=PC;
			if(opcode==JAL_R || opcode=='b001)	
				rs2type=Immediate;
			/*
			**************************instructions which support word lenght operation in RV64 are to be added in Alu **************************
			//need to be edited according to the supported instruction

			if(opcode==`IMM_ARITHW_op || opcode==`MULDIVW_op || opcode==`ARITHW_op || (opcode[4:3]=='b10 && funct7[0]==0) 
         					|| (opcode[4:1]=='b0101 && funct3[0]==0)) 
      		word32=True;
      		*/	

      		Instruction_type inst_type=NOP;
      		if(opcode[4:3]=='b11)begin
      			case(opcode[2:0])
      				'b011:inst_type=JAL_R;
      				'b000:inst_type=BRANCH;
      				'b100:inst_type=SYSTEM_INSTR;
      			endcase
      		end
      		else if(opcode[4:3]=='b00)begin
      			case(opcode[2:0])
      				'b000,'b001:inst_type=MEMORY;
      				'b101,'b100,'b110:inst_type=ALU;
      			endcase
      		end
      		Bit#(4) fn=0;
      		if(opcode==BRANCH)begin
      			if(funct3[2]==0)
      				fn={2'b0,1,funct3[0]};
      			else
      				fn={1'b1,funct3}	;
      		end
      		else if(opcode==JAL_R || opcode==Load || opcode==Store)
      			fn=0;
      		else if(opcode[4:3]=='b10)	
      			fn=opcode[3:0];
		endrule:decode


		/*Method description*/	
		method Put#(Bit#(32)) inst_in(Bit#(32) instruction);
			
		endmethod

		method Get#(Bit#(32)) inst_addr();
			return 
		endmethod



	endmodule:mkFetch_decode
endpackage:fetch_decode