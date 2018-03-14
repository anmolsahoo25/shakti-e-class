package fetch_decode;
	
	import GetPut::*;
	import common_types::*;
    `include "common_params.bsv"

    typedef struct
    {
      Bit#(4) fn;  
      Bit#(5) rs1;  
      Bit#(5) rs2;  
      Bit#(5) rd;  
      Operand_type rs1type;  
      Operand_type rs2type; 
      Operand_type rdtype; 
      Instruction_type inst_type; 
      Bit#(XLEN) immediate_value; 
      Bool word32; 
      Access_type mem_access;    
      Bit#(3) funct3; 
    } Decoder_returnvalue deriving(Bits,Eq,FShow);

    (*noinline*)
    function Decoder_returnvalue decoder_func(Bit#(32) inst);
			Bit#(5) rs1=inst[19:15];
			Bit#(5) rs2=inst[24:20];
			Bit#(5) rd =inst[11:7] ;
			Bit#(5) opcode= inst[6:2];//should it be [6:2]
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

			/*immediate value */
			Bit#(XLEN) immediate_value=signExtend(inst[31:20]);
			if(opcode==`JAL_R_op)
				immediate_value=signExtend({inst[31:20],1'b0});
			else if(opcode==`BRANCH_op)
				immediate_value=signExtend({inst[31],inst[7],inst[30:25],inst[11:8],1'b0}); 
			else if	(opcode==`STORE_op)
				immediate_value=signExtend({inst[31:25],inst[11:7]});
			else if(opcode==`SYSTEM_INSTR_op)//what should be done for systems instruction		
				immediate_value[16:12]=inst[19:15];

			/*instruction following U OR UJ TYPE INSTRUCTION FORMAT*/	
			if (opcode==`JAL_R_op || (opcode==`SYSTEM_INSTR_op && funct3[2]==1))	//funct3[2]==1 might not be required as division is not included till now
				rs1=0;
			/*instruction following I,U OR UJ INSTRUCTION FORMAT*/	
			if (opcode==`SYSTEM_INSTR_op || opcode[4:2]=='b000                           // CSR or ( (F)Load or FENCE ) 
  				||opcode[4:2]=='b001 || opcode==`JAL_R_op)	//LUI or JAL 
				rs2=0;
			/*insturction following S OR SB TYPE INSTRUCTION FORMAT*/	
			if (opcode==`BRANCH_op || opcode[4:1]=='b0100)	
				rd=0;

			if(opcode==`JAL_R_op)	
				rs1type=PC;
			if(opcode==`JAL_R_op || opcode=='b001)	
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
      				'b001:inst_type=JAL_R;
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
      		if(opcode==`BRANCH_op)begin
      			if(funct3[2]==0)
      				fn={2'b0,1,funct3[0]};
      			else
      				fn={1'b1,funct3}	;
      		end
      		else if(opcode==`JAL_R_op || opcode==`LOAD_op || opcode==`STORE_op)
      			fn=0;
      		else if(opcode[4:3]=='b10)	
      			fn=opcode[3:0];


      	return (Decoder_returnvalue {fn:fn,rs1:rs1,rs2:rs2,rd:rd, 
                        rs1type:rs1type,rs2type:rs2type,rdtype:rdtype, 
                        inst_type:inst_type,immediate_value:immediate_value, 
                        word32:word32,mem_access:mem_access,funct3:funct3});		
    endfunction

/***************************************************Interface for the fetch and decode unit***************************************/
	interface Ifc_fetch_decode;
		interface Put#(Bit#(32)) inst_in;//instruction whose addr is needed
		interface Get#(Bit#(32)) inst_addr;//addr of the given inst
	endinterface:Ifc_fetch_decode
/***************************************************************************************************************************/

	module mkFetch_decode(Ifc_fetch_decode);

		Reg#(Bit#(TDiv#(XLEN,2))) pc <- mkRegU;//making program counter
		/*Method description*/	
		interface inst_in=interface Put
			method Action put(Bit#(32) instruction);
			endmethod
		endinterface;

		interface inst_addr= interface Get
			/*method ActionValue(Bit#(32)) get;
				return 
			endmethod*/
		endinterface;

	endmodule:mkFetch_decode
endpackage:fetch_decode