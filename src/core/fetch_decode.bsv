package fetch_decode;
// packages to be imported
	import GetPut::*;
	import TxRx	::*;
	import FIFO::*;

// project files to be imported/included
	import isa_defs::*;
	import common_types::*;
  `include "common_params.bsv"

  (*noinline*)
    function Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple5#(Operand_type,Operand_type,Instruction_type,Access_type,Bit#(32)))
    		 decoder_func(Bit#(32) inst,Bit#(32) shadow_pc);
			Bit#(5) rs1=inst[19:15];
			Bit#(5) rs2=inst[24:20];
			Bit#(5) rd =inst[11:7] ;
			Bit#(5) opcode= inst[6:2];
			Bit#(3) funct3= inst[14:12];
			Bool word32 =False;
			Bit#(32) pc=shadow_pc;

			//operand types
			Operand_type rs1type=IntegerRF;
			Operand_type rs2type=IntegerRF;

			//memory access type
			Access_type mem_access=Load;
			if(opcode[3]=='b1 && opcode[1]==0)
				mem_access=Store;

			//immediate value 
			Bit#(XLEN) immediate_value=signExtend(inst[31:20]);
			if(opcode==`JAL_R_op)
				immediate_value=signExtend({inst[31:20],1'b0});
			else if(opcode==`BRANCH_op)
				immediate_value=signExtend({inst[31],inst[7],inst[30:25],inst[11:8],1'b0}); 
			else if	(opcode==`STORE_op)
				immediate_value=signExtend({inst[31:25],inst[11:7]});
			else if(opcode==`SYSTEM_INSTR_op)//what should be done for systems instruction		
				immediate_value[16:12]=inst[19:15];

			//instruction following U OR UJ TYPE INSTRUCTION FORMAT	
			//funct3[2]==1 might not be required as division is not included till now
			if (opcode==`JAL_R_op || (opcode==`SYSTEM_INSTR_op && funct3[2]==1))	
				rs1=0;
			//instruction following I,U OR UJ INSTRUCTION FORMAT	
			if (opcode==`SYSTEM_INSTR_op || opcode[4:2]=='b000// CSR or ( (F)Load or FENCE ) 
  				||opcode[4:2]=='b001 || opcode==`JAL_R_op)	//LUI or JAL 
				rs2=0;
			//insturction following S OR SB TYPE INSTRUCTION FORMAT
			if (opcode==`BRANCH_op || opcode[4:1]=='b0100)	
				rd=0;

			if(opcode==`JAL_R_op)	
				rs1type=PC;
			if(opcode==`JAL_R_op || opcode=='b001)	
				rs2type=Immediate;
			
			//instructions which support word lenght operation in RV64 are to be added in Alu
			//need to be edited according to the supported instruction

			//if(opcode==`IMM_ARITHW_op || opcode==`MULDIVW_op ||
			 //opcode==`ARITHW_op ||(opcode[4:3]=='b10 && funct7[0]==0)||
			  //(opcode[4:1]=='b0101 && funct3[0]==0)) 
      		//word32=True;
      			

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
      		else if(opcode==`IMM_ARITH_op)begin
			fn=case(funct3)
				'b010: 'b1100;
				'b011: 'b1110;
				'b101: 'b0101;
				default:{1'b0,funct3};
			endcase;
			end
			else if(opcode==`ARITH_op)begin
				fn=case(funct3)
					'b000:'b0000;
					'b010:'b1100;
					'b011:'b1110;
					'b101:'b0101;
					default:{1'b0,funct3};
			endcase;
			end		
      		else if(opcode[4:3]=='b10)	
      			fn=opcode[3:0];


      	/*return (Decoder_returnvalue {fn:fn,rs1:rs1,rs2:rs2,rd:rd, 
                        rs1type:rs1type,rs2type:rs2type,inst_type:inst_type,
                        immediate_value:immediate_value, 
                        word32:word32,mem_access:mem_access,funct3:funct3});*/		

            Tuple5#(Operand_type,Operand_type,Instruction_type,Access_type,Bit#(32)) type_tuple = 
            tuple5(rs1type,rs2type,inst_type,mem_access,pc);

            return tuple8(fn,rs1,rs2,rd,immediate_value, 
                        word32,funct3,type_tuple);            
    endfunction


  // Interface for the fetch and decode unit
	interface Ifc_fetch_decode;
	interface Get#(Bit#(32)) inst_in;//instruction whose addr is needed
	interface Put#(Bit#(32)) inst_addr;//addr of the given inst
  // rs1,rs2,rd,fn,funct3,instruction_type will be passed on to opfetch and execute unit
    interface TXe#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple5#(Operand_type,Operand_type,Instruction_type,Access_type,Bit#(32)))) to_opfetch_unit;

	endinterface:Ifc_fetch_decode
	(*synthesize*)
	module mkFetch_decode(Ifc_fetch_decode);

		Reg#(Bit#(32)) pc <- mkRegU;//making program counter
		Reg#(Bit#(32)) shadow_pc <-mkRegU;//shadow pc to preserve it
    //instantiating the tx interface with name tx
		TX#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple5#(Operand_type,Operand_type,Instruction_type,Access_type,Bit#(32)))) tx<-mkTX;
    
    //instruction whose addr is needed
		interface inst_in=interface Get
			method ActionValue#(Bit#(32)) get;
				shadow_pc<=pc;
				return pc+4;
			endmethod
		endinterface;
    
    //getting response from bus 
		interface inst_addr= interface Put
			method Action put (Bit#(32) inst);
				Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),Tuple5#(Operand_type,
			Operand_type,Instruction_type,Access_type,Bit#(32))) x= decoder_func(inst,shadow_pc);
				tx.u.enq(x);//enq the output of the decoder function in the tx interface
			endmethod
		endinterface;

    //providing the output of the decoder function to the opfetch unit via tx interface
		interface to_opfetch_unit=tx.e;
	endmodule:mkFetch_decode
endpackage:fetch_decode
