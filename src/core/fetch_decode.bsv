package fetch_decode;
/*packages to be imported*/	
	import GetPut::*;
	import Connectable::*;
	import FIFO::*;


/*files to be included*/
	import isa_defs::*;
	import common_types::*;

    `include "common_params.bsv"

    /*typedef struct
    {
      Bit#(4) fn;  
      Bit#(5) rs1;  
      Bit#(5) rs2;  
      Bit#(5) rd;  
      Operand_type rs1type;  
      Operand_type rs2type; 
      Instruction_type inst_type; 
      Bit#(XLEN) immediate_value; 
      Bool word32; 
      Access_type mem_access;    
      Bit#(3) funct3; 
    } Decoder_returnvalue deriving(Bits,Eq,FShow);*/


    (*noinline*)
    function Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type)) decoder_func(Bit#(32) inst);
			Bit#(5) rs1=inst[19:15];
			Bit#(5) rs2=inst[24:20];
			Bit#(5) rd =inst[11:7] ;
			Bit#(5) opcode= inst[6:2];
			Bit#(3) funct3= inst[14:12];
			Bool word32 =False;

			//operand types
			Operand_type rs1type=IntegerRF;
			Operand_type rs2type=IntegerRF;

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
                        rs1type:rs1type,rs2type:rs2type,inst_type:inst_type,immediate_value:immediate_value, 
                        word32:word32,mem_access:mem_access,funct3:funct3});*/		

            Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type) type_tuple = tuple4(rs1type,rs2type,inst_type,mem_access)            ;

            return tuple8(fn,rs1,rs2,rd,immediate_value, 
                        word32,funct3,type_tuple);            
    endfunction

/***************************************************Interface for the fetch and decode unit***************************************/
	interface Ifc_fetch_decode;
		interface Get#(Bit#(32)) inst_in;//instruction whose addr is needed
		interface Put#(Bit#(32)) inst_addr;//addr of the given inst
		interface Get#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) to_opfetch_unit;/*rs1,rs2,rd,fn,funct3,instruction_type  all of this will be passed on to opfetch and execute unit*/
	endinterface:Ifc_fetch_decode
/***************************************************************************************************************************/
	(*synthesize*)
	module mkFetch_decode(Ifc_fetch_decode);

		Reg#(Bit#(32)) pc <- mkRegU;//making program counter
		Reg#(Bit#(32)) shadow_pc <-mkRegU;//shadow pc to preserve it
		
		FIFO#((Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type)))) to_exe_unit<-mkSizedFIFO(1);
		

		/*********************************************Interface description****************************************/	
		
		interface inst_in=interface Get//instruction whose addr is needed
			method ActionValue#(Bit#(32)) get;
				shadow_pc<=pc;
				return pc+4;
			endmethod
		endinterface;

		interface inst_addr= interface Put//getting response from bus 
			method Action put (Bit#(32) inst);
				//let instruction=inst;//reading the value given by the bus
				//let {fn,rs1,rs2,rd,rs1type,rs2type,inst_type,immediate_value,word32,mem_access,funct3}=decoder_func(instruction);//calling the decoder function 
				Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	             Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type)) x= decoder_func(inst);
				to_exe_unit.enq(x);
			endmethod
		endinterface;

		interface to_opfetch_unit=interface Get//placing the inst. details in FIFO, which is to be read by opfetch unit
			method ActionValue#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	             Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) get;
					to_exe_unit.deq;
				return to_exe_unit.first;
			endmethod
		endinterface;
	endmodule:mkFetch_decode

	
	/*=======================================================test bench==========================================================*/
	
	module mkTest(Empty);

			Bit#(5) rs1_base=5'b00010;
			Bit#(5) rs2_base=5'b11111;
			Bit#(5) rd_base =5'b10101;
			Bit#(5) opcode_base=`JAL_R_op;
			Bit#(3) funct3_base=3'b000;
			Bit#(7) immediate_value_base=7'b0000000;
			Bool word32_base=False;

			Operand_type rs1type_base=IntegerRF;
			Operand_type rs2type_base=IntegerRF;

			//memory access type
			Access_type mem_access_base=Load;
			if(opcode_base[3]=='b1 && opcode_base[1]==0)
				mem_access_base=Store;


			//getting the value from decoder function	
			Bit#(32) instaddr_generation={immediate_value_base,rs2_base,rs1_base,funct3_base,rd_base,opcode_base,2'b11};	
									
									//=====================================			

//some how connect the put interface of the main module to a new get interface of test module and check the result through the output of the FIFO going to the opfetch or the execute unit

			rule rule1;
				let {a,b,c,d,e,f,g,h}=decoder_func(instaddr_generation);

				//=================correct output=======================

				let fn=0;
				let golden_rs1=0;	
				let golden_rs2=0;
				let golden_rd=instaddr_generation[11:7];
				let golden_immediate_value=signExtend({instaddr_generation[31:20],1'b0});
				let golden_rs1type=PC;
				let golden_rs2type=Immediate;
				let golden_mem_access=mem_access_base;
				Instruction_type golden_inst_type=JAL_R;
				
				Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type) golden_type_tuple=tuple4(golden_rs1type,golden_rs2type,golden_inst_type,mem_access_base);

				//checking the function generated output with the golden output
				if(fn!=a||golden_rs1!=b||golden_rs2!=c||rd_base!=d||golden_immediate_value!=e||word32_base!=f||funct3_base!=g||golden_type_tuple!=h)
					$display("issue with respect to the JAL_R instruction");
				$finish(0);
			endrule
	endmodule:mkTest

endpackage:fetch_decode