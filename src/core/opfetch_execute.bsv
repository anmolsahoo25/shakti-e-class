package opfetch_execute;
/*packages to be imported*/	
	import GetPut::*;
	import FIFO::*;
	import RegFile::*;



/*files to be included*/
	import isa_defs::*;
	import common_types::*;
	import TxRx	::*;
	import alu::*;//execute stage incorporated in the opfetch to generate a single unit "opfetch and execute unit"

   `include "common_params.bsv"

 
//==================================================interface declaration==================================================
interface Inf_op_ex;

	//put interface for the pc value
	interface Put#(Bit#(32)) pc_from_decode_unit;

	//rs1,rs2,rd,fn,funct3,instruction_type are given by the fetch and decode unit
	interface RXe#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) from_fetch_decode_unit;//to receive the decoded data from previous stage
	
	//result being transfered to the memory and write back
	interface TXe#(Bit#(64)) to_mem_wb_unit;
	//rd,valid and value given back by the mem and wb unit for eliminating congestion
	interface Put#(Tuple3#(Bit#(5),Bool,Bit#(64))) wb_mem_to_opfetch;
	//rd and value given back by the write back unit
	interface Put#(Tuple2#(Bit#(5),Bit#(64))) wb_to_regFile;

	//interface method for the regfile
	method ActionValue#(Tuple2#(Bit#(64),Bit#(64))) inputs_from_decode_stage(Bit#(5) rs1_addr, Operand_type rs1_type, Bit#(5) rs2_addr, Operand_type rs2_type, Bit#(PADDR) pc, Bit#(XLEN) imm);
	method Action write_rd(Bit#(5) r, Bit#(XLEN) d, Operand_type rdtype);//write in general purpose register
	method Bit#(XLEN) read_igpr(Bit#(5) r);//reading the general purpose register

endinterface

(*synthesize*)
module mkOpfetch_execute(Inf_op_ex);

	//===========================================generating the register file==========================================
	RegFile#(Bit#(5),Bit#(XLEN)) integer_rf <-mkRegFileWCF(0,31);
		Reg#(Bool) initialize<-mkReg(True);
		Reg#(Bit#(5)) rg_index<-mkReg(0);
		rule initialize_regfile(initialize);
			integer_rf.upd(rg_index,0);
			rg_index<=rg_index+1;
			if(rg_index=='d31)
				initialize<=False;
		endrule


	//========================================FIFO between the mem unit and opfetch unit========================================
	FIFO#(Bit#(XLEN)) exe_to_mem_fifo <-mkSizedFIFO(2);

	//=================================================TXRX interface instantiation=================================================
	TX#(Bit#(64)) tx<-mkTX;//instantiating the TX interface
	RX#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) rx<-mkRX;//intantiating the RX interface
	//========================================================================================================== 

	//==============================receiving the decoded data from the previous stage====================================
	let {fn,rs1_addr_in,rs2_addr_in,rd_addr_in,immediate,word32,funct3,rs1_type_in,rs2_type_in,insttype,mem_access}=rx.u.first;

//=====================================================================================================================

// rs1,rs2 will be passed to the register file and the recieve value along with the other parameters reqiured by the alu function 
//  will be passed
	let {op1,op2}=inputs_from_decode_stage(rs1_addr_in,rs1_type_in,rs2_addr_in,rs2_type_in,PC_in,immediate);

//======================================================using the alu function====================================
//how to obtain pc value from the previous stage,a seperate interface is required of the output of the previous stage has to be changed
	let {instType,address_op1_result,data_op2_effaddr,meminfo_rs1addr,funct3_addr}=fn_alu(fn,op1,op2,immediate_value,PC_in,insttype,funct3,mem_access,rd,word32);

//======================================================passing the result in FIFO===================================
	exe_to_mem_fifo.enq(address_op1_result)	;


//=========================================================interface definition=======================================================
interface from_fetch_decode_unit=rx.e;

interface to_mem_wb_unit=tx.e;

interface wb_mem_to_opfetch=interface Put
	method Action put (Tuple3#(Bit#(5),Bool,Bit#(XLEN)) from_mem_to_opfetch );
		{rd,valid,value}<=from_mem_to_opfetch;
		if(rs1==rd&&valid)
			let rs1_value=value;
		else
			rs1_value<=rs1;
		if(rs2==rd&&valid)	
			let rs2_value=value;
		else
			rs2_value<=rs2;		
	endmethod	
endinterface;

interface wb_to_regFile=interface Put
	method Action put (Tuple2#(Bit#(5),Bit#(XLEN)) from_mem_to_rf );
		{rd,value}<=from_mem_to_rf;
	endmethod
endinterface;

//getting the pc value from the bus on which the decoder puts the pc value
interface pc_from_decode_unit=interface Put
	method Action put (Bit#(32) pc);
		PC_in<=pc;//saving the pc value
	endmethod
endinterface;

method ActionValue#(Tuple2#(Bit#(64),Bit#(64))) inputs_from_decode_stage(Bit#(5) rs1_addr, Operand_type rs1_type, Bit#(5) rs2_addr, Operand_type rs2_type, Bit#(PADDR) pc, Bit#(XLEN) imm);
	
	Bit#(XLEN) rs1=0;
			Bit#(XLEN) rs2=0;

			if(rs1_type==PC)
				rs1=signExtend(pc);
			else if(rs1_addr==0 && rs1_type==IntegerRF)
				rs1=0;
			else if(rs1_type==IntegerRF)
				rs1=integer_rf.sub(rs1_addr);
			if(rs2_type==Immediate)
				rs2=imm;
			else if(rs2_addr==0 && rs2_type==IntegerRF)
				rs2=0;
			else if(rs2_type==IntegerRF)
				rs2=integer_rf.sub(rs2_addr);
			`ifdef verbose $display($time,"\nReg1 :%d : ",rs1_addr,fshow(rs1),"\nReg2 : %d : ",rs2_addr,fshow(rs2)); `endif
         return tuple2(rs1,rs2);

endmethod
method Action write_rd(Bit#(5) r, Bit#(XLEN) d, Operand_type rdtype) if(!initialize); // TODO if not in critical path shift the CReg ports.
			`ifdef verbose $display($time,"\tRF: Writing into reg: :%d data: %h ",r,d,fshow(rdtype)); `endif
			if(rdtype==IntegerRF)begin
				if(r!=0)begin
					integer_rf.upd(r,d);
				end
			end
endmethod
method Bit#(XLEN)read_igpr(Bit#(5) r);	
			return integer_rf.sub(r);
endmethod
endmodule:mkOpfetch_execute
endpackage
