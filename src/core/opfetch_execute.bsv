package opfetch_execute;
/*packages to be imported*/	
	import GetPut::*;
	import FIFO::*;


/*files to be included*/
	import isa_defs::*;
	import common_types::*;
	import TxRx	::*;
	import alu::*;//execute stage incorporated in the opfetch to generate a single unit "opfetch and execute unit"

   `include "common_params.bsv"

 
//==================================================interface declaration==================================================
interface Inf_op_ex;
	//rs1,rs2,rd,fn,funct3,instruction_type are given by the fetch and decode unit
	interface RXe#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) from_fetch_decode_unit;//to receive the decoded data from previous stage
	
	interface TXe#(Bit#(64)) to_mem_wb_unit;//result being transfered to the memory and write back

	interface Put#(Tuple3#(Bit#(5),Bool,Bit#(64))) wb_mem_to_opfetch;//rd,valid and value given back by the mem and wb unit for eliminating congestion

	interface Put#(Tuple2#(Bit#(5),Bit#(64))) wb_to_regFile;//rd and value given back by the write back unit

endinterface


module mkOpfetch_execute(Inf_op_ex);

	//=================================================TXRX interface instantiation=================================================
	TX#(Bit#(64)) tx<-mkTX;//instantiating the TX interface
	RX#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) rx<-mkRX;//intantiating the RX interface
	//========================================================================================================== 

	//receiving the decoded data from the previous stage
	let {fn,rs1_addr,rs2_addr,rd_addr,immediate,word32,funct3,rs1_type,rs2_type,insttype,mem_access}=rx.u.first;

//=====================================================================================================================
// rs1,rs2 will be passed to the register file and the recieve value along with the other parameters reqiured by the alu function 
//  will be passed


//using the alu function
//how to obtain pc value from the previous stage,a seperate interface is required of the output of the previous stage has to be changed
let {instType,address_op1_result,data_op2_effaddr,meminfo_rs1addr,funct3_addr}=fn_alu(fn,op1,op2,immediate_value,pc,insttype,funct3,mem_access,rd,word32);



//=========================================================interface definition=======================================================
interface from_fetch_decode_unit=rx.e;

interface to_mem_wb_unit=tx.e;

interface wb_mem_to_opfetch=interface Put
	method Action put (Tuple3#(Bit#(5),Bool,Bit#(64)) from_mem_to_opfetch );
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
	method Action put (Tuple2#(Bit#(5),Bit#(64)) from_mem_to_rf );
		{rd,value}<=from_mem_to_rf;

	endmethod
endinterface;

endmodule:mkOpfetch_execute
endpackage
