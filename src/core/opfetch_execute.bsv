package opfetch_execute;
/*packages to be imported*/	
	import GetPut::*;
	import FIFO::*;


/*files to be included*/
	import isa_defs::*;
	import common_types::*;
	import TxRx	::*;

   `include "common_params.bsv"

 

interface Inf_op_ex;
	//rs1,rs2,rd,fn,funct3,instruction_type are given by the fetch and decode unit
	interface RXe#(Tuple8#(Bit#(4),Bit#(5),Bit#(5),Bit#(5),Bit#(XLEN),Bool,Bit#(3),
    	Tuple4#(Operand_type,Operand_type,Instruction_type,Access_type))) from_opfetch_unit;//to receive the decoded data from previous stage
	
	interface TXe#(Bit#(64)) to_mem_wb_unit;//result being transfered to the memory and write back

	interface Put#(Tuple3#(Bit#(5),Bool,Bit#(64))) wb_mem_to_opfetch;//rd,valid and value given back by the mem and wb unit for eliminating congestion

	interface Put#(Tuple2#(Bit#(5),Bit#(64))) wb_to_regFile;//rd and value given back by the write back unit

endinterface

endpackage
