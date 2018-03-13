import GetPut::*;
package fetch_decode;
	interface Int_fetch_decode;
		method Action inst_in Put#(Bit#(32) instruction);//instruction whose addr is needed
		method Get#(Bit#(32)) inst_addr;//addr of the given inst
	endinterface:Int_fetch_decode
	module mkFetch_decode(Int_fetch_decode);

	endmodule:mkFetch_decode
endpackage:fetch_decode