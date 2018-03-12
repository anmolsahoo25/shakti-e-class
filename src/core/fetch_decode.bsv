import GetPut::*;
package fetch_decode
	interface Int_fetch_decode
		method Action put(Bit#(32) instruction);
		method ActionValue #(Int#(32)) get();
	endinterface:Int_fetch_decode
	

	module mkFetch_decode
	endmodule:mkFetch_decode
endpackage:fetch_decode