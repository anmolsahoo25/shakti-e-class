// Bluespec wrapper, created by Import BVI Wizard
// Created on: Sat Nov 17 20:17:10 IST 2018
// Created by: neel
// Bluespec version: 2018.10.beta1 2018-10-17 e1df8052c

package bram_1rw;
interface Ifc_bram_1rw#(numeric type addr_width, numeric type data_width, numeric type memsize);
	(*always_enabled*)
	method Action request (Bit#(data_width) dina, Bit#(addr_width) addra, Bit#(1) wea);
	(*always_enabled*)
	method Bit#(data_width) response ();
endinterface

import "BVI" bram_1rw =
module mkbram_1rw  (Ifc_bram_1rw#(addr_width, data_width, memsize));

	parameter ADDR_WIDTH = valueOf(addr_width);
	parameter DATA_WIDTH = valueOf(data_width);
	parameter MEMSIZE = valueOf(memsize);

	default_clock clk_clka;
	default_reset no_reset;

	input_clock clk_clka (clka)  <- exposeCurrentClock;


	method request (dina /*DATA_WIDTH-1:0*/, addra /*ADDR_WIDTH-1:0*/, wea /*0:0*/)
		 enable(ena) clocked_by(clk_clka);
	method douta /* DATA_WIDTH-1 : 0 */ response ()
		 clocked_by(clk_clka);

	schedule request C request;
	schedule request CF response;
	schedule response CF response;
endmodule

endpackage
