// Bluespec wrapper, created by Import BVI Wizard
// Created on: Tue Nov 13 14:52:16 IST 2018
// Created by: neel
// Bluespec version: 2018.10.beta1 2018-10-17 e1df8052c

package bram_1r1w;
interface Ifc_bram_1r1w#(numeric type addr_width, numeric type data_width, numeric type memsize);
	(*always_enabled*)
	method Action write (Bit#(data_width) dina, Bit#(addr_width) addra, Bit#(1) wea);
	(*always_enabled*)
	method Action read (Bit#(addr_width) addrb);
	(*always_enabled*)
	method Bit#(data_width) response ();
endinterface

import "BVI" bram_1r1w =
module mkbram_1r1w  (Ifc_bram_1r1w#(addr_width, data_width, memsize));

	parameter ADDR_WIDTH = valueOf(addr_width);
	parameter DATA_WIDTH = valueOf(data_width);
	parameter MEMSIZE = valueOf(memsize);

	default_clock clk_clka;
	default_reset no_reset;

	input_clock clk_clka (clka)  <- exposeCurrentClock;
	input_clock clk_clkb (clkb)  <- exposeCurrentClock;


	method write (dina /*DATA_WIDTH-1:0*/, addra /*ADDR_WIDTH-1:0*/, wea /*0:0*/)
		 enable(ena) clocked_by(clk_clka);
	method read (addrb /*ADDR_WIDTH-1:0*/)
		 enable(enb) clocked_by(clk_clkb);
	method doutb /* DATA_WIDTH-1 : 0 */ response ()
		 clocked_by(clk_clka);

	schedule write C write;
	schedule write CF read;
	schedule write CF response;
	schedule read C read;
	schedule read CF response;
	schedule response CF response;
endmodule

endpackage
