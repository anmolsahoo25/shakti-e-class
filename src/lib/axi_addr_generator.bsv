package axi_addr_generator;
	/*=== Project imports ===*/
	import common_types::*;
	`include "common_params.bsv"
	/*=======================*/

	// This function is used by the slaves on the AXI4 bus to generate the sequential addresses in 
  // burst mode. The different modes supported are :
	// FIXED: the same address is sent in all transactions. Typically used in polling modes.
	// INCR: The address is simply incremented arlen number of times from the starting address.
	// WRAP: This mode supports only 4 valid lengths: 2, 4 8 and 16 bursts. the increments happen in 
  // a wrap arouind fashion.
	function Bit#(awidth) burst_address_generator(Bit#(8) arlen, Bit#(3) arsize,
  Bit#(2) arburst, Bit#(awidth) address);

		// this variable will decide the index above which part of the address should
		// not change in WRAP mode. Bits below this index value be incremented according
		// to the value of arlen and arsize;
		Bit#(3) wrap_size;
		case(arlen)
			3: wrap_size= 2;
			7: wrap_size= 3;
			15: wrap_size=4;
			default:wrap_size=1;
		endcase
  
    // this is address will directly be used for INCR mode
		Bit#(awidth) new_address=address+(('b1)<<arsize);
		Bit#(awidth) mask;
		mask=('1)<<(zeroExtend(arsize)+wrap_size);	// create a mask for bits which will remain constant in WRAP.
		Bit#(awidth) temp1=address& mask;	  // capture the constant part of the addr in WRAP.
		Bit#(awidth) temp2=new_address& (~mask);//capture the incremental part of the addr in WRAP.

		if(arburst==0) // FIXED
			return address;
		else if(arburst==1) // INCR
			return new_address;
		else // WRAP
			return temp1|temp2; // create the new address in the wrap mode by ORing the masked values.
	endfunction
endpackage
