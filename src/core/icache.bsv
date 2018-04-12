package icache;
	import common_types::*;
	import BRAMCore::*;
	`include "common_params.bsv"
	interface Ifc_icache;
	//taking in the instruction required by the core
		method Action fromCore_toCache(Bit#(PADDR) inst_addr);
		//returning the required instruction to the core
		method Maybe#(Bit#(PADDR)) fromCache_toCore();
		//fromCache_to_mem only in case of cache miss
		method ActionValue#(Bit#(PADDR)) fromCache_to_mem(Bit#(PADDR) line_fetch_addr);
	endinterface
	(*synthesize*)
	module mkIcache(Ifc_icache);
		Reg#(Bool) cache_hit<-mkReg(False);

		let byte_bits=valueOf(TLog#(`ICACHE_WORD_SIZE));	// number of bits to select a byte within a word. = 2
		let word_bits=valueOf(TLog#(`ICACHE_BLOCK_SIZE));	// number of bits to select a word within a block. = 4
		let set_bits=valueOf(TLog#(`ICACHE_SETS));			// number of bits to select a set from the cache. = 
		
		//instantiating BRAM
		BRAM_DUAL_PORT#(Bit#(TLog#(`ICACHE_SETS)),Bit#(TAdd#(`ICACHE_TAG_BITS,2))) tag [`ICACHE_WAYS];
		BRAM_DUAL_PORT_BE#(Bit#(TLog#(`ICACHE_SETS)),Bit#(TMul#(TMul#(8,`ICACHE_WORD_SIZE),`ICACHE_BLOCK_SIZE)),64) data [`ICACHE_WAYS];
		for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin
			tag[i] <- mkBRAMCore2(`ICACHE_SETS,False);		
			data[i] <-mkBRAMCore2BE(`ICACHE_SETS,False);
		end

		//response to the core
		Wire#(Maybe#(Bit#(PADDR))) wr_response_to_cpu<-mkDWire(tagged Invalid);

		Reg#(Bit#(TAdd#(1,TLog#(`ICACHE_SETS)))) rg_index <-mkReg(0);
		Reg#(Maybe#(Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE)))) rg_we<-mkReg(tagged Invalid);
		Reg#(Bit#(TMul#(`ICACHE_WORD_SIZE,`ICACHE_BLOCK_SIZE))) line_bytes_written<-mkReg(0);

		//rule to read the cache data
		rule cache_read;

			Bit#(`ICACHE_WAYS) valid_values;
			Bit#(TMul#(TMul#(`ICACHE_BLOCK_SIZE,`ICACHE_WORD_SIZE),8)) dataline=0;

			for(Integer i=0;i<`ICACHE_WAYS;i=i+1)begin
						let stored_tag=tag[i].a.read[19:0];
						let stored_valid=tag[i].a.read[20];
						valid_values[i]=tag[i].a.read[20];
						if(stored_valid==1)begin // if a tag matches capture the tag and data
							cache_hit<=True;	
							dataline=data[i].a.read;
						end
					end
					Bit#(32) data_value=dataline[31:0];

			//value to be sent to core
			wr_response_to_cpu<=tagged Valid data_value;	

		endrule

		method Action fromCore_toCache(Bit#(PADDR) inst_addr);
			Bit#(PADDR) instaddr=inst_addr;
		endmethod

		method Maybe#(Bit#(PADDR)) fromCache_toCore();
			return wr_response_to_cpu;
		endmethod

		method ActionValue#(Bit#(PADDR)) fromCache_to_mem(Bit#(PADDR) line_fetch_addr);
			Bit#(PADDR) line_addr=line_fetch_addr;
			return line_addr;
		endmethod

	endmodule
endpackage