/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and/or other materials provided 
 with the distribution.  
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Author: Neel Gala
Email id: neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package memory;
  import BRAMCore :: *;
	import DReg::*;
	import Semi_FIFOF        :: *;
	import AXI4_Types   :: *;
	import AXI4_Fabric  :: *;
	import AXI4_Lite_Types   :: *;
	import AXI4_Lite_Fabric  :: *;
  import Tilelink_lite_Types::*;
	import BUtils::*;
  import GetPut::*;
	import axi_addr_generator::*;

  interface UserInterface#(numeric type awidth,  numeric type dwidth, numeric type mem_size);
    method Action read_request (Bit#(awidth) addr,  Bit#(2) size);
    method Action write_request (Tuple3#(Bit#(awidth), Bit#(dwidth),  Bit#(TDiv#(dwidth, 8))) req);
    method ActionValue#(Tuple2#(Bool, Bit#(dwidth))) read_response;
    method ActionValue#(Bool) write_response;
  endinterface
 
  // to make is synthesizable replace awidth with Physical Address width
  // dwidth with data lane width
  module mkmemory#(Bit#(awidth) base_address, parameter String 
      mem_init_file1, parameter String mem_init_file2 )(UserInterface#(awidth, dwidth, mem_size))
    provisos(Add#(dwidth, a, 64), // provisos ensures we support < 64-bit data width. 
             Add#(32, b, dwidth),  // provisos ensures we support >32-bit data width.
             Add#(4, a__, TDiv#(dwidth, 8)),  // wstrb is between 4 and 8
             Mul#(TDiv#(TSub#(dwidth, 32), 4), 4, TSub#(dwidth, 32)));
    Integer verbosity = `VERBOSITY;
    Integer byte_offset = valueOf(TDiv#(dwidth, 32));
  	// we create 2 32-bit BRAMs since the xilinx tool is easily able to map them to BRAM32BE cells
  	// which makes it easy to use data2mem for updating the bit file.
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(TSub#(dwidth, 32)),4) dmemMSB <- 
                   mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file1,False);
		BRAM_DUAL_PORT_BE#(Bit#(TSub#(mem_size,2)),Bit#(32),4) dmemLSB <- 
                   mkBRAMCore2BELoad(valueOf(TExp#(TSub#(mem_size,2))),False,mem_init_file2,False);
  
    Reg#(Bool) read_request_sent <-mkDReg(False);
    
    // A write request to memory. Single cycle operation.
    method Action write_request (Tuple3#(Bit#(awidth), Bit#(dwidth),  Bit#(TDiv#(dwidth, 8))) req);
      let {addr, data, strb}=req;

			Bit#(TSub#(mem_size,2)) index_address=(addr - base_address)[valueOf(mem_size)-1:byte_offset+1];
			dmemLSB.b.put(truncate(strb),index_address,truncate(data));
			dmemMSB.b.put(truncateLSB(strb),index_address,truncateLSB(data));
  	endmethod
  
    // The write response will always be an error.
    method ActionValue#(Bool) write_response;
      return False;
    endmethod
  
    // capture a read_request and latch the address on a BRAM.
    method Action read_request (Bit#(awidth) addr,  Bit#(2) size);
			Bit#(TSub#(mem_size,2)) index_address=(addr - base_address)[valueOf(mem_size)-1:byte_offset+1];
  		dmemLSB.a.put(0, index_address, ?);
      dmemMSB.a.put(0, index_address, ?);
      read_request_sent<= True;
  		if(verbosity!= 0)
        $display($time, "\tBootROM: Recieved Read Request for Address: %h Index Address: %h",  
                                                                            addr, index_address);
  	endmethod
  
    // respond with data from the BRAM.
    method ActionValue#(Tuple2#(Bool, Bit#(dwidth))) read_response if(read_request_sent);
      return tuple2(False, {dmemMSB.a.read(), dmemLSB.a.read()});
    endmethod
  endmodule

  interface Ifc_memory_AXI4#(numeric type awidth, numeric type dwidth, numeric type uwidth,  
                                                                           numeric type mem_size);
    interface AXI4_Slave_IFC#(awidth, dwidth, uwidth) slave; 
  endinterface

  typedef enum {Idle, Burst} Mem_State deriving(Eq, Bits, FShow);

  module mkmemory_AXI4#(Bit#(awidth) base, parameter String mem_init_file1, 
        parameter String mem_init_file2 )(Ifc_memory_AXI4#(awidth, dwidth, uwidth, mem_size))
    provisos(Add#(dwidth, a, 64), 
             Mul#(8, a__, dwidth), 
             Mul#(16, b__, dwidth), 
             Mul#(32, c__, dwidth), 
             Add#(32, b, dwidth), 
             Add#(4, d__, TDiv#(dwidth, 8)));
    UserInterface#(awidth, dwidth, mem_size) dut <- mkmemory(base, mem_init_file1, mem_init_file2);
	  AXI4_Slave_Xactor_IFC #(awidth, dwidth, uwidth)  s_xactor <- mkAXI4_Slave_Xactor;
    Integer verbosity = `VERBOSITY;
    Reg#(Bit#(4)) rg_rd_id <-mkReg(0);
    Reg#(Mem_State) read_state <-mkReg(Idle);
    Reg#(Mem_State) write_state <-mkReg(Idle);
	  Reg#(Bit#(8)) rg_readburst_counter<-mkReg(0);
	  Reg#(AXI4_Rd_Addr	#(awidth, uwidth)) rg_read_packet <-mkReg(?);
		Reg#(AXI4_Wr_Addr	#(awidth, uwidth)) rg_write_packet<-mkReg(?); 

    // If the request is single then simple send ERR. If it is a burst write request then change
    // state to Burst and do not send response.
    rule write_request_address_channel(write_state==Idle);
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
      dut.write_request(tuple3(aw.awaddr, w.wdata, w.wstrb));
	    let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: aw.awuser, bid:aw.awid};
      if(!w.wlast)
        write_state<= Burst;
      else
  	  	s_xactor.i_wr_resp.enq (b);
			rg_write_packet<=aw;
    endrule
    // if the request is a write burst then keeping popping all the data on the data_channel and
    // send a error response on receiving the last data.
    rule write_request_data_channel(write_state==Burst);
      let w  <- pop_o (s_xactor.o_wr_data);
  		let address=burst_address_generator(rg_write_packet.awlen, rg_write_packet.awsize, 
            rg_write_packet.awburst, rg_write_packet.awaddr);
      dut.write_request(tuple3(address, w.wdata, w.wstrb));
	    let b = AXI4_Wr_Resp {bresp: AXI4_OKAY, buser: rg_write_packet.awuser, 
                          bid:rg_write_packet.awid};
      rg_write_packet.awaddr<=address;
      if(w.wlast) begin
	  	  s_xactor.i_wr_resp.enq (b);
        write_state<= Idle;
      end
    endrule
    // read first request and send it to the dut. If it is a burst request then change state to
    // Burst. capture the request type and keep track of counter.
    rule read_request_first(read_state==Idle);
		  let ar<- pop_o(s_xactor.o_rd_addr);
      dut.read_request(ar.araddr, ar.arsize);
      rg_rd_id<= ar.arid;
      if(ar.arlen!=0)
        read_state<=Burst;
      rg_readburst_counter<= ar.arlen;
		  rg_read_packet<=ar;
    endrule
    // incase of burst read,  generate the new address and send it to the dut untill the burst
    // count has been reached.
    rule read_request_burst(read_state==Burst);
      if(rg_readburst_counter==rg_read_packet.arlen)
        read_state<=Idle;
      else begin
  		  let address=burst_address_generator(rg_read_packet.arlen, rg_read_packet.arsize, 
            rg_read_packet.arburst, rg_read_packet.araddr);
        rg_read_packet.araddr<=address;
        rg_readburst_counter<= rg_readburst_counter+1;
        dut.read_request(address, rg_read_packet.arsize);
      end
    endrule
    // get data from the memory. shift,  truncate, duplicate based on the size and offset.
    rule read_response;
      let {err, data0}<-dut.read_response;
  		let transfer_size=rg_read_packet.arsize;
      `ifdef RV64
        let shift_amount = {3'b0, rg_read_packet.araddr[2:0]}<<3;
      `else
        let shift_amount = {3'b0, rg_read_packet.araddr[1:0]}<<3;
      `endif
      data0=data0>>shift_amount;
      if(transfer_size=='d2)
        data0=duplicate(data0[31:0]);
      else if(transfer_size=='d1)
        data0=duplicate(data0[15:0]);
      else if(transfer_size=='d0)
        data0=duplicate(data0[7:0]);
      AXI4_Rd_Data#(dwidth, uwidth) r = AXI4_Rd_Data {rresp: AXI4_OKAY, rdata: data0 , 
        rlast:rg_readburst_counter==rg_read_packet.arlen, ruser: 0, rid:rg_read_packet.arid};
  		if(verbosity!=0) 
        $display($time, "\tBootROM : Responding Read Request with Data: %h ",data0);
      s_xactor.i_rd_data.enq(r);
    endrule
    interface slave = s_xactor.axi_side;
  endmodule
  
  interface Ifc_memory_AXI4Lite#(numeric type awidth, numeric type dwidth, numeric type uwidth, 
                                                                           numeric type mem_size);
    interface AXI4_Lite_Slave_IFC#(awidth, dwidth, uwidth) slave; 
  endinterface


  module mkmemory_AXI4Lite#(Bit#(awidth) base, parameter String mem_init_file1, 
        parameter String mem_init_file2 )(Ifc_memory_AXI4Lite#(awidth, dwidth, uwidth, mem_size))
    provisos(Add#(dwidth, a, 64), 
             Mul#(8, a__, dwidth), 
             Mul#(16, b__, dwidth), 
             Mul#(32, c__, dwidth), 
             Add#(32, b, dwidth), 
             Add#(4, d__, TDiv#(dwidth, 8)));
    UserInterface#(awidth, dwidth, mem_size) dut <- mkmemory(base, mem_init_file1, mem_init_file2);
	  AXI4_Lite_Slave_Xactor_IFC #(awidth, dwidth, uwidth)  s_xactor <- mkAXI4_Lite_Slave_Xactor;
    Integer verbosity = `VERBOSITY;
    Integer byte_offset = valueOf(TDiv#(dwidth, 32));
    Reg#(Bit#(2)) rg_size <-mkReg(3);
    Reg#(Bit#(TAdd#(1, TDiv#(dwidth, 32)))) rg_offset <-mkReg(0);
    // If the request is single then simple send ERR. If it is a burst write request then change
    // state to Burst and do not send response.
    rule write_request_address_channel;
      let aw <- pop_o (s_xactor.o_wr_addr);
      let w  <- pop_o (s_xactor.o_wr_data);
	    let b = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: aw.awuser};
      dut.write_request(tuple3(aw.awaddr, w.wdata, w.wstrb));
	  	s_xactor.i_wr_resp.enq (b);
    endrule
    // read first request and send it to the dut. If it is a burst request then change state to
    // Burst. capture the request type and keep track of counter.
    rule read_request_first;
		  let ar<- pop_o(s_xactor.o_rd_addr);
      dut.read_request(ar.araddr, ar.arsize);
      rg_size<= ar.arsize;
      rg_offset<= ar.araddr[byte_offset:0];
    endrule
    // get data from the memory. shift,  truncate, duplicate based on the size and offset.
    rule read_response;
      let {err, data0}<-dut.read_response;
  		let transfer_size=rg_size;
      let shift_amount = {3'b0, rg_offset}<<3;
      data0=data0>>shift_amount;
      if(transfer_size=='d2)
        data0=duplicate(data0[31:0]);
      else if(transfer_size=='d1)
        data0=duplicate(data0[15:0]);
      else if(transfer_size=='d0)
        data0=duplicate(data0[7:0]);
      AXI4_Lite_Rd_Data#(dwidth, uwidth) r = AXI4_Lite_Rd_Data {rresp: AXI4_LITE_OKAY, rdata: data0 , 
        ruser: 0};
  		if(verbosity!=0) 
        $display($time, "\tBootROM : Responding Read Request with Data: %h ",data0);
      s_xactor.i_rd_data.enq(r);
    endrule
    interface slave = s_xactor.axi_side;
  endmodule
  
  interface Ifc_memory_TLU#(numeric type a, numeric type w, numeric type z, numeric type mem_size);
    interface Ifc_fabric_side_slave_link_lite#(a, w, z) read_slave; 
    interface Ifc_fabric_side_slave_link_lite#(a, w, z) write_slave; 
  endinterface

  module mkmemory_TLU#(Bit#(a) base, parameter String mem_init_file1, parameter String
                                                mem_init_file2)(Ifc_memory_TLU#(a, w, z, mem_size))
    provisos(Mul#(w, 8, dwidth), 
             Add#(dwidth, e, 64), 
             Mul#(8, a__,  dwidth), 
             Mul#(16, b__, dwidth), 
             Mul#(32, c__, dwidth), 
             Div#(dwidth, 8, w),
             Add#(32, e__, dwidth), // required by BSV 
             Add#(4, f__, TDiv#(dwidth, 8)),  // required by BSV
             Add#(d__, 2, z)); // to ensure that we are only operating upto 64 bits
    
    UserInterface#(a, dwidth, mem_size) dut <- mkmemory(base, mem_init_file1, mem_init_file2);
    Ifc_Slave_link_lite#(a, w, z)  write_xactor <- mkSlaveXactorLite(True, True);
    Ifc_Slave_link_lite#(a, w, z)  read_xactor <- mkSlaveXactorLite(True, True);

    Integer verbosity = `VERBOSITY;
    Integer byte_offset = valueOf(TDiv#(dwidth, 32));
    Reg#(Bit#(z)) rg_size <-mkReg(3);
    Reg#(Bit#(2)) rg_source <- mkReg(0);
    Reg#(Bit#(TAdd#(1, TDiv#(dwidth, 32)))) rg_offset <-mkReg(0);
    // If the request is single then simple send ERR. If it is a burst write request then change
    // state to Burst and do not send response.
    rule write_request_address_channel;
      let req<- write_xactor.core_side.xactor_request.get;
      dut.write_request(tuple3(req.a_address, req.a_data, req.a_mask));
      let lv_resp = D_channel_lite{d_opcode: AccessAck, d_size: ?, d_source: req.a_source,
                                                      d_sink: ?, d_data: ?, d_error: False};
	  	write_xactor.core_side.xactor_response.put(lv_resp);
    endrule
    // read first request and send it to the dut. If it is a burst request then change state to
    // Burst. capture the request type and keep track of counter.
    rule read_request_first;
      let req <- read_xactor.core_side.xactor_request.get;
      dut.read_request(req.a_address, truncate(req.a_size));
      rg_size<= req.a_size;
      rg_offset<= req.a_address[byte_offset:0];
      rg_source<= req.a_source;
    endrule
    // get data from the memory. shift,  truncate, duplicate based on the size and offset.
    rule read_response;
      let {err, data0}<-dut.read_response;
  		let transfer_size=rg_size;
      let shift_amount = {3'b0, rg_offset}<<3;
      data0=data0>>shift_amount;
      if(transfer_size=='d2)
        data0=duplicate(data0[31:0]);
      else if(transfer_size=='d1)
        data0=duplicate(data0[15:0]);
      else if(transfer_size=='d0)
        data0=duplicate(data0[7:0]);

      D_channel_lite#(w, z) lv_resp=D_channel_lite { d_opcode : AccessAckData, d_size : rg_size, 
            d_source : rg_source, d_sink : ?, d_data : data0, d_error : False};
  		if(verbosity!=0) 
        $display($time, "\tBootROM : Responding Read Request with Data: %h ", data0);
	  	read_xactor.core_side.xactor_response.put(lv_resp);
    endrule
    interface read_slave = read_xactor.fabric_side;
    interface write_slave = write_xactor.fabric_side;
  endmodule

//  module mkTb(Empty);
//    Ifc_memory_AXI4#(32, 32, 0, 17) boot <-mkmemory_AXI4('h1000, "code.mem.MSB", "code.mem.LSB");
//    Ifc_memory_AXI4Lite#(32, 32, 0, 17) bootlite <-mkmemory_AXI4Lite('h1000, "code.mem.MSB", "code.mem.LSB");
//    Ifc_memory_TLU#(32, 8, 4, 17) memory_tlu <-mkmemory_TLU('h1000, "code.mem.MSB", "code.mem.LSB");
//  endmodule
endpackage
