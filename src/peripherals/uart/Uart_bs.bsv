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
package Uart_bs;

	`define Depth 16

	import common_types::*;
	`include "common_parameters.bsv"

	import AXI4_Lite_Types::*;
	import AXI4_Lite_Fabric::*;
	import AXI4_Types::*;
	import AXI4_Fabric::*;
	import Semi_FIFOF::*;
	import RS232_modified::*;
	import GetPut::*;
	import FIFO::*;
	import Clocks::*;

	interface Ifc_uart_bs_axi4lite;
  	interface AXI4_Lite_Slave_IFC#(PADDR,XLEN,USERSPACE) slave_axi_uart;
		interface RS232 coe_rs232;
	endinterface

	(*synthesize*)
	module mkuart_bs_axi4lite#(Clock core_clock, Reset core_reset)(Ifc_Uart_bs);

    let verbosity = valueOf(`VERBOSITY);
		Clock uart_clock<-exposeCurrentClock;
		Reset uart_reset<-exposeCurrentReset;
		Reg#(Bit#(16)) baud_value <-mkReg(BAUD_RATE);
		UART#(`Depth) uart <-mkUART(8,NONE,STOP_1,baud_value); // charasize,Parity,Stop Bits,BaudDIV
		AXI4_Lite_Slave_Xactor_IFC #(PADDR,XLEN,USERSPACE)  s_xactor <- 
                              mkAXI4_Lite_Slave_Xactor(clocked_by core_clock, reset_by core_reset);
		Reg#(Bit#(4)) rg_status <-mkReg(0);		//This register keeps track of whether some data
															//is pending to be sent out through the UART Tx
		
		SyncFIFOIfc#(AXI4_Lite_Rd_Addr#(PADDR,USERSPACE))	ff_rd_addr <-	
                                                            mkSyncFIFOToCC(1,core_clock,core_reset);
		SyncFIFOIfc#(AXI4_Lite_Wr_Addr#(PADDR, USERSPACE)) ff_wr_addr <-	
                                                            mkSyncFIFOToCC(1,core_clock,core_reset);
		SyncFIFOIfc#(AXI4_Lite_Wr_Data#(XLEN)) ff_wr_data <-	mkSyncFIFOToCC(1,core_clock,core_reset);

		SyncFIFOIfc#(AXI4_Lite_Rd_Data#(XLEN,USERSPACE))	ff_rd_resp <-	
                                                                    mkSyncFIFOFromCC(1,core_clock);
		SyncFIFOIfc#(AXI4_Lite_Wr_Resp#(USERSPACE))	ff_wr_resp <-	mkSyncFIFOFromCC(1,core_clock);

		rule capture_read_request;
			let req <- pop_o (s_xactor.o_rd_addr);
			ff_rd_addr.enq(req);
		endrule

		//Address 'h11304 is uart read data
		rule rl_handle_axi4_uart_read(ff_rd_addr.notEmpty && ff_rd_addr.first.araddr[3:2]=='d1);
			let req = ff_rd_addr.first;
			ff_rd_addr.deq;
			Bit#(8) data<-uart.tx.get;
			let lv_resp= AXI4_Lite_Rd_Data {rresp:AXI4_LITE_OKAY, rdata: zeroExtend(data), ruser: ?};

			if(verbosity>2) begin
        $display($time,"\tUART_BS:Req: RD_ADDR %h", req.araddr); 
			  $display($time,"\tUART_BS:Resp: RD_RESP %h", req.araddr); 
      end
			ff_rd_resp.enq(lv_resp);
		endrule

		//Address 'b11308 is uart read status
		rule rl_handle_axi4_uart_status(ff_rd_addr.notEmpty && ff_rd_addr.first.araddr[3:2]!='d1);
			let req =ff_rd_addr.first;
			ff_rd_addr.deq;
			let lv_resp= AXI4_Lite_Rd_Data {rresp:AXI4_LITE_OKAY, rdata: zeroExtend(rg_status), ruser: ?};
			if(req.araddr[3:2]==2)
				lv_resp.rdata=zeroExtend(rg_status);
			else if(req.araddr[3:2]==3)
				lv_resp.rdata=zeroExtend(baud_value);
			else
				lv_resp.rresp=AXI4_LITE_SLVERR;

			if(verbosity>2) begin
			  $display($time,"\tUART_BS:Req: RD_ADDR %h", req.araddr); 
			  $display($time,"\tUART_BS:Resp: RD_RESP %h Status: %b", req.araddr, rg_status);
      end
			ff_rd_resp.enq(lv_resp);
		endrule
		
		rule send_read_respone_to_bus;
			s_xactor.i_rd_data.enq(ff_rd_resp.first);
			ff_rd_resp.deq;
		endrule

		rule capture_write_request;
			let req <- pop_o (s_xactor.o_wr_addr);
			let wr_data <- pop_o(s_xactor.o_wr_data);
			ff_wr_addr.enq(req);
			ff_wr_data.enq(wr_data);
		endrule

		//Address 'b0000 is uart write data
		rule rl_handle_axi4_write_rx(ff_wr_addr.notEmpty && ff_wr_data.notEmpty 
                                                              && ff_wr_addr.first.awaddr[3:2]==0);
			let wr_addr = ff_wr_addr.first;
			ff_wr_addr.deq;
			let wr_data = ff_wr_data.first;
			ff_wr_data.deq;

			uart.rx.put(truncate(wr_data.wdata));
      let lv_resp = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: ?};
      ff_wr_resp.enq(lv_resp);
			if(verbosity>2) begin
			  $display($time,"\tUART_BS:Req: WR_ADDR %h", wr_addr.awaddr);
			  $display($time,"\tUART_BS:Req: WR_DATA %h", wr_data.wdata);
      end
		endrule

		rule rl_handle_axi4_write(ff_wr_addr.notEmpty && ff_wr_data.notEmpty 
                                                              && ff_wr_addr.first.awaddr[3:2]!=0);
			let wr_addr = ff_wr_addr.first;
			ff_wr_addr.deq;
			let wr_data = ff_wr_data.first;
			ff_wr_data.deq;

			if(wr_addr.awaddr[3:2]=='d3) begin // change the baud value
				baud_value<=truncate(wr_data.wdata);
        		let lv_resp = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: ?};
        		ff_wr_resp.enq(lv_resp);
			end
			else begin
        		let lv_resp = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_SLVERR, buser: ?};
        		ff_wr_resp.enq(lv_resp);
			end

			if(verbosity>2) begin
		    $display($time,"\tUART_BS:Req: WR_ADDR %h", wr_addr.awaddr);
		    $display($time,"\tUART_BS:Req: WR_DATA %h", wr_data.wdata); 
      end
		endrule

		rule send_write_response;
      s_xactor.i_wr_resp.enq(ff_wr_resp.first);
			ff_wr_resp.deq;
		endrule

		//The status register is 1 if the transmission FIFO is empty
		(*no_implicit_conditions, fire_when_enabled*)
		rule rl_update_status_reg;
			let lv_status= {pack(uart.receiver_not_empty), pack(uart.receiver_not_full), 
                                  pack(uart.transmittor_not_empty), pack(uart.transmission_done)};
			rg_status<= lv_status;
			if(lv_status==0 && verbosity>2)
				$display($time,"\tUART_BS: TX Fifo not empty"); 
		endrule

		interface slave_axi_uart = s_xactor.axi_side;
		interface coe_rs232= uart.rs232;
	endmodule

  // -------------------------------------------------------------------------------------------//
  // -------------------------------------------------------------------------------------------//
  // -------------------------------------------------------------------------------------------//
  // -------------------------------------------------------------------------------------------//
	
  interface Ifc_uart_bs_axi4;
  	interface AXI4_Slave_IFC#(PADDR,XLEN,USERSPACE) slave_axi_uart;
		interface RS232 coe_rs232;
	endinterface

	(*synthesize*)
	module mkuart_bs_axi4#(Clock core_clock, Reset core_reset)(Ifc_Uart_bs);

    let verbosity = valueOf(`VERBOSITY);
		Clock uart_clock<-exposeCurrentClock;
		Reset uart_reset<-exposeCurrentReset;
		Reg#(Bit#(16)) baud_value <-mkReg(BAUD_RATE);
		UART#(`Depth) uart <-mkUART(8,NONE,STOP_1,baud_value); // charasize,Parity,Stop Bits,BaudDIV
		AXI4_Slave_Xactor_IFC #(PADDR,XLEN,USERSPACE)  s_xactor <- 
                              mkAXI4_Slave_Xactor(clocked_by core_clock, reset_by core_reset);
		Reg#(Bit#(4)) rg_status <-mkReg(0);		//This register keeps track of whether some data
															//is pending to be sent out through the UART Tx
		
		SyncFIFOIfc#(AXI4_Rd_Addr#(PADDR,USERSPACE))	ff_rd_addr <-	
                                                            mkSyncFIFOToCC(1,core_clock,core_reset);
		SyncFIFOIfc#(AXI4_Wr_Addr#(PADDR, USERSPACE)) ff_wr_addr <-	
                                                            mkSyncFIFOToCC(1,core_clock,core_reset);
		SyncFIFOIfc#(AXI4_Wr_Data#(XLEN)) ff_wr_data <-	mkSyncFIFOToCC(1,core_clock,core_reset);

		SyncFIFOIfc#(AXI4_Rd_Data#(XLEN,USERSPACE))	ff_rd_resp <-	
                                                                    mkSyncFIFOFromCC(1,core_clock);
		SyncFIFOIfc#(AXI4_Wr_Resp#(USERSPACE))	ff_wr_resp <-	mkSyncFIFOFromCC(1,core_clock);

		rule capture_read_request;
			let req <- pop_o (s_xactor.o_rd_addr);
			ff_rd_addr.enq(req);
		endrule

		//Address 'h11304 is uart read data
		rule rl_handle_axi4_uart_read(ff_rd_addr.notEmpty && ff_rd_addr.first.araddr[3:2]=='d1);
			let req = ff_rd_addr.first;
			ff_rd_addr.deq;
			Bit#(8) data<-uart.tx.get;
			let lv_resp= AXI4_Rd_Data {rresp:AXI4_LITE_OKAY, rdata: zeroExtend(data), ruser: ?};

			if(verbosity>2) begin
        $display($time,"\tUART_BS:Req: RD_ADDR %h", req.araddr); 
			  $display($time,"\tUART_BS:Resp: RD_RESP %h", req.araddr); 
      end
			ff_rd_resp.enq(lv_resp);
		endrule

		//Address 'b11308 is uart read status
		rule rl_handle_axi4_uart_status(ff_rd_addr.notEmpty && ff_rd_addr.first.araddr[3:2]!='d1);
			let req =ff_rd_addr.first;
			ff_rd_addr.deq;
			let lv_resp= AXI4_Rd_Data {rresp:AXI4_LITE_OKAY, rdata: zeroExtend(rg_status), ruser: ?};
			if(req.araddr[3:2]==2)
				lv_resp.rdata=zeroExtend(rg_status);
			else if(req.araddr[3:2]==3)
				lv_resp.rdata=zeroExtend(baud_value);
			else
				lv_resp.rresp=AXI4_LITE_SLVERR;

			if(verbosity>2) begin
			  $display($time,"\tUART_BS:Req: RD_ADDR %h", req.araddr); 
			  $display($time,"\tUART_BS:Resp: RD_RESP %h Status: %b", req.araddr, rg_status);
      end
			ff_rd_resp.enq(lv_resp);
		endrule
		
		rule send_read_respone_to_bus;
			s_xactor.i_rd_data.enq(ff_rd_resp.first);
			ff_rd_resp.deq;
		endrule

		rule capture_write_request;
			let req <- pop_o (s_xactor.o_wr_addr);
			let wr_data <- pop_o(s_xactor.o_wr_data);
			ff_wr_addr.enq(req);
			ff_wr_data.enq(wr_data);
		endrule

		//Address 'b0000 is uart write data
		rule rl_handle_axi4_write_rx(ff_wr_addr.notEmpty && ff_wr_data.notEmpty 
                                                              && ff_wr_addr.first.awaddr[3:2]==0);
			let wr_addr = ff_wr_addr.first;
			ff_wr_addr.deq;
			let wr_data = ff_wr_data.first;
			ff_wr_data.deq;

			uart.rx.put(truncate(wr_data.wdata));
      let lv_resp = AXI4_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: ?};
      ff_wr_resp.enq(lv_resp);
			if(verbosity>2) begin
			  $display($time,"\tUART_BS:Req: WR_ADDR %h", wr_addr.awaddr);
			  $display($time,"\tUART_BS:Req: WR_DATA %h", wr_data.wdata);
      end
		endrule

		rule rl_handle_axi4_write(ff_wr_addr.notEmpty && ff_wr_data.notEmpty 
                                                              && ff_wr_addr.first.awaddr[3:2]!=0);
			let wr_addr = ff_wr_addr.first;
			ff_wr_addr.deq;
			let wr_data = ff_wr_data.first;
			ff_wr_data.deq;

			if(wr_addr.awaddr[3:2]=='d3) begin // change the baud value
				baud_value<=truncate(wr_data.wdata);
        		let lv_resp = AXI4_Wr_Resp {bresp: AXI4_LITE_OKAY, buser: ?};
        		ff_wr_resp.enq(lv_resp);
			end
			else begin
        		let lv_resp = AXI4_Wr_Resp {bresp: AXI4_LITE_SLVERR, buser: ?};
        		ff_wr_resp.enq(lv_resp);
			end

			if(verbosity>2) begin
		    $display($time,"\tUART_BS:Req: WR_ADDR %h", wr_addr.awaddr);
		    $display($time,"\tUART_BS:Req: WR_DATA %h", wr_data.wdata); 
      end
		endrule

		rule send_write_response;
      s_xactor.i_wr_resp.enq(ff_wr_resp.first);
			ff_wr_resp.deq;
		endrule

		//The status register is 1 if the transmission FIFO is empty
		(*no_implicit_conditions, fire_when_enabled*)
		rule rl_update_status_reg;
			let lv_status= {pack(uart.receiver_not_empty), pack(uart.receiver_not_full), 
                                  pack(uart.transmittor_not_empty), pack(uart.transmission_done)};
			rg_status<= lv_status;
			if(lv_status==0 && verbosity>2)
				$display($time,"\tUART_BS: TX Fifo not empty"); 
		endrule

		interface slave_axi_uart = s_xactor.axi_side;
		interface coe_rs232= uart.rs232;
	endmodule
endpackage
		
