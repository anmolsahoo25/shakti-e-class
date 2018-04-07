/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted 
provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions 
   and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of 
   conditions and the following disclaimer in the documentation and/or other materials provided 
   with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
   promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
-------------------------------------------------------------------------------------------------

Code inpired by the pwm module at: https://github.com/freecores/pwm

*/
package pwm;
  `define PWMWIDTH 32
  /*=== Project imports ==*/
  import Clocks::*;
  /*======================*/
  /*== Package imports ==*/
  import common_types::*;
  `include "common_params.bsv"
  import ClockDiv::*;
  import ConcatReg::*;
	import Semi_FIFOF::*;
	import BUtils ::*;
  `ifdef PWM_AXI4Lite
  	import AXI4_Lite_Types::*;
  `endif
  `ifdef PWM_AXI4
    import AXI4_Types::*;
  `endif
  /*======================*/

  interface UserInterface;
    method Action write_request(Tuple2#(Bit#(PADDR), Bit#(XLEN)) req);
    method Bool write_response;
    method Action read_request (Bit#(PADDR) addr);
    method Tuple2#(Bool, Bit#(XLEN)) read_response;
  endinterface

  interface PWMIO;
    method Bit#(1) pwm_o;
  endinterface

  interface PWM;
    interface UserInterface user;
    interface PWMIO io;
  endinterface

  (*synthesize*)
  module mkPWM#(Clock ext_clock)(PWM);

    let bus_clock <- exposeCurrentClock;
    let bus_reset <- exposeCurrentReset;

    Reg#(Bit#(`PWMWIDTH)) period <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) duty_cycle <- mkReg(0);
    Reg#(Bit#(`PWMWIDTH)) clock_divisor <- mkReg(0);
    // =========== Control registers ================== //
    Reg#(Bit#(1)) clock_selector <- mkReg(0);     // bit-0
    Reg#(Bit#(1)) pwm_enable <- mkReg(0);         // bit-1
    Reg#(Bit#(1)) pwm_start  <- mkReg(0);         // bit-2
    Reg#(Bit#(1)) continous_once <- mkReg(0);     // bit-3
    Reg#(Bit#(1)) pwm_output_enable <- mkReg(0);  // bit-4
    Reg#(Bit#(1)) interrupt <- mkReg(0);          // bit-5
    Reg#(Bit#(1)) reset_counter <- mkReg(0);      // bit-7
    Reg#(Bit#(8)) control = concatReg8(reset_counter, readOnlyReg(0), readOnlyReg(interrupt), 
                                       pwm_output_enable, continous_once, pwm_start, pwm_enable, 
                                       clock_selector);
    // ================================================ //

    // Generate a reset signal is there is a reset from the bus interface of if the reset_counter
    // bit in the control register is set. The new reset is called overall_reset. Only the counter
    // and the output signals need to be reset by this.
    MakeResetIfc control_reset <- mkReset(1,False, bus_clock);
    rule generate_reset;
      if(reset_counter==1)
        control_reset.assertReset;
    endrule
    Reset overall_reset <- mkResetEither(bus_reset,control_reset.new_rst);

    // Select between bus clock or external clock 
    MuxClkIfc clock_selection <- mkUngatedClockMux(ext_clock,bus_clock);
    Reset async_reset <- mkAsyncResetFromCR(0,clock_selection.clock_out);
    rule select_busclk_extclk;
      clock_selection.select(clock_selector==1);
    endrule

    // The following register is required to transfer the divisor value from bus_clock to 
    // external clock domain. This is necessary if the clock divider needs to operate on the
    // external clock. In this case, the divisor value should also come from the same clock domain.
    Reg#(Bit#(`PWMWIDTH)) clock_divisor_sync <- mkSyncRegFromCC(0, clock_selection.clock_out); 
    rule transfer_data_from_clock_domains;
      clock_divisor_sync <= clock_divisor;
    endrule
    
    // The PWM can operate on a slowed-down clock. The following module generates a slowed-down
    // clock based on the value given in register divisor. Since the clock_divider works on a muxed
    // clock domain of the external clock or bus_clock, the divisor (which operates on the bus_clock
    // will have to be synchronized and sent to the divider
    Ifc_ClockDiv#(`PWMWIDTH) clock_divider <- mkClockDiv(clocked_by clock_selection.clock_out, 
                                         reset_by async_reset);
    let downclock = clock_divider.slowclock; 
    Reset downreset <- mkAsyncReset(0,overall_reset,downclock);
    rule generate_slow_clock;
      clock_divider.divisor(clock_divisor_sync);
    endrule

    // ======= Actual Counter and PWM signal generation ======== //
    Reg#(Bit#(1)) pwm_output <- mkReg(0,clocked_by downclock,reset_by downreset);
    Reg#(Bit#(`PWMWIDTH)) rg_counter <-mkReg(0,clocked_by downclock,reset_by downreset); 
    
    // create synchronizers for clock domain crossing.
    Reg#(Bit#(1)) sync_pwm_output <- mkSyncRegToCC(0,downclock,downreset);
    ReadOnly#(Bit#(1)) pwm_signal <- mkNullCrossingWire(bus_clock, pwm_output);
    Reg#(Bit#(1)) sync_continous_once <- mkSyncRegFromCC(0,downclock);
    Reg#(Bit#(`PWMWIDTH)) sync_duty_cycle <- mkSyncRegFromCC(0,downclock);
    Reg#(Bit#(`PWMWIDTH)) sync_period <- mkSyncRegFromCC(0,downclock);
    Reg#(Bit#(1)) sync_pwm_enable <- mkSyncRegFromCC(0,downclock);
    Reg#(Bit#(1)) sync_pwm_start <- mkSyncRegFromCC(0,downclock);
    rule sync_pwm_output_to_default_clock;
      sync_pwm_output <= pwm_output;
    endrule

    // capture the synchronized values from the default clock domain to the downclock domain for
    // actual timer and pwm functionality.
    rule sync_from_default_to_downclock;
      sync_continous_once <= continous_once;
      sync_duty_cycle <= duty_cycle;
      sync_period <= period;
      sync_pwm_enable <= pwm_enable;
      sync_pwm_start <= pwm_start;
    endrule
    let temp = sync_period==0?0:sync_period-1;

    // This rule generates the interrupt in the timer mode and resets it if the user-write interface
    // writes a value of 1 to the reset_counter bit.
    rule generate_interrupt_in_timer_mode;
      if(pwm_enable==0)
        interrupt <= sync_pwm_output;
      else if(reset_counter==1)
        interrupt <= 0;
      else
        interrupt <= 0;
    endrule

    // This rule performs the actual pwm and the timer functionality. if pwm_enable is 1 then the
    // PWM mode is selected. Every time the counter value equals/crosses the period value it is
    // reset and the output pwm_output signal is toggled. 
    // The timer mode is selected when pwm_enable is 0. Here again 2 more modes are possible. if the
    // continous_once bit is 0 then the timer is in one time. In this case once the counter reaches
    // the period value it raises an interrupt and stops the counter. In the continuous mode
    // however, when the counter reaches the period value the interrupt is raise, the counter is
    // reset to 0 and continues counting. During continuous counting the interrupt can be cleared by
    // the user but will be set back when the counter reaches the period value.
    rule compare_and_generate_pwm(sync_pwm_start==1);
      let cntr = rg_counter+1;
      if(sync_pwm_enable==1)begin // PWM mode enabled
        if(rg_counter >= temp)
          rg_counter <= 0;
        else 
          rg_counter <= cntr;
        if(rg_counter < sync_duty_cycle)
          pwm_output <= 1;
        else
          pwm_output <= 0;
      end
      else begin  // Timer mode enabled.
        if(sync_continous_once==0) begin // One time mode.
          if(rg_counter >= temp)begin
              pwm_output <= 1;
          end
          else
            rg_counter <= cntr;
        end
        else begin // Continous mode.
          if(rg_counter >= temp)begin
            pwm_output <= 1;
            rg_counter <= 0;
          end
          else begin
            rg_counter <= cntr;
          end
        end
      end
    endrule

    // ========================================================= //
    interface user = interface UserInterface
      method ActionValue#(Bool) write(Bit#(PADDR) addr, Bit#(XLEN) data);
        Bool err = False;
        case(addr[4:2])
          0: period <= truncate(data);
          1: duty_cycle <= truncate(data);
          2: begin control <= truncate(data);end
          3: clock_divisor <= truncate(data);
          default: err = True;
        endcase
        return err;
      endmethod

      method Tuple2#(Bool, Bit#(XLEN)) read(Bit#(PADDR) addr);
        Bool err = False;
        Bit#(XLEN) data;
        case(addr[4:2])
          0: data = zeroExtend(period);
          1: data = zeroExtend(duty_cycle);
          2: data = zeroExtend(control);
          3: data =  zeroExtend(clock_divisor);
          default: begin err = True; data = 0; end
        endcase
        return tuple2(err,data);
      endmethod
    endinterface;
    interface io = interface PWMIO
      method pwm_o=pwm_output_enable==1?pwm_signal:0;
    endinterface;
  endmodule

  `ifdef PWM_AXI4Lite
    // the following interface and module will add the AXI4Lite interface to the PWM module
    interface Ifc_PWM_bus;
      interface PWMIO pwm_io;
	    interface AXI4_Lite_Slave_IFC#(PADDR, XLEN,USERSPACE) axi4_slave;
    endinterface

    (*synthesize*)
    module mkPWM_bus#(Clock ext_clock)(Ifc_PWM_bus);
      PWM pwm <-mkPWM(ext_clock);
	  	AXI4_Lite_Slave_Xactor_IFC#(PADDR,XLEN,USERSPACE) s_xactor<-mkAXI4_Lite_Slave_Xactor();

      rule read_request;
	  		let req <- pop_o (s_xactor.o_rd_addr);
        let {err,data} = pwm.user.read(req.araddr);
	  		let resp= AXI4_Lite_Rd_Data {rresp:err?AXI4_LITE_SLVERR:AXI4_LITE_OKAY, 
                                     rdata:data, ruser: ?};
	  		s_xactor.i_rd_data.enq(resp);
      endrule

      rule write_request;
        let addreq <- pop_o(s_xactor.o_wr_addr);
        let datareq <- pop_o(s_xactor.o_wr_data);
        let err <- pwm.user.write(addreq.awaddr, datareq.wdata);
        let resp = AXI4_Lite_Wr_Resp {bresp: err?AXI4_LITE_SLVERR:AXI4_LITE_OKAY, buser: ?};
        s_xactor.i_wr_resp.enq(resp);
      endrule

      interface pwm_io = pwm.io;
    endmodule
  `endif

  `ifdef PWM_AXI4
    // the following interface and module will add the AXI4 interface to the PWM module
    interface Ifc_PWM_bus;
      interface PWMIO pwm_io;
	    interface AXI4_Slave_IFC#(PADDR, XLEN,USERSPACE) axi4_slave;
    endinterface

    (*synthesize*)
    module mkPWM_bus#(Clock ext_clock)(Ifc_PWM_bus);
      PWM pwm <-mkPWM(ext_clock);
	  	AXI4_Slave_Xactor_IFC#(PADDR,XLEN,USERSPACE) s_xactor<-mkAXI4_Slave_Xactor();

      rule read_request;
	  		let req <- pop_o (s_xactor.o_rd_addr);
        let {err,data} = pwm.user.read(req.araddr);
        if(!(req.arsize == 2 && req.arlen == 0))
          err = True;
	  		let resp= AXI4_Rd_Data {rresp:err?AXI4_SLVERR:AXI4_OKAY, 
                                     rdata:data, ruser: ?, rid:req.arid, rlast: True};
	  		s_xactor.i_rd_data.enq(resp);
      endrule

      rule write_request;
        let addreq <- pop_o(s_xactor.o_wr_addr);
        let datareq <- pop_o(s_xactor.o_wr_data);
        let err <- pwm.user.write(addreq.awaddr, datareq.wdata);
        if(!(addreq.awsize == 2 && addreq.awlen == 0))
          err = True;
        let resp = AXI4_Wr_Resp {bresp: err?AXI4_SLVERR:AXI4_OKAY, buser: ?, 
                                      bid:datareq.wid};
        s_xactor.i_wr_resp.enq(resp);
      endrule

      interface pwm_io = pwm.io;
    endmodule
  `endif
  (*synthesize*)
  module mkTb(Empty);
    let clk <- exposeCurrentClock;
    PWM pwm <- mkPWM(clk);
    Reg#(Bit#(5)) rg_state <- mkReg(0);
    
    rule state1(rg_state==0);
      rg_state<=1;
      let x <- pwm.user.write(0,'d4);
    endrule
    rule state2(rg_state==1);
      rg_state<=2;
      let x <- pwm.user.write('d4,'d3);
    endrule
    rule state3(rg_state==2);
      rg_state<=3;
      let x <- pwm.user.write('hc,'d4);
    endrule
    rule state4(rg_state==3);
      rg_state<=4;
      let x <- pwm.user.write(8,'b0001_0110);
    endrule
  endmodule
endpackage
