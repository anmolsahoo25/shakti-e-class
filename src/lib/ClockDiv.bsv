package ClockDiv;
  /*=== Project imports ==*/
  import Clocks::*;
  /*======================*/
  // =========================== Clock divider module ================ //
  interface Ifc_ClockDiv#(numeric type width);
    interface Clock slowclock;
    method Action divisor(Bit#(width) in);
  endinterface

  module mkClockDiv(Ifc_ClockDiv#(width));
    let defclock <- exposeCurrentClock;
    Reg#(Bit#(1)) clk <- mkReg(0);
    Reg#(Bit#(width)) rg_divisor <- mkReg(0);
    Reg#(Bit#(width)) rg_counter <- mkReg(0);
    MakeClockIfc#(Bit#(1)) new_clock <- mkUngatedClock(0);
    MuxClkIfc clock_selector <- mkUngatedClockMux(new_clock.new_clk,defclock);
    Bool clockmux_sel = rg_divisor!=0;
    rule increment_counter;
      if(rg_divisor!=0 && rg_counter >= rg_divisor)begin
        rg_counter <= 0;
        clk <= ~ clk;
      end
      else
        rg_counter <= rg_counter + 1;
    endrule

    rule generate_clock;
      new_clock.setClockValue(clk);
    endrule

    rule select_clock;
      clock_selector.select(clockmux_sel);
    endrule

    method Action divisor(Bit#(width) in);
      rg_divisor <= in != 0 ? in - 1 : 0;
    endmethod

    interface slowclock=clock_selector.clock_out;
  endmodule
  // ============================================================== //
  
endpackage
