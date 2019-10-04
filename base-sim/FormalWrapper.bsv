import Soc::*;
import ModuleCollect::*;

(* always_ready *)
interface Ifc_FormalWrapper;
    //insn metadata
    method Bit#(1) rvfi_valid;
    method Bit#(64) rvfi_order;
    method Bit#(32) rvfi_insn;
    method Bit#(1) rvfi_trap;
    method Bit#(1) rvfi_halt;
    method Bit#(1) rvfi_intr;
    method Bit#(2) rvfi_mode;
    method Bit#(2) rvfi_ixl;

    // mem signals
    method Bit#(32) rvfi_mem_addr;
    method Bit#(4)  rvfi_mem_rmask;
    method Bit#(4)  rvfi_mem_wmask;
    method Bit#(32) rvfi_mem_rdata;
    method Bit#(32) rvfi_mem_wdata;

    // register
    method Bit#(5)  rvfi_rs1_addr;
    method Bit#(5)  rvfi_rs2_addr;
    method Bit#(32) rvfi_rs1_rdata;
    method Bit#(32) rvfi_rs2_rdata;
    method Bit#(5)  rvfi_rd_addr;
    method Bit#(32) rvfi_rd_wdata;

    // pc
    method Bit#(32) rvfi_pc_rdata;
    method Bit#(32) rvfi_pc_wdata;
endinterface

module mkFormalWrapper (Ifc_FormalWrapper);

    // instantiate core
    let def_clk <- exposeCurrentClock;
    let def_rst <- exposeCurrentReset;
    let soc <- mkSoc(def_clk,def_rst);

    // insn metadata
    method Bit#(1) rvfi_valid  = soc.rvfi_valid;
    method Bit#(64) rvfi_order = soc.rvfi_order;
    method Bit#(32) rvfi_insn  = soc.rvfi_insn;
    method Bit#(1) rvfi_trap   = 0;
    method Bit#(1) rvfi_halt   = 0;
    method Bit#(1) rvfi_intr   = 0;
    
    // insn metadata held constant
    method Bit#(2) rvfi_mode   = 3;
    method Bit#(2) rvfi_ixl    = 1;

    // mem signals
    method Bit#(32) rvfi_mem_addr =  soc.rvfi_mem_addr;
    method Bit#(4)  rvfi_mem_rmask = soc.rvfi_mem_rmask;
    method Bit#(4)  rvfi_mem_wmask = soc.rvfi_mem_wmask;
    method Bit#(32) rvfi_mem_rdata = soc.rvfi_mem_rdata;
    method Bit#(32) rvfi_mem_wdata = soc.rvfi_mem_wdata;

    // register signals
    method Bit#(5)  rvfi_rs1_addr  = soc.rvfi_rs1_addr;
    method Bit#(5)  rvfi_rs2_addr  = soc.rvfi_rs2_addr;
    method Bit#(32) rvfi_rs1_rdata = soc.rvfi_rs1_rdata;
    method Bit#(32) rvfi_rs2_rdata = soc.rvfi_rs2_rdata;
    method Bit#(5)  rvfi_rd_addr   = soc.rvfi_rd_addr;
    method Bit#(32) rvfi_rd_wdata  = soc.rvfi_rd_wdata; 

    // pc
    method Bit#(32) rvfi_pc_rdata = soc.rvfi_pc_rdata;
    method Bit#(32) rvfi_pc_wdata = soc.rvfi_pc_rdata + 4;
endmodule
