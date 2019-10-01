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

    // insn metadata
    method Bit#(1) rvfi_valid  = 0;
    method Bit#(64) rvfi_order = 0;
    method Bit#(32) rvfi_insn  = 0;
    method Bit#(1) rvfi_trap   = 0;
    method Bit#(1) rvfi_halt   = 0;
    method Bit#(1) rvfi_intr   = 0;
    method Bit#(2) rvfi_mode   = 3;
    method Bit#(2) rvfi_ixl    = 1;

    // mem signals
    method Bit#(32) rvfi_mem_addr = 0;
    method Bit#(4)  rvfi_mem_rmask = 0;
    method Bit#(4)  rvfi_mem_wmask = 0;
    method Bit#(32) rvfi_mem_rdata = 0;
    method Bit#(32) rvfi_mem_wdata = 0;

    // register signals
    method Bit#(5)  rvfi_rs1_addr  = 0;
    method Bit#(5)  rvfi_rs2_addr  = 0;
    method Bit#(32) rvfi_rs1_rdata = 0;
    method Bit#(32) rvfi_rs2_rdata = 0;
    method Bit#(5)  rvfi_rd_addr   = 0;
    method Bit#(32) rvfi_rd_wdata  = 0;

    // pc
    method Bit#(32) rvfi_pc_rdata = 0;
    method Bit#(32) rvfi_pc_wdata = 0;
endmodule
