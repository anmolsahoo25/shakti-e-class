/*
Copyright (c) 2013, IIT Madras
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

*  Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
*  Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
*  Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*/
package registerfile;
	/*==== Project Imports === */
	import defined_types::*;
	`include "defined_parameters.bsv"
	/*======================== */
	/*===== Package Imports ==== */
	import RegFile::*;
	import ConfigReg::*;
	/*===========================*/

	interface Ifc_registerfile;
		method ActionValue#(Output_for_operand_fetch) _inputs_from_decode_stage(Bit#(5) rs1_addr, Operand_type rs1_type, Bit#(5) rs2_addr, Operand_type rs2_type, Bit#(`VADDR) pc, Bit#(`Reg_width) imm `ifdef spfpu , Bool rs3_valid,Bit#(5) rs3_addr `endif );
		`ifdef Debug
			method Bit#(`Reg_width)			read_debug_igpr (Bit#(5) r);				 // Read a General-Purpose Register
			method Action write_debug_igpr (Bit#(5) r, Bit#(`Reg_width) d);				 // Write a General-Purpose Register
			method Bit#(`Reg_width)			read_debug_fgpr (Bit#(5) r);				 // Read a General-Purpose Register
			method Action write_debug_fgpr (Bit#(5) r, Bit#(`Reg_width) d);				 // Write a General-Purpose Register
		`endif
		method Action write_rd(Bit#(5) r, Bit#(`Reg_width) d, Operand_type rdtype);
	endinterface

	(*synthesize*)
	module mkregisterfile(Ifc_registerfile);
		RegFile#(Bit#(5),Bit#(`Reg_width)) integer_rf <-mkRegFileWCF(0,31);
		`ifdef spfpu 
			RegFile#(Bit#(5),Bit#(`Reg_width)) floating_rf <-mkRegFileWCF(0,31);
		`endif
		Reg#(Bool) initialize<-mkReg(True);
		Reg#(Bit#(5)) rg_index<-mkReg(0);
		rule initialize_regfile(initialize);
		`ifdef spfpu
			floating_rf.upd(rg_index,0);
		`endif
			integer_rf.upd(rg_index,0);
			rg_index<=rg_index+1;
			if(rg_index=='d31)
				initialize<=False;
		endrule

		method ActionValue#(Output_for_operand_fetch) _inputs_from_decode_stage(Bit#(5) rs1_addr, Operand_type rs1_type, Bit#(5) rs2_addr, Operand_type rs2_type, Bit#(`VADDR) pc, Bit#(`Reg_width) imm `ifdef spfpu , Bool rs3_valid,Bit#(5) rs3_addr `endif ) if(!initialize);	// recives the input from the decode stage.
			
			Bit#(`Reg_width) rs1=0;
			Bit#(`Reg_width) rs2=0;
			Bit#(`Reg_width) rs3=0;

			if(rs1_type==PC)
				rs1=signExtend(pc);
			else if(rs1_addr==0 && rs1_type==IntegerRF)
				rs1=0;
			else if(rs1_type==IntegerRF)
				rs1=integer_rf.sub(rs1_addr);
			`ifdef spfpu
				else
					rs1=floating_rf.sub(rs1_addr);
			`endif

			if(rs2_type==Immediate)
				rs2=imm;
			else if(rs2_addr==0 && rs2_type==IntegerRF)
				rs2=0;
			else if(rs2_type==IntegerRF)
				rs2=integer_rf.sub(rs2_addr);
			`ifdef spfpu
				else
					rs2=floating_rf.sub(rs2_addr);
			`endif

			`ifdef spfpu
				if(rs3_valid) begin
					rs3= floating_rf.sub(rs3_addr);
				end
				else
					rs3 = 0;	
			`endif
         
			`ifdef verbose $display($time,"\nReg1 :%d : ",rs1_addr,fshow(rs1),"\nReg2 : %d : ",rs2_addr,fshow(rs2) `ifdef spfpu ,"\nReg3: %d ; ",rs3_addr,fshow(rs3) `endif ); `endif
         return Output_for_operand_fetch{rs1:rs1,rs2:rs2`ifdef spfpu ,rs3: rs3 `endif };
		endmethod
		method Action write_rd(Bit#(5) r, Bit#(`Reg_width) d, Operand_type rdtype) if(!initialize); // TODO if not in critical path shift the CReg ports.
			`ifdef verbose $display($time,"\tRF: Writing into reg: :%d data: %h ",r,d,fshow(rdtype)); `endif
			if(rdtype==IntegerRF)begin
				if(r!=0)begin
					integer_rf.upd(r,d);
				end
			end
			`ifdef spfpu
			else if(rdtype==FloatingRF)begin
				floating_rf.upd(r,d);
			end
			`endif
		endmethod
		`ifdef Debug
			method Bit#(`Reg_width)			read_debug_igpr (Bit#(5) r);				 // Read a General-Purpose Register
				return integer_rf.sub(r);
			endmethod
			method Action write_debug_igpr (Bit#(5) r, Bit#(`Reg_width) d)if(!initialize);				 // Write a General-Purpose Register
				integer_rf.upd(r,d);
			endmethod
			`ifdef spfpu
			method Bit#(`Reg_width)			read_debug_fgpr (Bit#(5) r);				 // Read a General-Purpose Register
				return floating_rf.sub(r);
			endmethod
			method Action write_debug_fgpr (Bit#(5) r, Bit#(`Reg_width) d)if(!initialize);				 // Write a General-Purpose Register
				floating_rf.upd(r,d);
			endmethod
			`endif
		`endif
	endmodule
endpackage
