// Copyright (c) 2018 IIT- Madras
// Copyright (c) 2000-2011 Bluespec, Inc.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

module bram_1r1w(
             clka,
             ena,
             wea,
             addra,
             dina,
             clkb,
             enb,
             addrb,
             doutb
             );

   parameter                      ADDR_WIDTH = 6;
   parameter                      DATA_WIDTH = 256;
   parameter                      MEMSIZE    = 64;

   input                          clka;
   input                          ena;
   input                          wea;
   input [ADDR_WIDTH-1:0]         addra;
   input [DATA_WIDTH-1:0]         dina;

   input                          clkb;
   input                          enb;
   input [ADDR_WIDTH-1:0]         addrb;
   output [DATA_WIDTH-1:0]        doutb;

   (* RAM_STYLE = "BLOCK" *)
   reg [DATA_WIDTH-1:0]           ram[0:MEMSIZE-1] /* synthesis syn_ramstyle="no_rw_check" */ ;
   reg [DATA_WIDTH-1:0]           out_reg;

   // synopsys translate_off
   integer                        i;
   initial
   begin : init_block
      for (i = 0; i < MEMSIZE; i = i + 1) begin
         ram[i] = { ((DATA_WIDTH+1)/2) { 2'b10 } };
      end
      out_reg = { ((DATA_WIDTH+1)/2) { 2'b10 } };
   end
   // synopsys translate_on

   always @(posedge clka) begin
      if (ena) begin
         if (wea) begin
            ram[addra] <= dina;
         end
      end
   end

   always @(posedge clkb) begin
      if (enb) begin
        out_reg <= ram[addrb];
      end
   end

   // Output drivers
   assign doutb =  out_reg;

endmodule // BRAM2
