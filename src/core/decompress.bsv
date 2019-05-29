/* 
Copyright (c) 2018, IIT Madras All rights reserved.

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
package decompress;
                                          
  // Quad-0
  `define CADDI4SPN0  16'b0001??????????00  
  `define CADDI4SPN1  16'b000?1?????????00  
  `define CADDI4SPN2  16'b000??1????????00  
  `define CADDI4SPN3  16'b000???1???????00  
  `define CADDI4SPN4  16'b000????1??????00  
  `define CADDI4SPN5  16'b000?????1?????00  
  `define CADDI4SPN6  16'b000??????1????00  
  `define CADDI4SPN7  16'b000???????1???00  
  `define CFLD        16'b001???????????00  
  `define CLW         16'b010???????????00  
  `define CFLW        16'b011???????????00  
  `define CLD         16'b011???????????00  
  `define CFSD        16'b101???????????00  
  `define CSW         16'b110???????????00  
  `define CFSW        16'b111???????????00  
  `define CSD         16'b111???????????00  

  // Quad-1
  `define CADDI       16'b000???????????01  
  `define CJAL        16'b001???????????01  
  `define CADDIW0     16'b001?1?????????01  
  `define CADDIW1     16'b001??1????????01  
  `define CADDIW2     16'b001???1???????01  
  `define CADDIW3     16'b001????1??????01  
  `define CADDIW4     16'b001?????1?????01  
  `define CLI         16'b010???????????01  
  `define CADDI16SP0  16'b011100010?????01  
  `define CADDI16SP1  16'b011?000101????01  
  `define CADDI16SP2  16'b011?00010?1???01  
  `define CADDI16SP3  16'b011?00010??1??01  
  `define CADDI16SP4  16'b011?00010???1?01  
  `define CADDI16SP5  16'b011?00010????101  
  `define CLUI0       16'b01111?????????01  
  `define CLUI1       16'b0111?1????????01  
  `define CLUI2       16'b0111??1???????01  
  `define CLUI3       16'b0111???0??????01  
  `define CLUI4       16'b0111????1?????01  
  `define CLUI5       16'b011?1????1????01  
  `define CLUI6       16'b011??1???1????01  
  `define CLUI7       16'b011???1??1????01  
  `define CLUI8       16'b011????0?1????01  
  `define CLUI9       16'b011?????11????01  
  `define CLUI10      16'b011?1?????1???01  
  `define CLUI11      16'b011??1????1???01  
  `define CLUI12      16'b011???1???1???01  
  `define CLUI13      16'b011????0??1???01  
  `define CLUI14      16'b011?????1?1???01  
  `define CLUI15      16'b011?1??????1??01  
  `define CLUI16      16'b011??1?????1??01  
  `define CLUI17      16'b011???1????1??01  
  `define CLUI18      16'b011????0???1??01  
  `define CLUI19      16'b011?????1??1??01  
  `define CLUI20      16'b011?1???????1?01  
  `define CLUI21      16'b011??1??????1?01  
  `define CLUI22      16'b011???1?????1?01  
  `define CLUI23      16'b011????0????1?01  
  `define CLUI24      16'b011?????1???1?01  
  `define CLUI25      16'b011?1????????101  
  `define CLUI26      16'b011??1???????101  
  `define CLUI27      16'b011???1??????101  
  `define CLUI28      16'b011????0?????101  
  `define CLUI29      16'b011?????1????101  
  `define CSRLI       16'b100?00????????01
  `define CSRLI64     16'b100000???0000001  // only RV128
  `define CSRAI       16'b100?01????????01
  `define CSRAI64     16'b100001???0000001  // only RV128
  `define CANDI       16'b100?10????????01  
  `define CSUB        16'b100011???00???01  
  `define CXOR        16'b100011???01???01  
  `define COR         16'b100011???10???01  
  `define CAND        16'b100011???11???01  
  `define CSUBW       16'b100111???00???01
  `define CADDW       16'b100111???01???01
  `define CJ          16'b101???????????01
  `define CBEQZ       16'b110???????????01
  `define CBNEZ       16'b111???????????01

  // Quad-2
  `define CSLLI       16'b000???????????10
  `define CSLLI64     16'b000???????????10  // only in RV128
  `define CFLDSP      16'b001???????????10
  `define CLWSP0      16'b010?1?????????10
  `define CLWSP1      16'b010??1????????10
  `define CLWSP2      16'b010???1???????10
  `define CLWSP3      16'b010????1??????10
  `define CLWSP4      16'b010?????1?????10
  `define CFLWSP      16'b011???????????10  // only in RV32
  `define CLDSP       16'b011???????????10  // only in RV64/128
  `define CJR0        16'b10001????0000010
  `define CJR1        16'b1000?1???0000010
  `define CJR2        16'b1000??1??0000010
  `define CJR3        16'b1000???1?0000010
  `define CJR4        16'b1000????10000010
  `define CMV0        16'b1000?????1????10
  `define CMV1        16'b1000??????1???10
  `define CMV2        16'b1000???????1??10
  `define CMV3        16'b1000????????1?10
  `define CMV4        16'b1000?????????110
  `define CEBREAK     16'b1001000000000010
  `define CJALR0      16'b10011????0000010
  `define CJALR1      16'b1001?1???0000010
  `define CJALR2      16'b1001??1??0000010
  `define CJALR3      16'b1001???1?0000010
  `define CJALR4      16'b1001????10000010
  `define CADD0        16'b1001?????1????10  // subsumes CJR, CMV, CEBREAK, CJALR
  `define CADD1        16'b1001??????1???10  // subsumes CJR, CMV, CEBREAK, CJALR
  `define CADD2        16'b1001???????1??10  // subsumes CJR, CMV, CEBREAK, CJALR
  `define CADD3        16'b1001????????1?10  // subsumes CJR, CMV, CEBREAK, CJALR
  `define CADD4        16'b1001?????????110  // subsumes CJR, CMV, CEBREAK, CJALR
  `define CFSDSP      16'b101???????????10
  `define CSWSP       16'b110???????????10
  `define CFSWSP      16'b111???????????10  // only in RV32
  `define CSDSP       16'b111???????????10  // only in RV64/128

  // operand 1 encodings
  `define RS1_SP    5'b00010
  `define RS1_P     {2'b01, inst[9:7]}
  `define RS1       {inst[11:7]}
  `define RS1_0     5'b00000

  // operand 2 encodings
  `define RS2_SP    5'b00010
  `define RS2_P     {2'b01, inst[4:2]}
  `define RS2       {inst[6:2]}
  `define RS2_0     5'b00000

  // destination encodings
  `define RD_SP     5'b00010
  `define RD_RA     5'b00001
  `define RD_P      {2'b01, inst[4:2]}
  `define RD        inst[11:7]
  `define RD_0      5'b00000

  // Immediate field encodings
  `define IMM_4SP         zeroExtend({inst[10:7], inst[12:11], inst[5] , inst[6] ,2'b0})
  `define IMM_16SP        signExtend({inst[12], inst[4:3], inst[5], inst[2], inst[6], 4'd0})
  `define IMM_MEMLD       zeroExtend({inst[6:5], inst[12:10], 3'd0})
  `define IMM_MEMLW       zeroExtend({inst[5], inst[12:10], inst[6], 2'd0})
  `define IMM_MEMSD_low   {inst[11:10], 3'd0}
  `define IMM_MEMSD_hi    zeroExtend({inst[6:5], inst[12]})
  `define IMM_MEMSW_low   {inst[11:10], inst[6], 2'd0}
  `define IMM_MEMSW_hi    zeroExtend({inst[5], inst[12]})
  `define IMM_IOP         signExtend({inst[12], inst[6:2]})
  `define IMM_J           {inst[12], inst[8], inst[10:9], inst[6], inst[7], inst[2], inst[11], inst[5:3], inst[12], inst[12],inst[12],inst[12],inst[12],inst[12],inst[12],inst[12],inst[12]}
  `define IMM_SRLI        zeroExtend({6'b000000, inst[12], inst[6:2]})
  `define IMM_SRAI        zeroExtend({6'b010000, inst[12], inst[6:2]})
  `define IMM_SLLI        zeroExtend({6'b000000, inst[12], inst[6:2]})
  `define IMM_BRANCH_low  {inst[11:10], inst[4:3], inst[12]}
  `define IMM_BRANCH_hi   {inst[12], inst[12], inst[12], inst[12], inst[6:5], inst[2]}
  `define IMM_DSP         zeroExtend({inst[4:2], inst[12], inst[6:5], 3'd0})
  `define IMM_WSP         zeroExtend({inst[3:2], inst[12], inst[6:4], 2'd0})
  `define IMM_0           12'd0
  `define IMM_MEMDSP_lo   {inst[11:10], 3'd0}
  `define IMM_MEMDSP_hi   zeroExtend({inst[9:7], inst[12]})
  `define IMM_MEMWSP_lo   {inst[11:9], 2'd0}
  `define IMM_MEMWSP_hi   zeroExtend({inst[8:7], inst[12]})
  `define IMM_LUI         signExtend({inst[12], inst[6:2]})
  `define IMM_EBREAK      12'b000000000001


  // funct3 encodings
  `define F3_ADDI     3'b000
  `define F3_SLLI     3'b001
  `define F3_SRLSRAI  3'b101
  `define F3_ANDI     3'b111
  `define F3_ADDIW    3'b000
  `define F3_SW       3'b010
  `define F3_SD       3'b011
  `define F3_BEQ      3'b000
  `define F3_BNE      3'b001
  `define F3_LW       3'b010
  `define F3_LD       3'b011
  `define F3_ADD      3'b000
  `define F3_SUB      3'b000
  `define F3_XOR      3'b100
  `define F3_OR       3'b110
  `define F3_AND      3'b111
  `define F3_ADDW     3'b000
  `define F3_SUBW     3'b000
  `define F3_JALR     3'b000

  // opcode encodings
  `define OP_LUI      5'b01101
  `define OP_JAL      5'b11011
  `define OP_JALR     5'b11001
  `define OP_LOADS    5'b00000
  `define OP_STORES   5'b01000
  `define OP_BRANCH   5'b11000
  `define OP_IMM      5'b00100
  `define OP_IMMW     5'b00110
  `define OP_ARITH    5'b01100
  `define OP_ARITHW   5'b01110
  `define OP_FLOADS   5'b00001
  `define OP_FSTORES  5'b01001
  `define OP_EBREAK   5'b11100

  // f7 encodings
  `define F7_SUB      7'b0100000
  `define F7_ADD      7'b0000000
  `define F7_XOR      7'b0000000
  `define F7_OR       7'b0000000
  `define F7_AND      7'b0000000


  (*noinline*)
  function Bit#(32) fn_decompress(Bit#(16) inst);
    case (inst) matches
      // ------------------------------ C0 space decode ---------------------------------------- //
      `CADDI4SPN0 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
      `CADDI4SPN1 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11}; 
      `CADDI4SPN2 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
      `CADDI4SPN3 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
      `CADDI4SPN4 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
      `CADDI4SPN5 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
      `CADDI4SPN6 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
      `CADDI4SPN7 : return {`IMM_4SP      , `RS1_SP , `F3_ADDI  , `RD_P   , `OP_IMM     , 2'b11};
    `ifdef spfpu
      `CFLD       : return {`IMM_MEMLD    , `RS1_P  , `F3_LD    , `RD_P   , `OP_FLOADS  , 2'b11};
    `endif
      `CLW        : return {`IMM_MEMLW    , `RS1_P  , `F3_LW    , `RD_P   , `OP_LOADS   , 2'b11};
  `ifdef RV32
    `ifdef spfpu  
      `CFLW       : return {`IMM_MEMLW    , `RS1_P  , `F3_LW    , `RD_P   , `OP_FLOADS  , 2'b11};
    `endif
  `else
      `CLD        : return {`IMM_MEMLD    , `RS1_P  , `F3_LD    , `RD_P   , `OP_LOADS   , 2'b11};
  `endif
    `ifdef spfpu
      `CFSD       : return {`IMM_MEMSD_hi , `RS2_P  , `RS1_P    , `F3_SD  , `IMM_MEMSD_low  , `OP_FSTORES , 2'b11};
    `endif
      `CSW        : return {`IMM_MEMSW_hi , `RS2_P  , `RS1_P    , `F3_SW  , `IMM_MEMSW_low  , `OP_STORES  , 2'b11};
  `ifdef RV32
    `ifdef spfpu
      `CFSW       : return {`IMM_MEMSW_hi , `RS2_P  , `RS1_P    , `F3_SD  , `IMM_MEMSW_low  , `OP_FSTORES , 2'b11};
    `endif
  `else
      `CSD        : return {`IMM_MEMSD_hi , `RS2_P  , `RS1_P    , `F3_SD  , `IMM_MEMSD_low  , `OP_STORES  , 2'b11};
  `endif

      // ------------------------------ C1 space decode ---------------------------------------- //
      `CADDI      : return {`IMM_IOP  , `RS1    , `F3_ADDI    , `RD     , `OP_IMM     , 2'b11};
    `ifdef RV32
      `CJAL       : return {`IMM_J    ,                         `RD_RA  , `OP_JAL     , 2'b11};
    `else
      `CADDIW0    : return {`IMM_IOP  , `RS1    , `F3_ADDIW   , `RD     , `OP_IMMW    , 2'b11};
      `CADDIW1    : return {`IMM_IOP  , `RS1    , `F3_ADDIW   , `RD     , `OP_IMMW    , 2'b11};
      `CADDIW2    : return {`IMM_IOP  , `RS1    , `F3_ADDIW   , `RD     , `OP_IMMW    , 2'b11};
      `CADDIW3    : return {`IMM_IOP  , `RS1    , `F3_ADDIW   , `RD     , `OP_IMMW    , 2'b11};
      `CADDIW4    : return {`IMM_IOP  , `RS1    , `F3_ADDIW   , `RD     , `OP_IMMW    , 2'b11};
    `endif
      `CLI        : return {`IMM_IOP  , `RS1_0  , `F3_ADDI    , `RD     , `OP_IMM     , 2'b11};
      `CADDI16SP0 : return {`IMM_16SP , `RS1_SP , `F3_ADDI    , `RD_SP  , `OP_IMM     , 2'b11};
      `CADDI16SP1 : return {`IMM_16SP , `RS1_SP , `F3_ADDI    , `RD_SP  , `OP_IMM     , 2'b11};
      `CADDI16SP2 : return {`IMM_16SP , `RS1_SP , `F3_ADDI    , `RD_SP  , `OP_IMM     , 2'b11};
      `CADDI16SP3 : return {`IMM_16SP , `RS1_SP , `F3_ADDI    , `RD_SP  , `OP_IMM     , 2'b11};
      `CADDI16SP4 : return {`IMM_16SP , `RS1_SP , `F3_ADDI    , `RD_SP  , `OP_IMM     , 2'b11};
      `CADDI16SP5 : return {`IMM_16SP , `RS1_SP , `F3_ADDI    , `RD_SP  , `OP_IMM     , 2'b11};
      `CLUI0      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI1      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI2      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI3      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI4      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI5      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI6      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI7      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI8      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI9      : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI10     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI11     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI12     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI13     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI14     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI15     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI16     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI17     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI18     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI19     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI20     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI21     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI22     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI23     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI24     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI25     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI26     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI27     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI28     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CLUI29     : return {`IMM_LUI                          , `RD     , `OP_LUI     , 2'b11};
      `CSRLI      : return {`IMM_SRLI , `RS1_P  , `F3_SRLSRAI , `RS1_P  , `OP_IMM     , 2'b11};
      `CSRAI      : return {`IMM_SRAI , `RS1_P  , `F3_SRLSRAI , `RS1_P  , `OP_IMM     , 2'b11};
      `CANDI      : return {`IMM_IOP  , `RS1_P  , `F3_ANDI    , `RS1_P  , `OP_IMM     , 2'b11};
      `CSUB       : return {`F7_SUB   , `RS2_P  , `RS1_P      , `F3_SUB , `RS1_P , `OP_ARITH , 2'b11};
      `CXOR       : return {`F7_XOR   , `RS2_P  , `RS1_P      , `F3_XOR , `RS1_P , `OP_ARITH , 2'b11};
      `COR        : return {`F7_OR    , `RS2_P  , `RS1_P      , `F3_OR  , `RS1_P , `OP_ARITH , 2'b11};
      `CAND       : return {`F7_AND   , `RS2_P  , `RS1_P      , `F3_ANDI, `RS1_P , `OP_ARITH , 2'b11};
    `ifdef RV64
      `CSUBW      : return {`F7_SUB   , `RS2_P  , `RS1_P      , `F3_SUBW, `RS1_P , `OP_ARITHW, 2'b11};
      `CADDW      : return {`F7_ADD   , `RS2_P  , `RS1_P      , `F3_ADDW, `RS1_P , `OP_ARITHW, 2'b11};
    `endif
      `CJ         : return {`IMM_J    ,                         `RD_0   , `OP_JAL     , 2'b11};
      `CBEQZ      : return {`IMM_BRANCH_hi , `RS2_0, `RS1_P, `F3_BEQ, `IMM_BRANCH_low, `OP_BRANCH, 2'b11};
      `CBNEZ      : return {`IMM_BRANCH_hi , `RS2_0, `RS1_P, `F3_BNE, `IMM_BRANCH_low, `OP_BRANCH, 2'b11};


      // ------------------------------ C2 space decode ---------------------------------------- //
      `CSLLI      : return {`IMM_SLLI , `RS1    , `F3_SLLI, `RD , `OP_IMM     , 2'b11};
    `ifdef spfpu
      `CFLDSP     : return {`IMM_DSP  , `RS1_SP , `F3_LD  , `RD , `OP_FLOADS  , 2'b11};
    `endif
      `CLWSP0     : return {`IMM_WSP  , `RS1_SP , `F3_LW  , `RD , `OP_LOADS   , 2'b11};
      `CLWSP1     : return {`IMM_WSP  , `RS1_SP , `F3_LW  , `RD , `OP_LOADS   , 2'b11};
      `CLWSP2     : return {`IMM_WSP  , `RS1_SP , `F3_LW  , `RD , `OP_LOADS   , 2'b11};
      `CLWSP3     : return {`IMM_WSP  , `RS1_SP , `F3_LW  , `RD , `OP_LOADS   , 2'b11};
      `CLWSP4     : return {`IMM_WSP  , `RS1_SP , `F3_LW  , `RD , `OP_LOADS   , 2'b11};
  `ifdef RV32
    `ifdef spfpu
      `CFLWSP     : return {`IMM_WSP  , `RS1_SP , `F3_LW  , `RD , `OP_FLOADS  , 2'b11};
    `endif
  `else
      `CLDSP      : return {`IMM_DSP  , `RS1_SP , `F3_LD  , `RD , `OP_LOADS   , 2'b11};
  `endif
      `CJR0       : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_0   , `OP_JALR    , 2'b11};
      `CJR1       : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_0   , `OP_JALR    , 2'b11};
      `CJR2       : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_0   , `OP_JALR    , 2'b11};
      `CJR3       : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_0   , `OP_JALR    , 2'b11};
      `CJR4       : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_0   , `OP_JALR    , 2'b11};
      `CMV0       : return {`F7_ADD   , `RS2  , `RS1_0  , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CMV1       : return {`F7_ADD   , `RS2  , `RS1_0  , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CMV2       : return {`F7_ADD   , `RS2  , `RS1_0  , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CMV3       : return {`F7_ADD   , `RS2  , `RS1_0  , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CMV4       : return {`F7_ADD   , `RS2  , `RS1_0  , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CEBREAK    : return {`IMM_EBREAK       , `RS1_0  , `F3_ADD   , `RD_0   , `OP_EBREAK  , 2'b11};
      `CJALR0     : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_RA  , `OP_JALR    , 2'b11};
      `CJALR1     : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_RA  , `OP_JALR    , 2'b11};
      `CJALR2     : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_RA  , `OP_JALR    , 2'b11};
      `CJALR3     : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_RA  , `OP_JALR    , 2'b11};
      `CJALR4     : return {`IMM_0            , `RS1    , `F3_JALR  , `RD_RA  , `OP_JALR    , 2'b11};
      `CADD0      : return {`F7_ADD   , `RS2  , `RS1    , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CADD1      : return {`F7_ADD   , `RS2  , `RS1    , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CADD2      : return {`F7_ADD   , `RS2  , `RS1    , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CADD3      : return {`F7_ADD   , `RS2  , `RS1    , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
      `CADD4      : return {`F7_ADD   , `RS2  , `RS1    , `F3_ADD   , `RD     , `OP_ARITH   , 2'b11};
    `ifdef spfpu
      `CFSDSP     : return {`IMM_MEMDSP_hi , `RS2  ,  `RS1_SP, `F3_SD  ,`IMM_MEMDSP_lo  , `OP_FSTORES , 2'b11};
    `endif
      `CSWSP      : return {`IMM_MEMWSP_hi , `RS2  ,  `RS1_SP, `F3_SW  ,`IMM_MEMWSP_lo  , `OP_STORES  , 2'b11};
  `ifdef RV32
    `ifdef spfpu
      `CFSWSP     : return {`IMM_MEMWSP_hi , `RS2  ,  `RS1_SP, `F3_SW  ,`IMM_MEMWSP_lo  , `OP_FSTORES , 2'b11};
    `endif
  `else
      `CSDSP      : return {`IMM_MEMDSP_hi , `RS2  ,  `RS1_SP, `F3_SD  ,`IMM_MEMDSP_lo  , `OP_STORES  , 2'b11};
  `endif
      default: return 0;
    endcase
  endfunction
endpackage

