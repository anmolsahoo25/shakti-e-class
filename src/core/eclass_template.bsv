/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
 * Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and / or other materials provided 
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

schema-start
module:
    type: string
    regex: ^eclass$
type:
    type: string
    regex: ^device$
clusters:
    type: list
    schema:
        type: string
roles:
    type: dict
    schema:
        master:
            type: list
        slave:
            type: list
parameters:
    type: dict
    schema:
        isa:
            type: string
            regex: ^(rv64imac|rv32imac|rv64imc|rv32imc|rv64im|rv32im|rv64ic|rv32ic)$
        resetpc:
            type: integer
macros:
    type: dict
    schema:
        ISA:
            type: string
            regex:  ^(RV64IMAC|RV32IMAC|RV64IMC|RV32IMC|RV64IM|RV32IM|RV64IC|RV32IC)$
        MUL:
            type: string
            regex: ^(sequential|fpga)$
        SYNTH: 
            type: string
            regex: ^(SIM|FPGA|ASIC)
        VERBOSITY:
            type: integer
            min: 0
            max: 3
        USERTRAPS:
            type: boolean
        MULSTAGES:
            type: integer
            allowed: [1, 2, 4, 8]
        DIVSTAGES:
            type: integer
            allowed: [32, 64]
        FPGA:
            type: string
        COUNTERS:
            type: integer
            min: 0
            max: 8
        USERTRAPS:
            type: string
            allowed: [enable,disable]
        USER:
            type: string
            allowed: [enable,disable]
        RTLDUMP:
            type: string
            allowed: [enable,disable]
        ASSERTIONS:
            type: string
            allowed: [enable,disable]
        PMP:
            type: string
            allowed: [enable,disable]
        ARITHTRAP:
            type: string
            allowed: [enable,disable]
        DEBUG:
            type: string
            allowed: [enable,disable]
        OPENOCD:
            type: string
            allowed: [enable,disable]
        TRIGGERS: 
            type: integer
        COVERAGE:
            type: string
        TRACE:
            type: string
            allowed: [enable,disable]
        VERILATESIM:
            type: string
            allowed: [fast,slow]
        PMPSIZE:
            type: integer
        PADDR:
            type: integer
        CAUSESIZE:
            type: integer
        THREADS:
            type: integer
        
sideband:
    type: dict
    schema:
        sb_clint_mtime:
          type: string
        sb_clint_msip:
          type: string
        sb_clint_mtip:
          type: string
schema-end
--------------------------------------------------------------------------------------------------
 */
