/* 
Copyright (c) 2018, IIT Madras All rights reserved.

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

Author : Neel Gala
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package pmp_func;

  import Vector :: * ;

  import common_types :: * ;
  `include "common_params.bsv"

  function Bool fn_compare_elem(Tuple2#(Bool, Bool) a);
    return tpl_1(a);
  endfunction

  (*noinline*)
  /*doc : func: */
  function Tuple2#(Bool, Bit#(`causesize)) fn_pmp_lookup( PMPReq req, Privilege_mode priv,
                                                          Vector#(`PMPSIZE, Bit#(8)) pmpcfg,
                                                          Vector#(`PMPSIZE, Bit#(`paddr)) pmpaddr);
    Bit#(`causesize) cause = case(req.access_type) 
      'd0 : `Load_access_fault;
      'd1 : `Store_access_fault;
      default : `Inst_access_fault; 
    endcase;

    function Tuple2#(Bool, Bool) fn_single_lookup( PMPCfg cfg, Bit#(`paddr) addr, 
                                                    Bit#(`paddr) bottom);

      Bool access_trap =  (!(!cfg.lock && priv == Machine) &&
                            ((req.access_type == 0 && !cfg.read)  ||
                            (req.access_type == 1 && !cfg.write)  ||
                            (req.access_type == 2 && !cfg.exec)) );

      Bit#(`paddr) mask = ((~0) << 3) << countZerosLSB(~addr);
      Bit#(`paddr) reqbase = req.address;
      Bit#(`paddr) reqtop = req.address + zeroExtend(req.num_bytes);

      Bool address_match = case(cfg.access)
        TOR   : (bottom <= reqbase && reqtop <= addr);
        NA4   : (addr == reqbase && bottom == addr);
        NAPOT : ( ((addr & mask) == (reqbase & mask)) && 
                  ((addr & mask) == (reqtop  & mask)) );
        OFF   : False;
      endcase;
      return tuple2(address_match, access_trap);  
    endfunction

    // convert bits to PMPCfg struct
    let v_pmpcfg = map(fn_unpack_cfg, pmpcfg);

    // run the function on all pmpcfgs and pmpaddrs
    let pmpmatch = zipWith3(fn_single_lookup, v_pmpcfg, pmpaddr, shiftInAt0(pmpaddr, 0));

    // find the first element which has had a matched address
    let x = find(fn_compare_elem, pmpmatch);
    let {addrmatch, accesstrap} = fromMaybe(tuple2(False, False), x);

    return (priv == Machine && !addrmatch) ? tuple2(False, cause): 
                                            tuple2((addrmatch && accesstrap), cause);
  endfunction

  (*synthesize*)
  module mkTb(Empty);
  endmodule
endpackage

