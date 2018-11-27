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
  list of performance counters:
  0. Total accesses
  1. Total Hits in Cache
  2. Total Hits in LB
  3. Total IO requests
  4. Total Fills from FB to Cache
  

--------------------------------------------------------------------------------------------------
*/
package l1icache;
  import Vector::*;
  import FIFOF::*;
  import DReg::*;
  import SpecialFIFOs::*;
  import BRAMCore::*;
  import FIFO::*;
  import Assert::*;
  import GetPut::*;
  import BUtils::*;

  import cache_types::*;
  import mem_config::*;
  import replacement::*;
  
  interface Ifc_l1icache#( numeric type wordsize, 
                           numeric type blocksize,  
                           numeric type sets,
                           numeric type ways,
                           numeric type paddr,
                           numeric type fbsize
                           );

    interface Put#(ICore_request#(paddr)) core_req;
    interface Get#(ICore_response#(TMul#(wordsize,8))) core_resp;
    interface Get#(IMem_request#(paddr)) read_mem_req;
    interface Put#(IMem_response#(TMul#(wordsize,8))) read_mem_resp;
    interface Get#(IMem_request#(paddr)) io_read_req;
    interface Put#(IMem_response#(TMul#(wordsize,8))) io_read_resp;
//    `ifdef simulate
//      interface Get#(Bit#(1)) meta;
//    `endif
    `ifdef perf
      method Bit#(5) perf_counters;
    `endif
    method Action cache_enable(Bool c);
  endinterface

  (*conflict_free="request_to_memory,update_fb_with_memory_response"*)
  (*conflict_free="respond_to_core,fence_operation"*)
  (*conflict_free="request_to_memory,fence_operation"*)
  (*conflict_free="request_to_memory,release_from_FB"*)
  (*conflict_free="respond_to_core,release_from_FB"*)
  module mkl1icache#(function Bool is_IO(Bit#(paddr) addr, Bool cacheable))
    (Ifc_l1icache#(wordsize,blocksize,sets,ways,paddr,fbsize)) 
    provisos(
          Mul#(wordsize, 8, respwidth),        // respwidth is the total bits in a word
          Mul#(blocksize, respwidth,linewidth),// linewidth is the total bits in a cache line
          Log#(wordsize,wordbits),      // wordbits is no. of bits to index a byte in a word
          Log#(blocksize, blockbits),   // blockbits is no. of bits to index a word in a block
          Log#(sets, setbits),           // setbits is the no. of bits used as index in BRAMs.
          Add#(wordbits,blockbits,_a),  // _a total bits to index a byte in a cache line.
          Add#(_a, setbits, _b),        // _b total bits for index+offset, 
          Add#(tagbits, _b, paddr),     // tagbits = 32-(wordbits+blockbits+setbits)

          `ifdef ASSERT
          Add#(1, e__, TLog#(TAdd#(1, fbsize))),
          Add#(1, f__, TLog#(TAdd#(1, ways))),
          `endif

          Add#(a__, respwidth, linewidth),
          Add#(b__, 32, respwidth),
          Add#(c__, 16, respwidth),
          Add#(d__, 8, respwidth),
          Add#(TAdd#(tagbits, setbits), g__, paddr),
          Add#(h__, 1, blocksize),

          Add#(i__, TLog#(ways), 4),
          Mul#(TDiv#(linewidth, 8), 8, linewidth),
          Add#(j__, TDiv#(linewidth, 8), linewidth),
          Add#(k__, TLog#(ways), TLog#(TAdd#(1, ways)))
          
    );
    let v_sets=valueOf(sets);
    let v_setbits=valueOf(setbits);
    let v_wordbits=valueOf(wordbits);
    let v_blockbits=valueOf(blockbits);
    let v_linewidth=valueOf(linewidth);
    let v_tagbits=valueOf(tagbits);
    let verbosity=`VERBOSITY;
    let v_paddr=valueOf(paddr);
    let v_ways=valueOf(ways);
    let v_wordsize=valueOf(wordsize);
    let v_blocksize=valueOf(blocksize);
    let v_fbsize=valueOf(fbsize);
    let v_respwidth=valueOf(respwidth);

    //Following function returns the info regarding word_position in line getting filled
    function Bit#(blocksize) fn_enable(Bit#(blockbits)word_index);
       Bit#(blocksize) write_enable ='h0; //
       write_enable[word_index]=1;
       return write_enable;
    endfunction
  
    String alg ="RROBIN";

    // ----------------------- FIFOs to interact with interface of the design -------------------//
    // This fifo stores the request from the core.
    FIFOF#(ICore_request#(paddr)) ff_core_request <- mkSizedFIFOF(2); 
    // This fifo stores the response that needs to be sent back to the core.
    FIFOF#(ICore_response#(respwidth))ff_core_response <- mkSizedFIFOF(2);
    // this fifo stores the read request that needs to be sent to the next memory level.
    FIFOF#(IMem_request#(paddr)) ff_read_mem_request    <- mkSizedFIFOF(2);
    // This fifo stores the response from the next level memory.
    FIFOF#(IMem_response#(respwidth)) ff_read_mem_response  <- mkSizedBypassFIFOF(1);
    
    FIFOF#(IMem_request#(paddr)) ff_io_read_request    <- mkSizedFIFOF(2);
    // This fifo stores the response from the next level memory.
    FIFOF#(IMem_response#(respwidth)) ff_io_read_response  <- mkSizedBypassFIFOF(1);
    Wire#(Bool) wr_takingrequest <- mkDWire(False);
    Wire#(Bool) wr_cache_enable<-mkWire();
//    `ifdef simulate
//      FIFOF#(Bit#(1)) ff_meta <- mkSizedFIFOF(2);
//    `endif
    `ifdef perf
      Wire#(Bit#(1)) wr_total_access <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_cache_hits <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_fb_hits <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_io <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_fbfills <- mkDWire(0);
    `endif
    // ------------------------------------------------------------------------------------------//

   
    // ------------------------ Structures required for cache RAMS ------------------------------//
    Ifc_mem_config#(sets, linewidth, 1) data_arr [v_ways]; // data array
    Ifc_mem_config#(sets, tagbits, 1) tag_arr [v_ways];// one extra valid bit
    for(Integer i=0;i<v_ways;i=i+1)begin
      data_arr[i]<-mkmem_config_h(False, "single"); // TODO parameterize arguments
      tag_arr[i]<-mkmem_config_h(False, "single");
    end
    Ifc_replace#(sets,ways) repl <- mkreplace(alg);
    Reg#(Bit#(ways)) rg_valid[v_sets];
//    Reg#(Bit#(ways)) rg_dirty[v_sets];
    for(Integer i=0;i<v_sets;i=i+1)begin
      rg_valid[i]<-mkReg(0);
//      rg_dirty[i]<-mkReg(0);
    end
    Wire#(RespState) wr_cache_response <- mkDWire(None);
    Wire#(Bit#(respwidth)) wr_cache_hitword <-mkDWire(0);
    Wire#(Bit#(TLog#(ways))) wr_hitway <-mkDWire(0);
    Reg#(Bool) rg_miss_ongoing <- mkReg(False);
    Reg#(Bool) rg_fence_stall <- mkReg(False);
    Wire#(Maybe#(Bit#(TLog#(sets)))) wr_cache_hitindex <-mkDWire(tagged Invalid);
    Reg#(Bit#(TLog#(sets))) rg_latest_index<- mkReg(0);
    Reg#(Bool) rg_replaylatest<-mkReg(False);
    Wire#(RespState) wr_io_response <- mkDWire(None);
    Wire#(Bit#(respwidth)) wr_io_word <-mkDWire(0);
    Wire#(Bool) wr_io_err <-mkDWire(False);
    // ------------------------------------------------------------------------------------------//


    // ----------------- Fill buffer structures -------------------------------------------------//
    Reg#(Bit#(linewidth)) fb_dataline [v_fbsize];
    Reg#(Bit#(paddr)) fb_addr [v_fbsize];
    Reg#(Bit#(blocksize)) fb_enables [v_fbsize];
//    Reg#(Bit#(fbsize)) fb_dirty <-mkReg(0);
    Reg#(Bit#(fbsize)) fb_valid <-mkReg(0);
    for(Integer i=0;i<v_fbsize;i=i+1)begin
      fb_dataline[i]<-mkReg(0);
      fb_addr[i]<-mkReg(0);
      fb_enables[i]<-mkReg(0);
    end
    Wire#(RespState) wr_fb_response <- mkDWire(None);
    Wire#(Bit#(respwidth)) wr_fb_word <-mkDWire(0);
    Reg#(Bool) rg_fb_err <-mkDReg(False);
    // this register is used to ensure that the cache does not do a tag match when FB is polling on
    // a line for the requested word.
    Reg#(Bool) rg_polling <-mkReg(False);
    `ifdef simulate
      Wire#(Bool) wrpolling<-mkDWire(False);
    `endif

    // This register indicates which entry in the FB should be allocated when there is miss in the
    // FB and the cache for a given request.
    Reg#(Bit#(TLog#(fbsize))) rg_fbmissallocate <-mkReg(0);

    // This register follows the rg_fbmissallocate register but is updated when the last word of a
    // line is filled in the FB on a miss.
    Reg#(Bit#(TLog#(fbsize))) rg_fbbeingfilled <-mkReg(0);

    // This register points to the entry in the FB which needs to be written back to the cache
    // whenever possible.
    Reg#(Bit#(TLog#(fbsize))) rg_fbwriteback <-mkReg(0);
    Reg#(Bit#(blocksize))     rg_fbfillenable <- mkReg(0);

    Bool fb_full=unpack(&fb_valid);
    Bool fb_empty=!unpack(|fb_valid);
    Bit#(TLog#(sets)) fillindex=fb_addr[rg_fbwriteback][v_setbits+v_blockbits+v_wordbits-1:
                                                                          v_blockbits+v_wordbits];
    Bool fill_oppurtunity=(!ff_core_request.notEmpty || !wr_takingrequest) && !fb_empty &&
         /*countOnes(fb_valid)>0 &&*/ (fillindex!=rg_latest_index);
    // ------------------------------------------------------------------------------------------//

    rule display_stuff;
      if(verbosity!=0)begin
        $display($time,"\tDACHE: fb_full: %b fb_empty: %b rg_fbwriteback: %d rg_fbmissallocate: %d\
 rg_fbbeingfilled:%d",fb_full,fb_empty,rg_fbwriteback,rg_fbmissallocate,rg_fbbeingfilled);
        $display($time,"\tICACHE: ff_core_response.notFull: %b rg_fence_stall: %b",
          ff_core_response.notFull,rg_fence_stall);
      end
    endrule

    // This rull fires when the fence operation is signalled by the core. For the i-cache this rule
    // will take only a single cycle since all the valid signals in the cache and FB are registers
    // which can be reset in one-shot. The replacement policies for each set should also be reset.
    // Since they too are implemented as array of registers it can be done in a single cycle.
    rule fence_operation(tpl_2(ff_core_request.first) && rg_fence_stall && fb_empty);
      for(Integer i=0;i<v_sets;i=i+1)
        rg_valid[i]<=0;
      fb_valid<=0;
      rg_fence_stall<=False;
      ff_core_request.deq; // TODO depends on how fence should be handled.
      repl.reset_repl;
      if(verbosity!=0)begin
        $display($time,"\tICACHE: Fence operation in progress");
      end
    endrule
    
    // This rule is fired when there is a hit in the cache. The word received is further modified
    // depending on the request made by the core.
    rule respond_to_core(wr_cache_response==Hit || wr_fb_response==Hit || wr_io_response==Hit);
      let {addr, fence, epoch, prefetch} =ff_core_request.first();
      Bit#(respwidth) word=0;
      Bool err=False;
      let set_index=addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      if(wr_cache_response==Hit)begin
        word=wr_cache_hitword;
        if(alg=="PLRU") begin
          wr_cache_hitindex<=tagged Valid set_index;
          repl.update_set(set_index, wr_hitway);//wr_replace_line); // TODO update for PLRU should happen here
        end
        `ifdef perf
          wr_total_cache_hits<=1;
        `endif
      end
      else if(wr_fb_response==Hit)begin
        word=wr_fb_word;
        err=rg_fb_err;
        `ifdef perf
          // Only when the hit in the LB is not because of a miss should the counter be enabled.
          if(!rg_miss_ongoing)
            wr_total_fb_hits<=1;
        `endif
      end
      else if(wr_io_response==Hit)begin
        word=wr_io_word;
        err=wr_io_err;
        `ifdef perf
          wr_total_io<=1;
        `endif
      end
      rg_miss_ongoing<=False;
      // depending onthe request made by the core, the word is either sigextended/zeroextend and
      // truncated if necessary.
      $display($time,"\tICACHE: Sending response to core. Word: %d for address: %h",word,addr);
      ff_core_response.enq(tuple3(word,err,epoch));
      ff_core_request.deq;
      `ifdef ASSERT
        dynamicAssert(!(wr_cache_response==Hit && wr_fb_response==Hit),
                                                  "Cache and FB both are hit simultaneously");
        dynamicAssert(!(wr_io_response==Hit && wr_fb_response==Hit),
                                                  "IO and FB both are hit simultaneously");
        dynamicAssert(!(wr_cache_response==Hit && wr_io_response==Hit),
                                                  "Cache and IO both are hit simultaneously");
      `endif
    endrule

    // This rule will perform the check on the tags from the cache and detect is there is a hit or a
    // miss. On a hit the required word is forwarded to the rule respond_to_core. On a miss the
    // address is forwarded to the rule request_to_memory;
    rule tag_match(ff_core_response.notFull && !rg_miss_ongoing && !rg_polling &&
          !tpl_2(ff_core_request.first()) );
      let {addr, fence, epoch, prefetch} =ff_core_request.first();
      Bit#(TAdd#(3,TAdd#(wordbits,blockbits)))block_offset={addr[v_blockbits+v_wordbits-1:0],3'b0};
      Bit#(blockbits) word_index= addr[v_blockbits+v_wordbits-1:v_wordbits];
      Bit#(tagbits) request_tag = addr[v_paddr-1:v_paddr-v_tagbits];
      Bit#(setbits) set_index= addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];

      Bit#(linewidth) dataline[v_ways];
      Bit#(tagbits) tag[v_ways];
      for(Integer i=0;i<v_ways;i=i+1)begin
        tag[i]<- tag_arr[i].read_response;
        dataline[i]<- data_arr[i].read_response;
      end
      Bit#(linewidth) hitline=0;
      Bit#(ways) hit=0;

/*
      Bit#(linewidth) temp[v_ways]; // mask for each way dataline.
      // Find the line that was hit
      for(Integer i=0;i<v_ways;i=i+1)begin
        hit[i]=pack(tag[i]==request_tag && rg_valid[set_index][i]==1);
        temp[i]=duplicate(hit[i])&dataline[i];
      end
      for(Integer i=0;i<v_ways;i=i+1)
        hitline=hitline|temp[i];
*/
      for(Integer i=0;i<v_ways;i=i+1)begin
        if(rg_valid[set_index][i]==1 && request_tag==tag[i])begin
          hit[i]=1'b1;
          hitline=dataline[i];
        end
      end
      Bool cache_hit=unpack(|(hit));
      wr_hitway<=truncate(pack(countZerosLSB(hit)));
      Bit#(respwidth) response_word=truncate(hitline>>block_offset);
      if(is_IO(addr,wr_cache_enable))begin // TODO make this programmable;
        wr_cache_response<=None;
        ff_io_read_request.enq(tuple3(addr,0,fromInteger(v_wordbits)));
      end
      else if(cache_hit)begin
        wr_cache_response<=Hit;
        wr_cache_hitword<=response_word;
      end
      else begin
        wr_cache_response<=Miss;
      end

      if(verbosity!=0)begin
        $display($time,"\tICACHE: TAGMATCH: addr:%h hit: %b hitline: %h",addr,hit,hitline);
      end

      `ifdef ASSERT
        dynamicAssert(countOnes(hit)<=1,"More than one way is a hit in the cache");
      `endif
    endrule

    // This rule will fire whenever a core request is present. This rule will check the entire FB if
    // the requested line and hence the word is present. A "TRUE" miss in the cache is only
    // generated when the line containing the requested word is not present in FB and the cache. 
    // There can be a case where the line requested is present but the word is not preset since it
    // is being filled by the lower level memory.
    rule check_fb_for_corerequest(ff_core_response.notFull && !tpl_2(ff_core_request.first));
      Bool wordhit=False;
      let {addr, fence, epoch, prefetch} =ff_core_request.first();
      Bit#(tagbits) read_tag = addr[v_paddr-1:v_paddr-v_tagbits];
      Bit#(setbits) read_set = addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      Bit#(TAdd#(3,TAdd#(wordbits,blockbits)))block_offset={addr[v_blockbits+v_wordbits-1:0],3'b0};
      Bit#(blockbits) word_index=addr[v_blockbits+v_wordbits-1:v_wordbits];
      Bit#(TAdd#(tagbits,setbits)) t=truncateLSB(addr);
      Bit#(fbsize) fbhit=0;
      Bit#(linewidth) hitline=0;
 
 /*
      Bit#(linewidth) data_t [v_fbsize];
      for (Integer i=0; i<v_fbsize ; i=i+1) begin
        fbhit[i]=pack( truncateLSB(fb_addr[i])==t && fb_valid[i]==1 );
        data_t[i]=duplicate(fbhit[i])&fb_dataline[i];
        if(fb_enables[i][word_index]==1'b1)
            wordhit=(fbhit[i]==1'b1);
      end
      for(Integer i=0; i<v_fbsize ;i=i+1)
        hitline=hitline|data_t[i];
 */     
      
      for (Integer i=0;i<v_fbsize;i=i+1)begin
        // we use truncateLSB because we need to match only the tag and set bits
        if( truncateLSB(fb_addr[i])==t && fb_valid[i]==1)begin
          hitline=fb_dataline[i];
          fbhit[i]=1;
          if(fb_enables[i][word_index]==1'b1) begin
            wordhit=True;
          end
        end          
      end
      
      Bool linehit=unpack(|fbhit);
      if(wordhit)
        wr_fb_response<=Hit;
      else if(!linehit) // generate a miss only if the line is missing.
        wr_fb_response<=Miss;
      // setting this register will prevent the rule tag_match from firing when polling is expected.
      rg_polling<=(linehit && !wordhit);
      wrpolling<=rg_polling;
      wr_fb_word<=truncate(hitline>>block_offset); 

      if(verbosity!=0)begin
        $display($time,"\tICACHE: Polling addr: %h linehit: %b wordhit: %b rg_polling: %b",
            addr,linehit,wordhit,rg_polling);
      end

      `ifdef ASSERT
        dynamicAssert(countOnes(fbhit)<=1,"More than one line in FB is hit");
      `endif
    endrule
//
//    `ifdef simulate
//      rule put_meta;
//        if(wr_cache_response==Hit)
//          ff_meta.enq(1);
//        else if(wr_fb_response==Hit)
//          ff_meta.enq(pack(!wrpolling));
//      endrule
//    `endif

    // This rule will generate a miss request to the next level memory. The address from the core
    // cannot be directly sent to the bus. The address will have to made word-aligned before sending
    // it to the next level.
    // Here as soon as a miss is detected we allocate a line in the fill buffer for the requested
    // access. The line-allocation is done using rg_fbmissallocate register which follows a
    // round-robin mechanism for now. 
    // the register rg_fbbeingfilled follows the rg_fbmissallocate but gets updated only when the
    // entire line in the FB has been filled.
    // It is not possible at any point of time for rg_fbmissallocate and rg_fbbeingfilled to update
    // the same entry in the FB.
    rule request_to_memory(wr_cache_response==Miss && !rg_miss_ongoing && wr_fb_response==Miss
                                                                                        &&!fb_full);
                                                                                        
      let {addr, fence, epoch, prefetch} =ff_core_request.first();
      addr= (addr>>v_wordbits)<<v_wordbits; // align the address to be one word aligned.
      ff_read_mem_request.enq(tuple3(addr,fromInteger(v_blocksize-1),fromInteger(v_wordbits)));
      rg_miss_ongoing<=True;
      rg_fbmissallocate<=rg_fbmissallocate+1;
      fb_valid[rg_fbmissallocate]<=1'b1;
      fb_addr[rg_fbmissallocate]<=addr;
      fb_enables[rg_fbmissallocate]<=0;
      
      if(verbosity!=0)begin
        $display($time,"\tICACHE: Sending memory request. Addr: %h",addr);
        $display($time,"\tICACHE: Allocating FB line: %d",rg_fbmissallocate);
      end

    endrule

    // This rule will update an entry pointed by the register rg_fbbeingfilled with the incoming
    // response from the lower memory level.
    rule update_fb_with_memory_response;
      let {word,last,err}=ff_read_mem_response.first();
      rg_fb_err<=err;
      ff_read_mem_response.deq;
      Bit#(blocksize) temp=0;
      Bit#(blockbits) word_index=fb_addr[rg_fbbeingfilled][v_blockbits+v_wordbits-1:v_wordbits];
      if(fb_enables[rg_fbbeingfilled]==0)
        temp=fn_enable(word_index);
      else
        temp=rg_fbfillenable;
      fb_enables[rg_fbbeingfilled]<=fb_enables[rg_fbbeingfilled]|temp;
      rg_fbfillenable <= {temp[valueOf(blocksize)-2:0],temp[valueOf(blocksize)-1]};

      Bit#(linewidth) mask=0;
      for (Integer i=0;i<v_blocksize;i=i+1)begin
        Bit#(respwidth) we=duplicate(temp[i]);
        mask[i*v_respwidth+v_respwidth-1:i*v_respwidth]=we;
      end
      fb_dataline[rg_fbbeingfilled]<=(~mask&fb_dataline[rg_fbbeingfilled])|(mask&duplicate(word));
      if(last)
        rg_fbbeingfilled<=rg_fbbeingfilled+1;

      if(verbosity!=0)begin
        $display($time,"\tICACHE: Filling up FB. rg_fbbeingfilled: %d fb_addr: %h fb_dataline: %h \
fb_enables: %h",rg_fbbeingfilled,fb_addr[rg_fbbeingfilled],fb_dataline[rg_fbbeingfilled],
fb_enables[rg_fbbeingfilled]);
      end
        
    endrule

    rule receive_io_response;
      let {word,last,err}=ff_io_read_response.first;
      ff_io_read_response.deq;
      wr_io_err<=err;
      wr_io_word<=word;
      wr_io_response<=Hit;
      `ifdef ASSERT
        dynamicAssert(last,"Why is IO response a burst");
      `endif
    endrule

    `ifdef ASSERT
      rule assertions;
        dynamicAssert(rg_fbbeingfilled==rg_fbmissallocate || rg_fbbeingfilled==rg_fbmissallocate-1
        || rg_fbbeingfilled==rg_fbmissallocate-2,
            "rg_fbbeingfilled and rg_fbmissallocate are too far apart");
        dynamicAssert(fb_valid[rg_fbmissallocate]==0 || &fb_valid==1,
          "rg_fbmissallocate points to  non-empty entry even though one exists in the FB");
      endrule
    `endif

    // This rule will evict an entry from the fill-buffer and update it in the cache RAMS. Multiple
    // conditions under which this rule can fire:
    // 1. when the FB is full
    // 2. when the core is not requesting anything in a particular cycle and there exists a valid
    //    filled entry in the FB
    // 3. The rule will not fire when the entry being evicted is a line that has been recently
    // requested by the core (present in the ff_core_request). Writing this line would cause a
    // replay of the latest request. This would cause another cycle delay which would eventually be
    // a hit in the cache RAMS. 
    rule release_from_FB((fb_full || fill_oppurtunity || rg_fence_stall) && !rg_replaylatest &&
              !fb_empty && fb_valid[rg_fbwriteback]==1 && (&fb_enables[rg_fbwriteback])==1);
      // if line is valid and is completely filled.
      let addr=fb_addr[rg_fbwriteback];
      Bit#(setbits) set_index=addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      Bit#(tagbits) tag = addr[v_paddr-1:v_paddr-v_tagbits];
      let waynum<-repl.line_replace(set_index, rg_valid[set_index]);
      if(&(rg_valid[set_index])==1)begin
        if(alg!="PLRU")
          repl.update_set(set_index,waynum);
        else begin
          if(wr_cache_hitindex matches tagged Valid .i &&& i==set_index)begin
          end
          else
            repl.update_set(set_index,waynum);
        end
      end
      rg_valid[set_index][waynum]<=1'b1;
      tag_arr[waynum].write_request(set_index,tag);
      data_arr[waynum].write_request(set_index,fb_dataline[rg_fbwriteback]);
      rg_fbwriteback<=rg_fbwriteback+1;
      fb_valid[rg_fbwriteback]<=0;
      if(fb_full && fillindex==rg_latest_index)
        rg_replaylatest<=True;
      `ifdef perf
        wr_total_fbfills<=1;
      `endif
      if(verbosity!=0)begin
        $display($time,"\tICACHE: release from FB firing");
        $display($time,"\tICACHE: rg_fbwriteback: %d fb_valid: %b fb_enables: %b setindex: %d \
addr:%h way: %d",
         rg_fbwriteback,fb_valid[rg_fbwriteback],fb_enables[rg_fbwriteback],set_index,
         fb_addr[rg_fbwriteback], waynum);
      end
    endrule

    rule replay_latest_request(rg_replaylatest);
      rg_replaylatest<=False;
      for(Integer i=0;i<v_ways;i=i+1)begin
        data_arr[i].read_request(rg_latest_index);
        tag_arr[i].read_request(rg_latest_index);
      end
      if(verbosity!=0)begin
        $display($time,"\tICACHE: replaying last request to index: %d maintain sync",rg_latest_index);
      end
    endrule

    interface core_req=interface Put
      method Action put(ICore_request#(paddr) req)if( ff_core_response.notFull &&
                                !rg_replaylatest &&  !rg_fence_stall && !fb_full);
        `ifdef perf
          wr_total_access<=1;
        `endif
        let {addr, fence, epoch, prefetch} =req;
        Bit#(setbits) set_index=addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
        ff_core_request.enq(req);
        rg_fence_stall<=fence;
        for(Integer i=0;i<v_ways;i=i+1)begin
          data_arr[i].read_request(set_index);
          tag_arr[i].read_request(set_index);
        end
        wr_takingrequest<=True;
        if (verbosity!=0) begin
		      $display($time,"\tICACHE: Receiving request to address:%h Fence: %b epoch: %b index: %d",
          addr, fence, epoch, set_index); 
        end
        rg_latest_index<=set_index;
      endmethod
    endinterface;

    interface core_resp = interface Get
      method ActionValue#(ICore_response#(respwidth)) get();
        ff_core_response.deq;
        return ff_core_response.first;
      endmethod
    endinterface;
    
    interface read_mem_req = interface Get
      method ActionValue#(IMem_request#(paddr)) get;
        ff_read_mem_request.deq;
        return ff_read_mem_request.first;
      endmethod
    endinterface;

    interface read_mem_resp= interface Put
     method Action put(IMem_response#(respwidth) resp);
        ff_read_mem_response.enq(resp);
     endmethod
    endinterface;
    
    interface io_read_req = interface Get
      method ActionValue#(IMem_request#(paddr)) get;
        ff_io_read_request.deq;
        return ff_io_read_request.first;
      endmethod
    endinterface;

    interface io_read_resp= interface Put
     method Action put(IMem_response#(respwidth) resp);
        ff_io_read_response.enq(resp);
     endmethod
    endinterface;
//    `ifdef simulate 
//      interface meta = interface Get
//        method ActionValue#(Bit#(1)) get();
//          ff_meta.deq;
//          return ff_meta.first;
//        endmethod
//      endinterface;
//    `endif 
    method Action cache_enable(Bool c);
      wr_cache_enable<=c;
    endmethod
    `ifdef perf
      method Bit#(5) perf_counters;
        return {wr_total_fbfills,wr_total_io,wr_total_fb_hits,wr_total_cache_hits,wr_total_access};
      endmethod
    `endif
  endmodule
 
  function Bool isIO(Bit#(32) addr, Bool cacheable);
    if(!cacheable)
      return True;
    else if( addr < 4096)
      return True;
    else
      return False;    
  endfunction


  (*synthesize*)
  module mkicache(Ifc_l1icache#(4, 16, 64, 1 ,32,1));
    let ifc();
    mkl1icache#(isIO) _temp(ifc);
    return (ifc);
  endmodule
endpackage

