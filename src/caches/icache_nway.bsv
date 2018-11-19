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
Author: Neel Gala,Deepa N. Sarma
Email id: neelgala@gmail.com
Details:
--------------------------------------------------------------------------------------------------
// TODO :- Need to add functionality to dynamically update the way field in lb_control.
// In case of PLRU, when there is a miss, a particular way (say way 3) is taken and stored in lb_control,
// now if there are hits to way 3, the next repl state is gets updated but lb_control still has stale value.
// if there is another miss now, lb will write back to cache and evict line 3 which shouldnt happen.
// Thus need for dynamic updation of way field. test 12 in gen_test.py checks this scenario.
*/

package icache_nway;
  import cache_types::*;
  import mem_config::*;
  import GetPut::*;
  import FIFOF::*;
  import BUtils ::*;  
  import DReg::*;
  import Assert::*;
  import replacement::*;
  import SpecialFIFOs::*;
  
  //parameters:
  // wordsize: number of bytes per word. This is what is responded back to the core.
  // blocksize: number of words per data line.
  // sets: number of sets within the cache.

  // list of performance counters:
  // 0. Total accesses
  // 1. Total Hits in Cache
  // 2. Total Hits in LB
  // 3. Total IO requests
  // 4. Misses which cause evictions
  
  interface Ifc_icache_dm#(numeric type wordsize, 
                           numeric type blocksize,  
                           numeric type sets,
                           numeric type ways,
                           numeric type respwidth, 
                           numeric type paddr
                           );
    interface Put#(ICore_request#(paddr)) core_req;
    interface Get#(ICore_response#(respwidth)) core_resp;
    interface Get#(IMem_request#(paddr)) mem_req;
    interface Put#(IMem_response#(respwidth)) mem_resp;
    `ifdef simulate
      interface Get#(Bit#(1)) meta;
    `endif
    `ifdef perf
      method Bit#(5) perf_counters;
    `endif
  endinterface

  (*conflict_free="rl_response_to_core,rl_request_to_memory"*)
  (*conflict_free="fence_cache,rl_request_to_memory"*)
  module mkicache_dm#(function Bool is_IO(Bit#(paddr) addr, Bool cacheable), 
           parameter Bool ramreg, String alg, Bool prefetch_en, parameter String porttype)
           (Ifc_icache_dm#(wordsize,blocksize,sets,ways,respwidth, paddr))
  provisos(
            Mul#(wordsize, 8, _w),        // _w is the total bits in a word
            Mul#(blocksize, _w,linewidth),// linewidth is the total bits in a cache line
            Log#(wordsize,wordbits),      // wordbits is no. of bits to index a byte in a word
            Log#(blocksize, blockbits),   // blockbits is no. of bits to index a word in a block
            Log#(sets, setbits),          // setbits is the no. of bits used as index in BRAMs.
            Add#(wordbits,blockbits,_a),  // _a total bits to index a byte in a cache line.
            Add#(_a, setbits, _b),        // _b total bits for index+offset, 
            Add#(tagbits, _b, paddr),        // tagbits = 32-(wordbits+blockbits+setbits)
            Mul#(wordsize,8,word_len),    // word_len = number of bits in a word
            Div#(respwidth,word_len,num_words),//num_words=number of words fetched from memory in a
            //cycle
            // Provisos for mem_config. If the number of banks have changed then the following
            // provisos will have to be re-written.
            Add#(a__, TDiv#(linewidth, 8), linewidth),
            Mul#(TDiv#(TDiv#(linewidth, 8), TDiv#(TDiv#(linewidth, 8), 8)),
              TDiv#(TDiv#(linewidth, 8), 8), TDiv#(linewidth, 8)),
            Mul#(TDiv#(linewidth, 8), 8, linewidth),
            // provisos required by the replacement policy
            Add#(g__, TLog#(ways), 4), // max 16-way associative replacement supported for now
            // following provisos required by compiler:
            Bits#(Tuple2#(Bit#(respwidth), Bool), c__),
            Add#(d__, 1, blocksize),
            Add#(e__, respwidth, linewidth),
            Mul#(respwidth, f__, linewidth),
            Add#(1, b__, TLog#(TAdd#(1, ways))),

            Add#(h__, TLog#(ways), TLog#(TAdd#(1, ways)))
            );
  
    let v_sets=valueOf(sets);
    let v_setbits=valueOf(setbits);
    let v_wordbits=valueOf(wordbits);
    let v_blockbits=valueOf(blockbits);
    let v_linewidth=valueOf(linewidth);
    let v_tagbits=valueOf(tagbits);
    let v_num_words=valueOf(num_words);
    let verbosity=`VERBOSITY;
    let v_paddr=valueOf(paddr);
    let v_ways=valueOf(ways);

    staticAssert(valueOf(TExp#(TLog#(ways)))==v_ways,"\n\tWays should always be a power of 2\n");

    //Following function returns the info regarding word_position in line getting filled
    function Bit#(blocksize) fn_enable(Bit#(blockbits)word_index);
       Bit#(blocksize) write_enable ='h0; //
       for(Integer i=0;i<v_num_words;i=i+1)
         write_enable[word_index+fromInteger(i)]=1;
       return write_enable;
    endfunction

    Ifc_mem_config#(sets, linewidth, 1) data_arr [v_ways]; // data array
    Ifc_mem_config#(sets, TAdd#(1, tagbits), 1) tag_arr [v_ways];// one extra valid bit
    Ifc_replace#(sets,ways) repl <- mkreplace(alg);
    for(Integer i=0;i<v_ways;i=i+1)begin
      data_arr[i]<-mkmem_config_h(ramreg, porttype);
      tag_arr[i]<-mkmem_config_h(ramreg, porttype);
    end
   
    // FIFOs for interface communication
    FIFOF#(ICore_response#(respwidth))ff_core_response <- mkSizedBypassFIFOF(1);
    FIFOF#(IMem_request#(paddr)) ff_mem_request    <- mkSizedFIFOF(2);
    FIFOF#(IMem_response#(respwidth)) ff_mem_response  <- mkSizedFIFOF(2);
    FIFOF#(ICore_request#(paddr)) ff_req_queue <- mkSizedFIFOF(2); 

    // This register is used to indicate that a miss is ongoing and thus prevents further requests
    // from being handled.
    Reg#(Bool) rg_miss_ongoing <- mkReg(False);

    // The following set of wires indicate if there was a hit or miss by the cache, LB or io
    // respectively.
    Wire#(RespState) wr_cache_state <- mkDWire(None);
    Wire#(RespState) wr_lb_state <- mkDWire(None);
    Wire#(Bool) wr_io_response <- mkDWire(False);

    // The following wire holds the request that needs to be made to the memory for a miss in cache
    // and LB.
    Wire#(IMem_request#(paddr)) wr_miss_from_cache <- mkDWire(tuple3(0,0,0));
    Wire#(Bit#(TLog#(ways))) wr_replace_line <- mkDWire(0);
    `ifdef simulate
      Wire#(IMem_request#(paddr)) wr_miss_lb_cache <- mkDWire(tuple3(0,0,0));
    `endif

    // The following wires hold the word that was received on a hit in the cache, LB or io.
    Wire#(Tuple2#(Bit#(respwidth),Bool)) wr_hit_cache <- mkDWire(tuple2(0,False));
    Wire#(Tuple2#(Bit#(respwidth),Bool)) wr_hit_lb <- mkDWire(tuple2(0,False));
    Wire#(Tuple2#(Bit#(respwidth),Bool)) wr_hit_io <- mkDWire(tuple2(0,False));

    `ifdef simulate
      FIFOF#(Bit#(1)) ff_meta <- mkSizedFIFOF(2);
    `endif

    Reg#(Bit#(TLog#(sets))) rg_fence_index <- mkReg(0);
    Reg#(Bool) rg_init <- mkReg(True);
    Reg#(Bool) rg_fence_stall <- mkReg(True);
    Reg#(Bit#(blocksize))rg_blockenable <- mkReg(0);
    Reg#(Bit#(blockbits))index<-mkReg(0);
    //linebuffer control
    FIFOF#(Tuple5#(Bit#(tagbits), Bit#(setbits),Bit#(blocksize), Bit#(TLog#(ways)), Bool)) 
                                                                ff_lb_control <- mkUGSizedFIFOF(2);
    Reg#(Bit#(1)) rg_lbvalid <- mkReg(0);
    Reg#(Bit#(linewidth)) rg_lbdataline <- mkReg(0);
    Reg#(Bit#(blocksize)) rg_lbenables <- mkReg(0);
    Reg#(Bool) rg_lberr <- mkReg(False);

    Reg#(Bool) rg_deq_lb <- mkDReg(False);

    // The following register is used to capture the latest index which has been latched into the
    // data/tag array. This is used during deque of the LB. If the request to the SRAM was to the 
    // same line as that held in the LB, then the same line is indexed again while lb deq
    Reg#(Bit#(setbits)) rg_latest_index <- mkReg(0);

    `ifdef perf
      Wire#(Bit#(1)) wr_total_access <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_cache_hits <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_lb_hits <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_io <- mkDWire(0);
      Wire#(Bit#(1)) wr_total_evictions <- mkDWire(0);
    `endif

    Wire#(Bool) wr_line_valid <- mkDWire(False);

    // The following register is used only when ramreg is set to True - i.e. when the BRAM outputs
    // are registered. In this case, when a core-requests arrives, the output of the BRAMs should
    // only be checked on the next-to-next cycle (i.e. not the immediately next cycle). Also this
    // should only happen when the ff_req_queue has only one entry within it.
    Reg#(Bool) rg_delay <- mkDReg(False);
    Bool delay_checking= ramreg && rg_delay && ff_req_queue.notFull && ff_req_queue.notEmpty;
    // on reset we issue a fence instruction to initiliase the cache.
    rule initialize(rg_init);
      ff_req_queue.enq(tuple4(?,True,?, False));
      rg_init<=False;
    endrule

    //Fencing the cache
    // rule to fire only when there is not a pending fill to LB.
    // If this condition is not added then it is possible that LB populates the CACHE line after
    // being fenced which is wrong behavior
    rule fence_cache(tpl_2(ff_req_queue.first) && !ff_lb_control.notEmpty && rg_fence_stall);
      for(Integer i=0;i<v_ways;i=i+1)
       tag_arr[i].write_request(rg_fence_index,'d0);
      rg_fence_index<= rg_fence_index+1;
      if(alg=="PLRU" || alg=="RROBIN")
        repl.reset_repl(truncate(rg_fence_index));
      if(verbosity>0)
        $display($time,"\tICACHE: Fence in progress. Index: %d",rg_fence_index);
      if(rg_fence_index==(fromInteger(v_sets-1))) begin
        if(alg!="PLRU" && alg!="RROBIN")
          repl.reset_repl(truncate(rg_fence_index));
        ff_req_queue.deq;
        rg_fence_stall<=False;
        if(verbosity>1)begin
          $display($time,"\tICACHE Params:");
          $display($time,"\tv_sets: %d",v_sets);
          $display($time,"\tv_ways: %d",v_ways);
          $display($time,"\tv_setbits: %d",v_setbits);
          $display($time,"\tv_wordbits: %d",v_wordbits);
          $display($time,"\tv_blockbits: %d",v_blockbits);
          $display($time,"\tv_tagbits: %d",v_tagbits);
          $display($time,"\tv_num_words: %d",v_num_words);
        end
      end
    endrule
    
    // Checking the data and tag arrays of the cache for a hit or miss.
    // This rule will fire for every request from the core that is not a Fence operation.
    // This rule will also check if the request is cacheable. If not, then a miss generated for that
    // one request and not the entire line.
    rule check_hit_or_miss(!tpl_2(ff_req_queue.first) && !rg_miss_ongoing && ff_lb_control.notFull
    && !delay_checking && ff_core_response.notFull);
      let {request, fence, epoch} =ff_req_queue.first();
      Bit#(TAdd#(3,TAdd#(wordbits,blockbits)))block_offset=
                                                          (request[v_blockbits+v_wordbits-1:0])<<3;
      Bit#(blockbits) word_index=request[v_blockbits+v_wordbits-1:v_wordbits];
      Bit#(tagbits) request_tag = request[v_paddr-1:v_paddr-v_tagbits];
      Bit#(setbits) set_index=request[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];


      // The following logic will check if there is a hit in any of the ways. If so, then the data
      // line corresponding to that way is extracted
      Bit#(linewidth) dataline[v_ways];
      Bit#(TAdd#(1, tagbits)) tag[v_ways];
      Bit#(ways) valid;
      Bit#(tagbits) stored_tag [v_ways];
      Bit#(ways) hit=0;
      Bit#(linewidth) temp[v_ways]; // mask for each way dataline.
      Bit#(linewidth) temp2[v_ways]; // dataline of each way after masking.
      Bit#(linewidth) ex_dataline=0; // dataline which was a hit across all ways.

      for(Integer i=0;i<v_ways;i=i+1)begin
        tag[i]<- tag_arr[i].read_response;
        dataline[i]<- data_arr[i].read_response;
      end
      for(Integer i=0;i<v_ways;i=i+1) begin
        valid[i]=tag[i][v_tagbits];
        stored_tag[i]=tag[i][v_tagbits-1:0];
      end
      // Find the line that was hit
      for(Integer i=0;i<v_ways;i=i+1)begin
        hit[i]=pack(stored_tag[i]==request_tag && valid[i]==1);
        temp[i]=duplicate(hit[i]);
        temp2[i]=temp[i]&dataline[i];
      end
      for(Integer i=0;i<v_ways;i=i+1)
        ex_dataline=ex_dataline|temp2[i];
      
      Bool cachehit=unpack(|hit);
      
//      `ifdef simulate
//        dynamicAssert(countOnes(hit)<=1,"Multiple tags provide a hit");
//        for(Integer i=0;i<v_ways;i=i+1)
//          dynamicAssert(data_arr[i].read_index==set_index,"Cache response is for wrong index");
//      `endif

      if(verbosity!=0)begin
        $display($time,"\tICACHE: Check for Address:%h ReqTag: %h ReqIndex: %d",
            request,request_tag,set_index);
        for(Integer i=0;i<v_ways;i=i+1)
          $display($time,"\tICACHE: Way: %2d Valid: %d StoredTag: %h",i,valid[i],stored_tag[i]);
      end

      // check if the request is cacheable or not
      if(is_IO(request, True))begin
        if(verbosity>0)
          $display($time,"\tICACHE: Cache received IO request for address: %h",request);
        wr_cache_state<=Miss;
        wr_miss_from_cache<=(tuple3(request,0,2));        
      end
      // check if hit  in the cache
      else if(cachehit) begin // hit in cache
        Bit#(respwidth) word_response = truncate(ex_dataline>>block_offset); 
        wr_hit_cache<= tuple2(word_response, False);// word and no bus-error;
        wr_cache_state<=Hit;
        wr_replace_line<=truncate(pack(countZerosLSB(hit)));  
        if(verbosity!=0)
          $display($time,"\tHIT IN CACHE for addr:%h data:%h",request,word_response);
      end
      // generate a miss
      else begin
        wr_cache_state<=Miss;
        wr_miss_from_cache<=  (tuple3(request,fromInteger(valueOf(blocksize)-1),2));

        // We need to make sure that the replacement policy accounts for the fact that
        // there exists a line in the line-buffer which is mapped to the same index as the current
        // request is. If this is not done, it is possible that the two consecutive misses replace
        // the same line. This happens because the line-buffer write-back to the cache is lazy in
        // nature.
        if (ff_lb_control.notEmpty)begin
          let {lbtag,lbset,init_we, way, isIO}=ff_lb_control.first();
          if(lbset==set_index)
            valid[way]=1;
        end
        wr_line_valid<=(&(valid)==1);
        let x<-repl.line_replace(set_index, valid);
        wr_replace_line<=x; 
        if(verbosity!=0)
            $display($time,"\tICACHE: Miss in Cache for addr: %h. Replacing line: %d",request,x);
      end
    endrule

    // This rule will fire for every request from the core that is not a Fence operation.
    rule poll_on_lb(!tpl_2(ff_req_queue.first) && ff_core_response.notFull);
      
      // We first check if the requested word is in the line-buffer. This is done by checking the
      // if the tags match. While this means that the line-buffer should have the data
      // required, it might not be available unless it has been filled by the memory. We can confirm
      // this by checking the byte-enables which indicate which bytes of the line are available and
      // also confirm if the valid bit is set.

      
      let {lbtag,lbset,init_we, way, isIO}=ff_lb_control.first();
      let {request, fence, epoch, prefetch}=ff_req_queue.first();

      Bit#(tagbits) request_tag = request[v_paddr-1:v_paddr-v_tagbits]; 
      let request_index=request[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      Bit#(blockbits) word_index=request[v_blockbits+v_wordbits-1:v_wordbits];
      Bit#(TAdd#(3,TAdd#(wordbits,blockbits)))block_offset=
                                                          (request[v_blockbits+v_wordbits-1:0])<<3;

      // check if line-buffer holds the line containing the word requested
      if(lbtag==request_tag && lbset==request_index && ff_lb_control.notEmpty) begin
        if(verbosity!=0)
          $display($time,"\tICACHE: Polling LB Holds the line for address: %h",request);
        // check if the word is available in the line-buffer.
        if(rg_lbenables[word_index]!=1||rg_lbvalid!=1) begin
          if(verbosity!=0)
            $display($time,"\tICACHE: Polling Miss. Word not found in LB for address: %h",request);
        end  
        else begin
          if(verbosity!=0)
              $display($time,"\tICACHE: Polling Hit. Word present in LB for address: %h",request);
          Bit#(respwidth) word_response = truncate(rg_lbdataline>>block_offset); 
          wr_hit_lb<=(tuple2(word_response,rg_lberr));// word and no bus-error;
          wr_lb_state<=Hit;
        end
      end
      else begin
        if(verbosity!=0)
          $display($time,"\tICACHE: Miss in LB for address: %h",request);
        wr_lb_state<=Miss;
        `ifdef simulate
          wr_miss_lb_cache<=(tuple3(request,fromInteger(valueOf(blocksize)-1),2));
        `endif
      end
    endrule

    // If the miss generated is cacheable then update the line-buffer with the word responded by the
    // next level memory structure. If the request if not cacheable then deque the line-buffer and
    // generate an IO hit response.
    rule rl_response_to_core(!tpl_2(ff_req_queue.first) && (wr_lb_state==Hit || wr_cache_state==Hit
        || wr_io_response));
      let {addr, fence, epoch, prefetch}=ff_req_queue.first();
      ff_req_queue.deq();
      Bit#(respwidth) word=0;
      Bool err=False;
      let request_index=addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      if(wr_cache_state == Hit) begin
        if(alg=="PLRU")
          repl.update_set(request_index, wr_replace_line);
        `ifdef perf
          wr_total_cache_hits<=1;
        `endif
        {word,err}=wr_hit_cache;
      end
      else if(wr_lb_state == Hit)begin
        `ifdef perf
          // Only when the hit in the LB is not because of a miss should the counter be enabled.
          if(!rg_miss_ongoing)
            wr_total_lb_hits<=1;
        `endif
        {word,err}=wr_hit_lb;
      end
      else if(wr_io_response)begin
        `ifdef perf
          wr_total_io<=1;
        `endif
        {word,err}=wr_hit_io;
      end
      if(prefetch_en)begin
        if(!prefetch)
          ff_core_response.enq(tuple3(word,err,epoch)); //icache integration
      end
      else
        ff_core_response.enq(tuple3(word,err,epoch)); // icache integration
      rg_miss_ongoing<=False;
      `ifdef simulate
        if (verbosity>0)
          $display($time,"\tICACHE: Sending Response to the Core word: %h for addr: %h",word,addr);
        dynamicAssert(!(wr_lb_state==Hit && wr_cache_state==Hit), "Hit in Both LB and Cache found");
      `endif
//      `ifdef simulate
//        if(rg_miss_ongoing)
//          ff_meta.enq(0);
//        else
//          ff_meta.enq(1);  
//      `endif
    endrule

    rule rl_request_to_memory(wr_cache_state==Miss && wr_lb_state==Miss);
      rg_miss_ongoing<=True;
      `ifdef simulate
        if (verbosity>0)
          $display($time,"\tICACHE: Sending Request to Memory: ",fshow(wr_miss_from_cache));
        dynamicAssert(rg_miss_ongoing==False,"Issuing a Memory request while one is ongoing");
        dynamicAssert(tpl_1(wr_miss_from_cache)==tpl_1(wr_miss_lb_cache),"Miss from LB and Cache for different\
addresses");
      `endif
      let {request, fence, epoch, prefetch}=ff_req_queue.first();
      Bit#(tagbits) request_tag = request[v_paddr-1:v_paddr-v_tagbits]; 
      let request_index=request[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
      Bit#(blockbits) word_index=request[v_blockbits+v_wordbits-1:v_wordbits];
      ff_mem_request.enq(wr_miss_from_cache);
      ff_lb_control.enq(tuple5(request_tag,request_index,fn_enable(word_index),wr_replace_line,
                                                          tpl_2(wr_miss_from_cache)==0));
      if(wr_line_valid) begin
        repl.update_set(request_index, wr_replace_line);
        `ifdef perf
          wr_total_evictions<=1;
        `endif
      end
    endrule
    
    //Capturing memory_response
    rule capture_memory_response(&(rg_lbenables)!=1 && ff_lb_control.notEmpty);
     
      let {word,err} = ff_mem_response.first;
      ff_mem_response.deq;

      let {lbtag,lbset, init_we, way, isIO}=ff_lb_control.first();
      let lbenables=rg_lbenables;

      Bit#(blocksize) temp = 0;
      if(rg_blockenable==0)
        temp=init_we;
      else
        temp=rg_blockenable;

      lbenables = lbenables|temp;

     //Each bit in write_enable register refers to corresponding word in block 
      
      Bit#(linewidth) mask = 0;
      for(Integer i=0;i<valueOf(blocksize);i=i+1)
      begin
            Bit#(respwidth) ex_we=duplicate(temp[i]);
            let v_word_len = valueOf(word_len);
            mask[((i*v_word_len)+(v_word_len-1)):i*v_word_len]=ex_we;
      end

      if (verbosity!=0) begin
        $display($time,"\tICACHE: Receiving Memory Response. Word: %h err: %b",word,err);
        $display($time,"\tICACHE: Lbenables changes to:%b",lbenables);
        $display($time,"\tICACHE: WE :%b",temp);
        $display($time,"\tICACHE: MASK:%h",mask);
      end

      Bit#(linewidth) y  = duplicate(word) ; 
      let new_word_line  = y & mask;
      Bit#(linewidth) x  = rg_lbdataline|new_word_line;

      if(isIO) begin
        wr_hit_io<=tuple2(word,err);
        wr_io_response<=True;
        ff_lb_control.deq;
      end
      else begin
        rg_lbvalid<=1;
        rg_lbdataline<=x;
        rg_lbenables<=lbenables;
        rg_lberr<=rg_lberr||err;
        rg_blockenable <= {temp[valueOf(blocksize)-2:0],
                                                temp[valueOf(blocksize)-1]};
      end
      if(verbosity!=0)
        $display($time,"\tICACHE: Updating line_buffer:%h",x);

    endrule

    //Loading data into the cache from line_buffer
    rule upd_data_into_cache(&(rg_lbenables)==1 && (!ff_lb_control.notFull|| rg_fence_stall) && ff_lb_control.notEmpty  && !rg_deq_lb);
      let {lbtag,lbset,init_we, way, isIO}=ff_lb_control.first();

      tag_arr[way].write_request(lbset,{1,lbtag});//lbtag
      data_arr[way].write_request(lbset,truncate(rg_lbdataline));
      if(verbosity!=0)
        $display($time,"\tICACHE: LB Replacing Way :%d in set: %d tag:%h with dataline",way,
                                        lbset,lbtag,rg_lbdataline);
      rg_deq_lb<=True;
    endrule
    rule deq_lb(rg_deq_lb && &(rg_lbenables)==1 && (!ff_lb_control.notFull|| rg_fence_stall) && ff_lb_control.notEmpty);
      ff_lb_control.deq;
      rg_blockenable<=0;
      rg_lbenables<=0;
      rg_lbvalid<=0;
      rg_lbdataline<=0;
      rg_lberr<=False;
      Bit#(setbits) set_index=rg_latest_index;
      let {lbtag,lbset, init_we, way, isIO}=ff_lb_control.first();
      if (lbset==set_index) begin
        if(verbosity>0)
          $display($time,"\tICACHE: Resending request to cache for index: %d",set_index);
        for(Integer i=0;i<v_ways;i=i+1)begin
          data_arr[i].read_request(set_index);
          tag_arr[i].read_request(set_index);
        end
      end
    endrule

    interface core_req=interface Put
      method Action put(ICore_request#(paddr) req) if(!rg_init && !rg_fence_stall && ff_core_response.notFull);
        `ifdef perf
          wr_total_access<=1;
        `endif
        let {addr, fence, epoch} =req;
        if(fence)
          rg_fence_stall<=True;

        ff_req_queue.enq(req);
        Bit#(setbits) set_index=addr[v_setbits+v_blockbits+v_wordbits-1:v_blockbits+v_wordbits];
        for(Integer i=0;i<v_ways;i=i+1)begin
          data_arr[i].read_request(set_index);
          tag_arr[i].read_request(set_index);
        end
        rg_latest_index<=set_index;
        if (verbosity!=0) begin
		      $display($time,"\tICACHE: Receiving request to address:%h Fence: %b epoch: %b index: %d",
              addr, fence, epoch, set_index); 
		      $display($time,"\tICACHE: Access Cache for Addr: %h Index: %d",addr,set_index); 
        end
        if(ramreg)
          rg_delay<=True;
      endmethod
    endinterface;

    interface core_resp = interface Get
      method ActionValue#(ICore_response#(respwidth)) get();
        ff_core_response.deq;
        return ff_core_response.first;
      endmethod
    endinterface;
    
    interface mem_req = interface Get
      method ActionValue#(IMem_request#(paddr)) get;
        ff_mem_request.deq;
        return ff_mem_request.first;
      endmethod
    endinterface;

    interface mem_resp= interface Put
     method Action put(IMem_response#(respwidth) resp);
        ff_mem_response.enq(resp);
     endmethod
    endinterface;
    `ifdef simulate 
      interface meta = interface Get
        method ActionValue#(Bit#(1)) get();
          ff_meta.deq;
          return ff_meta.first;
        endmethod
      endinterface;
    `endif 
    `ifdef perf
      method Bit#(5) perf_counters;
        return {wr_total_evictions,wr_total_io,wr_total_lb_hits,wr_total_cache_hits,wr_total_access};
      endmethod
    `endif
  endmodule

endpackage
