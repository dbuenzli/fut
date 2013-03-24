(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests future queues. *)

open Fut.Ops;;
open Test;;

let apply () = 
  log "Test apply\n";  
  let tid () = Thread.id (Thread.self ()) in 
  let main = tid () in 
  let f = Fut.apply tid () >>= fun qtid -> 
    assert (qtid <> main); 
    assert (tid () = main); 
    Fut.ret true
  in
  is_undet f; ignore (Fut.await f); is_det f true

let never () = 
  log "Test never exn\n"; 
  let never = Fut.apply (fun () -> raise Fut.Never) () in
  is_undet never; ignore (Fut.await never); is_never never

let abort_ref () = 
  log "Test abort ref\n"; 
  let abort = ref false in 
  let aborted, set = promise () in
  let fn () = 
    while (not !abort) do Thread.yield (); done; 
    Fut.Runtime.action (fun () -> (Fut.set set (`Det true)))
  in
  let f = Fut.apply ~abort fn () in 
  is_undet f; 
  ignore (Fut.await ~timeout:0.01 aborted); 
  ignore (Fut.abort f);
  is_never f; 
  ignore (Fut.await aborted); 
  is_det aborted true
  
let trap () = 
  log "Test trap\n"; 
  assert_trap_init ();
  let f = Fut.apply failwith "bla" in
  ignore (Fut.await f);
  assert_trap (`Exn (`Queue "Fut.concurrent", (Failure "bla"))); 
  assert_trap_init ();
  let f = Fut.apply ~queue:(Fut.Queue.create ~label:"bla" ()) failwith "bli" in
  ignore (Fut.await f);
  assert_trap (`Exn (`Queue "bla", (Failure "bli")))

let stress () = 
  log "Stress test\n";
  let tcount = Fut.Runtime.thread_count () in 
  let qcount = tcount * 3 in
  assert (qcount > 0); (* is ok if other tests were made before. *) 
  let qs = Array.init qcount (fun _ -> Fut.Queue.create (), ref 0) in
  let max = 10_000 in
  let last_jobs = ref [] in
  for i = 1 to max do
    for q = 0 to qcount - 1 do
      let queue, jobs = qs.(q) in
      let f = (Fut.apply ~queue (fun jobs -> incr jobs; !jobs) jobs) in 
      if i = max then last_jobs := f :: !last_jobs; 
    done;
  done; 
  let f = Fut.fold ( + ) 0 !last_jobs in
  ignore (Fut.await f);
  is_det f (qcount * max)
    
let test () = 
  Printexc.record_backtrace true;
  apply ();
  never ();
  abort_ref ();
  trap (); 
  stress ();
  log "All tests suceeded.\n"

let () = if not (!Sys.interactive) then test ()
    
(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
