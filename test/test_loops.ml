(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Fut.Ops
open Testing

(* Testing that loops don't leak and don't blow the stack. *) 

  
(* Immediate, should not blow the stack. *)

let simple_loop () = 
  log "* Test simple loop.\n";
  let rec loop n = 
    if n = 0 then Fut.ret 0 else 
    Fut.ret (pred n) >>= loop
  in
  let l = loop 500_000_000 in 
  is_det l 0

(* Example from Vouillon's paper. Tests the aliasing mechanism. *)

let vouillon_loop () = 
  log "* Test Vouillon loop.\n";
  let queue = Queue.create () in
  let yield () = 
    let p = Fut.promise () in
    Queue.push p queue;
    Fut.future p
  in
  let rec run () = 
    match try Some (Queue.take queue) with Queue.Empty -> None with
    | Some p -> 
        Fut.set p (`Det ());
        run ()
    | None -> ()
  in
  let rec loop n = 
    if n = 0 then Fut.ret 0 else
    (yield () >>= fun () -> loop (n - 1))
  in
  let l = loop 50_000_000 in
  is_undet l; run () ; is_det l 0

(* Picking, here the waiters of [fut] may grow unbound, if there is
   no provision for compacting aborted waiters. 
   TODO redo, this changed because of the abort semantic change *)

let pick_loop () = 
  log "* Test pick loop\n";
  let fut = Fut.future (Fut.promise ()) in
  let rec loop n = 
    if n = 0 then Fut.ret 0 else 
    Fut.pick (Fut.map succ fut) (Fut.ret (pred n)) >>= loop
  in
  let l = loop 200_000_000 in 
  is_never fut; is_det l 0

let suite () =
  log "Testing loops (witness constant memory usage with top)\n";
  simple_loop ();
  vouillon_loop ();
  pick_loop ();
  ()

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
