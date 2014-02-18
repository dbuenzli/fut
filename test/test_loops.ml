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
  log_test "Test simple loop.";
  let rec loop n = 
    if n = 0 then Fut.ret 0 else 
    Fut.ret (pred n) >>= loop
  in
  let l = loop 1_000_000 in 
  is_det l 0;
  Gc.full_major ();
  ()

(* This is not a loop per se. It tests the runtime loops, namely that 
   waiter execution doesn't stack overflow. *) 
let deep_future () = 
  log_test "The future may be deep (or high depending on your perspective)"; 
  let f, p = promise () in 
  let rec build n f = 
    if n = 0 then f else build (n - 1) (Fut.map (fun x -> x) f)
  in
  let try_blow = build 1_000_000 f in 
  is_undet try_blow; 
  is_undet f;
  Fut.set p (`Det 0);
  ignore (Fut.await ~timeout:0. try_blow);
  is_det try_blow 0;
  Gc.full_major ();
  ()

(* Example from Vouillon's paper. Tests the aliasing mechanism. *)
        
let vouillon_loop () = 
  log_test "Vouillon loop";
  let ps = Queue.create () in
  let rec run () = 
    let p = try Some (Queue.pop ps) with Queue.Empty -> None in 
    match p with 
    | None -> ()
    | Some p -> 
        Fut.set p (`Det ()); (* ignore (Fut.await ~timeout:0. l); *)
        run ()
  in
  let yield () = 
    let p = Fut.promise () in
    Queue.push p ps;
    Fut.future p
  in
  let rec loop n = 
    if n = 0 then Fut.ret () else
    (yield () >>= fun () -> loop (n - 1))
  in
  let l = loop 50_000_000 in
  is_undet l;
  run (); 
  is_det l ();
  ()

(* Picking, here the waiters of [fut] may grow unbound, if there is
   no provision for compacting aborted waiters. 
   TODO redo, this changed because of the abort semantic change *)

let pick_loop () = 
  log_test "Test pick loop";
  let fut = Fut.future (Fut.promise ()) in
  let rec loop n = 
    if n = 0 then Fut.ret 0 else 
    Fut.pick (Fut.map succ fut) (Fut.ret (pred n)) >>= loop
  in
  let l = loop 200_000_000 in 
  is_never fut; is_det l 0; 
  Gc.full_major (); 
  ()

let suite () =
  log_suite "Testing loops (witness constant memory usage with top)";
  deep_future ();
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
