(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests the runtime's timer functionality and timer combinators. *)

open Fut.Op
open Testing

let timers () = 
  log_test "Delays and ticks";
  let diff = ref 0. in
  let d0 = Fut.delay 0.4 in
  let d1 = Fut.tick 0.5 in
  let fdiff = Fut.map (fun d -> diff := d) d0 in
  is_undet d0; 
  is_undet d1; 
  ignore (Fut.await d1); 
  is_det fdiff ();
  is_det d0 !diff;
  is_det d1 ();
  ()

let timer_aborts () = 
  log_test "Abort delays and ticks"; 
  let d0 = Fut.delay 0.4 in
  let d1 = Fut.tick 0.4 in 
  let stop = Fut.tick 0.2 in
  let d2 = Fut.pick stop (Fut.ignore d0) in
  let d3 = Fut.pick stop d1 in 
  let wait = Fut.tick 0.6 in 
  is_undet d0; 
  is_undet d1; 
  is_undet stop; 
  is_undet d2; 
  is_undet d3; 
  is_undet wait;
  ignore (Fut.await wait); 
  is_never d0; 
  is_never d1;
  is_det stop (); 
  is_det d2 (); 
  is_det d3 (); 
  is_det wait (); 
  ()
  
let suite () = 
  log_suite "Testing timers";
  timers ();
  timer_aborts ();
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
