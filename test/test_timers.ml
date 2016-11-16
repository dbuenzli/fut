(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
