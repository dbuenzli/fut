(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests the runtime's system signal functionality and Futu.signal *)

open Fut.Op
open Testing

let send_signal =
  let self = Unix.getpid () in
  fun s -> Unix.kill self s

let signal_twice () =
  log_test "Test signal twice";
  let s = Sys.sigusr1 in
  let f = Futu.signal s in
  is_undet f;
  send_signal s;
  is_undet f;
  ignore (Fut.await f);
  is_det f s;
  send_signal s;
  ignore (Fut.await f);
  is_det f s;
  ()

let signal_abort () =
  log_test "Test signal abort";
  let s = Sys.sigusr1 in
  let f = Fut.map (fun x -> Some x) (Futu.signal s) in
  is_undet f;
  Fut.abort f;
  is_never f;
  send_signal s;
  ignore (Fut.await f);
  is_never f;
  ()

let suite () =
  log_suite "Testing signals";
  signal_twice ();
  signal_abort ();
  ()







(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

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
