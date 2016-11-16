(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests future queues. *)

open Fut.Op
open Testing

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
