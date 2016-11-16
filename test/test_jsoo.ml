(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing;;

module Futj : sig
  val yield : (unit -> unit) -> unit
  val async_await : ?timeout:float -> 'a Fut.t -> ('a Fut.state -> unit) -> unit
end = struct
  let yield f =
    ignore (Dom_html.window ## (setTimeout (Js.wrap_callback f) (0.)))

  let now_ms () = (new%js Js.date_now) ## getTime

  let async_await ?(timeout = max_float) fut k =
    let start = now_ms () in
    match Fut.state fut with
    | `Det _ | `Never as set -> k set
    | `Undet ->
        let rec cb () = match Fut.state fut with
        | `Det _ | `Never as set -> k set
        | `Undet ->
            if timeout <> max_float && (now_ms () -. start) > timeout *. 1000.
          then k `Undet
          else
          ignore (Dom_html.window ## (setTimeout (Js.wrap_callback cb) (0.)))
        in
      ignore (Dom_html.window ## (setTimeout (Js.wrap_callback cb) (0.)))


(*
  let async_await ?(timeout = max_float) fut k = match Fut.state fut
  | `Det _ | `Never as set -> k set
  | `Undet ->
    let def abort =
      let finally = ref Some (fun () -> abort (); k (Fut.state fut)) in
      let finally _ = match !finally with None -> () | Some f -> () in
      let _ = Fut.finally finally fut in
      let timer _ = finally := None; k (Fut.state fut) in
      timer, ()
    in
    Runtime.timer_action timeout def
  *)


end

let timers k () =
  log_test "Delays and ticks";
  let diff = ref 0. in
  let d0 = Fut.delay 0.4 in
  let d1 = Fut.tick 0.5 in
  let fdiff = Fut.map (fun d -> diff := d) d0 in
  is_undet d0;
  is_undet d1;
  Futj.async_await ~timeout:20.0 d1 begin function
  | `Never -> assert false (* this should fail the test and invoke k ()*)
  | `Undet -> assert false (* ibid *)
  | `Det () ->
      is_det fdiff ();
      is_det d0 !diff;
      is_det d1 ();
      k ()
  end

let timer_aborts k () =
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
  Futj.async_await ~timeout:20.0 wait begin function
  | `Never -> assert false (* this should fail the test and invoke k ()*)
  | `Undet -> assert false (* ibid *)
  | `Det () ->
      is_never d0;
      is_never d1;
      is_det stop ();
      is_det d2 ();
      is_det d3 ();
      is_det wait ();
      k ()
  end

let test_timers k () =
  log_suite "Testing timers";
  timers (timer_aborts k) ()

let main _ =
  Futj.yield begin fun () ->
    Test_base.suite ();
    test_timers (fun () -> ignore (Testing.log_results ())) ();
  end;
  Js._false

let () = Dom_html.window ##. onload := Dom_html.handler main

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
