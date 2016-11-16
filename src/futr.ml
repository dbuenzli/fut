(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Fut.Op
open React

let to_event ?never f =
  let e, send = E.create () in
  let finally f = match Fut.state f with
  | `Det v -> send v
  | `Never -> (match never with None -> () | Some v -> send v)
  | `Undet -> assert false
  in
  ignore (Fut.delay 0.0001 (* TODO Fut.defer *) >>= fun _ ->
          let f = f () in
          Fut.finally finally f f);
  e

let of_event e =
  let p = Fut.promise () in
  let set v = Fut.set p (`Det v) in
  let set_ev = E.map set e in
  let finally set_ev = E.stop set_ev in (* just to keep a ref on the event *)
  Fut.delay 0.0001 (* TODO Fut.defer *)
  >>= fun _ -> (Fut.finally finally set_ev (Fut.future p))

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
