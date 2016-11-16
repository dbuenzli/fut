(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* js_of_ocaml backend *) 

let name = "fut.jsoo" 

let now_ms () = (new%js Js.date_now) ## getTime
 
let start () = ()
let stop () = ()
let step ~timeout = failwith "await unsupported"

let action a = failwith "TODO action"
let signal_action s a = failwith "TODO signal action"

let timer_action d def = 
  (* We don't use clearTimeout for abort, it seems unreliable in certain 
     browsers, we'd still need a ref anyways since we only get the clearing
     id after having called setTimeout() *)
  let action_ref = ref None in 
  let abort () = action_ref := None in
  let action, v = def abort in
  action_ref := Some action;
  let ms = d *. 1000. in
  let exp_time_ms = now_ms () +. ms in
  let cb () = match !action_ref with 
  | None -> ()
  | Some action -> action ((now_ms () -. exp_time_ms) /. 1000.)
  in
  ignore (Dom_html.window ## (setTimeout (Js.wrap_callback cb) ms)); 
  v

let fd_action state fd a = failwith "TODO"
let fd_close fd = failwith "TODO"
let worker_count () = failwith "TODO"
let set_worker_count count = 
  if count < 0 
  then invalid_arg (Fut_backend_base.err_invalid_worker_count count)
  else
  failwith "TODO"
    
module Queue = struct
  type t = unit
  let concurrent = ()
  let create ?(label = Fut_backend_base.queue_auto_label ()) () = 
    failwith "TODO"
      
  let label q = failwith "TODO"
  let add_work q w = failwith "TODO"
end

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
