(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* js_of_ocaml backend *) 

let name = "fut.jsoo" 

let now_ms () = (jsnew Js.date_now () ## getTime ())
 
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
  ignore (Dom_html.window ## setTimeout (Js.wrap_callback cb, ms)); 
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
   Copyright (c) 2014 Daniel C. Bünzli.
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
