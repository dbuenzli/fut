(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Exception trap *)

let str = Printf.sprintf
let pp = Format.fprintf

type exn_ctx = 
  [ `Queue of string | `Future | `Finalizer | `Backend
  | `Fd_action | `Timer_action | `Runtime_action | `Signal_action ]
  
type exn_info = exn_ctx * exn * string
                
let split_backtrace bt =                                       (* Grrrr... *)
  let split_string s sep =
    let rec split acc j = 
      let i = try (String.rindex_from s j sep) with Not_found -> -1 in
      if (i = -1) then 
        let p = String.sub s 0 (j + 1) in 
        if p <> "" then p :: acc else acc
        else 
        let p = String.sub s (i + 1) (j - i) in
        let acc' = if p <> "" then p :: acc else acc in
        split acc' (i - 1)
    in
      split [] (String.length s - 1)
  in
  split_string bt '\n'
    
let pp_exn_info ppf (ctx, e, bt) =
  let l = match ctx with 
  | `Future -> "future" 
  | `Queue l -> str "queue %s" l
  | `Finalizer -> "finalizer"
  | `Backend -> "multiplexer"
  | `Timer_action -> "timer action"
  | `Fd_action -> "file descriptor action" 
  | `Signal_action -> "signal action" 
  | `Runtime_action -> "runtime action"
  in
  pp ppf "@[<v>%s raised:@,@[<v>%s" l (Printexc.to_string e);
  List.iter (pp ppf "@,%s") (split_backtrace bt); 
  pp ppf "@]@]"
    
let default_exn_trap ei = pp Format.err_formatter "%a@." pp_exn_info ei
let exn_trap : (exn_info -> unit) ref = ref default_exn_trap
let set_exn_trap t = exn_trap := t
let exn_trap ctx exn bt = !exn_trap (ctx, exn, bt)
    
let trap ctx f v = try f v with 
| e -> 
    let bt = Printexc.get_backtrace () in 
    exn_trap ctx e bt 

(* Queues *)       

let queue_auto_label = 
  let q = ref 0 in 
  fun () -> incr q; str "Fut.queue%d" !q

(* Backend interface *) 

type abort = unit -> unit

module type Backend = sig 
  val name : string
  val start : unit -> unit
  val stop : unit -> unit
  val step : timeout:float -> float
  val action : (unit -> unit) -> unit
  val signal_action : int -> (unit -> unit) -> unit
  val deadline : unit -> float option
  val timer_action : float -> (abort -> (float -> unit) * 'a) -> 'a
  val fd_action : [`R | `W] -> Unix.file_descr -> (bool -> unit) -> unit
  val fd_close : Unix.file_descr -> unit

  module Queue : sig
    type t 
    val concurrent : t 
    val create : ?label:string -> unit -> t 
    val label : t -> string
    val add_work : t -> (unit -> unit) -> unit      
    val worker_count : unit -> int 
    val set_worker_count : int -> unit 
    val ensure_worker : unit -> unit 
  end
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
