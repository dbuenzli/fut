(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Exception trap *)

let str = Printf.sprintf
let pp = Format.fprintf

type exn_ctx =
  [ `Queue of string | `Future | `Finalizer | `Backend
  | `Fd_action | `Timer_action | `Runtime_action | `Signal_action | `Exn_trap ]

type exn_info = exn_ctx * exn * Printexc.raw_backtrace

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
  split_string (Printexc.raw_backtrace_to_string bt) '\n'

let pp_exn_info ppf (ctx, e, bt) =
  let l = match ctx with
  | `Exn_trap -> "The exception trap itself"
  | `Future -> "A future"
  | `Queue l -> str "A queue %s" l
  | `Finalizer -> "A finalizer"
  | `Backend -> "The backend"
  | `Timer_action -> "A timer action"
  | `Fd_action -> "A file descriptor action"
  | `Signal_action -> "A signal action"
  | `Runtime_action -> "A runtime action"
  in
  pp ppf "@[<v>%s raised:@,@[<v>%s" l (Printexc.to_string e);
  List.iter (pp ppf "@,%s") (split_backtrace bt);
  pp ppf "@]@]"

let default_exn_trap ei = pp Format.err_formatter "%a@." pp_exn_info ei
let exn_trap : (exn_info -> unit) ref = ref default_exn_trap
let set_exn_trap t = exn_trap := t
let exn_trap ctx exn bt =
  try !exn_trap (ctx, exn, bt) with
  | exn -> (* The trap itself raised ! Report it to the trap.  *)
      try
        let bt = Printexc.get_raw_backtrace () in
        !exn_trap (`Exn_trap, exn, bt)
      with
      | exn -> () (* Avoid inifinite loops.xs *)

let trap ctx f v = try f v with
| exn ->
    let bt = Printexc.get_raw_backtrace () in
    exn_trap ctx exn bt

(* Misc *)

let err_invalid_worker_count c = str "worker count must be positive (%d)" c

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
  val signal_action : int -> (abort -> (int -> unit) * 'a) -> 'a
  val timer_action : float -> (abort -> (float -> unit) * 'a) -> 'a
  val fd_action : [`R | `W] -> Unix.file_descr -> (bool -> unit) -> unit
  val fd_close : Unix.file_descr -> unit
  val worker_count : unit -> int
  val set_worker_count : int -> unit
  module Queue : sig
    type t
    val concurrent : t
    val create : ?label:string -> unit -> t
    val label : t -> string
    val add_work : t -> (unit -> unit) -> unit
  end
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
