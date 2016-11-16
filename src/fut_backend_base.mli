(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Fut backend base module. 

    Implements the exception trap and defines backend tools and signatures. *)


(** {1 Exception trap} *) 

type exn_ctx = 
  [ `Queue of string | `Future | `Finalizer | `Backend
  | `Fd_action | `Timer_action | `Signal_action | `Runtime_action | `Exn_trap ]
(** The type for exception contexts. *)
  
type exn_info = exn_ctx * exn * Printexc.raw_backtrace
(** The type for info about trapped exceptions. The context, 
      the exception, and the backtrace. *)
                
val set_exn_trap : (exn_info -> unit) -> unit
(** [set_exn_trap h] is a function called whenever the runtime
      system catches an exception. *)
  
val pp_exn_info : Format.formatter -> exn_info -> unit
(** [pp_exn_info ppf i] prints an unspecified representation of [i]
      on [ppf]. *)

val exn_trap : exn_ctx -> exn -> Printexc.raw_backtrace -> unit
(** [exn_traps ctx exn bt] traps the exception [exn] in context [ctx] with
    backtrace [bt]. *) 

val trap : exn_ctx -> ('a -> unit) -> 'a -> unit
(** [trap ctx f x] executes [f x] and if it raises traps the exception 
    with context [ctx]. *)

(** {1 Backend tools} *)

val err_invalid_worker_count : int -> string
(** [err_invalid_worker_count n] is a string to raise [Invalid_argument]
    whenever {!set_worker_count} is called with [n < 0]. *)

(** {2 Queues} *) 

val queue_auto_label : unit -> string 
(** [queue_auto_label ()] is a new label for a queue. *)

(** {1 Backend interface} *) 

type abort = unit -> unit 
(** See {!Fut.Runtime.abort}. *)

(** Fut backend interface. 

    {b Note.} While the Unix module interface is needed to compile
       backends, there's no need to {b link} against it. *) 
module type Backend = sig 

  (** {1 Runtime} *) 

  val name : string
  val start : unit -> unit
  val stop : unit -> unit
  val step : timeout:float -> float  
  (** [timeout] is guaranteed to be equal or greater than [0.], if it is 
      equal to max_float, means unbounded wait. *)

  (** {1 Actions} *) 

  val action : (unit -> unit) -> unit

  (** {2 Signal actions} *) 

  val signal_action : int -> (abort -> (int -> unit) * 'a) -> 'a
  (** See {!Fut.Runtime.signal_action}. *)

  (** {2 Timer actions} *) 

  val timer_action : float -> (abort -> (float -> unit) * 'a) -> 'a 
  (** See {!Fut.Runtime.timer_action}. *)   

  (** {2 File descriptor actions} *) 

  val fd_action : [`R | `W] -> Unix.file_descr -> (bool -> unit) -> unit
  val fd_close : Unix.file_descr -> unit
    
  (** {1 Workers} *) 

  val worker_count : unit -> int 
  (** See {!Fut.Runtime.worker_count}. *) 
    
  val set_worker_count : int -> unit 
  (** See {!Fut.Runtime.set_worker_count}. *)

  (** {1 Queues} *) 
    
  module Queue : sig
    type t 
    (** See {!Fut.queue}. *) 

    val concurrent : t 
    (** See {!Fut.Queue.concurrent}. *) 

    val create : ?label:string -> unit -> t 
    (** See {!Fut.Queue.create}. *) 

    val label : t -> string
    (** See {!Fut.Queue.label}. *) 

    val add_work : t -> (unit -> unit) -> unit      
    (** TODO *)
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
