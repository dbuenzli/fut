(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
    
  (** {1 Queues} *) 
    
  module Queue : sig
    type t 
    val concurrent : t 
    val create : ?label:string -> unit -> t 
    val label : t -> string
    val add_work : t -> (unit -> unit) -> unit
      
    (** {1 Workers} *) 
      
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
