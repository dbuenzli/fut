(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

val init : unit -> unit
(** [init ()] initalizes the module. *)

(** {1 Work queues} *)

type queue
(** The type for work queues. 

    A work queue executes the work added to it sequentially in FIFO
    order. Works submitted in two different queue is executed
    concurrently. *)

val concurrent : queue
(** [concurrent] is a special queue. Work added to it is executed
    concurrently. *)

val queue : ?label:string -> unit -> queue
(** [queue ()] is a new queue. *)

val add : queue -> (unit -> unit) -> unit
(** [add q w] adds the work [w] to [q]. *)

(** {1 Work sources} *)

type source 
type source_kind = [ 
  | `Read of Unix.file_descr
  | `Write of Unix.file_descr
  | `Signal of int 
  | `Timeout of float ]

val source_kind : source -> source_kind
val rem_source : source -> unit
val add_source : queue -> source_kind -> (source -> unit) -> unit


(** {1 Workers} *)

val worker_count : unit -> int
(** [worker_count] is the number of working threads. *)

val set_worker_count : int -> unit
(** [set_worker_count] sets the number of working threads. *)

val set_exn_logger : ([ `Worker | `Selector ] -> exn -> string -> unit) -> unit
(** [set_exn_logger f] sets the function used by threads to report
    exceptions raised and uncaught by works. *)



(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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

