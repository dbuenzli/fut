(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
   Copyright (c) 2016 Daniel C. Bünzli

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
