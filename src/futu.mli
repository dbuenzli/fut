(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unix system calls as futures.

    This module wraps {!Unix} system calls that support asynchronous
    operation and a few other as futures abstracting
    away the underylying IO multiplexing mechanism.

    For other blocking [Unix] system calls you can invoke them with
    {!Unix.apply} which uses {{!queues}future queues} to perform the
    call, catches unix errors and automatically handles the [EINTR]
    error by retrying the call.

    {b Important.} File descriptors created outside this module
    must be set to non-blocking mode with {!Unix.set_nonblock}
    before they are used with functions of this module. *)


(** {1 Unix results and errors} *)

type error = [ `Unix of Unix.error * string * string ]
(** The type for Unix errors as reported by {!Unix.Unix_error}
      exceptions. *)

val apply : ?queue:Fut.queue -> ('a -> 'b) -> 'a -> ('b, [> error]) Fut.status
  (** [apply queue f v] applies [f v] on [queue] and catches
      {!Unix.Unix_error}. [EINTR] is handled by retrying the call. *)

val call : ('a -> 'b) -> 'a -> ('b, [> error]) Fut.status
(** [call f v] applies [f v] synchronously and catches
      {!Unix.Unix_error}. [EINTR] is handled by retrying the call. *)

(** {1 Signals} *)

  val signal : int -> int Fut.t
  (** [signal s] determines with [s] the next time the signal [s] is
      received by the program.

      {b Warning.} The first time [signal s] is called for a given [s]
      [Fut] overwrites any handler that could be already installed by
      {!Sys.signal} for that signal. Conversly if any other part of
      the program overwrites the handler installed by [Fut] for [s]
      don't expect the futures returned by [signal s] to ever
      determine. *)

(** {1 File descriptors} *)

val nonblock_stdio : unit -> (unit, [> error]) Fut.status
(** [nonblock_stdio ()] sets {!Unix.stdin}, {!Unix.stdout}, {!Unix.stderr}
      to non-blocking mode. *)

val close : Unix.file_descr -> (unit, [> error]) Fut.status
(** [close fd] is like [Unix.close fd], except it handles [EINTR] and
      sets any pending read or write on [fd] to never determine. *)

val dup2 : Unix.file_descr -> Unix.file_descr -> (unit, [> error]) Fut.status
(** [dup2 fd1 fd2] is like [Unix.dup2 fd1 fd2], except it handles [EINTR]
      and sets any pending read or write on [fd2] to never determine. *)

val pipe : unit -> ((Unix.file_descr * Unix.file_descr), [> error]) Fut.status
(** [pipe ()] is like [Unix.pipe ()], except is sets both file descriptors
      to non-blocking mode with [Unix.set_nonblock]. *)

(** {1 Sockets} *)

val socket : Unix.socket_domain -> Unix.socket_type -> int ->
  (Unix.file_descr, [> error]) Fut.status
(** [socket d t p] is like [Unix.socket d t p] except it sets the
      resulting file descriptor to non-blocking mode with
      [Unix.set_nonblock]. *)

val socketpair : Unix.socket_domain -> Unix.socket_type -> int ->
  ((Unix.file_descr * Unix.file_descr), [> error]) Fut.status
(** [socketpair d t p] is like [Unix.socketpair d t p] except it
      sets the resulting file descriptors to non-blocking mode with
      [Unix.set_nonblock].  *)

val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** [accept fd] is like [Unix.accept fd] except it handles
      [EINTR] and [EWOULDBLOCK] and sets the resulting file descriptor
      to non-blocking mode with [Unix.set_nonblock]. *)

val connect : Unix.file_descr -> Unix.sockaddr -> (unit, [> error]) Fut.status
(** [connect] is like {!Unix.connect} except it handles [EINTR] and
      [EINPROGRESS]. *)

val bind : Unix.file_descr -> Unix.sockaddr -> (unit, [> error]) Fut.status

(** {1 IO}

      {b Important.} If you use these functions on a file descriptor
      [fd] use {!close} and {!dup2} instead of the corresponding
      functions of the [Unix] module. This will prevent any [EBADF]
      errors if there are undetermined futures concerning [fd]. *)

val read : Unix.file_descr -> Bytes.t -> int -> int ->
  (int, [> error]) Fut.status
(** [read fd s j l] is like [Unix.read fd s j l] except it handles [EINTR],
    [EAGAIN] and [EWOULDBLOCK]. It is set to never determine if [fd] is
    closed with {!close} or {!dup2}. *)

val write : Unix.file_descr -> Bytes.t -> int -> int ->
  (int, [> error]) Fut.status
(** [write fd s j l] is like [Unix.single_write fd s j l] except it handles
    [EINTR], [EAGAIN] and [EWOULDBLOCK]. It is set to never determine if [fd]
    is closed with {!close} or {!dup2}. *)

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
