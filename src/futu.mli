(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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

type error = Unix.error * string * string 
(** The type for Unix errors as reported by {!Unix.Unix_error} 
      exceptions. *)

type 'a result = ('a, error) Fut.result 
(** The type for Unix function results. *)
                 
val apply : ?queue:Fut.queue -> ('a -> 'b) -> 'a -> [> 'b result] Fut.t
  (** [apply queue f v] applies [f v] on [queue] and catches 
      {!Unix.Unix_error}. [EINTR] is handled by retrying the call. *)
    
val call : ('a -> 'b) -> 'a -> [> 'b result] Fut.t
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
      
val nonblock_stdio : unit -> [> unit result ] Fut.t
(** [nonblock_stdio ()] sets {!Unix.stdin}, {!Unix.stdout}, {!Unix.stderr}
      to non-blocking mode. *)
    
val close : Unix.file_descr -> [> unit result ] Fut.t
(** [close fd] is like [Unix.close fd], except it handles [EINTR] and
      sets any pending read or write on [fd] to never determine. *)
    
val dup2 : Unix.file_descr -> Unix.file_descr -> [> unit result] Fut.t
(** [dup2 fd1 fd2] is like [Unix.dup2 fd1 fd2], except it handles [EINTR]
      and sets any pending read or write on [fd2] to never determine. *)
    
val pipe : unit -> [> (Unix.file_descr * Unix.file_descr) result ] Fut.t
(** [pipe ()] is like [Unix.pipe ()], except is sets both file descriptors
      to non-blocking mode with [Unix.set_nonblock]. *)
    
(** {1 Sockets} *)
    
val socket : Unix.socket_domain -> Unix.socket_type -> int -> 
  [> Unix.file_descr result] Fut.t
(** [socket d t p] is like [Unix.socket d t p] except it sets the
      resulting file descriptor to non-blocking mode with
      [Unix.set_nonblock]. *)
    
val socketpair : Unix.socket_domain -> Unix.socket_type -> int -> 
  [> (Unix.file_descr * Unix.file_descr) result] Fut.t
(** [socketpair d t p] is like [Unix.socketpair d t p] except it
      sets the resulting file descriptors to non-blocking mode with
      [Unix.set_nonblock].  *)
    
val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr 
(** [accept fd] is like [Unix.accept fd] except it handles 
      [EINTR] and [EWOULDBLOCK] and sets the resulting file descriptor
      to non-blocking mode with [Unix.set_nonblock]. *)
                                                    
val connect : Unix.file_descr -> Unix.sockaddr -> [> unit result ] Fut.t
(** [connect] is like {!Unix.connect} except it handles [EINTR] and
      [EINPROGRESS]. *)
    
val bind : Unix.file_descr -> Unix.sockaddr -> 
  [`Error of Unix.error | `Ok ] Fut.t
    
(** {1 IO} 
    
      {b Important.} If you use these functions on a file descriptor
      [fd] use {!close} and {!dup2} instead of the corresponding
      functions of the [Unix] module. This will prevent any [EBADF]
      errors if there are undetermined futures concerning [fd]. *)
    
val read : Unix.file_descr -> string -> int -> int -> [> int result ] Fut.t
(** [read fd s j l] is like [Unix.read fd s j l] except it handles [EINTR], 
      [EAGAIN] and [EWOULDBLOCK]. It is set to never determine if [fd] is
      closed with {!close} or {!dup2}. *)
    
val write : Unix.file_descr -> string -> int -> int -> [> int result ] Fut.t
(** [write fd s j l] is like [Unix.single_write fd s j l] except it handles 
      [EINTR], [EAGAIN] and [EWOULDBLOCK]. It is set to never determine if [fd]
      is closed with {!close} or {!dup2}. *)

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
