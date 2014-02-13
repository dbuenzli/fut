(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Testing for Fut. 

    A few tools to write tests for [Fut].  *)

(** {1 Logging} *)

val str : ('a, unit, string) format -> 'a
(** [str] is {!Format.sprintf} *)  

val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** [pp] is {!Format.fprintf}. *) 

val log : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
(** [log msg] logs [msg] on stdout. *)

val fail : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail msg] raises [Failure msg]. *)

(** {1 Asserting futures} 

    Assert functions just raise [Failure _] when the assertion doesn't
    hold. Compile with [-g] (and to byte code if the trace is lacunar). *)

val promise : unit -> 'a Fut.t * 'a Fut.promise
(** [promise ()] is a future and its promise. *) 

val is_state : 'a Fut.t -> 'a Fut.state -> unit
(** [is state f s] asserts that [f] as state [s]. *)

val is_never : 'a Fut.t -> unit
(** [is_never f] assert that [f] is set to never determine. *) 

val is_det : 'a Fut.t -> 'a -> unit
(** [is_det f v] asserts that [f] determined to [v]. *) 

val is_undet : 'a Fut.t -> unit
(** [is_undet f] asserts that [f] is undetermined. *) 

val record_trap : unit -> unit
(** [record_trap ()] must be called before operations after
    which you want to {!assert_trap}. A single exception trap invocation 
    should occur between [record_trap] and {!assert_trap} otherwise
    the test fails. *) 

val trapped : [ `Exn of Fut.Runtime.exn_ctx * exn | `Nothing ] -> unit
(** [trapped v] asserts according to [v]:
    {ul 
    {- [`Exn _], trap should have been once called with the corresponding 
       arguments since the last {!record_trap}.}
    {- [`Nothing], the trap should not have been invoked since the last
       {!record_trap}.}} *)

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
