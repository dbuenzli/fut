(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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

val log_test : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
(** [log_test msg] logs [msg] on stdout. *)

val log_suite : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
(** [log_suite msg] logs [msg] on stdout. *)

val fail : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail msg] raises [Failure msg]. *)

val log_results : unit -> bool
(** [log_results ()] logs the result of the tests performed and returns
    [true] if there were no errors. *) 

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
