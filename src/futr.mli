(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Futures as {!React} events and vice-versa. *) 

open React

(** {1 Events} *) 

val to_event : ?never:'a -> (unit -> 'a Fut.t) -> 'a event 
(** [to_event f] occurs {e once} when [f ()] determines. If [f] is 
    set to never determine the event never occurs unless [never]
    is specified in which case it occurs with the value [never]. 

    {b Important.} It is better if [f] actually creates the future 
    rather than being a closure that captures an already created
    future. The reason is that TODO. *)

val of_event : 'a event -> 'a Fut.t
(** [of_event e] is a future that determines on the next occurence of [e]. 

    {b Note.} The React update step generating the occurence of [s]
    is guaranteed to terminate before the future determines. *)

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
