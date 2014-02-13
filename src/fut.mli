(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Future values for asynchronous programming.

    [Fut] provides support to define and combine futures.  A future is
    an undetermined value that becomes determined at an arbitrary
    point in the future. The future acts as a place-holder for the
    value while it is undetermined. 

    {{!promises}Promises} determine the value of futures cooperatively
    while {{!queues}future queues} determine the value of blocking or
    long running function applications with a set of system threads
    and act as a mutual exclusion synchronization primitive.

    The {!Fut.Unix} module wraps [Unix] non-blocking calls as futures,
    abstracting away the underlying IO multiplexing mechanism. It also
    provides functions to handle [Unix] blocking calls and their errors.

    Consult the {{!basics}basics}, the {{!semantics}semantics} 
    and {{!examples}examples}. 

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1:values Futures} *)

type 'a t
(** The type for a future that determines a value of type ['a]. *)

type 'a state = [ `Never | `Undet | `Det of 'a ]
(** The type for future states. [`Det v] if the future determined [v]. 
    [`Never] if the future is known to never determine. [`Undet]
    if the future is undetermined. If the state is different from [`Undet],
    the future is {e set}. *)

val state : 'a t -> 'a state
(** [state f] is the current state of [f]. 

    {b Note.} This function can be called by other threads. *)

val await : ?timeout:float -> 'a t -> 'a state
(** [await timeout f], is like {!state} except if [f] is undetermined
    it waits until [f] is set or [timeout] seconds passed. If [timeout] 
    is unspecified, [await] blocks until [f] is set. *)

val finally : ('a -> 'b) -> 'a -> 'c t -> 'c t
(** [finally fn v f] is [f] but [fn v] is called (and its result ignored) 
    whenever [f] is set. If [fn v] raises an exception it is reported
    to the exception trap specified with {!Runtime.set_exn_trap}. *)

(** {2:applicative Applicative combinators} 

    These combinators have no effects on the determination
    of their arguments. *)

val never : unit -> 'a t
(** [never ()] is a future that never determines a value. 
    {ul {- \[[never ()]\]{_t} [= `Never]}} *)

val ret : 'a -> 'a t
(** [ret v] is a future that determines the value [v] immediately. 
    {ul {- \[[ret v]\]{_t} [= `Det v]}} *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind f fn] is the future [fn v] where [v] is the value
    determined by [f]. 
    {ul 
    {- \[[bind f fn]\]{_t} [= ] \[[fn v]\]{_t} if \[[f]\]{_t} [= `Det v]}
    {- \[[bind f fn]\]{_t} [= `Never] if \[[f]\]{_t} [= `Never]}}
*)

val app : ('a -> 'b) t -> 'a t -> 'b t
(** [app ff fv] is a future that determines [f v] where [f]
    is determined by [ff] and [v] by [fv]. 
    {ul
    {- \[[app ff fv]\]{_t} [= `Det (f v)] if \[[ff]\]{_t} [= `Det f]
    and \[[fv]\]{_t} = [`Det v]}
    {- \[[app ff fv]\]{_t} [= `Never] if \[[ff]\]{_t} [= `Never] or
    \[[fv]\]{_t} [= `Never]}} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map fn f] is a future that determines the value [fn v] where [v]
    is the value determined by [f]. 
    {ul
    {- \[[map fn f]\]{_t} [= `Det (fn v)] if \[[f]\]{_t} [= `Det v]}
    {- \[[map fn f]\]{_t} [= `Never] if \[[f]\]{_t} [= `Never].}} *)

val ignore : 'a t -> unit t
(** [ignore f] is {!map} [(fun _ -> ()) f]. *) 

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t list -> 'a t
(** [fold fn acc \[f]{_1}[; ...; f]{_n}[\]] is 
    [List.fold_left fn acc \[v]{_1}[; ...; v]{_n}[\]] where
    [v]{_i} is the value determined by the future [f]{_i}. 
    {ul 
    {- \[[fold fn acc \[f]{_1}[; ...; f]{_n}[\]]\]{_t}
    [= List.fold_left fn acc \[v]{_1}[; ...; v]{_n}[\]] if for all [i]
    \[[f]{_i}\]{_t} [= v]{_i}}
    {- \[[fold fn acc \[f]{_1}[; ...; f]{_n}[\]]\]{_t} 
    [= `Never] if there is [i] with \[[f]{_i}\]{_t} [= `Never]}} *)

val barrier : ?set:bool -> unit t list -> unit t
(** [barrier set \[f]{_1}[; ...; f]{_n}[\]] determines
    when all [f]{_i} do. If [set] is [true] (defaults to [false]), 
    determines when all [f]{_i} are set.
    {ul 
    {- \[[barrier false \[f]{_1}[; ...; f]{_n}[\]]\]{_t} 
     [= `Det ()] if for all [i] \[[f]{_i}\]{_t} [= `Det ()]}
    {- \[[barrier false \[f]{_1}[; ...; f]{_n}[\]]\]{_t}
     [= `Never] if there is [i] with \[[f]{_i}\]{_t} [= `Never]}
    {- \[[barrier true \[f]{_1}[; ...; f]{_n}[\]]\]{_t} 
     [= `Det ()] if for all [i] \[[f]{_i}\]{_t} [<> `Undet]}}*)

val sustain : 'a t -> 'a t -> 'a t
(** [sustain f f'] determines like [f] if it does and like [f'] 
    if [f] never determines.
    {ul
    {- \[[sustain f f']\]{_t} [= `Det v] if \[[f]\]{_t} [= `Det v]}
    {- \[[sustain f f']\]{_t} [=] \[[f']\]{_t} if \[[f]\]{_t} [= `Never]}} *)

val first : 'a t -> 'a t -> ('a * 'a t) t
(** [first f f'] is a future that determines [(v, snd)] where [v]
    is the value of the first future to determine and [snd]
    is the other one. If both futures are already determined
    the left one is taken.
    {ul
    {- \[[first f f']\]{_t} [= `Det (v, f')] if \[[f]\]{_t} [= `Det v] and
      for all [t' < t] \[[f']\]{_t'} [<> `Det _]}
    {- \[[first f f']\]{_t} [= `Det (v, f)] if \[[f']\]{_t} [= `Det v] and
      for all [t' <= t] \[[f]\]{_t'} [<> `Det _]}
    {- \[[first f f']\]{_t} [= `Never] if \[[f]\]{_t} [=] \[[f']\]{_t} 
      [= `Never]}}
*)

val firstl : 'a t list -> ('a * 'a t list) t
(** [next fs] is a future that determines [(v, fs')] where [v] is
    the first value to determine in [fs] and [fs'] are the remaining
    ones. If more than one future is already determined the first
    left one is taken. 
    {ul
    {- \[[firstl \[f]{_1}[; ...; f]{_n}[\]]\]{_t}
    [= `Det (v, \[f]{_1}[; ...; f]{_j-1}[; f]{_j+1}[; ...;f]{_n}[\])]
    if \[[f]{_j}\]{_t} [= `Det v] and for all t' < t and i < j
    \[[f]{_i}\]{_t'} [<> `Det _]}
    {- \[[firstl \[f]{_1}[; ...; f]{_n}[\]]\]{_t} [= `Never] if for all [i]
    \[[f]{_i}\]{_t} [= `Never]}} *)

(** {2:effectful Effectful combinators} 

    These combinators may set their [`Undet]ermined arguments to
    [`Never], aborting their determination. 

    {b Important.} When a future is aborted, only that future is
    affected and set to never determine. It doesn't affect the
    determination of futures it may be waiting on. Also, aborting an a
    future that is set has no effect since once the future is set it
    never changes again.

    {b Warning.}  If the aborted future is an [`Undet]ermined future from 
    a future queue, there's no guarantee that the corresponding application
    will be/has not been executed when it is set to [`Never]. In any
    case the semantics of futures is preserved, it will switch from [`Undet]
    to [`Never] and remain so forever but depending on what the 
    function application does, the world may be affected. *)

val abort : 'a t -> 'a t
(** [abort f] is a future that never determines. If [f] was [`Undet]ermined
    at application time, it is set to never determine. 
    {ul
    {- \[[abort f]\]{_t} [= `Never]}
    {- \[[f]\]{_t'} [= `Never] with t' > ta, if \[[f]\]{_ta} = [`Undet]
    where [ta] is [abort]'s application time.}} *)

val link : 'a t -> ('a -> 'b t) -> 'b t
(** [link f fn] is like {!Fut.bind} except that if it is aborted
    before [f] determined then [f] is also aborted. *)


val pick : 'a t -> 'a t -> 'a t
(** [pick f f'] is a future that determines as the first future 
    that does and sets the other to never determine.
    If both future are already determined the left one is taken. 
    {ul
    {- If \[[f]\]{_t} [= `Det v] then
     {ul 
     {- \[[pick f f']\]{_t} [= `Det v]}
     {- \[[f']\]{_t'} [= `Never] with t' > t, if \[[f']\]{_t} = [`Undet].}}
    }
    {- If \[[f']\]{_t} [= `Det v] and \[[f]\]{_t} [<> `Det _] then
     {ul 
     {- \[[pick f f']\]{_t} [= `Det v]}
     {- \[[f]\]{_t'} [= `Never] with t' > t, if \[[f]\]{_t} = [`Undet].}}
    }
    {- If \[[f]\]{_t} = \[[f']\]{_t} [= `Never] then 
    \[[pick f f']\]{_t} [= `Never]}}
*)

val pickl : 'a t list -> 'a t
(** [pickl fs] is [List.fold_left Fut.pick (Fut.never ()) fs]. *)

(** {1:promises Promises} *)

type 'a promise
(** The type for promises. A promise can {!set} the {!future} 
    it is associated to. *)

val promise : ?abort:(unit -> unit) -> unit -> 'a promise
(** [promise abort ()] is a new promise. [abort] is called if the
    future of the promise is set to never determine via {!set} or
    an {{!effectful}effectful combinator}. Once the future is set
    [abort] is eventually garbage collected. 
    
    {b Note.} This function can be called from other threads. *)

val future : 'a promise -> 'a t
(** [future p] is the future set by promise [p]. 

    {b Note.} This function can be called from other threads. *)

val set : 'a promise -> [`Det of 'a | `Never ] -> unit
(** [set p s] sets {!future} [p] to [s]. 

    @raise Invalid_argument if the future of [p] is already 
    set. *)

(** {1:queues Future queues} *)

type queue
(** The type for future queues. A future queue determines function 
    applications sequentially in FIFO order. Applications submitted in 
    two different queues are determined concurrently. *)

(** Future queues. *)
module Queue : sig
  
  (** {1:queues Queues} *)
  
  type t = queue
  (** See {!queue}. *)

  val create : ?label:string -> unit -> queue
  (** [queue ~label ()] is a new queue with the given [label], if unspecified
      a label is automatically generated. *)

  val concurrent : queue
  (** [concurrent] is a special future queue. Applications added to it
      are all executed concurrently. *)

  val label : queue -> string
  (** [queue_label q] is [q]'s label. *)
end

val apply : ?queue:queue -> ?abort:bool ref -> ('a -> 'b) -> 'a -> 'b t 
(** [apply queue abort fn v] is a future that determines the value [fn
    v] on [queue] (defaults to {!Queue.concurrent}). If provided, the
    [abort] argument is set to [true] by the runtime system if the
    future is set to [`Never], e.g. because it was {!abort}ed; [fn]
    can consult this reference cell to stop its computation or take
    appropriate action.
    
    [fn] can raise {!Never} to silently set the future to never determine.
    Any uncaught exception of [fn v] also sets the future to never determine
    however these exceptions are reported to the exception trap specified
    with {!Runtime.set_exn_trap}. 

    {b Note.} If your program is not multi-threaded yet, it will 
    become at that point. 

    {b Warning.} The future won't determine if {!await} is not 
    called regularly. *)

exception Never
(** [Never] is raised by a function application in a queue to set
    its corresponding future to never determine. *)

(** {1:timers Timers} 

    Timer determine values at specific points in time. All the combinators
    have an optional [abs] argument that defaults to [false]. If set to 
    [true] the given time is interpreted as an absolute time in POSIX
    seconds since 1970-01-01 00:00:00 UTC. If [d] and [abs] denote 
    an earlier time the future determines immediately.

    {b TODO.} not sure the [abs] argument is a good idea, let's leave
    POSIX time out of the equation. Ideally backends should implement
    these functions using a monotonic clock and not POSIX time.

    {b Warning.} These futures don't determine if {!await} is not
    called regularly.
*)

val tick : ?abs:bool -> float -> unit t
(** [tick abs d] is a value that determines [()] in [d] seconds.
    {ul
    {- \[[tick false d]\]{_ta+d} [= `Det ()] where [ta] is 
       [tick]'s application time}
    {- \[[tick true t]\]{_t} [= `Det `()]}} *)

val delay : ?abs:bool -> float -> (float -> float -> 'a) -> 'a t
(** [delay abs d fn] is like {!tick} except it determines the value 
    [fn d d'] where [d'] is the delay (or absolute time) 
    that was actually performed by the runtime sytem. *)

val timeout : ?abs:bool -> float -> 'a t -> [> `Timeout | `Ok of 'a ] t
(** [timeout abs d f] is a value that determines [`Ok v] if [f]
    determines with [v] before [d] seconds. In any other case 
    it determines with [`Timeout] after [d] seconds and aborts [f] if
    it is still undetermined.
    {ul 
     {- \[[timeout false d f]\]{_ta + d} [= `Det (`Ok v)] 
        if there is [t < ta + d] with \[[f]\]{_t} [= `Det v] where
        [ta] is [timeout]'s application time.}
     {- [timeout false d f]\]{_ta + d} [= `Det `Timeout] and
        \[[f]\]{_ta + d} [= `Never] otherwise.}
     {- \[[timeout true t f]\]{_t} [= `Det (`Ok v)] 
        if there is [t' < t] with \[[f]\]{_t'} [= `Det v].}
     {- \[[timeout true t f]\]{_t} [= `Det `Timeout] and
        \[[f]\]{_t} [= `Never] otherwise.}} 
    Can be seen as a short hand for
{[
    Fut.pick (Fut.link (fun v -> ret (`Ok v)) f) 
             (Fut.link (fun () -> ret `Timeout) Fut.tick abs d)
]}
*)

(** {1 Runtime system} *)

(** Runtime system configuration and interaction. *)
module Runtime : sig

  (** {1 Exception trap} 

      See also the section about {{!exceptions}exceptions}. *)

  type exn_ctx = 
  [ `Queue of string | `Future | `Finalizer | `Backend
  | `Fd_action | `Timer_action | `Signal_action | `Runtime_action ]

  (** The type for exception contexts. *)

  type exn_info = exn_ctx * exn * string
  (** The type for info about trapped exceptions. The context, 
      the exception, and the backtrace. *)

  val set_exn_trap : (exn_info -> unit) -> unit
  (** [set_exn_trap h] is a function called whenever the runtime
      system catches an exception. *)

  val pp_exn_info : Format.formatter -> exn_info -> unit
  (** [pp_exn_info ppf i] prints an unspecified representation of [i]
      on [ppf]. *)
    
  (** {1 System threads} *)
    
  val thread_count : unit -> int 
  (** [thread_count] is the number of threads used for future queues. *)

  val set_thread_count : int -> unit
  (** [set_worker_count n] sets the number of threads used for worker
      queues to [n]. 

      {b Note.} Calling this function explicitely with [n > 0] makes the 
      program multithreaded. *)
  
  (** {1 Runtime actions} *)

  val action : (unit -> unit) -> unit
  (** [action a] executes [a ()] as soon as possible on the runtime system
      thread. Actions are executed in FIFO order as received. 

      {b Note.} This function can be called from other threads. *)

  (** {1 Signal actions} *)

  val signal_action : int -> (unit -> unit) -> unit
  (** [signal_action s a] executes [a ()] whenever signal [s] is received. *)

  (** {1 Timer actions} *)

  val deadline : unit -> float option 
  (** [deadline ()] is the duration to the next timer action
      (if any). If the duration is negative, the runtime
      system is late. *)

  val timer_action : float -> (unit -> unit) -> (unit -> unit)
  (** [timer_action t a] executes [a ()] at time [t] where [t] is
      interpreted as an absolute time in POSIX seconds since
      1970-01-01 00:00:00 UTC. If [t] is earlier than the current
      time it must be executed at some point, but not immediatly. *)

  (** {1 File descriptor actions} *)

  type fd_state = [ `R | `W ]
  (** The type for file descriptor states. [`R] means readable and 
      [`W] writable. *)

  val fd_action : fd_state -> Unix.file_descr -> (bool -> unit) -> unit
  (** [fd_action fds fd a] executes [a true] whenever [fd] is in the
      state [fds]. If [fd] is closed [a false] executed. *)

  (** {1 Runtime backend} *)

  (* TODO *)
end

(** {1 Unix} *)

(** Unix system calls as futures. 

    This module wraps {!Unix} system calls that support asynchronous
    operation and a few other as futures.

    For other blocking [Unix] system calls you can invoke them with
    {!Unix.apply} which uses {{!queues}future queues} to perform the
    call, catches unix errors and automatically handles the [EINTR]
    error by retrying the call.
    
    {b Important.} File descriptors created outside this module
    must be set to non-blocking mode with {!Unix.set_nonblock}
    before they are used with functions of this module.
*)
module Unix : sig

  (** {1 Unix results and errors} *)

  type error = Unix.error * string * string 
  (** The type for Unix errors as reported by {!Unix.Unix_error} 
      exceptions. *)

  type 'a result = [ `Error of error | `Ok of 'a ]
  (** The type for Unix function results. *)

  val ubind : [> 'b result] t -> ('a -> [> 'b result] t) -> [> 'b result] t

  val apply : ?queue:queue -> ('a -> 'b) -> 'a -> [> 'b result] t
  (** [apply queue f v] applies [f v] on [queue] and catches 
      {!Unix.Unix_error}. [EINTR] is handled by retrying the call. *)

  val call : ('a -> 'b) -> 'a -> [> 'b result] t
  (** [call f v] applies [f v] synchronously and catches 
      {!Unix.Unix_error}. [EINTR] is handled by retrying the call. *)

  (** {1 Signals} *)

  val signal : int -> int t 
  (** [signal s] determines with [s] when the signal [s] is received.

      {b Warning.} This replaces any handler set with {!Sys.set_signal}. *)

  (** {1 File descriptors} *)

  val nonblock_stdio : unit -> [> unit result ] t
  (** [nonblock_stdio ()] sets {!Unix.stdin}, {!Unix.stdout}, {!Unix.stderr}
      to non-blocking mode. *)

  val close : Unix.file_descr -> [> unit result ] t
  (** [close fd] is like [Unix.close fd], except it handles [EINTR] and
      sets any pending read or write on [fd] to never determine. *)

  val dup2 : Unix.file_descr -> Unix.file_descr -> [> unit result] t
  (** [dup2 fd1 fd2] is like [Unix.dup2 fd1 fd2], except it handles [EINTR]
      and sets any pending read or write on [fd2] to never determine. *)

  val pipe : unit -> [> (Unix.file_descr * Unix.file_descr) result ] t
  (** [pipe ()] is like [Unix.pipe ()], except is sets both file descriptors
      to non-blocking mode with [Unix.set_nonblock]. *)
      
  (** {1 Sockets} *)

  val socket : Unix.socket_domain -> Unix.socket_type -> int -> 
    [> Unix.file_descr result] t
  (** [socket d t p] is like [Unix.socket d t p] except it sets the
      resulting file descriptor to non-blocking mode with
      [Unix.set_nonblock]. *)

  val socketpair : Unix.socket_domain -> Unix.socket_type -> int -> 
    [> (Unix.file_descr * Unix.file_descr) result] t
  (** [socketpair d t p] is like [Unix.socketpair d t p] except it
      sets the resulting file descriptors to non-blocking mode with
      [Unix.set_nonblock].  *)

  val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr 
  (** [accept fd] is like [Unix.accept fd] except it handles 
      [EINTR] and [EWOULDBLOCK] and sets the resulting file descriptor
      to non-blocking mode with [Unix.set_nonblock]. *)

  val connect : Unix.file_descr -> Unix.sockaddr -> [> unit result ] t
  (** [connect] is like {!Unix.connect} except it handles [EINTR] and
      [EINPROGRESS]. *)

  val bind : Unix.file_descr -> Unix.sockaddr -> 
    [`Error of Unix.error | `Ok ] t

  (** {1 IO} 

      {b Important.} If you use these functions on a file descriptor
      [fd] use {!close} and {!dup2} instead of the corresponding
      functions of the [Unix] module. This will prevent any [EBADF]
      errors if there are undetermined futures concerning [fd]. *)

  val read : Unix.file_descr -> string -> int -> int -> [> int result ] t
  (** [read fd s j l] is like [Unix.read fd s j l] except it handles [EINTR], 
      [EAGAIN] and [EWOULDBLOCK]. It is set to never determine if [fd] is
      closed with {!close} or {!dup2}. *)
    
  val write : Unix.file_descr -> string -> int -> int -> [> int result ] t 
  (** [write fd s j l] is like [Unix.single_write fd s j l] except it handles 
      [EINTR], [EAGAIN] and [EWOULDBLOCK]. It is set to never determine if [fd]
      is closed with {!close} or {!dup2}. *)

end

(** {1 Infix operators} 

    Use of these operators may kill a few parentheses. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [f >>= fn] is [bind f fn]. *)

(** Infix operators. 

    In a module to open. *)
module Ops : sig

  (** {1 Binding operators} *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [f >>= fn] is [bind f fn]. *)

  val (|>) : 'a Unix.result t -> ('a -> [> 'b Unix.result]) -> 'b t
end


(** {1:basics Basics} 
    
    A future is a place-holder for a value that may become determined
    at an arbitrary point in the future. Futures can be combined
    together to define new futures that will determine whenever the
    underlying futures do. For example, below [revolt] determines once
    [time_to_revolt] does.
{[
let time_to_revolt = Fut.tick 1.871 in
let revolt = Fut.bind time_to_revolt 
  (fun `Tick -> Fut.apply Printf.printf "Revolt!")

let () = ignore (Fut.await revolt)   
]}
    A future can be in three different states described by the type 
    {!type:state} and returned by the (non-blocking) function {!Fut.state}. 
    It is:
    {ul 
    {- [`Undet], if the future is [`Undet]ermined and may still determine
       a value at some point in the future.}
    {- [`Det v], if the future [`Det]ermined the value [v].}
    {- [`Never], if the future will never determine a value.}}

    If the state of a future [f] is [`Det] or [`Never] we say that
    the future is {e set}.

    TODO note non-blocking.
    
    The module {!Fut.Ops} defines a few infix operators to make the code more 
    readable. For example the {!Ops.(>>=)} operator can be used for
    {!bind}. This makes it easier to sequence binds:
{[
open Fut.Ops;;

let time_to_revolt d = Fut.tick d in
let revolt =
  time_to_revolt 1.871 >>= fun `Tick ->
  Fut.apply Printf.printf "Revolt!" >>= fun () -> 
  time_to_revolt 0.72 >>= fun `Tick -> 
  Fut.apply Printf.printf "Again!"

let () = ignore (Fut.sync revolt)
]}

    {2:semantics Semantics}
  
    The semantic function \[\][: 'a Fut.t -> time -> 'a Fut.state]
    gives meaning to a future [f] by mapping it to a function
    \[[f]\] that give its {{!type:state}state} at any point in time 
    (after its creation). We write \[[f]\]{_t} the evaluation of 
    this {e semantic} function at time [t]. 

    Remember that once a future is set it never changes again.
    This is captured by the following axiom that holds for any future
    [f] at any time [t]:
    {ul 
    {- If \[[f]\]{_t} [<> `Undet] then for all [t' > t] we have 
       \[[f]\]{_t'} = \[[f]\]{_t}}}
    The semantics of combinators are given in terms of \[\] by describing
    only the instants at which the resulting future is set. In the remaining 
    undescribed instants the state of the future is implicitly taken to be 
    unset, i.e. [`Undet]etermined. The descriptions also implicitly 
    quantify universally over the free variable [t]. 
   
    {2:neverdet Never determined futures} 

    [Fut] makes a distinction between a future that is undetermined
    and {e may} remain so forever and a future that is {e known} to be
    never determined (e.g. {!Fut.never}), a future with state
    [`Never]. Futures depending on a future value that [`Never]
    determines also never determine. This means that most combinator
    whenever they are given a future that never determines return a
    future that never determines.

    One way to get never determined futures is by using the future
    exclusive choice combinator {!first} between two futures. The
    first one that determines makes the other become never
    determined. Note that for future created with {!Fut.apply}, the
    application (and its effects) may (or not) still be executed, but
    it's value is discarded.

    {2:exceptions Exceptions}

    If the computation to determine a future's value raises an
    exception it is set to [`Never] determine and the exception is
    logged by the runtime system to the handler provided by
    {!Runtime.set_exn_trap} (default does nothing). An exception to
    this behaviour is the {!Never} exception which makes a future
    never determine without having the exception logged.

    
    It should be considered bad style to rely on the fact that
    exceptions are automatically trapped by the system. This mechanism
    is in place so that {e unexpected} exceptions do not kill the
    runtime system for long running programs. If you deal with a
    library that is designed around exceptions you should catch and
    handle them, if only to use {!Fut.never} if you wish so. For example
    if looking up a data structure does return [Not_found] don't do that:
{[
let find k m = Fut.ret (M.find k m)
]}
    This may significantly obfuscate what's happening. Explicitely handle
    the exception:
{[
let find k m = try Fut.ret (M.find k m) with Not_found -> Fut.never () 
]} 
in this example using option types may anway be clearer: 
{[
let find k m = Fut.ret (try Some (M.find k m) with Not_found -> None) 
]}

    TODO default trap logs to stderr.

    {2:futqueues Future queues}

    Future queues determine long-running or blocking function
    applications with a set of system worker threads. The FIFO
    application order of future queues also makes them a mutual
    exclusion synchronization primitive. 

    TODO, no [Fut] calls in applied function. 

    {2:promises Promises}

    Promises are used to expose the functionality of third-party
    libraries as futures. A call to {!promise} returns a promise that
    has an associated future (returned by {!future}). The promise
    can be {!set}, which sets the associated future. See
    {{!promiseex}this example} to see how promises can be used to
    expose callback based asynchronous interfaces as futures.

    Note that {!Fut.set} must be called on [Fut]'s thread. If one
    needs to set futures from another thread, a {!Runtime.action}
    can be used. See the next section.
    
    {2:futandthreads Fut and system threads}
    
    Except for applications determined on {{!queues}future queues},
    [Fut] has a single-threaded cooperative concurency model. This
    means that at any time there's only one future that is trying to
    determine its value on [Fut]'s thread.
    
    {b Thread-safety rule.} The thread-safety rule of [Fut] is simple.
    A single thread must run [Fut]'s runtime system via a call to
    {!await} and {b no other} thread is allowed to use {b any} function of
    [Fut] except, {!promise}, {!future}, {!state} and
    {!Runtime.action}.

    In particular the thread-safety rule {b does apply} to function
    applications performed with {!Fut.app} as they are performed on
    other threads.

    [Fut] tries to abstract away all your thread needs via the concept
    of {{!queues}future queues}.  Sometimes this is however infeasible
    for example because a library prevents you from sharing structures
    across threads and future queues provides you no support to
    control on which thread an application will happen. In that case
    you can still create your own system thread and interact with
    [Fut] by using {!promise} and a {!Runtime.action} to {!Fut.set} it
    once it determined. See {{!threadex}this example}.

    Before any call to {!apply} or {!Runtime.set_thread_count} the 
    program is single threaded.

    {2:futandprocesses Fut and system processes}

    In general [Unix.fork] does not cope well with threads. The best
    approach is thus to fork all the processes you need before the
    first call to {!await}. If you plan to use future queues TODO

*)

(** {1:examples Examples} 

    {2 The universal client} 
    {2 The echo server}
    {2:promiseex Integration with asynchronous functions}
    
    Suppose we have a function to perform asynchronous reads on a regular
    file whose signature is as follows:
{[
type read = [ `Error | `Read of int ]
val Aio.read : Unix.file_descr -> string -> int -> int -> (read -> unit) -> unit
]} 
    where [read fd s j l k] tries to read [l] bytes from [fd] to 
    store them in [s] starting at [j] calling [k] when it has 
    completed. 

    Exposing this function call as a future is very simple. Create
    a promise and set it in the callback [k]:
{[
val read : Unix.file_descr -> string -> int -> int -> read Fut.t
let read fd s j l = 
  let p = Fut.promise () in 
  let k r = Fut.set p (`Det r) in 
  Aio.read fd s j l k;
  Fut.future p
]}
    It is however important to make sure that the thread that
    calls the continuation [k] is [Fut]'s thread. If this is
    not the case the callback [k] can be replaced by:
{[
let k r = Fut.Runtime.action (fun () -> Fut.set p (`Det r))
]}
    {2:threadex Integration with threads}

    If for some reason you cannot use {{!queues}future queues} and
    need to run a service in its own thread, the code below shows how
    to interact with [Fut] and respect its {{!futandthreads}thread-safety}
    rule.
{[
module Service : sig
  type request 
  type response 
  val request : request -> response Fut.t
  val run : unit -> unit
end = struct
  type request 
  type response

  let m = Mutex.create ()
  let not_empty = Condition.create () 
  let requests = Queue.create ()

  let request req = (* executed by [Fut]'s thread. *)
    let abort' = ref false in 
    let abort () = abort' := true in
    let promise = Fut.promise ~abort () in
    Mutex.lock m; 
    Queue.add (req, promise, abort') requests;
    Condition.signal not_empty;
    Mutex.unlock m;
    Fut.future promise
      
  let rec serve () = (* executed by the service's thread. *)
    Mutex.lock m;
    while Queue.is_empty requests do Condition.wait not_empty m done;
    let (req, promise, abort) = Queue.pop requests in
    Mutex.unlock m; 
    let resp = do_service req abort in 
    let set () = Fut.set promise (`Det resp) in
    if not !abort then Fut.Runtime.action set;
    serve ()
    
    let run () = ignore (Thread.create serve ())
end
]}  

    {2:multip Integration with multiprocessing}
    
    You may want to use futures to delegate work to other processes.

    {2:signals Integration with signals} 

    {2:backend Other backends}

    For now [Fut] supports only a backend based on [select(2)]. New
    backends may be written and added with {!Runtime.set_backend}. 
    Note that changing backend while values are determined in 
    future queues is unsupported.
        
*)
    
(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
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
