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
    long running function applications with a set of concurrent
    workers and act as a mutual exclusion synchronization primitive.

    The separate {!Futu} library exposes {!Unix} system calls as
    futures. 

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

    {b Thread-safe.} This function can be called by other threads. *)

val await : ?timeout:float -> 'a t -> 'a state
(** [await timeout f], is like {!state} except if [f] is undetermined
    it waits until [f] is set or [timeout] seconds passed. If [timeout] 
    is unspecified, [await] blocks until [f] is set. 

    @raise Invalid_argument if [timeout] is negative. *)

val finally : ('a -> 'b) -> 'a -> 'c t -> 'c t
(** [finally fn v f] is [f] but [fn v] is called (and its result
    ignored) whenever [f] is set and immediately if [f] is already set
    at call time. If [fn v] raises an exception it is reported to the
    exception trap specified with {!Runtime.set_exn_trap}. *)

(** {2:applicative Applicative combinators} 

    These combinators have no effects on the determination
    of their arguments. *)

val never : unit -> 'a t
(** [never ()] is a future that never determines a value. 
    {ul {- \[[never ()]\]{_t} [= `Never]}} *)

val ret : 'a -> 'a t
(** [ret v] is a future that determines the value [v] immediately. 
    {ul {- \[[ret v]\]{_t} [= `Det v]}} *)

val recover : 'a t -> [ `Det of 'a | `Never ] t 
(** [recover f] is a future that determines [`Det v] if [f] determined
    [v] or [`Never] if [f] was set to never determine. 
    {ul
    {- \[[recover f]\]{_t} [= `Det (`Det v)] if \[[f]\]{_t} [= `Det v]}
    {- \[[recover f]\]{_t} [= `Det (`Never)] if \[[f]\]{_t} [= `Never]}} *)

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
    [`Never] and abort their determination.

    {b Important.} When a future is aborted, it is set to never 
    determine and all the future it may be waiting on (and
    recursively) are also aborted, except those that are protected
    by {!protect}. Aborting a future that is set has no effect since
    once the future is set it never changes again. 

    {b Warning.}  If the aborted future is an [`Undet]ermined future from 
    a future queue, there's no guarantee that the corresponding application
    will be/has not been executed when it is set to [`Never]. In any
    case the semantics of futures is preserved, it will switch from [`Undet]
    to [`Never] and remain so forever but depending on what the 
    function application does, the world may be affected. *)

val abort : 'a t -> unit
(** [abort f] aborts the future [f]. If [f] was [`Undet]ermined
    at application time, it is set to never determine and all the
    futures it is waiting on are also aborted.  
    {ul
    {- \[[f]\]{_t'} [= `Never] with t' > ta, if \[[f]\]{_ta} = [`Undet]
    where [ta] is [abort]'s application time.}}
    TODO semantics we need to define a waits(f) that is the set of of futures 
    [f] waits on. *)

val protect : 'a t -> 'a t 
(** [protect f] is a future that behaves like [f] but is insensitive 
    to {!abort}. It may of course still never determine because of its
    definition but its dependents are not able to abort it. [f] of 
    course remains abortable. *)

val pick : 'a t -> 'a t -> 'a t
(** [pick f f'] is a future that determines as the first future 
    that does and sets the other to never determine.
    If both futures are already determined the left one is taken. 
    If both futures never determine, the future never determines.
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

    TODO work out the semantics to see if it is equal to 
{[
  first f f' >>= fun (v, f) -> ignore (abort f); ret v
]}
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
    [abort] is eventually garbage collected. If you want to call 
    a function whenever the future of the promise is set, use 
    {!finally} on the future.
    
    {b Thread-safe.} This function can be called from other threads. *)

val future : 'a promise -> 'a t
(** [future p] is the future set by promise [p]. 

    {b Thread-safe.} This function can be called from other threads. *)

val set : 'a promise -> [`Det of 'a | `Never ] -> unit
(** [set p s] sets {!future} [p] to [s]. Does nothing if the 
    future of [p] is already set. 
    
    Not thread safe, if you need to set the promise from another 
    thread use a {!Runtime.action}. *) 

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

  val concurrent : queue
  (** [concurrent] is a special future queue. Applications added to it
      are all executed concurrently, the FIFO order is not guaranteed. *)

  val create : ?label:string -> unit -> queue
  (** [queue ~label ()] is a new queue with the given [label], if unspecified
      a label is automatically generated. *)

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

    {b Warning.} 
    {ul 
    {- [fn] is executed on another thread and as such it must 
       respect Fut's {{!futandthreads}thread safety rule}.}
    {- The future won't determine if {!await} is not 
       called regularly.}
    {- If your program is not multi-threaded yet, it will 
       become at that point.}} *)

exception Never
(** [Never] can be raised by a function application in a queue to set
    its corresponding future to never determine. *)

(** {1:timers Timers} 

    Timer futures determine values after a specific amount of time has 
    passed.

    {b Warning.} These futures don't determine if {!await} is not
    called regularly. *)

val delay : float -> float t
(** [delay d] is a value that determines [diff = d - d'] in [d] seconds where 
    [d'] is the actual time the runtime system took to determine the timer.
    If [diff] is [0.] the timer was on time, if positive the delay 
    was early and if negative it was late. 
    {ul
    {- \[[delay d]\]{_ta+d'} [= `Det (d-d')] where [ta] is 
       [delay]'s application time and [d'] is determined by 
       the runtime system (but should be as near as possible
       from [d]).}} *)

val tick : float -> unit t
(** [tick d] is {!ignore} [(]{!delay} [d)]. *)

(** {1 Error handling} *)

type ('a, 'b) result = [ `Ok of 'a | `Error of 'b ]
(** The type for results with errors. *)
 
val ebind : ('a, 'c) result t -> ('a -> ('b, 'c) result t) -> 
  ('b, 'c) result t

val ok : 'a -> ('a, 'b) result t
val error : 'b -> ('a, 'b) result t

(** {1 Infix operators} 

    Use of these operators may kill a few parentheses. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [f >>= fn] is [bind f fn]. *)

val (&>>=) : ('a, 'c) result t -> ('a -> ('b, 'c) result t) -> 
  ('b, 'c) result t
(** [f &>>= fn] is [ebind f fn]. *)

(** Infix operators. 

    In a module to open. *)
module Ops : sig

  (** {1 Binding operators} *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [f >>= fn] is [bind f fn]. *)

  val (&>>=) : ('a, 'c) result t -> ('a -> ('b, 'c) result t) -> 
    ('b, 'c) result t
  (** [f &>>= fn] is [ebind f fn]. *)
end

(** {1 Runtime system} *)

(** Runtime system configuration and interaction. *)
module Runtime : sig

  val name : string 
  (** [name] is the backend name. *) 

  (** {1 Exception trap} 

      See also the section about {{!exceptions}exceptions}. *)

  type exn_ctx = 
  [ `Queue of string | `Future | `Finalizer | `Backend | `Fd_action 
  | `Timer_action | `Signal_action | `Runtime_action | `Exn_trap ]
  (** The type for exception contexts. *)

  type exn_info = exn_ctx * exn * Printexc.raw_backtrace
  (** The type for info about trapped exceptions. The context, 
      the exception, and the backtrace. *)

  val set_exn_trap : (exn_info -> unit) -> unit
  (** [set_exn_trap h] is a function called whenever the runtime
      system traps an exception. *)

  val pp_exn_info : Format.formatter -> exn_info -> unit
  (** [pp_exn_info ppf i] prints an unspecified representation of [i]
      on [ppf]. *)
    
  (** {1 Actions} *)

  type abort = unit -> unit 
  (** The type for action's abort functions. Calling an abort function 
      associated to an [action] function has the following
      effects and must be guaranteed by the backends:
      {ul 
      {- If the [action] wasn't executed yet. It guarantees that 
         [action] will never be called and will be eventually gc'd.}
      {- If the [action] was already executed, it has no effects.}} *)

  (** {2 Runtime actions} *) 

  val action : (unit -> unit) -> unit
  (** [action a] executes [a ()] as soon as possible on the runtime system
      thread. Actions are executed in FIFO order as received. 

      {b Thread-safe.} This function can be called from other threads. *)

  (** {2 Signal actions} *)

  val signal_action : int -> (abort -> (int -> unit) * 'a) -> 'a
  (** [signal_action s def] calls [def] with an [abort] function 
      to get [(action,v)]. The function [action] is scheduled for 
      execution whenever the next signal [s] is received and will 
      be called once with [s] when that happens or never if [abort]
      was called before, see {!abort} for details. The value [v] is 
      simply returned by [signal_action]. *)

  (** {2 Timer actions} *)

  val timer_action : float -> (abort -> (float -> unit) * 'a) -> 'a
  (** [timer_action d def] calls [def] with an [abort] function to 
      get [(action,v)]. The function [action] is scheduled for execution 
      in [d] seconds and will be called once with the actualy delay that 
      was performed or never if [abort] was called before, see {!abort} 
      for details. The value [v] is simply returned by [timer_action]. *)

  (** {2 File descriptor actions and closing} *)
                                                
  val fd_action : [ `R | `W ] -> Unix.file_descr -> (bool -> unit) -> unit
  (** [fd_action fds fd a] executes [a true] whenever [fd] is in the
      state [fds]. If [fd] is closed [a false] executed. *)

  val fd_close : Unix.file_descr -> unit
  (** TODO *) 

  (** {1 Workers} 

      {b Note.} Most of the time a worker maps to a system thread, but
      the exact semantics is backend dependent. Workers are typically
      used to determine the future of futures queues.

      {b Threads.} If a backend uses threads it guarantees
      that the number of threads is [0] before any call is made 
      to {!Fut.apply} or {!set_worker_count}. This allows to 
      fork TODO we need more to be able to fork. *)
    
  val worker_count : unit -> int 
  (** [worker_count ()] is the number of workers used by the backend. *)

  val set_worker_count : int -> unit
  (** [set_worker_count n] sets the number of workers to [n]. 
      
      @raise Invalid_argument if [n] is negative.
      
      {b Note.} Clients don't need to call this function explicitely, 
      backends will automatically adjust workers once {!Fut.apply}
      is called. Also if the number of workers is set to [0], 
      a new call to {!Fut.apply} may change the worker count again.

      {b Warning.} In most backends calling this function explicitely 
      with [n > 0] makes the program multithreaded.. *)

end

(** {1:basics Basics} 
    
    A future is a place-holder for a value that may become determined
    at an arbitrary point in the future. Futures can be combined
    together to define new futures that will determine whenever the
    underlying futures do. For example, below [revolt] determines once
    [time_to_revolt] does.
{[
let time_to_revolt = Fut.tick 1.871 in
let revolt = 
  Fut.bind time_to_revolt 
    (fun () -> Fut.apply Printf.printf "Revolt!")

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
    
    The module {!Fut.Ops} defines a few infix operators to make the code more 
    readable. For example the {!Ops.(>>=)} operator can be used for
    {!bind}. This makes it easier to sequence binds:
{[
open Fut.Ops;;

let time_to_revolt d = Fut.tick d in
let revolt =
  time_to_revolt 1.871              >>= fun () ->
  Fut.apply Printf.printf "Revolt!" >>= fun () -> 
  time_to_revolt 0.72               >>= fun () -> 
  Fut.apply Printf.printf "Again!"

let () = ignore (Fut.await revolt)
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
    {!Runtime.set_exn_trap}. The default handler is backend dependent
    but usually logs the exception on [stderr].

    There is a single exception to this behaviour. If the exception {!Never} 
    is raised by an application given to {!Fut.apply}, the corresponding
    future never determines but the exception is not logged.

    Do not rely on the fact that exceptions are automatically trapped
    by the system, this mechanism is in place so that {e unexpected}
    exceptions do not kill the runtime system for long running
    programs. If you deal with a library that is designed around
    exceptions you should catch and handle them, if only to use
    {!Fut.never} if you wish so. For example if looking up a data
    structure may return [Not_found] don't do that:
{[
let find k m = Fut.ret (M.find k m)
]}
    This may significantly obfuscate what's happening (and won't be 
    efficient). Explicitely handle the exception:
{[
let find k m = try Fut.ret (M.find k m) with Not_found -> Fut.never () 
]} 
In this example, using option types may anway be clearer: 
{[
let find k m = Fut.ret (try Some (M.find k m) with Not_found -> None) 
]}

    {2:futqueues Future queues}

    Future queues determine long-running or blocking function
    applications with a set of concurrent workers usually implemented
    as system threads. 

    A future queue determines the applications submitted to it through
    the {!Fut.apply} function {e sequentially} and in FIFO order. This
    property also makes them a mutual exclusion synchronization
    primitive. Applications submitted in two different queues are
    determined concurrently. In the following examples [f1] and [f2]
    will be set sequentially, while [f3] will do so concurrently to
    those.
{[
let q1 = Fut.Queue.create ()
let q2 = Fut.Queue.create ()
let f1 = Fut.apply ~queue:q1 Printf.printf "first f1"
let f2 = Fut.apply ~queue:q1 Printf.printf "then f2" 
let f3 = Fut.apply ~queue:q2 Printf.printf "f3 at anytime"
]}
    Note that the function applications are executed on another 
    threads and as such must respect Fut's {{!futandthreads}thread safety 
    rule}.

    There is a special queue the {{!Queue.concurrent}concurrent} queue
    which is the default argument of {!apply}. This queue has no FIFO
    ordering constraint: anything submitted to it is determined
    concurrently. It is useful to just apply a long-running function 
    to avoid starving the runtime system:
{[
let long_work = Fut.apply long_running_function ()
]}
    It is also possible to provide a reference cell [abort] to {!apply}. 
    This reference cell will be set [true] by the runtime system 
    if the future is aborted by the runtime system. This can be used to 
    stop the application. For example:
{[
let long_running_work abort = 
  while (not !abort || !finished) do ... done 

let abort = ref false 
let long_work = Fut.apply ~abort long_running_work abort
let _ = Fut.abort long_work (* will set abort to [true] *)
]}
   Note that the runtime system never reads abort it just sets it to 
   [true] when appropriate.


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
    first call to {!await}.
*)

(** {1:examples Examples} 

    {2:backend Backends}

    TODO explain how to link against a backend. 

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
