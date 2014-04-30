(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf 

let err_promise_set = str "promise is already set" 
let err_invalid_timeout t = str "timeout value must be positive (%f)" t

let nop () = ()

(* A future is a mutable state of type ['a _state] (the interface does
   however only expose ['a state]). Once the state of a future is set
   to a value of type ['a set] it never changes again. *)

type 'a set =                              (* states when the future is set. *)
  [ `Never                                  (* future that never determines. *)
  | `Det of 'a ]                         (* future that determined with [v]. *)

type 'a state =                (* future state as exported in the interface. *)
  [ 'a set 
  | `Undet ]                                         (* undetermined future. *)

(* A waiter is a function associated to an undetermined future [f] to
   be called when [f] is set. The waiter is usually (but not only
   e.g. see [finally]) registered by another undetermined future that
   needs to wait on that result to determine itself.  A waiter is a
   mutable option to avoid space leaks if the waiter is no longer
   interested, for example undetermined futures sometimes set this
   reference to [None] when they are set through a call to the
   [stop_waiting_on] field of ['a undet]. *)

type 'a waiter = ('a set -> unit) option ref (* called when a future is set. *)

and 'a waiters =                       (* sets of waiters on a future value. *)
  | Wempty                                                     (* empty set. *)
  | Waiter of 'a waiter                                       (* waiter [w]. *)
  | Wunion of 'a waiters * 'a waiters        (* union of sets [ws] and [ws']. *)

(* A dependency is a future in which an undetermined future [f] registred
   a waiter. Dependencies are kept for walking up the dependency in 
   deep aborts and for allowing to unregister waiters [f] installed in 
   its dependencies (see [stop_waiting_on]). *) 

and deps =                           (* sets of futures a future depends on. *) 
  | Dempty : deps                                              (* empty set. *) 
  | Dep : 'a t * 'a waiter -> deps             (* dependency and its waiter. *) 
  | Dunion : deps * deps -> deps        (* union of sets [deps] and [deps']. *) 

and 'a undet =                                        (* undetermined state. *)
  { mutable ws : 'a waiters;                (* waiters on this undetermined. *)
    mutable ws_adds : int;     (* number of adds in [ws] since last cleanup. *)
    mutable deps : deps;                (* futures on which the undet waits. *) 
    mutable abort : unit -> unit; }(* custom action invoked once if aborted. *)

(* Future [`Det]ermines
   --------------------

   If a future [fut] is set to [`Det]ermine the following happens: 
   
   1) The state of [fut] is set to [`Det _]. 
   2) The [deps] of [fut] are walked over and the waiter [fut] registred
      in them is set to [None].
   3) The waiters [ws] of [fut] are executed with [fut]'s [`Det _].

   Note that after this [fut]'s undet value will be gc'd.

   See the [fut_det] function.

   Future [`Never] determines
   --------------------------

   If a future [fut] is set to [`Never] determine it is aborted. There are 
   two kind of aborts: shallow and deep aborts. 

   ## Shallow aborts (applicative combinators)
  
   A shallow abort occurs whenever a future [fut] is set to [`Never]
   determine because of the behaviour of the futures it is waiting on. 
   Shallow aborts are typically performed by applicative combinators when 
   their semantics sets them to never determine. In that case the following 
   happens:

   1) The state of [fut] is set to [`Never].
   2) The custom [abort] function of [fut] is called. 
   3) The [deps] of [fut] are walked over and the waiter [fut] registred
      in them is set to [None].
   4) The waiters [ws] of [fut] are executed with [`Never].

   See the [fut_shallow_abort] function.

   ## Deep aborts (Fut.abort or effectful combinators)

   A deep abort occurs whenever a future [fut] is set to [`Never]
   determine because [Fut.abort fut] is called or because [fut] is
   given to an effectuful combinator that calls [Fut.abort] on it. In
   that case the following happens:

   1) The state of [fut] is set to [`Never]
   2) The custom [abort] function of [fut] is called.
   3) The [deps] of [fut] are walked over and the waiter [fut] registred
      in them is set to [None].
   4) Each [deps] of [fut] is aborted.
   5) The waiters [ws] of [fut] are executed with [`Never].
   
   The reason in we first set all the waiters to [None] and only abort 
   their corresponding dependency later (effectively walking over the deps 
   twice) is if we remove the waiter and then directly call abort on the dep 
   one of our waiters could still be called e.g. if we have this dependency 
   graph:
  
       F0 <--- F1 <--- F2
       ^--------------/
   
   See the [fut_deep_abort] function.

   Waiter set compaction
   ---------------------

   As we have seen above whenever a future is set, either to determine
   or to never determine, it has the effect of setting waiters it
   registred to [None] in the waiter set [ws] of other undetermined
   futures. If nothing is done to remove these [None] waiters this can
   lead to space leaks. There are two mechanism to remove these none
   waiters:

   1. When a waiter is added to a set [ws] if it sees a top level
      [None] waiter it removes it. See the [wunion] function.  

   2. When a waiter is added to a set [ws] we increment [ws_adds]. If
      [ws_adds] exceeds [Runtime.waiter_cleanup_limit], all [None]
      waiters are removed from [ws]. See the [wcleanup] function.  *)

and 'a _state =                                   (* internal future state. *)
  [ 'a set
  | `Undet of 'a undet                                      (* undetermined. *)
  | `Alias of 'a t ]                             (* alias to the future [f]. *)

(* The `Alias state points to another future to define its state. This
   is needed to avoid space leaks in recursive definitions, see §5.5.2
   in Jérôme Vouillon, Lwt a cooperative thread library, ACM SIGPLAN
   Workshop on ML, 2008. *)

and 'a t = { mutable state : 'a _state }                        (* future. *)

(* Runtime system. *)

module Runtime = struct

  (* Waiter execution *) 

  module Waiters : sig
    val cleanup_limit : int 
    val exec : 'a waiters -> 'a set -> unit
  end = struct
    let cleanup_limit = 97

    (* To avoid blowing the stack on waiter execution, only the first
       call to [exec] executes them. If a waiter sets a future the
       execution of these subsequent waiters is put in [queue] and
       will be eventually poped by the call to exec that initiated the
       execution. *)

    type exec = Exec : 'a waiters * 'a set -> exec

    let queue : exec Queue.t = Queue.create ()    
    let executing = ref false
    let exec ws set =
      Queue.add (Exec (ws, set)) queue;
      if !executing then ((* only the initial exec executes *)) else 
      begin 
        executing := true;
        try 
          while true do
            let Exec (ws, set) = Queue.pop queue in 
            let rec loop set = function 
            | Wempty :: todo -> loop set todo 
            | Waiter { contents = None } :: todo -> loop set todo
            | Waiter ({ contents = Some wf }) :: todo -> 
                (* Execs resulting from this call go into [queue] *) 
                wf set; loop set todo 
            | Wunion (w, w') :: todo -> loop set (w :: w' :: todo)
            | [] -> ()
            in
            loop set [ws]
          done
        with Queue.Empty -> executing := false
      end
  end

  include Fut_backend_base
  include Fut_backend
                           
  let () = 
    Fut_backend.start (); 
    at_exit Fut_backend.stop

end 

(* Waiter sets *)

let wunion ws ws' = match ws, ws' with             (* unions [ws] and [ws']. *)
| Wempty, ws | ws, Wempty
| Waiter { contents = None }, ws | ws, Waiter { contents = None } -> ws
| ws, ws' -> Wunion (ws, ws')

let wcleanup u =                           (* remove None waiters in [u.ws]. *)
  let rec loop acc = function 
  | Wempty :: todo -> loop acc todo 
  | Waiter { contents = None } :: todo -> loop acc todo
  | Waiter { contents = Some _ } as w :: todo -> 
      if acc = Wempty then loop w todo else loop (Wunion (w, acc)) todo
  | Wunion (ws, ws') :: todo -> loop acc (ws :: ws' :: todo)
  | [] -> acc
  in
  if u.ws_adds > Runtime.Waiters.cleanup_limit 
  then (u.ws_adds <- 0; u.ws <- loop Wempty [u.ws])

let add_waiter u wf = (* adds waiter with function [wf] to undetermined [u]. *)
  let w = ref (Some wf) in
  u.ws <- wunion (Waiter w) u.ws;
  u.ws_adds <- u.ws_adds + 1;
  wcleanup u;
  w

(* Dependency sets *) 

let dunion ds ds' = match ds, ds' with             (* unions [ds] and [ds']. *) 
| Dempty, ds | ds, Dempty -> ds 
| ds, ds' -> Dunion (ds, ds')

let add_dep fut ~on:dep wf = (* [fut] deps on [dep] and waits with fun [wf]. *)
  match dep.state with 
  | `Undet depu ->
      let w = add_waiter depu wf in 
      begin match fut.state with 
      | `Undet futu -> futu.deps <- dunion futu.deps (Dep (dep, w)); 
      | _ -> assert false 
      end
  | _ -> assert false 

(* Abort actions *) 

let concat_aborts a a' =
  if a == nop then a' else 
  if a' == nop then a else 
  fun () -> a (); a' ()

(* Low-level future determination *) 

let src fut =            (* get [fut]'s src and compacts the alias chain. *)
  match fut.state with
  | `Alias src ->
      begin match src.state with 
      | `Alias src -> (* compact *) 
          let rec loop to_compact src = match src.state with
          | `Alias next -> loop (src :: to_compact) next
          | _ -> 
              let alias = `Alias src in
              List.iter (fun fut -> fut.state <- alias) to_compact; src
          in
          loop [fut] src
      | _ -> src 
      end
  | _ -> fut

let stop_waiting_on deps =
  let rec loop = function
  | Dempty :: todo -> loop todo
  | Dep (_, w) :: todo -> w := None; loop todo
  | Dunion (deps, deps') :: todo -> loop (deps :: deps' :: todo)
  | [] -> ()
  in
  loop [deps]

let fut_det fut (det : [ `Det of 'a ]) = 
  let fut = src fut in
  match fut.state with 
  | `Undet u ->
      fut.state <- (det :> 'a _state);
      stop_waiting_on u.deps;
      Runtime.Waiters.exec u.ws (det :> 'a set)
  | _ -> assert false
    
let fut_shallow_abort fut = 
  let fut = src fut in
  match fut.state with 
  | `Undet u ->
      fut.state <- `Never;
      u.abort ();
      stop_waiting_on u.deps;
      Runtime.Waiters.exec u.ws `Never
  | _ -> assert false 

type wnever = Wnever : 'a waiters -> wnever (* wait on `Never, hide 'a *) 

let rec abort_deps_and_exec_waiters depss notifs = match depss with 
| deps :: rest -> 
    let rec loop = function 
    | Dempty :: todo -> loop todo 
    | Dep (dep, _) :: todo ->
        let dep = src dep in 
        begin match (src dep).state with
        | `Undet u -> deep_abort dep (todo :: rest) notifs
        |  _ -> loop todo 
        end
    | Dunion (deps, deps') :: todo -> loop (deps :: deps' :: todo)
    | [] -> 
        begin match notifs with 
        | Wnever ws :: notifs -> 
            (* This won't stack overflow because if waiters also 
               deeply abort these aborts are put in the execution 
               queue so only the first time we pass through here 
               we get stuck and the stack is thus bounded. *) 
            Runtime.Waiters.exec ws `Never; 
            abort_deps_and_exec_waiters rest notifs
        | [] -> assert false
        end
    in
    loop deps
| [] -> () 
        
and deep_abort : 'a. 'a t -> deps list list -> wnever list -> unit =
  fun fut depss notifs ->
    let fut = src fut in
    match fut.state with 
    | `Undet u -> 
        fut.state <- `Never; 
        u.abort (); 
        stop_waiting_on u.deps; 
        abort_deps_and_exec_waiters 
          ([u.deps] :: depss) ((Wnever u.ws) :: notifs)
    | _ -> assert false 
    
and fut_deep_abort : 'a. 'a t -> unit = fun fut ->
  deep_abort fut [] [] 

let fut_det_trap fut fn v = try fut_det fut (`Det (fn v)) with 
| e -> 
    let bt = Printexc.get_raw_backtrace () in 
    Runtime.exn_trap `Future e bt; 
    fut_shallow_abort fut 

let det_trap fn v = try { state = `Det (fn v) }  with 
| e ->
    let bt = Printexc.get_raw_backtrace () in 
    Runtime.exn_trap `Future e bt; 
    { state = `Never } 

let undet_state abort = { ws = Wempty; ws_adds = 0; deps = Dempty; abort } 
let undet () = { state = `Undet (undet_state nop) }
let undet_abort abort = { state = `Undet (undet_state abort) }
let never () = { state = `Never } 

let alias internal ~src:fut =       
  (* aliases [internal] to undet [fut]. The client has no reference 
     to [internal] but may have one on [fut]. This allows to gc
     [internal] if it is not needed. *)
  let internal = src internal in
  let fut = src fut in 
  match fut.state with
  | `Undet futu -> 
      begin match internal.state with 
      | `Det _ as det -> fut_det fut det
      | `Never -> fut_shallow_abort fut
      | `Undet internalu ->
          (* [internal] is an alias for [fut]. The other way round would
             leak since potentially we a reference on [fut]. Aliasing
             the later to [internal] would mean we keep a ref on [internal]
             and prevent its garbage collection. *) 
          internal.state <- `Alias fut; 
          (* union waiter sets, no waiter on these two futures shall be 
             orphaned, *)
          futu.ws <- wunion futu.ws internalu.ws;            
          futu.ws_adds <- futu.ws_adds + internalu.ws_adds; 
          wcleanup futu;
          (* since [fut] shall behave like [internal] it gets its deps
             and waiters. Calls to fut_{det,shallow_abort,deep_abort} in 
             these waiters on [internal] will forward to [fut] through the 
             aliasing mechanism (see the [src] function) *) 
          futu.deps <- futu.deps;
          (* concatenate abort actions, no abort action shall be orphaned. *)
          futu.abort <- concat_aborts futu.abort internalu.abort
      | _ -> assert false 
      end
  | _ -> assert false 

(* Futures *)
    
let state fut = match (src fut).state with
| `Det _ | `Never as v -> v
| `Undet _ -> `Undet
| `Alias _ -> assert false
    
let await ?(timeout = max_float) fut = 
  if timeout < 0. then invalid_arg (err_invalid_timeout timeout) else
  match (src fut).state with
  | `Det _ | `Never as v -> v
  | `Undet _ ->
      let rec loop timeout fut =
        let elapsed = Runtime.step timeout in
        match (src fut).state with 
        | `Det _ | `Never as v -> v
        | `Undet _ -> 
            if timeout = max_float then loop max_float fut else
            let rem = timeout -. elapsed in 
            if rem > 0. then loop rem fut else `Undet
        | `Alias _ -> assert false
      in
      loop timeout fut
  | `Alias _ -> assert false

let finally fn v fut = 
  let trap_finally fn v = try ignore (fn v) with
  | e ->
      let bt = Printexc.get_raw_backtrace () in 
      Runtime.exn_trap `Finalizer e bt
  in
  let fut = src fut in
  match fut.state with
  | `Det _ | `Never -> trap_finally fn v; fut
  | `Undet u -> ignore (add_waiter u (fun _ -> trap_finally fn v)); fut
  | `Alias _ -> assert false

(* Applicative combinators *)

let ret v = { state = `Det v }

let recover fut = 
  let fut = src fut in 
  match fut.state with 
  | `Never | `Det _ as set -> { state = `Det set } 
  | `Undet _ -> 
      let fnew = undet () in 
      let waiter = function 
      | `Never | `Det _ as set -> fut_det fnew (`Det set)
      in
      add_dep fnew ~on:fut waiter; 
      fnew 
  | `Alias _ -> assert false

let rec bind fut fn = 
  let fut = src fut in
  match fut.state with
  | `Never -> never ()
  | `Det v -> fn v       (* do not [trap_fut fn v] it breaks tail-recursion. *)
  | `Undet _ ->
      let fnew = undet () in
      let waiter = function
      | `Never -> fut_shallow_abort fnew
      | `Det v -> 
          let fut = try fn v with 
          | e -> 
              let bt = Printexc.get_raw_backtrace () in 
              Runtime.exn_trap `Future e bt; 
              never () 
          in
          alias fut ~src:fnew
      in
      add_dep fnew ~on:fut waiter;
      fnew
  | `Alias _ -> assert false
    
let app ff fv = 
  let ff = src ff in
  match ff.state with
  | `Never -> never ()
  | `Det fn -> 
      let fv = src fv in
      begin match fv.state with 
      | `Never -> never ()
      | `Det v -> det_trap fn v
      | `Undet _ ->
          let fnew = undet () in 
          let waiter = function 
          | `Never -> fut_shallow_abort fnew
          | `Det v -> fut_det_trap fnew fn v 
          in
          add_dep fnew ~on:fv waiter;
          fnew
      | `Alias _ -> assert false
      end
  | `Undet _ -> 
      let fv = src fv in
      begin match fv.state with
      | `Never -> never ()
      | `Det v ->
          let fnew = undet () in 
          let waiter = function 
          | `Never -> fut_shallow_abort fnew
          | `Det fn -> fut_det_trap fnew fn v
          in
          add_dep fnew ~on:ff waiter;
          fnew
      | `Undet _ ->
          let fnew = undet () in
          let waiter_f fv = function
          | `Never -> fut_shallow_abort fnew
          | `Det f ->
              match (src fv).state with 
              | `Undet _ -> () 
              | `Det v -> fut_det_trap fnew f v
              | `Never       (* the other waiter will have removed this one *) 
              | `Alias _ -> assert false 
          in 
          let waiter_v ff = function 
          | `Never -> fut_shallow_abort fnew
          | `Det v -> 
              match (src ff).state with 
              | `Undet _ -> () 
              | `Det f -> fut_det_trap fnew f v 
              | `Never       (* the other waiter will have removed this one *) 
              | `Alias _ -> assert false 
          in
          add_dep fnew ff (waiter_f fv);
          add_dep fnew fv (waiter_v ff); 
          fnew
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false
    
let map fn fut = 
  let fut = src fut in 
  match fut.state with
  | `Never -> never ()
  | `Det v -> det_trap fn v
  | `Undet _ ->
      let fnew = undet () in 
      let waiter = function
      | `Never -> fut_shallow_abort fnew
      | `Det v -> fut_det_trap fnew fn v
      in
      add_dep fnew ~on:fut waiter; 
      fnew
  | `Alias _ -> assert false
    
let ignore fut = 
  let fut = src fut in
  match fut.state with 
  | `Never -> never ()
  | `Det v -> { state = `Det () } 
  | `Undet _ ->
      let fnew = undet () in 
      let waiter = function 
      | `Never -> fut_shallow_abort fnew
      | `Det _ -> fut_det fnew (`Det ())
      in
      add_dep fnew ~on:fut waiter; 
      fnew
  | `Alias _ -> assert false 
    
let fold fn acc futs = 
  let fnew = undet () in
  let fold () =                    (* when called [futs] are all determined. *)
    let fn' acc fut = match (src fut).state with 
    | `Det v -> fn acc v | _ -> assert false 
    in
    fut_det_trap fnew (List.fold_left fn' acc) futs
  in
  let undet = ref 0 in                        (* remaining undets in [futs]. *) 
  let waiter = function
  | `Never -> fut_shallow_abort fnew
  | `Det _ -> decr undet; if !undet = 0 then fold ()
  in
  let rec add_deps = function 
  | dep :: deps -> 
      let dep = src dep in
      begin match dep.state with
      | `Det _ -> add_deps deps
      | `Never -> fut_shallow_abort fnew
      | `Undet _ ->
          incr undet;
          add_dep fnew ~on:dep waiter; 
          add_deps deps
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fold ()
  in
  add_deps futs;
  fnew
    
let sustain fut fut' =
  let waiter_fut' fnew = function 
  | `Never -> fut_shallow_abort fnew 
  | `Det _ as det -> fut_det fnew det
  in
  let fut = src fut in
  match fut.state with 
  | `Det _  -> fut
  | `Never -> 
      let fut' = src fut' in 
      begin match fut'.state with 
      | `Det _ | `Never -> fut'
      | `Undet _ ->
          let fnew = undet () in
          add_dep fnew ~on:fut' (waiter_fut' fnew);
          fnew
      | `Alias _ -> assert false
      end
  | `Undet _ -> 
      let fnew = undet () in
      let waiter_fut = function 
      | `Det _ as det -> fut_det fnew det
      | `Never ->
          let fut' = src fut' in 
          begin match fut'.state with 
          | `Det _ as det -> fut_det fnew det
          | `Never -> fut_shallow_abort fnew
          | `Undet _ -> add_dep fnew ~on:fut' (waiter_fut' fnew)
          | `Alias _ -> assert false
          end
      in
      add_dep fnew ~on:fut waiter_fut;
      fnew
  | `Alias _ -> assert false
    
let first fut fut' =
  let fut = src fut in
  match fut.state with
  | `Det v -> { state = `Det (v, fut') }
  | `Never -> 
      let fut' = src fut' in
      begin match fut'.state with
      | `Det v -> { state = `Det (v, fut) }
      | `Never -> never ()
      | `Undet _ -> 
          let fnew = undet () in
          let waiter = function 
          | `Never -> fut_shallow_abort fnew 
          | `Det v -> fut_det fnew (`Det (v, fut)) 
          in
          add_dep fnew ~on:fut' waiter; 
          fnew
      | `Alias _ -> assert false
      end
  | `Undet _ ->
      let fut' = src fut' in
      begin match fut'.state with
      | `Det v -> { state = `Det (v, fut) }
      | `Never -> 
          let fnew = undet () in 
          let waiter = function 
          | `Never -> fut_shallow_abort fnew 
          | `Det v -> fut_det fnew (`Det (v, fut')) 
          in
          add_dep fnew ~on:fut waiter; 
          fnew
      | `Undet _ -> 
          let fnew = undet () in
          let waiter other = function
          | `Det v -> fut_det fnew (`Det (v, other))
          | `Never ->
              begin match (src other).state with 
              | `Never -> 
                  (* N.B. if fut = fut' this is not problematic as the 
                     first waiter will call fut_shallow_abort which will
                     stop the other waiter. So fut_shallow_abort is really 
                     only called once. *) 
                  fut_shallow_abort fnew
              | `Undet _ -> ()
              | `Det _ (* If the other future determined its waiter called 
                          [fut_det] which will have the effect of stopping 
                          this waiter, so this case is impossible. *)
              | `Alias _ ->
                  assert false 
              end
          in
          add_dep fnew ~on:fut (waiter fut'); 
          add_dep fnew ~on:fut' (waiter fut);
          fnew
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false
    
let firstl futs =
  let rec remove to_rem acc = function        (* removes [to_rem] from list. *)
  | fut :: futs ->
      if fut == to_rem then List.rev_append acc futs else
      remove to_rem (fut :: acc) futs
  | [] -> assert false 
  in
  let fnew = undet () in 
  let undet = ref 0 in                        (* remaining undets in [futs]. *)
  let waiter fut = function
  | `Never -> decr undet; if !undet = 0 then fut_shallow_abort fnew
  | `Det v -> decr undet; fut_det fnew (`Det (v, remove fut [] futs))
  in
  let rec add_deps = function 
  | dep :: deps ->
      let dep = src dep in
      begin match dep.state with 
      | `Det v -> fut_det fnew (`Det (v, remove dep [] futs))
      | `Never -> add_deps deps (* TODO what's that bizness ? *)
      | `Undet u ->
          incr undet; 
          add_dep fnew ~on:dep (waiter dep);
          add_deps deps
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fut_shallow_abort fnew
  in
  add_deps futs;
  fnew

(* Effectful combinators *)

let abort fut =
  let fut = src fut in
  match fut.state with
  | `Det _ | `Never -> ()
  | `Undet _ -> fut_deep_abort fut
  | `Alias _ -> assert false 

let protect fut = 
  let fut = src fut in
  match fut.state with 
  | `Det _ | `Never -> fut
  | `Undet u -> 
      let fnew = undet () in 
      (* We register a waiter in [fut]'s undet but make a fake dependency so 
         that a deep abort on [fnew] will not touch [fut]. *) 
      let waiter = function 
      | `Never -> fut_shallow_abort fnew 
      | `Det _ as det -> fut_det fnew det 
      in
      let w = add_waiter u waiter in
      let fake_dep = Dep ({ state = `Never }, w) in
      begin match fnew.state with 
      | `Undet u' -> u'.deps <- fake_dep 
      | _ -> assert false
      end; 
      fnew
  | `Alias _ -> assert false

let pick fut fut' =
  let fut = src fut in
  match fut.state with
  | `Never -> fut'
  | `Det _ ->
      let fut' = src fut' in 
      begin match fut'.state with 
      | `Never | `Det _ -> fut
      | `Undet _ -> fut_deep_abort fut'; fut
      | `Alias _ -> assert false
      end
  | `Undet _ ->
      let fut' = src fut' in 
      begin match fut'.state with
      | `Never -> fut
      | `Det _ -> fut_deep_abort fut; fut'
      | `Undet _ ->  
          let fnew = undet () in
          let waiter other = function
          | `Never ->
              let other = src other in
              begin match other.state with 
              | `Never -> fut_shallow_abort fnew
              | `Undet _ -> ()
              | `Det _  (* The other waiter would have removed this waiter. *)
              | `Alias _ -> assert false
              end
          | `Det _ as det ->
              let other = src other in
              begin match other.state with
              | `Never -> fut_det fnew det
              | `Undet _ ->
                  (* The order is important here, determining fnew first
                     will remove the waiters it has in [other]. *) 
                  fut_det fnew det;
                  fut_deep_abort other
              | `Det _ (* The other waiter would have removed this waiter. *)
              | `Alias _ -> assert false 
              end
          in  
          add_dep fnew ~on:fut (waiter fut'); 
          add_dep fnew ~on:fut' (waiter fut); 
          fnew 
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false 

let pickl futs = (* FIXME implement directly *)
  List.fold_left pick (never ()) futs 

(* Promises *)

type 'a promise = 'a t                        (* The promise is the future ! *)

let promise ?(abort = nop) () = undet_abort abort
let future promise = promise
let set promise set =
  let promise = src promise in
  match promise.state with
  | `Undet _ -> 
      begin match set with 
      | `Never -> fut_shallow_abort promise 
      | `Det _ as det -> fut_det promise det 
      end
  | `Det _ -> () | `Never -> ()
  | `Alias _ -> assert false

(* Future queues *)

module Queue = Runtime.Queue
type queue = Queue.t
exception Never
  
let apply ?(queue = Queue.concurrent) ?abort f v =
  let abort = match abort with 
  | None -> None 
  | Some a -> Some (fun () -> a := true) (* set the user reference cell *) 
  in
  let p = promise ?abort () in
  let work () = 
    try 
      let r = f v in 
      Runtime.action (fun () -> set p (`Det r)) 
    with 
    | Never -> Runtime.action (fun () -> set p `Never)
    | exn ->
        let bt = Printexc.get_raw_backtrace () in 
        let a () = 
          Runtime.exn_trap (`Queue (Queue.label queue)) exn bt; 
          set p `Never 
        in
        Runtime.action a        
  in
  Queue.add_work queue work;
  future p

(* Timers *)

let delay d =
  let def abort =
    let fnew = undet_abort abort (* abort the timer action on fut abort *) in 
    (fun diff -> fut_det fnew (`Det diff)), fnew
  in
  Runtime.timer_action d def 

let tick d = 
  let def abort =
    let fnew = undet_abort abort (* abort the timer action on fut abort *) in
    (fun _ -> fut_det fnew (`Det ())), fnew
  in
  Runtime.timer_action d def

(* Error handling *) 

type ('a, 'b) result = [ `Ok of 'a | `Error of 'b ]
type ('a, 'b) status = ('a, 'b) result t

let ebind f fn =
  bind f @@ function
  | `Error _  as e -> ret e 
  | `Ok v -> fn v

let ok v = { state = `Det (`Ok v) }
let error e = { state = `Det (`Error e) }

let ( >>= ) = bind           (* we have it here aswell for Fut.() notation. *) 
let ( &>>= ) = ebind
 
module Ops = struct
  let ( >>= ) = bind
  let ( &>>= ) = ebind
end

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
