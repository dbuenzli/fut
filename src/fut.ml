(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf 

let err_promise_set = str "promise is already set" 
let err_invalid_worker_count c = str "worker count must be positive (%d)" c
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
   be called when the future [f] is set. The waiter is usually (but
   not only e.g. see [finally]) registered by another undetermined
   future that needs to wait on that result to determine itself.  A
   waiter is a mutable option to avoid space leaks if the waiter is no
   longer interested, for example undetermined futures sometimes set this 
   reference to [None] when they are set through a call to the [stop_wait] 
   field of ['a undet]. *)

type 'a waiter = ('a set -> unit) option ref (* called when a future is set. *)

and 'a waiters =                       (* sets of waiters on a future value. *)
  | Wempty                                                     (* empty set. *)
  | Waiter of 'a waiter                                       (* waiter [w]. *)
  | Wunion of 'a waiters * 'a waiters        (* union of sets [ws] and [ws']. *)

and deep_aborts =                (* sets of futures to abort on deep aborts. *) 
  | Dempty : deep_aborts
  | Dabort : 'a t -> deep_aborts 
  | Dunion : deep_aborts * deep_aborts -> deep_aborts

and 'a undet =                                        (* undetermined state. *)
  { mutable ws : 'a waiters;                (* waiters on this undetermined. *)
    mutable ws_adds : int;     (* number of adds in [ws] since last cleanup. *)
    mutable stop_wait : unit -> unit;    (* stops the wait on other futures. *)
    mutable deep_aborts : deep_aborts;   (* futures to abort in deep aborts. *)
    mutable abort : unit -> unit; }(* custom action invoked once if aborted. *)

(* If a future [fut] is set to [`Det]ermine the following happens: 
   
   1) The state of [fut] is set to [`Det _]. 
   2) The [stop_wait] function of [fut] is called. This sets all 
      the waiters [fut] registred in other futures to [None]. 
   3) The waiters [ws] of [fut] are notified that [fut] is set to  [`Det _].
   
   See [fut_det]. 
   
   If a future [fut] is set to [`Never] determine it needs to be aborted. 
   There are two kind of aborts: shallow and deep aborts. 

   Shallow aborts (applicative combinators)
   ----------------------------------------
  
   A shallow abort occurs whenever a future [fut] is set to [`Never]
   determine because of the behaviour of the futures it is waiting on. 
   Shallow aborts are typically performed by applicative combinators when 
   they are set to never determine. In that case the following happens:

   1) The state of [fut] is set to [`Never].
   2) The [stop_wait] function of [fut] is called. This sets all the 
      waiters [fut] registred in other futures to [None].
   3) The custom [abort] function of [fut] is called.
   4) The waiters [ws] of [fut] are notified that [fut] is set to [`Never].

   See [fut_shallow_abort]. 

   Deep aborts (Fut.abort or effectful combinators)
   ------------------------------------------------

   A deep abort occurs whenever a future [fut] is set to [`Never]
   determine because [Fut.abort fut] is called. This can occur 
   because the client manually uses this function or because an 
   effectful combinator is used that invokes [Fut.abort] on one
   of its arguments. In that case the following happens: 

   1-3) Is like the corresponding steps in shallow aborts.
   4) The futures in [deep_aborts] of [fut] get a call to [Fut.abort]
   5) The waiters [ws] of [fut] are notified that [fut] is set to [`Never].

   See [fut_deep_abort].

   Waiter set compaction
   ---------------------

   Whenever a future is set, either to determine or to never
   determine, it has the effect of setting waiters it registred to
   [None] in the waiter set [ws] of other undetermined futures (the
   step 2 in all algorithms above). If nothing is done to remove these 
   [None] waiters this can lead to space leaks. There are two mechanism 
   to remove these none waiters:

   a) When a waiter is added to a set [ws] if it sees a top level [None] 
      waiter it removes it, see the [union] function. 
   b) When a waiter is added to a set [ws] we increment [ws_adds]. If 
      [ws_adds] exceeds [Runtime.cleanup_limit], all [None] waiters
      are removed from [ws], see the [cleanup] function. 

   Deep aborts compaction 
   ----------------------

   TODO do we get a strategy here ? We could leak. 

*)

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

  include Fut_backend_base 
  include Fut_backend     

  let cleanup_limit = 97
  let worker_count = Queue.worker_count
  let set_worker_count count = 
    if count < 0 then invalid_arg (err_invalid_worker_count count) else
    Queue.set_worker_count count
                           
  let () = Fut_backend.start (); at_exit Fut_backend.stop
end 

(* Deep aborts *) 

let dunion ds ds' = match ds, ds' with              (* unions [ds] and [ds'] *) 
| Dempty, ds | ds, Dempty -> ds 
| ds, ds' -> Dunion (ds, ds')

(* Waiter sets *)

let cleanup ws =                             (* remove None waiters in [ws]. *)
  let rec loop acc = function 
  | Wempty :: todo -> loop acc todo 
  | Waiter { contents = None } :: todo -> loop acc todo
  | Waiter { contents = Some _ } as w :: todo -> 
      if acc = Wempty then loop w todo else loop (Wunion (w, acc)) todo
  | Wunion (ws, ws') :: todo -> loop acc (ws :: ws' :: todo)
  | [] -> acc
  in
  loop Wempty [ws]

let wunion ws ws' = match ws, ws' with             (* unions [ws] and [ws']. *)
| Wempty, ws | ws, Wempty
| Waiter { contents = None }, ws | ws, Waiter { contents = None } -> ws
| ws, ws' -> Wunion (ws, ws')

let notify_waiters ws s =                   (* notifies [ws] with state [s]. *)
  let rec loop s = function 
  | Wempty :: todo -> loop s todo 
  | Waiter { contents = None } :: todo -> loop s todo
  | Waiter { contents = Some w } :: todo -> w s; loop s todo
  | Wunion (w, w') :: todo -> loop s (w :: w' :: todo)
  | [] -> ()
  in
  loop s [ws]

let add_waiter u wf = (* adds waiter with function [wf] to undetermined [u]. *)
  let w = ref (Some wf) in
  u.ws <- wunion (Waiter w) u.ws;
  u.ws_adds <- u.ws_adds + 1; 
  if u.ws_adds > Runtime.cleanup_limit 
  then (u.ws_adds <- 0; u.ws <- cleanup u.ws);
  w

let waits fut wf ~on:dep = (* [f] waits with function [wf] on 
                              undetermined [dep]. *)
  begin match dep.state with 
  | `Undet u -> 
      let w = add_waiter u wf in
      begin match fut.state with 
      | `Undet u ->
          u.stop_wait <- fun () -> w := None; 
          u.deep_aborts <- dunion u.deep_aborts (Dabort dep);
      | _ -> assert false
      end
  | _ -> assert false
  end


(* Abort actions *) 

let concat_aborts a a' =
  if a == nop then a' else 
  if a' == nop then a else 
  fun () -> a (); a' ()

(* Futures *)

let src f = match f.state with (* get [f]'s src and compacts the alias chain. *)
| `Alias src ->
    begin match src.state with 
    | `Alias src -> (* compact *) 
        let rec loop to_compact src = match src.state with
        | `Alias next -> loop (src :: to_compact) next
        | _ -> 
            let alias = `Alias src in
            List.iter (fun f -> f.state <- alias) to_compact; src
        in
        loop [f] src
    | _ -> src 
    end
| _ -> f

let state fut = match (src fut).state with
| `Det _ | `Never as v -> v
| `Undet _ -> `Undet
| `Alias _ -> assert false
    
let await ?(timeout = max_float) f = 
  if timeout < 0. then invalid_arg (err_invalid_timeout timeout) else
  match (src f).state with
  | `Det _ | `Never as v -> v
  | `Undet _ -> 
      let rec loop timeout f =
        let elapsed = Runtime.step timeout in
        match (src f).state with 
        | `Det _ | `Never as v -> v
        | `Undet _ -> 
            if timeout = max_float then loop max_float f else
            let rem = timeout -. elapsed in 
            if rem > 0. then loop rem f else `Undet
        | `Alias _ -> assert false
      in
      loop timeout f
  | `Alias _ -> assert false

let finally fn v f = 
  let trap_finally fn v = try ignore (fn v) with
  | e -> 
      let bt = Printexc.get_backtrace () in 
      Runtime.exn_trap `Finalizer e bt
  in
  let f = src f in
  match f.state with 
  | `Det _ | `Never -> trap_finally fn v; f
  | `Undet u -> ignore (add_waiter u (fun _ -> trap_finally fn v)); f
  | `Alias _ -> assert false

let set_stop_wait fut fn = match fut.state with 
| `Undet u -> u.stop_wait <- fn 
| _ -> assert false

let add_deep_abort fut dep = match fut.state with 
| `Undet u -> u.deep_aborts <- dunion u.deep_aborts (Dabort dep) 
| _ -> assert false 

let set_abort fut fn = match fut.state with
| `Undet u -> u.abort <- fn 
| _ -> assert false

let fut_det fut (det : [> `Det of 'a ]) = match fut.state with 
| `Undet u ->
    fut.state <- (det :> 'a _state);
    u.stop_wait ();
    notify_waiters u.ws (det :> 'a set)
| _ -> assert false

let fut_shallow_abort fut = match fut.state with 
| `Undet u -> 
    fut.state <- `Never; 
    u.stop_wait (); 
    u.abort (); 
    notify_waiters u.ws `Never
| _ -> assert false 

let rec abort_deep_aborts ds =                     (* TODO this is not T.R. *)
  let rec loop = function
  | Dempty :: todo -> loop todo
  | Dabort fut :: todo -> 
      begin match (src fut).state with 
      | `Undet u -> fut_deep_abort fut; loop todo
      | _ -> loop todo
      end
  | Dunion (ds, ds') :: todo -> loop (ds :: ds' :: todo)
  | [] -> ()
  in
  loop [ds]

and fut_deep_abort : 'a. 'a t -> unit = fun fut -> match fut.state with 
| `Undet u -> 
    fut.state <- `Never; 
    u.stop_wait ();
    u.abort ();
    abort_deep_aborts u.deep_aborts;
    notify_waiters u.ws `Never
| _ -> assert false 

let undet_state abort = 
  { ws = Wempty; ws_adds = 0; stop_wait = nop; deep_aborts = Dempty; abort } 

let undet () = { state = `Undet (undet_state nop) }
let undet_abort abort = { state = `Undet (undet_state abort) }
let never () = { state = `Never } 

let trap_fut fn v = try fn v with
| e -> 
    let bt = Printexc.get_backtrace () in 
    Runtime.exn_trap `Future e bt;
    never ()

let trap_det fn v = try `Det (fn v) with
| e ->
    let bt = Printexc.get_backtrace () in
    Runtime.exn_trap `Future e bt; 
    `Never

let alias fut ~src:res =                    (* aliases [fut] to undet [res]. *)
  let fut = src fut in
  let res = src res in 
  match res.state with 
  | `Undet u -> 
      begin match fut.state with 
      | `Det _ as det -> fut_det res det
      | `Never -> fut_shallow_abort res
      | `Undet u' ->
          fut.state <- `Alias res;
          (* union waiter sets, cleanup if needed. *)
          u.ws <- wunion u.ws u'.ws;            
          u.ws_adds <- u.ws_adds + u'.ws_adds; 
          if u.ws_adds > Runtime.cleanup_limit 
          then (u.ws_adds <- 0; u.ws <- cleanup u.ws);
          (* union deep aborts and concatenate abort actions *)
          u.deep_aborts <- dunion u.deep_aborts u'.deep_aborts;
          u.abort <- concat_aborts u.abort u'.abort
      | _ -> assert false 
      end
  | _ -> assert false 

(* Applicative combinators *)

let ret v = { state = `Det v }
let rec bind fut fn = 
  let fut = src fut in
  match fut.state with
  | `Never -> never ()
  | `Det v -> fn v       (* do not [trap_fut fn v] it breaks tail-recursion. *)
  | `Undet _ ->
      let fnew = undet () in
      let waiter = function
      | `Never -> fut_shallow_abort fnew
      | `Det v -> alias (trap_fut fn v) ~src:fnew
      in
      waits fnew waiter ~on:fut; 
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
      | `Det v -> { state = (trap_det fn v) }
      | `Undet _ ->
          let fnew = undet () in 
          let waiter = function 
          | `Never -> fut_shallow_abort fnew
          | `Det v -> fut_det fnew (trap_det fn v)
          in
          waits fnew waiter ~on:fv; 
          fnew
      | `Alias _ -> assert false
      end
  | `Undet uf -> 
      let fv = src fv in
      begin match fv.state with
      | `Never -> never () 
      | `Det v ->
          let fnew = undet () in 
          let waiter = function 
          | `Never -> fut_shallow_abort fnew
          | `Det fn -> fut_det fnew (trap_det fn v)
          in
          waits fnew waiter ~on:ff; 
          fnew
      | `Undet uv ->
          let fnew = undet () in
          let waiter_f fv = function 
          | `Never -> fut_shallow_abort fnew
          | `Det f ->
              match (src fv).state with 
              | `Undet _ -> () 
              | `Det v -> fut_det fnew (trap_det f v)
              | `Alias _ | `Never -> assert false 
          in 
          let waiter_v ff = function 
          | `Never -> fut_shallow_abort fnew
          | `Det v -> 
              match (src ff).state with 
              | `Undet _ -> () 
              | `Det f -> fut_det fnew (trap_det f v) 
              | `Alias _ | `Never -> assert false 
          in
(* TODO abstract like waits *)
          let wf = add_waiter uf (waiter_f fv) in 
          let wv = add_waiter uv (waiter_v ff) in 
          set_stop_wait fnew (fun () -> wf := None; wv := None);
          add_deep_abort fnew fv; 
          add_deep_abort fnew ff;
          fnew
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false
    
let map fn fut = 
  let fut = src fut in 
  match fut.state with
  | `Never -> never ()
  | `Det v -> { state = (trap_det fn v) }
  | `Undet _ ->
      let fnew = undet () in 
      let waiter = function
      | `Never -> fut_shallow_abort fnew
      | `Det v -> fut_det fnew (trap_det fn v)
      in
      waits fnew waiter ~on:fut; fnew
  | `Alias _ -> assert false
    
let ignore fut = 
  let fut = src fut in
  match fut.state with 
  | `Never -> never () 
  | `Det v -> { state = `Det () } 
  | `Undet u ->
      let fnew = undet () in 
      let waiter = function 
      | `Never -> fut_shallow_abort fnew
      | `Det _ -> fut_det fnew (`Det ())
      in
      waits fnew waiter ~on:fut; fnew
  | `Alias _ -> assert false 
    
let fold fn acc futs = 
  let fnew = undet () in
  let fold () =                      (* when called [fs] are all determined. *)
    let fn' acc fut = match (src fut).state with 
    | `Det v -> fn acc v | _ -> assert false 
    in
    fut_det fnew (trap_det (List.fold_left fn' acc) futs)
  in
  let undet = ref 0 in                        (* remaining undets in [futs]. *) 
  let waits = ref [] in                                (* registred waiters. *)
  let waiter = function
  | `Never -> fut_shallow_abort fnew
  | `Det _ -> decr undet; if !undet = 0 then fold ()
  in
  let rec add_waits = function 
  | fut :: futs' -> 
      let fut = src fut in
      begin match fut.state with
      | `Det _ -> add_waits futs'
      | `Never -> fut_shallow_abort fnew
      | `Undet u ->
          let w = add_waiter u waiter in 
          add_deep_abort fnew fut;
          waits := w :: !waits; incr undet; add_waits futs'
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fold ()
  in
  set_stop_wait fnew (fun () -> List.iter (fun w -> w := None) !waits);
  add_waits futs;
  fnew

let barrier ?(set = false) futs =
  let fnew = undet () in 
  let undet = ref 0 in                        (* remaining undets in [futs]. *)
  let waits = ref [] in                                (* registred waiters. *)
  let waiter = function 
  | `Never when not set -> fut_shallow_abort fnew
  | `Det _ | `Never -> decr undet; if !undet = 0 then fut_det fnew (`Det ())
  in
  let rec add_waits = function 
  | fut :: futs' -> 
      let fut = src fut in
      begin match fut.state with 
      | `Det _ -> add_waits futs'
      | `Never -> 
          if set then add_waits futs' else fut_shallow_abort fnew
      | `Undet u -> 
          let w = add_waiter u waiter in 
          add_deep_abort fnew fut;
          waits := w :: !waits; incr undet; add_waits futs'
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fut_det fnew (`Det ())
  in
  set_stop_wait fnew (fun () -> List.iter (fun w -> w := None) !waits);
  add_waits futs; 
  fnew
    
let sustain fut fut' =
  let fnew = undet () in
  let use_fut' () = 
    let fut' = src fut' in
    match fut'.state with
    | `Det _ as det -> fut_det fnew det 
    | `Never -> fut_shallow_abort fnew
    | `Undet _ ->
        let waiter_fut' s = fut_det fnew s in
        waits fnew waiter_fut' ~on:fut'
    | `Alias _ -> assert false
  in
  let waiter_fut = function 
  | `Never -> use_fut' () 
  | `Det _ as det -> fut_det fnew det
  in
  let fut = src fut in
  begin match fut.state with 
  | `Det _  as det -> fut_det fnew det
  | `Never -> use_fut' ()
  | `Undet _ -> waits fnew waiter_fut ~on:fut
  | `Alias _ -> assert false
  end; 
  fnew

let first fut fut' =
  let fnew = undet () in 
  let waiter other = function
  | `Never ->
      begin match (src other).state with 
      | `Never -> 
          (* N.B. if fut = fut' this is not problematic as the 
             first of [w] or [w'] (see below) that calls 
             fut_shallow_abort, will have the effect of stopping the 
             other waiter. So fut_shallow_abort is really only called once. *) 
          fut_shallow_abort fnew
      | `Undet _ -> ()
      | `Alias _ 
      | `Det _ -> 
          (* If the other future determined its waiter called [fut_det] which
             will have the effect of stopping this waiter, so this case is 
             impossible. *)
          assert false 
      end
  | `Det v -> fut_det fnew (`Det (v, other))
  in
  let fut = src fut in
  begin match fut.state with 
  | `Det v -> fut_det fnew (`Det (v, fut'))
  | `Never -> 
      let fut' = src fut' in
      begin match fut'.state with
      | `Det v -> fut_det fnew (`Det (v, fut))
      | `Never -> fut_shallow_abort fnew 
      | `Undet _ -> waits fnew (waiter fut) ~on:fut'
      | `Alias _ -> assert false
      end
  | `Undet u ->
      let fut' = src fut' in
      begin match fut'.state with
      | `Det v -> fut_det fnew (`Det (v, fut))
      | `Never -> waits fnew (waiter fut') ~on:fut
      | `Undet u' -> 
          let w = add_waiter u (waiter fut') in 
          let w' = add_waiter u' (waiter fut) in
          set_stop_wait fnew (fun () -> w := None; w' := None);
          add_deep_abort fnew fut; 
          add_deep_abort fnew fut'
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false
  end; 
  fnew
        
let firstl futs =
  let rec rem to_rem acc = function           (* removes [to_rem] from list. *)
  | fut :: futs -> 
      if fut == to_rem then List.rev_append acc futs else
      rem to_rem (fut :: acc) futs
  | [] -> assert false 
  in
  let fnew = undet () in 
  let undet = ref 0 in                        (* remaining undets in [futs]. *)
  let waits = ref [] in                                (* registred waiters. *)
  let waiter fut = function
  | `Never -> decr undet; if !undet = 0 then fut_shallow_abort fnew
  | `Det v -> decr undet; fut_det fnew (`Det (v, rem fut [] futs))
  in
  let rec add_waits = function 
  | fut :: futs' ->
      let fut = src fut in 
      begin match fut.state with 
      | `Det v -> fut_det fnew (`Det (v, rem fut [] futs))
      | `Never -> add_waits futs'
      | `Undet u ->
          let w = add_waiter u (waiter fut) in 
          add_deep_abort fnew fut;
          incr undet; waits := w :: !waits; add_waits futs'
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fut_shallow_abort fnew
  in
  set_stop_wait fnew (fun () -> List.iter (fun w -> w := None) !waits);
  add_waits futs;
  fnew

(* Effectful combinators *)

let rec abort fut = 
  let fut = src fut in
  match fut.state with
  | `Det _ | `Never -> ()
  | `Undet _ -> fut_deep_abort fut
  | `Alias _ -> assert false 

let protect fut = 
  match (src fut).state with 
  | `Det _ | `Never -> fut 
  | `Undet u -> 
      let fnew = undet () in 
      let waiter fut = function 
      | `Never -> fut_shallow_abort fnew 
      | `Det _ as det -> fut_det fnew det 
      in
      let w = add_waiter u (waiter fut) in 
      set_stop_wait fnew (fun () -> w := None); 
      (* no deep_aborts, this cuts the reverse dependency. *) 
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
  | `Undet u -> 
      let fut' = src fut' in 
      begin match fut'.state with
      | `Never -> fut
      | `Det _ -> fut_deep_abort fut; fut'
      | `Undet u' ->  
          let fnew = undet () in
          let waiter other = function
          | `Never -> 
              begin match (src other).state with 
              | `Never -> fut_shallow_abort fut
              | `Undet _ -> ()
              | `Det _ -> 
                  (* The other waiter would have removed this waiter. *)
                  assert false 
              | `Alias _ -> assert false
              end
          | `Det _ as det ->
              begin match (src other).state with
              | `Never -> fut_det fnew det
              | `Undet _ -> 
                  (* The order is important here, determining fnew first
                     will remove the waiters it has in [other]. *) 
                  fut_det fnew det; fut_deep_abort other
              | `Alias _ | `Det _ -> assert false
              end
          in  
          let w = add_waiter u (waiter fut') in 
          let w' = add_waiter u' (waiter fut) in 
          set_stop_wait fnew (fun () -> w := None; w' := None);
          add_deep_abort fnew fut; 
          add_deep_abort fnew fut';
          fnew
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false 

let pickl futs = (* FIXME implement directly *)
  List.fold_left pick (never ()) futs 

(* Promises *)

type 'a promise = 'a t                        (* The promise is the future ! *)
let promise ?(abort = nop) () = undet_abort abort
let future p = p
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
  let abort = match abort with None -> None 
  | Some a -> Some (fun () -> a := true) 
  in
  let p = promise ?abort () in
  let work () = 
    try let r = f v in Runtime.action (fun () -> set p (`Det r)) with 
    | Never -> Runtime.action (fun () -> set p `Never)
    | exn ->
        let bt = Printexc.get_backtrace () in 
        let a () = 
          Runtime.exn_trap (`Queue (Queue.label queue)) exn bt; 
          set p `Never 
        in
        Runtime.action a        
  in
  Queue.ensure_worker ();
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

let timeout d fut =   (* FIXME: this should be optimized by inlining defs. *) 
  let timeout = map (fun () -> `Timeout) (tick d) in
  let fut = map (fun v -> `Ok v) fut in 
  pick fut timeout 

let ( >>= ) = bind           (* we have it here aswell for Fut.() notation. *) 
module Ops = struct
  let (>>=) = bind
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
