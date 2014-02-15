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
  | Empty                                                      (* empty set. *)
  | Waiter of 'a waiter                                       (* waiter [w]. *)
  | Union of 'a waiters * 'a waiters        (* union of sets [ws] and [ws']. *)

and 'a undet =                                        (* undetermined state. *)
  { mutable ws : 'a waiters;                (* waiters on this undetermined. *)
    mutable ws_adds : int;     (* number of adds in [ws] since last cleanup. *)
    mutable stop_wait : unit -> unit;    (* stops the wait on other futures. *)
    mutable abort : unit -> unit; }(* custom action invoked once if aborted. *)
     
(* If an undetermined future is aborted, the following happens.  

   1) Its state is set to [`Never].
   2) The [stop_wait] function is called. This sets all the waiters 
      it registred in other futures to [None].
   3) The custom [abort] function is called.
   4) Waiters [ws] are notified that future is set to [`Never].

   With step 2), aborting an undetermined future has the effect of
   setting the waiters it registred to [None] in the waiter set [ws]
   of other undetermined futures. If nothing is done to remove these
   [None] waiters this can lead to space leaks. There are two mechanism
   to remove these none waiters:

   a) When a waiter is added to a set [ws] if it sees a top level [None] 
      waiter it removes it, see the [union] function. 
   b) When a waiter is added to a set [ws] we increment [ws_adds]. If 
      [ws_adds] exceeds [Runtime.cleanup_limit], all [None] waiters
      are removed from [ws], see the [cleanup] function. *)

type 'a _state =                                   (* internal future state. *)
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

(* Waiter sets *)

let cleanup ws =                             (* remove None waiters in [ws]. *)
  let rec loop acc = function 
  | Empty :: todo -> loop acc todo 
  | Waiter { contents = None } :: todo -> loop acc todo
  | Waiter { contents = Some _ } as w :: todo -> 
      if acc = Empty then loop w todo else loop (Union (w, acc)) todo
  | Union (ws, ws') :: todo -> loop acc (ws :: ws' :: todo)
  | [] -> acc
  in
  loop Empty [ws]

let union ws ws' = match ws, ws' with              (* unions [ws] and [ws']. *)
| Empty, ws | ws, Empty
| Waiter { contents = None }, ws | ws, Waiter { contents = None } -> ws
| ws, ws' -> Union (ws, ws')

let notify_waiters ws s =                   (* notifies [ws] with state [s]. *)
  let rec loop s = function 
  | Empty :: todo -> loop s todo 
  | Waiter { contents = None } :: todo -> loop s todo
  | Waiter { contents = Some w } :: todo -> w s; loop s todo
  | Union (w, w') :: todo -> loop s (w :: w' :: todo)
  | [] -> ()
  in
  loop s [ws]

let add_waiter u wf = (* adds waiter with function [wf] to undetermined [u]. *)
  let w = ref (Some wf) in
  u.ws <- union (Waiter w) u.ws;
  u.ws_adds <- u.ws_adds + 1; 
  if u.ws_adds > Runtime.cleanup_limit 
  then (u.ws_adds <- 0; u.ws <- cleanup u.ws);
  w

let waits f wf ~on:u = (* [f] waits with function [wf] on undetermined [u]. *)
  let w = add_waiter u wf in
  match f.state with 
  | `Undet u -> u.stop_wait <- fun () -> w := None
  | _ -> assert false

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

let fset f (s : 'a set) = match f.state with             (* sets the future. *)
| `Undet u ->
    f.state <- (s :> 'a _state);
    if s = `Never then u.abort ();
    notify_waiters u.ws s;
| _ -> assert false 

let stop_wait f = match f.state with     (* removes [f]'s registred waiters. *)
| `Undet u -> u.stop_wait () | _ -> assert false

let set_stop_wait f fn = match f.state with 
| `Undet u -> u.stop_wait <- fn | _ -> assert false

let set_abort f fn = match f.state with
| `Undet u -> u.abort <- fn | _ -> assert false

let undet_state abort = { ws = Empty; ws_adds = 0; stop_wait = nop; abort } 
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

let alias f ~src:res =                        (* aliases [f] to undet [res]. *)
  let f = src f in
  let res = src res in 
  match res.state with 
  | `Undet u -> 
      begin match f.state with 
      | `Det _ | `Never as set -> fset res set
      | `Undet u' -> 
          f.state <- `Alias res;
          (* union waiter sets, cleanup if needed. *)
          u.ws <- union u.ws u'.ws;            
          u.ws_adds <- u.ws_adds + u'.ws_adds; 
          if u.ws_adds > Runtime.cleanup_limit 
          then (u.ws_adds <- 0; u.ws <- cleanup u.ws);
          (* concatenate abort actions *) 
          u.abort <- concat_aborts u.abort u'.abort
      | _ -> assert false 
      end
  | _ -> assert false 

(* Applicative combinators *)

let ret v = { state = `Det v }
let rec bind f fn = match (src f).state with
| `Never -> never ()
| `Det v -> fn v        (* do not [trap_fut fn v] it breaks tail-recursion. *)
| `Undet u ->
    let res = undet () in
    let waiter = function
    | `Never -> fset res `Never
    | `Det v -> alias (trap_fut fn v) ~src:res
    in
    waits res waiter ~on:u; 
    res
| `Alias _ -> assert false

let app ff fv = match (src ff).state with 
| `Never -> never ()
| `Det fn -> 
    begin match (src fv).state with 
    | `Never -> never () 
    | `Det v -> { state = (trap_det fn v) }
    | `Undet uv ->
        let res = undet () in 
        let waiter = function 
        | `Never -> fset res `Never
        | `Det v -> fset res (trap_det fn v)
        in
        waits res waiter ~on:uv; res
    | `Alias _ -> assert false
    end
| `Undet uf -> 
    begin match (src fv).state with 
    | `Never -> never () 
    | `Det v ->
        let res = undet () in 
        let waiter = function 
        | `Never -> fset res `Never 
        | `Det fn -> fset res (trap_det fn v)
        in
        waits res waiter ~on:uf; res
    | `Undet uv ->
        let res = undet () in
        let waiter_f fv = function 
        | `Never -> stop_wait res; fset res `Never
        | `Det f ->
            match (src fv).state with 
            | `Undet _ -> () 
            | `Det v -> fset res (trap_det f v)
            | `Alias _ | `Never -> assert false 
        in 
        let waiter_v ff = function 
        | `Never -> stop_wait res; fset res `Never
        | `Det v -> 
            match (src ff).state with 
            | `Undet _ -> () 
            | `Det f -> fset res (trap_det f v) 
            | `Alias _ | `Never -> assert false 
        in
        let wf = add_waiter uf (waiter_f fv) in 
        let wv = add_waiter uv (waiter_v ff) in 
        set_stop_wait res (fun () -> wf := None; wv := None);
        res
    | `Alias _ -> assert false
    end
| `Alias _ -> assert false

let map fn f = match (src f).state with
| `Never -> never ()
| `Det v -> { state = (trap_det fn v) }
| `Undet u ->
    let res = undet () in 
    let waiter = function
    | `Never -> fset res `Never 
    | `Det v -> fset res (trap_det fn v)
    in
    waits res waiter ~on:u; res
| `Alias _ -> assert false

let ignore f = match (src f).state with 
| `Never -> never () 
| `Det v -> { state = `Det () } 
| `Undet u -> 
    let res = undet () in 
    let waiter = function 
    | `Never -> fset res `Never 
    | `Det _ -> fset res (`Det ())
    in
    waits res waiter ~on:u; res
| `Alias _ -> assert false 

let fold fn acc fs = 
  let res = undet () in
  let fold () =                      (* when called [fs] are all determined. *)
    let fn' acc f = match (src f).state with 
    | `Det v -> fn acc v | _ -> assert false 
    in
    fset res (trap_det (List.fold_left fn' acc) fs)
  in
  let undet = ref 0 in                          (* remaining undets in [fs]. *) 
  let waits = ref [] in                                (* registred waiters. *)
  let waiter = function 
  | `Never -> stop_wait res; fset res `Never
  | `Det _ -> decr undet; if !undet = 0 then fold ()
  in
  let rec add_waits = function 
  | f :: fs' -> 
      begin match (src f).state with 
      | `Det _ -> add_waits fs'
      | `Never -> stop_wait res; fset res `Never
      | `Undet u -> 
          let w = add_waiter u waiter in 
          waits := w :: !waits; incr undet; add_waits fs'
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fold ()
  in
  set_stop_wait res (fun () -> List.iter (fun w -> w := None) !waits);
  add_waits fs; res

let barrier ?(set = false) fs =
  let res = undet () in 
  let undet = ref 0 in                          (* remaining undets in [fs]. *)
  let waits = ref [] in                                (* registred waiters. *)
  let waiter = function 
  | `Never when not set -> stop_wait res; fset res `Never
  | `Det _ | `Never -> decr undet; if !undet = 0 then fset res (`Det ())
  in
  let rec add_waits = function 
  | f :: fs' -> 
      begin match (src f).state with 
      | `Det _ -> add_waits fs'
      | `Never -> if set then add_waits fs' else(stop_wait res; fset res `Never)
      | `Undet u -> 
          let w = add_waiter u waiter in 
          waits := w :: !waits; incr undet; add_waits fs'
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fset res (`Det ())
  in
  set_stop_wait res (fun () -> List.iter (fun w -> w := None) !waits);
  add_waits fs; res
    
let sustain f f' =
  let res = undet () in
  let use_f' () = match (src f').state with
  | `Det _ | `Never as s -> fset res s
  | `Undet u' ->
      let waiter_f' s = fset res s in
      waits res waiter_f' ~on:u'
  | `Alias _ -> assert false
  in
  let waiter_f = function `Never -> use_f' () | `Det _ as s -> fset res s in
  begin match (src f).state with 
  | `Det _  as s -> fset res s
  | `Never -> use_f' ()
  | `Undet u -> waits res waiter_f ~on:u
  | `Alias _ -> assert false
  end; 
  res

let first f f' =
  let res = undet () in 
  let waiter other = function
  | `Never ->
      begin match (src other).state with 
      | `Never -> stop_wait res (* careful if f = f' *); fset res `Never
      | `Undet _ -> () 
      | `Alias _ | `Det _ -> assert false 
      end
  | `Det v -> stop_wait res; fset res (`Det (v, other))
  in
  begin match (src f).state with 
  | `Det v -> fset res (`Det (v, f'))
  | `Never -> 
      begin match (src f').state with
      | `Det v -> fset res (`Det (v, f))
      | `Never -> fset res `Never
      | `Undet u' -> waits res (waiter f) ~on:u'
      | `Alias _ -> assert false
      end
  | `Undet u ->
      begin match (src f').state with 
      | `Det v -> fset res (`Det (v, f))
      | `Never -> waits res (waiter f') ~on:u
      | `Undet u' -> 
          let w = add_waiter u (waiter f') in 
          let w' = add_waiter u' (waiter f) in 
          set_stop_wait res (fun () -> w := None; w' := None)
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false
  end; 
  res
        
let firstl fs =
  let rec rem f acc = function                     (* removes [f] from list. *)
  | f' :: fs' -> 
      if f = f' then List.rev_append acc fs' else rem f (f' :: acc) fs' 
  | [] -> assert false 
  in
  let res = undet () in 
  let undet = ref 0 in                          (* remaining undets in [fs]. *)
  let waits = ref [] in                                (* registred waiters. *)
  let waiter f = function 
  | `Never -> decr undet; if !undet = 0 then fset res `Never 
  | `Det v -> stop_wait res; fset res (`Det (v, rem f [] fs))
  in
  let rec add_waits = function 
  | f :: fs' -> 
      begin match (src f).state with 
      | `Det v -> stop_wait res; fset res (`Det (v, rem f [] fs))
      | `Never -> add_waits fs'
      | `Undet u ->
          let w = add_waiter u (waiter f) in 
          waits := w :: !waits; incr undet; add_waits fs'
      | `Alias _ -> assert false
      end
  | [] -> if !undet = 0 then fset res `Never
  in
  set_stop_wait res (fun () -> List.iter (fun w -> w := None) !waits);
  add_waits fs; res

(* Effectful combinators *)

let fabort f u =
  f.state <- `Never; 
  u.stop_wait ();
  u.abort ();
  notify_waiters u.ws `Never

let abort f = 
  let f = src f in 
  match f.state with 
  | `Det v -> never () 
  | `Never -> f
  | `Undet u -> fabort f u; f
  | `Alias _ -> assert false 

let pick f f' =
  let f = src f in 
  match f.state with
  | `Never -> f'
  | `Det v -> 
      let f' = src f' in 
      begin match f'.state with 
      | `Never | `Det _ -> f
      | `Undet u' -> fabort f' u'; f
      | `Alias _ -> assert false
      end
  | `Undet u -> 
      let f' = src f' in 
      begin match f'.state with 
      | `Never -> f
      | `Det v -> fabort f u; f' 
      | `Undet u' ->  
          let res = undet () in
          let waiter fo = function 
          | `Never -> 
              begin match (src fo).state with 
              | `Never -> fset res `Never
              | `Undet _ | `Det _ -> ()
              | `Alias _ -> assert false
              end
          | `Det _ as d -> 
              begin match (src fo).state with 
              | `Never -> fset res d
              | `Undet u -> fabort fo u; fset res d
              | `Alias _ | `Det _ -> assert false
              end
          in  
          let w = add_waiter u (waiter f') in 
          let w' = add_waiter u' (waiter f) in 
          set_stop_wait res (fun () -> w := None; w' := None);
          res
      | `Alias _ -> assert false
      end
  | `Alias _ -> assert false 

let pickl fs = List.fold_left pick (never ()) fs (* TODO implement directly *)
let link f = failwith "TODO"
  
(* Promises *)

type 'a promise = 'a t                         (* the promise is the future. *)
let promise ?(abort = nop) () = undet_abort abort
let future p = p
let set promise set = 
  let promise = src promise in
  match promise.state with 
  | `Undet _ -> fut_set promise set 
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
    let fnew = undet_abort abort in 
    (fun diff -> fset fnew (`Det diff)), fnew
  in
  Runtime.timer_action d def 

let tick d = 
  let def abort =
    let fnew = undet_abort abort in
    (fun _ -> fset fnew (`Det ())), fnew
  in
  Runtime.timer_action d def

let timeout d fut = (* FIXME: this should be optimized by inlining defs. *) 
  let timeout = map (fun () -> `Timeout) (tick d) in
  let fut = map (fun v -> `Ok v) fut in 
  pick fut timeout 

let ( >>= ) = bind
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
