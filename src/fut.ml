(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf 
let pp = Format.fprintf

let err_promise_set = str "promise is already set" 
let err_invalid_thread_count c = str "thread count must be positive (%d)" c

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

(* A waiter is a function associated to an undetermined future to be
   called when the future is set. The waiter is usually registered by
   another undetermined future that needs to wait on that result to
   determine itself.  A waiter is a mutable option to avoid space
   leaks if the waiter is no longer interested, see the [stop_wait]
   field in ['a undet]. *)

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

and 'a t = { mutable state : 'a _state }                          (* future. *)

(* Runtime system. *)

module R = struct
 
  let cleanup_limit = 97

  (* Exception trap *)

  type exn_ctx = 
    [ `Queue of string | `Future | `Finalizer | `Backend
    | `Fd_action | `Timer_action | `Runtime_action | `Signal_action ]

  type exn_info = exn_ctx * exn * string

  let split_backtrace bt =                                       (* Grrrr... *)
    let split_string s sep =
      let rec split acc j = 
        let i = try (String.rindex_from s j sep) with Not_found -> -1 in
        if (i = -1) then 
        let p = String.sub s 0 (j + 1) in 
        if p <> "" then p :: acc else acc
        else 
        let p = String.sub s (i + 1) (j - i) in
        let acc' = if p <> "" then p :: acc else acc in
        split acc' (i - 1)
      in
      split [] (String.length s - 1)
    in
    split_string bt '\n'

  let pp_exn_info ppf (ctx, e, bt) =
    let l = match ctx with 
    | `Future -> "future" 
    | `Queue l -> str "queue %s" l
    | `Finalizer -> "finalizer"
    | `Backend -> "multiplexer"
    | `Timer_action -> "timer action"
    | `Fd_action -> "file descriptor action" 
    | `Signal_action -> "signal action" 
    | `Runtime_action -> "runtime action"
    in
    pp ppf "@[<v>%s raised:@,@[<v>%s" l (Printexc.to_string e);
    List.iter (pp ppf "@,%s") (split_backtrace bt); 
    pp ppf "@]@]"

  let default_exn_trap ei = pp Format.err_formatter "%a@." pp_exn_info ei
  let exn_trap : (exn_info -> unit) ref = ref default_exn_trap
  let set_exn_trap t = exn_trap := t
  let exn_trap ctx exn bt = !exn_trap (ctx, exn, bt)
  let trap ctx f v = try f v with 
  | e -> 
      let bt = Printexc.get_backtrace () in 
      exn_trap ctx e bt 
  
  (* Runtime backend *)

  type fd_state = [ `R | `W ]

  module type Backend = sig
    val name : string
    val start : unit -> unit
    val stop : unit -> unit
    val action : (unit -> unit) -> unit
    val signal_action : int -> (unit -> unit) -> unit
    val deadline : unit -> float option
    val timer_action : float -> (unit -> unit) -> (unit -> unit)
    val fd_action : fd_state -> Unix.file_descr -> (bool -> unit) -> unit
    val fd_close : Unix.file_descr -> unit
    val run : float option -> 'a t -> unit
  end

  module Invalid_backend = struct 
    let name = "invalid backend"
    let start () = assert false 
    let stop () = ()
    let action a = assert false
    let signal_action s a = assert false
    let deadline () = assert false
    let timer_action d a = assert false
    let fd_action state fd a = assert false
    let fd_close fd = assert false
    let run d f = assert false
  end

  let b = ref (module Invalid_backend : Backend)
  let action a = let module B = (val !b : Backend) in B.action a
  let signal_action s a = 
    let module B = (val !b : Backend) in B.signal_action s a
  let deadline () = let module B = (val !b : Backend) in B.deadline ()
  let timer_action t a = let module B = (val !b : Backend) in B.timer_action t a
  let fd_action fd s a = let module B = (val !b : Backend) in B.fd_action fd s a
  let fd_close fd = let module B = (val !b : Backend) in B.fd_close fd
  let run d f =  let module B = (val !b : Backend) in B.run d f

  let set_backend n =
    (* N.B. to hot swap backends while queues are running a lock 
       would be needed for the whole runtime. For now we don't support 
       it. *) 
    let module New = (val n : Backend) in
    let module Old = (val !b : Backend) in
    Old.stop (); New.start (); b := n
      
  let stop_backend () = let module B = (val !b : Backend) in B.stop ()
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
  if u.ws_adds > R.cleanup_limit then (u.ws_adds <- 0; u.ws <- cleanup u.ws);
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


let src f = match f.state with (* ret [f]'s src and compacts the alias chain. *)
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

let await ?timeout f = match (src f).state with
| `Det _ | `Never as v -> v
| `Undet _ -> R.run timeout f; state f
| `Alias _ -> assert false

let finally fn v f = 
  let trap_finally fn v = try ignore (fn v) with
  | e -> 
      let bt = Printexc.get_backtrace () in 
      R.exn_trap `Finalizer e bt
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
let never () = { state = `Never } 

let trap_fut fn v = try fn v with
| e -> 
    let bt = Printexc.get_backtrace () in 
    R.exn_trap `Future e bt;
    never ()

let trap_det fn v = try `Det (fn v) with
| e ->
    let bt = Printexc.get_backtrace () in
    R.exn_trap `Future e bt; 
    `Never

let alias f ~src:res = (* aliases [f] to undet [res]. *) 
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
          if u.ws_adds > R.cleanup_limit 
          then (u.ws_adds <- 0; u.ws <- cleanup u.ws);
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

let pickl l = failwith "TODO"
let link f = failwith "TODO"
  
(* Promises *)

type 'a promise = 'a t                         (* the promise is the future. *)
let promise ?(abort = nop) () = { state = `Undet (undet_state abort) }
let future p = p
let set p s = match (src p).state with 
| `Undet ws -> fset p s
| `Det _ | `Never -> invalid_arg err_promise_set
| `Alias _ -> assert false


(* Future queues *)

module Queue = struct                 (* work queues over a pool of threads. *)
  type t =
    { label : string;                                        (* queue label. *)
      q : (unit -> unit) Queue.t;               (* queue of work to perform. *)
      mutable busy : bool; }         (* [true] if waiting or executing work. *)
      
  let label q = q.label
  let auto_label = let q = ref 0 in fun () -> incr q; (str "Fut.queue%d" !q)
  let create ?(label = auto_label ()) () = 
    { label; q = Queue.create (); busy = false}

  (* The queue [scheduled_queues] holds queues that (1) have work
     waiting to be executed and (2) are not executing work at the
     moment. Except for the [concurrent] queue, no queue should appear
     twice in that queue. These invariants are enforced via the
     [q.busy] field of a queue [q]. This field is set to [true] as
     long as it is in [scheduled_queues] or executing work. *)
  
  let concurrent = create ~label:"Fut.concurrent" ()    
  let scheduled_queues = Queue.create ()
  let queue_waiting () = Queue.length scheduled_queues <> 0 
  let get_queue () = Queue.take scheduled_queues
  let schedule_queue q = q.busy <- true; Queue.add q scheduled_queues
  let reschedule_queue q = 
    if Queue.length q.q <> 0 && q != concurrent then schedule_queue q else
    q.busy <- false

  let m = Mutex.create ()
  let w = Condition.create ()
  let sleep_thread () = Condition.wait w m
  let wakeup_threads () = Condition.broadcast w
  let with_scheduler f x = 
    try Mutex.lock m; let v = f x in Mutex.unlock m; v 
    with e -> (Mutex.unlock m; raise e)
     
  let add_work q work =                     (* assert [work] must not raise. *)
    let add q work =
      Queue.add work q.q; 
      if not q.busy || q == concurrent then 
      begin 
        let no_queue_before = not (queue_waiting ()) in 
        schedule_queue q;
        if no_queue_before then wakeup_threads ();
      end
    in
    with_scheduler (add q) work
      
  let threads = ref 0                                  (* number of threads. *)
  let excess = ref 0                           (* number of threads to kill. *)
  let thread () =                                            (* thread loop. *)
    let rec get_work () = 
      if !excess > 0 then (decr excess; decr threads; raise Exit) else 
      if queue_waiting () then let q = get_queue () in q, Queue.take q.q else
      (sleep_thread (); get_work ())
    in
    try 
      while true do 
        let q, work = with_scheduler get_work () in 
        work ();                               (* assert work doesn't raise. *)
        with_scheduler reschedule_queue q
      done
    with Exit -> ()
      
  let default_thread_count = 4
  let thread_count () = !threads
  let set_thread_count count = 
    let aux count = match count - !threads with 
    | 0 -> () 
    | n when n > 0 -> 
        for i = 1 to n do Pervasives.ignore (Thread.create thread ()) done;
        threads := count;
    | n -> 
        excess := -n; 
        wakeup_threads () (* to kill some *)
    in
    if count < 0 then invalid_arg (err_invalid_thread_count count);
    with_scheduler aux count

  let ensure () = if !threads = 0 then set_thread_count default_thread_count
end

type queue = Queue.t
exception Never
  
let apply ?(queue = Queue.concurrent) ?abort f v =
  let abort = match abort with None -> None 
  | Some a -> Some (fun () -> a := true) 
  in
  let p = promise ?abort () in
  let work () = 
    try let r = f v in R.action (fun () -> set p (`Det r)) with 
    | Never -> R.action (fun () -> set p `Never)
    | exn ->
        let bt = Printexc.get_backtrace () in 
        let a () = R.exn_trap (`Queue queue.Queue.label) exn bt; set p `Never in
        R.action a        
  in
  Queue.ensure ();
  Queue.add_work queue work;
  future p

(* Timers *)

let tick ?(abs = false) d =
  let now = Unix.gettimeofday () in
  let t = if abs then d else now +. d in
  if t <= now then { state = `Det () } else
  let res = undet () in
  let a () = fset res (`Det ()) in
  let _ = R.timer_action t a in
  res
    
let delay ?(abs = false) d fn =
  let now = Unix.gettimeofday () in 
  let t = if abs then d else now +. d in 
  if t <= now then 
    let d, d' = if abs then (t, now) else (d, now -. t) in
    { state = trap_det (fn d) d' }
  else 
    let res = undet () in 
    let a () = 
      let now' = Unix.gettimeofday () in
      let d, d' = if abs then (t, now') else (d, now' -. t) in
      fset res (trap_det (fn d) d')
    in
    let _ = R.timer_action t a in
    res

let timeout ?(abs = false) d f =
  let f = src f in 
  match f.state with 
  | `Never -> never () 
  | `Det v -> ret (`Ok v)
  | `Undet u -> 
      let now = Unix.gettimeofday () in 
      let t = if abs then d else now +. d in
      if t <= now then (fabort f u; ret `Timeout) else 
      let res = undet () in
      let timeout () = 
        let f = src f in
        begin match f.state with 
        | `Never -> () 
        | `Undet u -> stop_wait res; fabort f u
        | `Alias _ | `Det _ -> assert false
        end;
        fset res (`Det `Timeout) 
      in
      let cancel_timeout = R.timer_action t timeout in
      let waiter = function 
      | `Never -> ()
      | `Det v -> cancel_timeout (); fset res (`Det (`Ok v))
      in
      let w = add_waiter u waiter in 
      set_stop_wait res (fun () -> w := None);
      set_abort res (fun () -> cancel_timeout ());
      res
  | `Alias _ -> assert false


let (>>=) = bind
module Ops = struct
  let (>>=) = bind
  let (|>) x y = failwith "TODO"
end

(* select(2) backend *)

module Select = struct                           
  let name = "Fut-select"  
    
  module Fd = struct
    type t = Unix.file_descr
    let compare : Unix.file_descr -> Unix.file_descr -> int = compare
  end
    
  module Fdmap = struct                            (* file descriptors maps. *)
    include Map.Make (Fd)
    let domain m = fold (fun k _ acc -> k :: acc) m []   
    let add_action m fd a = 
      let acts = a :: try find fd m with Not_found -> [] in 
      add fd acts m
  end

  module Sig = struct
    type t = int
    let compare : int -> int -> int = compare
  end

  module Sigmap = struct
    include Map.Make (Sig)
    let add_action m s a = 
      let acts = a :: try find s m with Not_found -> [] in 
      add s acts m
  end
    
  module Timeline : sig
    type t
    val add_action : t -> float -> (unit -> unit) -> (unit -> unit)
    val deadline : t -> float option   (* next deadline in seconds. *)
    val expired : t -> (unit -> unit) option
    val create : ?size:int -> unit -> t
  end = struct        
    
    (* Actions are sorted on the timeline in a imperative heap. Action
       cancellation is handled by having the stored action mutable to
       [None].  *)
  
    type action =                                 (* action on the timeline. *)
      { time : float;                           (* absolute POSIX fire time. *)
        mutable action : (unit -> unit) option }                  (* action. *)

    type t =                                           (* heaps for actions. *)
      { mutable heap : action array;
        mutable max : int; }
 
    let init_size = 256
    let farthest = { time = max_float; action = None }  (* greater than any. *)
    let create ?(size = init_size) () = 
      { heap = Array.make size farthest; max = -1; }

    let grow h =                                     
      let len = h.max + 1 in
      let heap' = Array.make (2 * len) farthest in
      Array.blit h.heap 0 heap' 0 len; h.heap <- heap'

    let shrink_threshold = 262144
    let shrink h =                                   (* assert (h.max < 0). *)
      if Array.length h.heap < shrink_threshold then () else
      h.heap <- Array.make init_size farthest
        
    let compare heap i i' = compare heap.(i).time heap.(i').time      
    let swap heap i i' = 
      let v = heap.(i) in heap.(i) <- heap.(i'); heap.(i') <- v

    let rec up heap i =
      if i = 0 then () else
      let p = (i - 1) / 2 in                                (* parent index. *)
      if compare heap i p < 0 then (swap heap i p; up heap p)

    let rec down heap max i =
      let start = 2 * i in
      let l = start + 1 in                              (* left child index. *) 
      let r = start + 2 in                             (* right child index. *)
      if l > max then () (* no child, stop *) else (* find smallest child k. *)
      let k = if r > max then l else (if compare heap l r < 0 then l else r) in
      if compare heap i k > 0 then (swap heap i k; down heap max k)
          
    let add_action h time a =
      let max = h.max + 1 in 
      if max = Array.length h.heap then grow h;
      let t = { time; action = Some a } in
      let cancel () = t.action <- None in
      h.heap.(max) <- t; h.max <- max; up h.heap max; cancel

    let pop h =                                   (* assert not (h.max < 0). *)
      let last = h.heap.(h.max) in
      h.heap.(h.max) <- farthest;
      h.max <- h.max - 1; 
      if h.max < 0 then () else (h.heap.(0) <- last; down h.heap h.max 0)

    let rec expired h =
      let rec loop now = 
        if h.max < 0 then (shrink h; None) else
        if h.heap.(0).action = None then (pop h; loop now) else
        if h.heap.(0).time > now then None else
        let action = h.heap.(0).action in
        (pop h; action)
      in
      loop (Unix.gettimeofday ())
      
    let deadline h = 
      let rec loop now = 
        if h.max < 0 then None else
        if h.heap.(0).action = None then (pop h; loop now) else 
        Some (h.heap.(0).time -. now)
      in
      loop (Unix.gettimeofday ())
  end 

  (* Runtime globals *)

  (* Timer actions *)

  let timeline = ref (Timeline.create ~size:0 ())
  let start_timer_actions () = timeline := Timeline.create ()
  let stop_timer_actions () = timeline := Timeline.create ~size:0 ()
  let deadline () = Timeline.deadline (!timeline)
  let timer_action t a = Timeline.add_action (!timeline) t a
  let rec exec_timer_actions () = match Timeline.expired (!timeline) with
  | Some a -> R.trap `Timer_action a (); exec_timer_actions ()
  | None -> ()

  (* File descriptor actions *)

  let fd_r = ref Fdmap.empty
  let fd_w = ref Fdmap.empty
  let stop_fd_actions () = fd_r := Fdmap.empty; fd_w := Fdmap.empty
  let exec_fdmap_actions m fd_valid fd =
    let actions = try Fdmap.find fd !m with Not_found -> [] in 
    m := Fdmap.remove fd !m; 
    List.iter (fun a -> R.trap `Fd_action a fd_valid) actions 
      
  let fd_close fd = 
    exec_fdmap_actions fd_r false fd;
    exec_fdmap_actions fd_w false fd

  let fd_action state fd a =
    let m = match state with `R -> fd_r | `W -> fd_w in 
    m := Fdmap.add_action !m fd a

  let exec_fd_actions timeout = 
    let fds_r = Fdmap.domain !fd_r in 
    let fds_w = Fdmap.domain !fd_w in
    let timeout = match deadline (), timeout with 
    | Some t, Some t' -> let t = min t t' in if t < 0. then 0. else t
    | Some t, None | None, Some t -> if t < 0. then 0. else t
    | None, None -> -1.
    in
    try
      let fds_r', fds_w', _ = Thread.select fds_r fds_w [] timeout in
      List.iter (exec_fdmap_actions fd_r true) fds_r'; 
      List.iter (exec_fdmap_actions fd_w true) fds_w'
    with 
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) -> ()
    | e ->
        let bt = Printexc.get_backtrace () in
        R.exn_trap `Backend e bt

  (* Unblock select () via pipe *) 

  let unblock_r = ref Unix.stdin (* dummy *) 
  let unblock_w = ref Unix.stdout (* dummy *)          

  let rec unblock () =                               (* write on !unblock_w *) 
    try Pervasives.ignore (Unix.single_write !unblock_w "\x2A" 0 1) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> unblock ()
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | e ->
        let bt = Printexc.get_backtrace () in 
        R.exn_trap `Backend e bt
                                           
  let rec unblocked _ =                     (* consume data from !unblock_r *) 
    try 
      Pervasives.ignore (Unix.read !unblock_r "0123456789" 0 10); unblocked true
    with
    | Unix.Unix_error (err, _, _) as e -> match err with 
      | Unix.EINTR -> unblocked true 
      | Unix.EAGAIN | Unix.EWOULDBLOCK -> fd_action `R !unblock_r unblocked 
      | _ ->
          let bt = Printexc.get_backtrace () in 
          R.exn_trap `Backend e bt; 
          fd_action `R !unblock_r unblocked

  let start_unblock () =
    let pipe () = 
      let r, w = Unix.pipe () in
      Unix.set_nonblock r; Unix.set_nonblock w;
      unblock_r := r; unblock_w := w; 
      fd_action `R r unblocked
    in
    R.trap `Backend pipe ()

  let stop_unblock () = 
    let rec close rfd dummy =
      if !rfd = dummy then () else 
      try Unix.close !rfd; rfd := dummy
      with Unix.Unix_error (Unix.EINTR,_,_) -> close rfd dummy
    in
    R.trap `Backend (close unblock_r) Unix.stdin; 
    R.trap `Backend (close unblock_w) Unix.stdout
    
  (* Signal actions *)

  let sigs = ref []                                    (* delivered signals. *) 
  let sigactions = ref Sigmap.empty
  let stop_signal_actions () = sigs := []; sigactions := Sigmap.empty
  let signal_action s a = 
    sigactions := Sigmap.add_action !sigactions s a;
    let h s = sigs := s :: !sigs; unblock () in
    Pervasives.ignore (Sys.signal s (Sys.Signal_handle h))
    
  let exec_signal_actions () = 
    let exec s =
      let acts = try Sigmap.find s !sigactions with Not_found -> [] in
      sigactions := Sigmap.remove s !sigactions;
      List.iter (fun a -> R.trap `Signal_action a ()) acts
    in
    let ss = !sigs in
    sigs := []; List.iter exec ss
      
  (* Runtime actions *)

  let am = Mutex.create ()            (* other threads may access [actions]. *)
  let actions = ref []
  let stop_runtime_actions () = actions := []
  let action a = 
    Mutex.lock am; 
    actions := a :: !actions; unblock (); 
    Mutex.unlock am

  let exec_runtime_actions () = 
    Mutex.lock am; 
    let acts = !actions in 
    actions := []; 
    Mutex.unlock am; 
    List.iter (fun a -> R.trap `Runtime_action a ()) (List.rev acts)

  (* Start, stop and run the runtime *)

  let start () = start_timer_actions (); start_unblock ()
  let stop () = 
    stop_fd_actions ();
    stop_signal_actions ();
    stop_timer_actions (); 
    stop_runtime_actions (); 
    stop_unblock ()
     
  let run timeout f =
    let rec loop start timeout f =
      exec_fd_actions timeout;
      exec_signal_actions ();
      exec_timer_actions ();
      exec_runtime_actions ();
      match (src f).state with 
      | `Det _ | `Never -> ()
      | `Undet _ ->
          begin match timeout with 
          | None -> loop start timeout f 
          | Some t -> 
              if Unix.gettimeofday () -. start >= t then () else
              loop start timeout f
          end
      | `Alias _ -> assert false
    in
    loop (Unix.gettimeofday ()) timeout f
end 

module Runtime = struct
  include R
  let thread_count = Queue.thread_count
  let set_thread_count = Queue.set_thread_count
  let () = set_backend (module Select : Backend); at_exit stop_backend
end

(* Unix *)

module Unix = struct
  open Unix;;

  type error = Unix.error * string * string
  type 'a result = [ `Error of error | `Ok of 'a ]
  let ubind f fn = failwith "TODO"
  let apply ?queue f v =
    let rec f' v = try `Ok (f v) with 
    | Unix_error (EINTR, _, _) -> f' v 
    | Unix_error (e, fn, v) -> `Error (e, fn, v)
    in
    apply ?queue f' v 

  let call f v = failwith "TODO"

  (* Signals *) 

  let signal s = 
    let res = undet () in
    let a () = fset res (`Det s) in
    R.signal_action s a; res

  (* File descritpors *)

  let nonblock_stdio () = try
    Unix.set_nonblock Unix.stdin;
    Unix.set_nonblock Unix.stdout;
    Unix.set_nonblock Unix.stderr; 
    ret (`Ok ())
  with Unix_error (e, fn, v) -> ret (`Error (e, fn, v))
    
  let close fd = R.fd_close fd; apply Unix.close fd
  let dup2 fd1 fd2 = R.fd_close fd2; apply (Unix.dup2 fd1) fd2
  let pipe () = try
    let r, w as p = Unix.pipe () in 
    Unix.set_nonblock r; 
    Unix.set_nonblock w; 
    ret (`Ok p)
  with Unix_error (e, fn, v) -> ret (`Error (e, fn, v))

  (* IO *)

  let read fd s j k = try ret (`Ok (Unix.read fd s j k)) with
  | Unix_error (e, f, v) -> match e with 
    | EINTR | EAGAIN | EWOULDBLOCK -> 
        let aborted = ref false in 
        let abort () = aborted := true in
        let res = { state = `Undet (undet_state abort) } in
        let rec a valid_fd =
          if !aborted then () else 
          if not valid_fd then fset res `Never else
          try fset res (`Det (`Ok (Unix.read fd s j k))) with 
          | Unix_error (e, f, v) -> match e with 
            | EINTR | EAGAIN | EWOULDBLOCK -> Runtime.fd_action `R fd a
            | e -> fset res (`Det (`Error (e, f, v)))
        in
        Runtime.fd_action `R fd a; res
    | e -> ret (`Error (e, f, v)) 

  let write fd s j k = try ret (`Ok (Unix.single_write fd s j k)) with 
  | Unix_error (e, f, v) -> match e with 
    | EINTR | EAGAIN | EWOULDBLOCK -> 
        let aborted = ref false in 
        let abort () = aborted := true in 
        let res = { state = `Undet (undet_state abort) } in 
        let rec a valid_fd = 
          if !aborted then () else 
          if not valid_fd then fset res `Never else
          try fset res (`Det (`Ok (Unix.single_write fd s j k))) with 
          | Unix_error (e, f, v) -> match e with 
            | EINTR | EAGAIN | EWOULDBLOCK -> Runtime.fd_action `W fd a
            | e -> fset res (`Det (`Error (e, f, v)))
        in
        Runtime.fd_action `W fd a; res 
    | e -> ret (`Error (e, f, v))

  (* Sockets *)
    
  let socket d t p = try
    let s = Unix.socket d t p in 
    Unix.set_nonblock s; 
    ret (`Ok s)
  with Unix_error (e, fn, v) -> ret (`Error (e, fn, v))

  let socketpair d t p = try 
    let s0, s1 as p = Unix.socketpair d t p in 
    Unix.set_nonblock s0; 
    Unix.set_nonblock s1; 
    ret (`Ok p)
  with Unix_error (e, fn, v) -> ret (`Error (e, fn, v))

  let accept fd = failwith "TODO"
  let connect fd addr = failwith "TODO"
  let bind fd addr = failwith "TODO"
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
