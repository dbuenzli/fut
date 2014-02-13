(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* select(2) backend *)

let name = "fut.select"  
    
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


(* FIXME. This should be a monotonic clock, gettimeofday () can run 
   backwards. Use:
   - mach_absolute_time () on macosx.
   - clock_gettime(CLOCK_MONOTONIC,t) on linux
   - QueryPerformanceCounter() on windows. *)
let now = Unix.gettimeofday 
  
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
      loop (now ())
        
    let deadline h = 
      let rec loop now = 
        if h.max < 0 then None else
        if h.heap.(0).action = None then (pop h; loop now) else 
        Some (h.heap.(0).time -. now)
      in
      loop (now ())
end 

(* Runtime globals *)

(* Timer actions *)

let timeline = ref (Timeline.create ~size:0 ())
let start_timer_actions () = timeline := Timeline.create ()
let stop_timer_actions () = timeline := Timeline.create ~size:0 ()
let deadline () = Timeline.deadline (!timeline)
let timer_action t a = Timeline.add_action (!timeline) t a
let rec exec_timer_actions () = match Timeline.expired (!timeline) with
| Some a -> Fut_backend_base.trap `Timer_action a (); exec_timer_actions ()
| None -> ()
          
(* File descriptor actions *)

let fd_r = ref Fdmap.empty
let fd_w = ref Fdmap.empty
let stop_fd_actions () = fd_r := Fdmap.empty; fd_w := Fdmap.empty
let exec_fdmap_actions m fd_valid fd =
  let actions = try Fdmap.find fd !m with Not_found -> [] in 
  m := Fdmap.remove fd !m; 
  List.iter (fun a -> Fut_backend_base.trap `Fd_action a fd_valid) actions 
    
let fd_close fd = 
  exec_fdmap_actions fd_r false fd;
  exec_fdmap_actions fd_w false fd
    
let fd_action state fd a =
  let m = match state with `R -> fd_r | `W -> fd_w in 
  m := Fdmap.add_action !m fd a
      
let exec_fd_actions timeout = 
  let fds_r = Fdmap.domain !fd_r in 
  let fds_w = Fdmap.domain !fd_w in
  let timeout = match deadline () with 
  | None -> if timeout = max_float then -1. else timeout
  | Some t -> if t < 0. then 0. else min t timeout
  in
  try
    let fds_r', fds_w', _ = Thread.select fds_r fds_w [] timeout in
    List.iter (exec_fdmap_actions fd_r true) fds_r'; 
    List.iter (exec_fdmap_actions fd_w true) fds_w'
  with 
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) -> ()
  | e ->
      let bt = Printexc.get_backtrace () in
      Fut_backend_base.exn_trap `Backend e bt
        
(* Unblock select () via pipe *) 
        
let unblock_r = ref Unix.stdin (* dummy *) 
let unblock_w = ref Unix.stdout (* dummy *)          
    
let rec unblock () =                               (* write on !unblock_w *) 
  try Pervasives.ignore (Unix.single_write !unblock_w "\x2A" 0 1) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> unblock ()
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
  | e ->
      let bt = Printexc.get_backtrace () in 
      Fut_backend_base.exn_trap `Backend e bt
        
let rec unblocked _ =                     (* consume data from !unblock_r *) 
  try 
    Pervasives.ignore (Unix.read !unblock_r "0123456789" 0 10); unblocked true
  with
  | Unix.Unix_error (err, _, _) as e -> match err with 
  | Unix.EINTR -> unblocked true 
  | Unix.EAGAIN | Unix.EWOULDBLOCK -> fd_action `R !unblock_r unblocked 
  | _ ->
      let bt = Printexc.get_backtrace () in 
      Fut_backend_base.exn_trap `Backend e bt; 
      fd_action `R !unblock_r unblocked
        
let start_unblock () =
  let pipe () = 
    let r, w = Unix.pipe () in
    Unix.set_nonblock r; Unix.set_nonblock w;
    unblock_r := r; unblock_w := w; 
    fd_action `R r unblocked
  in
  Fut_backend_base.trap `Backend pipe ()
    
let stop_unblock () = 
  let rec close rfd dummy =
    if !rfd = dummy then () else 
    try Unix.close !rfd; rfd := dummy
    with Unix.Unix_error (Unix.EINTR,_,_) -> close rfd dummy
  in
  Fut_backend_base.trap `Backend (close unblock_r) Unix.stdin; 
  Fut_backend_base.trap `Backend (close unblock_w) Unix.stdout
    
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
    List.iter (fun a -> Fut_backend_base.trap `Signal_action a ()) acts
  in
  let ss = !sigs in
  sigs := []; List.iter exec ss
    
(* Runtime actions *)
    
let am = Mutex.create ()            (* other workers may access [actions]. *)
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
  List.iter (fun a -> Fut_backend_base.trap `Runtime_action a ()) 
    (List.rev acts)
    
(* Start, stop and run the runtime *)
    
let start () = start_timer_actions (); start_unblock ()
let stop () = 
  stop_fd_actions ();
  stop_signal_actions ();
  stop_timer_actions (); 
  stop_runtime_actions (); 
  stop_unblock ()

let step ~timeout = 
  let start = now () in
  exec_fd_actions timeout;
  exec_signal_actions ();
  exec_timer_actions ();
  exec_runtime_actions ();
  now () -. start 


module Queue = struct               (* work queues over a pool of threads. *)
  type t =
    { label : string;                                      (* queue label. *)
      q : (unit -> unit) Queue.t;             (* queue of work to perform. *)
      mutable busy : bool; }       (* [true] if waiting or executing work. *)
    
  let label q = q.label
  let create ?(label = Fut_backend_base.queue_auto_label ()) () = 
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
  let sleep_worker () = Condition.wait w m
  let wakeup_workers () = Condition.broadcast w
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
          if no_queue_before then wakeup_workers ();
        end
    in
    with_scheduler (add q) work
      
  let workers = ref 0                                  (* number of workers. *)
  let excess = ref 0                           (* number of workers to kill. *)
  let worker () =                                            (* worker loop. *)
    let rec get_work () = 
      if !excess > 0 then (decr excess; decr workers; raise Exit) else 
      if queue_waiting () then let q = get_queue () in q, Queue.take q.q else
      (sleep_worker (); get_work ())
    in
    try 
      while true do 
        let q, work = with_scheduler get_work () in 
        work ();                               (* assert work doesn't raise. *)
        with_scheduler reschedule_queue q
      done
    with Exit -> ()
                 
  let default_worker_count = 4
  let worker_count () = !workers
  let set_worker_count count = 
    let aux count = match count - !workers with 
    | 0 -> () 
    | n when n > 0 -> 
        for i = 1 to n do Pervasives.ignore (Thread.create worker ()) done;
        workers := count;
    | n -> 
        excess := -n; 
        wakeup_workers () (* to kill some *)
    in
    with_scheduler aux count
        
  let ensure_worker () = 
    if !workers = 0 then set_worker_count default_worker_count
end

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
