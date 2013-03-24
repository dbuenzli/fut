(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(** TODO add priority low, normal, high *)

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

let with_lock l f x = 
  Mutex.lock l; 
  try let v = f x in (Mutex.unlock l; v) 
  with e -> Mutex.unlock l; raise e 

let default_logger src e bt = 
  begin match src with
  | `Worker -> Printf.eprintf "Worker exception:\n"
  | `Selector -> Printf.eprintf "Selector exception:\n"
  end;
  Printf.eprintf "%s\n%s\n%!" (Printexc.to_string e) bt

let exn_logger = ref default_logger
let set_exn_logger f = exn_logger := f


let scheduler = Mutex.create ()
let with_scheduler f x = with_lock scheduler f x  

let sleep, wakeup_workers = 
  let wakeup = Condition.create () in 
  (fun () -> Condition.wait wakeup scheduler),
  (fun () -> Condition.broadcast wakeup)
    
      
let workers = ref 0
let workers_to_kill = ref 0 

type queue = { q : (unit -> unit) Queue.t; mutable scheduled : bool; }
let queue ?label () = { q = Queue.create (); scheduled = false }
let concurrent = queue ()

(* The following queue holds work queues that have work waiting to be
   executed. Note that except for the concurrent queue, no queue
   should appear twice in that queue. *)

let waiting_queues = Queue.create ()
let work_waiting () = Queue.length waiting_queues <> 0 
let work_get () = Queue.take waiting_queues
let work_schedule q =
  q.scheduled <- true;
  Queue.add q waiting_queues

let work_reschedule q = 
  q.scheduled <- false;
  if Queue.length q.q <> 0 && q != concurrent then (work_schedule q)

let add q w = 
  let aux q w = 
    Queue.add w q.q;
    if not q.scheduled || q == concurrent then 
      begin
	let work_waiting = work_waiting () in
	work_schedule q;
	if not work_waiting then wakeup_workers ();
      end;
  in
  with_scheduler (aux q) w
    
let worker () =
  let get_work () = 
    let work_waiting_or_kill () = 
      if !workers_to_kill = 0 then work_waiting () else
      (decr workers_to_kill; decr workers; raise Exit)
    in
    while not (work_waiting_or_kill ()) do sleep () done;
    let q = work_get () in
    q, Queue.take q.q
  in
  try
    while true do 
      let q, w = with_scheduler get_work () in
      (try w () with e -> !exn_logger `Worker e (Printexc.get_backtrace ()));
      with_scheduler work_reschedule q
    done
  with Exit -> ()	

let worker_count () = !workers
let set_worker_count c = 
  let aux c = match c - !workers with 
  | 0 -> ()
  | n when n > 0 ->  
      for i = 1 to n do ignore (Thread.create worker ()) done;
      workers := c;
  | n -> 
      workers_to_kill := -n;
      wakeup_workers () (* to kill some *)
  in
  with_scheduler aux c
  
module Source = struct
  (* TODO while a notification is scheduled, should 
     be removed from the fd set until the handler has been invoked. 

     Or make them one shot by default. 
*)


  type kind = [ 
    | `Read of Unix.file_descr 
    | `Write of Unix.file_descr
    | `Signal of int 
    | `Timeout of float ]

  type t = { kind : kind; 
	     mutable stopped : bool;
	     action : t -> unit }

  let r_set = ref []
  let w_set = ref []
  let add_fd fd l = if not (List.mem fd !l) then l := fd :: !l 
  let rem_fd fd l = l := List.filter (fun fd' -> fd <> fd') !l
  let selector = Mutex.create ()

  let create_source q kind f = 
    let action s = 
      let work () = if s.stopped then () else f s in 
      if not s.stopped then add q work 
    in
    { kind; stopped = false; action } 

  let source_table = Hashtbl.create 256 
  let add_source s = 
    try 
      let sources = Hashtbl.find source_table s.kind in
      Hashtbl.replace source_table s.kind (s :: sources)
    with Not_found -> Hashtbl.add source_table s.kind [s]
	
  let rem_source s = 
    try
      let sources = Hashtbl.find source_table s.kind in
      let sources' = List.filter (fun s' -> s != s') sources in 
      if sources' = [] then Hashtbl.remove source_table s.kind else
      Hashtbl.replace source_table s.kind sources'
    with Not_found -> invalid_arg "inexistant source"
	
  let actionate_sources kind = 
    try
      let sources = Hashtbl.find source_table kind in
      List.iter (fun s -> s.action s) sources
    with Not_found -> ()
	      
  let unblock =
    let read, write = Unix.pipe () in
    let rec unblock () =
      let rec aux () = if Unix.single_write write "1" 0 1 <> 1 then aux () in
      print_endline "unblock";
      try aux () with 
      | Unix.Unix_error (Unix.EINTR, _, _) -> unblock ()
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    in
    let rec consume_unblock s = 
      let aux () = ignore (Unix.read read "AAAA" 0 4) in
      print_endline "consuming unblock";
      try aux () with 
      | Unix.Unix_error (Unix.EINTR, _, _) -> consume_unblock s
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    in
    let s = { kind = `Read read; stopped = false; action = consume_unblock } in
    Unix.set_nonblock read;
    Unix.set_nonblock write;
    add_source s;
    unblock
        
  let add q kind f = 
    let s = create_source q kind f in
    let aux s = 
      begin match s.kind with
      | `Read fd -> add_fd fd r_set; unblock ()	
      | `Write fd -> add_fd fd w_set; unblock ()	
      | `Signal s  -> failwith "unimpl"
      | `Timeout f -> failwith "unimpl"
      end;
      add_source s;
    in
    with_lock selector aux s

  let rem s =
    let aux s = 
      begin match s.kind with 
      | `Read fd -> rem_fd fd r_set      
      | `Write fd -> rem_fd fd r_set
      | `Signal s -> failwith "unimpl"
      | `Timeout f -> failwith "unimpl"
      end; 
      rem_source s;
    in 
    with_lock selector aux s 

  let next_deadline () = failwith "unimpl"

  let gather () = 
    let aux () = 
      try
	while true do
	  let prepare () = !r_set, !w_set, (-1.) in
	  let r_set, w_set, timeout = with_lock selector prepare () in 
	  try
	    match Thread.select r_set w_set [] timeout with
	    | [], [], [] -> (* time out *) ()
	    | r_set, w_set, [] -> 
		let actionate () = 
		  List.iter (fun fd -> actionate_sources (`Read fd)) r_set;
		  List.iter (fun fd -> actionate_sources (`Write fd)) w_set
		in
		with_lock selector actionate ()
	    | _ -> assert false
	  with
	  | Unix.Unix_error (Unix.EINTR, _, _) -> ()
	done;
      with e -> !exn_logger `Selector e (Printexc.get_backtrace ())
    in
    ignore (Thread.create aux ())  
end

type source = Source.t
type source_kind = Source.kind

let source_kind s = s.Source.kind
let rem_source = Source.rem
let add_source = Source.add

let init () = 
  set_worker_count 2;
  Source.gather ()

(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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


  

