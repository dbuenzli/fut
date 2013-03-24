
(* Immediate, easy *)

let rec loop n =
  if n = 0 then Lwt.return () else
  Lwt.(return (pred n) >>= loop)

(* Not immediate, link trick *)

let queue = Queue.create () 
let yield () = 
  let t, w = Lwt.wait () in 
  Queue.push w queue;
  t

let rec loop n = (* proxy *) 
  if n = 0 then Lwt.return () else 
  Lwt.(yield () >>= fun () -> loop (n - 1))

let rec run () = match try Some (Queue.take queue) with Queue.Empty -> None with
| None -> () 
| Some w -> Lwt.wakeup w (); run ()

(* Cancel *)

let t = fst (Lwt.wait ()) 
let rec loop n = 
  if n = 0 then Lwt.return () else
  Lwt.(pick [ map succ t; return (pred n) ] >>= loop)


(* Cancel bis, this doesn't do what I expect because 
   pick cancels [t], which means that the whole thing
   may fail with cancel, because pick may return the cancelled 
   one. *)

let t = fst (Lwt.task ()) 
let rec loop n = 
  if n = 0 then Lwt.return () else
  Lwt.(pick [ map succ t; return (pred n) ] >>= loop)

(* leaks *)

let t = fst (Lwt.task ()) 
let rec loop n = 
  if n = 0 then Lwt.return () else
  Lwt.(choose [ map succ t; return (pred n) ] >>= loop)
