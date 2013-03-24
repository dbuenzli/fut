


let sites = ["http://erratique.ch", "http://caml.inria.fr"]

(* Integrating with another thread. *)

let do_service _ _ = failwith "unimplemented"

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
