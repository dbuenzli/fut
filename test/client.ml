
(* This code is in the public domain *)

open Fut.Op

let localhost port = Unix.ADDR_INET (Unix.inet_addr_any, port)

let log s = Fut.ret s

let rec rw msg buf s =
  let stop s =
    Unix.shutdown s Unix.SHUTDOWN_ALL;
    Unix.close s;
    Fut.ret ()
  in
  match Random.int 101 with
  | 99 | 100 -> stop s >>= log "stop"
  | 98  -> (* Be late *)
      Fut.tick 2. >>= fun _ ->
      log "late" >>= fun _ ->
      rw msg buf s
  | _ ->
      Fut.Unix.really_write s msg (String.length msg) >>= fun len ->
      if len = 0 then (stop s >>= log "stop") else
      Fut.Unix.read s buf (String.length buf) >>= fun len ->
      if len = 0 then (stop s >>= log "stop") else
      log buf >>= fun _ ->
      rw msg buf s

let client i port =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect s (localhost port);
  Fut.ignore (rw "bla" buf s)

let clients n port =
  let rec loop n acc =
    if n = 0 then acc else
    let client = Fut.tick (Random.float 3.) >>= fun _ -> client n port in
    loop (n - 1) (client :: acc)
  in
  match Fut.await (Fut.barrier (loop n [])) with
  | `Never | `Undet -> assert false | `Det () -> ()

let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf
    "Usage: %s [OPTION]...\n\
     Echo clients\n\
     Options:" exec
  in
  let options = [] in
  let clients = ref 10 in
  let port = ref 8001 in
  Arg.parse options anon (fun _ -> raise (Arg.Bad "invalid argument"));
  clients n port
