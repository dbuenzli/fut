(* This code is in the public domain *)

open Fut.Ops;;

let uerror (e, _, _) =
  let e = Unix.error_message e in 
  Fut.Unix.write Unix.stderr e 0 (String.length e) >>= function 
  | `Ok _ -> Fut.ret `Error | `Error e -> Fut.ret `Error


let usize = 65535 (* UNIX_BUFFER_SIZE *)

let echo s = 
  let rec loop s buf = 
    Fut.timeout 2. (Fut.Unix.read s buf 0 (String.length s)) >>= function
    | `Timeout ->
        log "Client too slow closing connection.";
        Unix.shutdown s Unix.SHUTDOWN_ALL;
    | `Ok 0 -> 
        log "Client closed connection.";
        Unix.shutdown s Unix.SHUTDOWN_SEND; Fut.ret ()
    | `Ok len ->
        Fut.Unix.write s buf 0 len >>= loop s buf
  in
  loop s (String.create usize) >>= fun () -> 
  Unix.close s; Fut.ret ()
    
let echo_server host port =  (* TODO ERRORS *)
  let host = Unix.gethostbyname host.h_addr_list.(0) in 
  let addr = Unix.ADDR_INET (addr, port) in 
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr; 
  Unix.listen sock 1024;
  let rec loop sock = 
    Unix.accept sock >>= fun s -> 
    ignore (echo s); loop sock
  in
  Fut.await (loop sock)
   
let main () = 
  let exec = Filename.basename Sys.executable_name in 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... HOST PORT\n\
     Echoes connections to HOST:PORT\n\
     Options:" exec
  in
  let options = [] in
  let host = ref "" in 
  let port = ref 8001 in
  let anon s = 
    if !host = "" then host := s else 
    try port := (int_of_string s) with 
    | Failure _ -> raise (Arg.Bad (Printf.sprintf "`%s' not an integer" s))
  in
  Arg.parse options anon usage;
  match echo_server !host !port with 
  | `Ok _ -> failwith "TODO" | _ -> failwith "TODO"





