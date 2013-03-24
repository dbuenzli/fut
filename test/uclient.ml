(* This code is in the public domain. *)

open Fut.Ops;;

let uerror (e, _, _) =
  let e = Unix.error_message e in 
  Fut.Unix.write Unix.stderr e 0 (String.length e) >>= function 
  | `Ok _ -> Fut.ret `Error | `Error e -> Fut.ret `Error

let relay fdi fdo = 
  let buf = String.create 65536 (* UNIX_BUFFER_SIZE 4.0.0 *) in
  let rec copy buf fdi fdo =
    Fut.Unix.read fdi buf 0 (String.length buf) >>= function
    | `Error e -> uerror e
    | `Ok 0 -> Fut.ret `Done
    | `Ok n -> 
        Fut.Unix.write fdo buf 0 n >>= function
        | `Ok _ -> copy buf fdi fdo
        | `Error e -> uerror e
  in
  copy buf fdi fdo

let request sock = 
  relay Unix.stdin sock >>= fun _ -> 
  Fut.ret (Unix.shutdown sock Unix.SHUTDOWN_SEND)

let response sock = 
  relay sock Unix.stdout >>= fun _ ->
  Fut.ret (Unix.close Unix.stdout)

let uclient host port = 
  let gethost h = try Some ((Unix.gethostbyname h).Unix.h_addr_list.(0)) with 
  | Not_found -> None 
  in
  Fut.Unix.setup_standard_fds ();
  let f = 
    Fut.ret (gethost host) >>= function 
    | None -> failwith "TODO host not found" | Some addr -> 
      Fut.Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 |> fun fd -> 
      Fut.Unix.connect fd (Unix.ADDR_INET (addr, port)) |> fun () ->
      ignore (Fut.join [request fd; response fd]);
      failwith "TODO"
  in
  Fut.value f

let main () = 
  let exec = Filename.basename Sys.executable_name in 
  let usage = Printf.sprintf 
    "Usage: %s [OPTION]... HOST PORT\n\
     Writes stdin to HOST:PORT and the response to stdout\n\
     Options:" exec
  in
  let options = [] in
  let host = ref "" in 
  let port = ref 80 in
  let anon s = 
    if !host = "" then host := s else 
    try port := (int_of_string s) with 
    | Failure _ -> raise (Arg.Bad (Printf.sprintf "`%s' not an integer" s))
  in
  Arg.parse options anon usage;
  match uclient !host !port with 
  | `Ok _ -> failwith "TODO" | _ -> failwith "TODO"
