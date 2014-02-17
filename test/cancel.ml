
open Fut.Ops;;

let log f = Format.printf (f ^^ "@?") 
let str = Format.sprintf 
let () = Random.self_init () 

let string_of_file : string -> string Fut.t = 
  let count = ref 0 in
  fun file -> incr count; 
    let count = !count in
    let d = 2. +. Random.float 3. in
    Printf.printf "Create %s: %d %gs\n%!" file count d;
    Fut.tick d >>= fun () ->             (* simulate taking time. *) 
    Fut.ret (str "%d time:%g" count d)
(* fun file ->
  let len = 65535 in
  let buf = String.create len in
  let acc = Buffer.create len in 
  let rec read fd = 
    Futu.read fd buf 0 len >>= fun count -> 
    if count = 0 then Fut.ret (Buffer.contents acc) else 
    Buffer.add_substring acc buf 0 count; read fd
  in
  Futu.openfile file >>= fun fd -> 
  Fut.finally Futu.close fd (read fd)
*)

let print =
  let stdout = Fut.Queue.create () in 
  fun (file, contents) ->
    Fut.apply ~queue:stdout (Printf.printf "%s: %s\n%!" file) contents
 
let files = [ "bli"; "bla"; "blo" ]
let files = 
  let slurp f = Fut.map (fun contents -> f, contents) (string_of_file f) in 
  List.rev_map slurp files 

let stop = 
  let cancel = Futu.signal Sys.sigusr1 >>= fun _ -> Fut.ret `Cancel in
  let timeout = Fut.(tick 4. >>= fun () -> ret `Timeout) in
  Fut.map (fun v -> `Stop v) (Fut.pick cancel timeout)

let consume stop files =
(*
  let rec loop files acc =
    Fut.determine files ~until:stop >>= function 
    | `Abort reason -> Fut.ret (reason, acc)
    | `Det (file, files) -> 
        print file >>= fun () -> 
        if files = [] then Fut.ret (`Done, acc) else 
        loop (Fut.firstl files) (fst file :: acc) 
  in
  loop (Fut.firstl files) 
*)
  let process files = Fut.map (fun v -> `Files v) files in
  let rec loop files acc =
    Fut.first stop files >>= function
    | `Stop v, files -> Fut.abort (files); Fut.ret (v, acc) 
    | `Files (file, files), _ ->
        print file >>= fun () ->
        if files = [] then Fut.ret (`Done, acc) else 
        loop (process (Fut.firstl files)) (fst file :: acc)
  in
  loop (process (Fut.firstl files)) []
  
let () = match Fut.await (consume stop files) with
| `Never -> assert false
| `Undet -> assert false 
| `Det d -> 
    match d with 
    | `Timeout, ps -> log "Timed out, processed: %d" (List.length ps)
    | `Cancel, ps -> log "Cancelled, processed: %d" (List.length ps)
    | `Done, ps -> log "All done" 
 
