
open Testing
open Fut.Op

let () =
  let u = Futu.apply Unix.environment () in
  match Fut.await u with
  | `Det (`Ok v) -> Printf.printf "env: %s" (String.concat "," (Array.to_list v))
  | `Det (`Error (e, _, _)) -> Printf.printf "error: %s" (Unix.error_message e)
  | `Undet -> Printf.printf "Did not determine\n%!"

(*

let par_map f l = Fut.fold (fun acc v -> v :: acc) [] (List.rev_map f l)
let seq_map f l =
  let rec loop f acc = function
  | [] -> Fut.ret (List.rev acc)
  | v :: vs -> (f v) >>= fun r -> loop f (r :: acc) vs
  in
  loop f [] l
*)
let () = ()
(*
  let list = [ 0.5; 1.; 1. ] in
  let process d = Printf.printf "start %g\n%!" d; Fut.delay d in
  let m = seq_map process list in
  match Fut.await m with
  | `Det v -> List.iter (fun d -> Printf.printf "%g %!" d) v
  | _ -> assert false
  *)
let () =
(*
  let f = Fut.delay 0.5 >>= fun _ ->
    Fut.map (fun x -> x) (Fut.delay 0.5)
  in
  begin match Fut.await f with
  | `Det v -> Printf.printf "%g\n%!" v
  | _ -> assert false
  end;
*)
  ()

(*
let every_d d =
  let rec loop d =
    Fut.delay d >>= fun diff -> loop (d +. diff)
  in
  loop d

let every_d' d = (* avoids fp error accumulation (are you sure ?) *)
  let rec loop t d =
    Fut.delay d >>= fun diff ->
    let t = t +. d -. diff in
    let next = ceil (t /. d.) * d. in
    loop (next - t.)
  in
  loop 0. d
*)
