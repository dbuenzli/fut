
open Fut.Ops

let par_map f l = Fut.fold (fun acc v -> v :: acc) [] (List.rev_map f l)
let seq_map f l = 
  let rec loop f acc = function 
  | [] -> Fut.ret (List.rev acc)
  | v :: vs -> (f v) >>= fun r -> loop f (r :: acc) vs
  in
  loop f [] l

let () = 
  let list = [ 0.5; 1.; 1. ] in
  let process d = Printf.printf "start %g\n%!" d; Fut.delay d in 
  let m = seq_map process list in
  match Fut.await m with 
  | `Det v -> List.iter (fun d -> Printf.printf "%g %!" d) v 
  | _ -> assert false 
  
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

