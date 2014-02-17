
open Fut.Ops

let () =
  let f = Fut.delay 0.5 >>= fun _ -> 
    Fut.map (fun x -> x) (Fut.delay 0.5)
  in
  begin match Fut.await f with 
  | `Det v -> Printf.printf "%g\n%!" v
  | _ -> assert false
  end;
  ()

