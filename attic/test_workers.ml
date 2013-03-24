
let printer = Work.queue () 
let print l seq () = 
  Printf.printf "%s:" l;
  List.iter (fun i -> Printf.printf " %d," i) seq;
  Printf.printf "\n%!"

let work_adder l q = 
  let w = ref 0 in 
  let seq = ref [] in 
  let work n () = 
    Unix.sleep (Random.int 3);
    seq := n :: !seq;
    Work.add printer (print l (List.rev !seq))
  in
  fun () -> incr w; Work.add q (work !w)
  
let () = 
  let () = Random.self_init () in
  let () = Work.init () in 
  let () = Work.set_worker_count 10 in
  let w = [| 
    work_adder "q1" (Work.queue ());
    work_adder "q2" (Work.queue ());
    work_adder "q3" (Work.queue ());
    work_adder "qc" (Work.concurrent); |]
  in
  for i = 1 to 100 do w.(Random.int 4) () done;
  Unix.sleep ~-1
