(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Tests the basic combinators. *)

open Fut.Op
open Testing

let finally () = 
  log_test "Test finally"; 
  let called = ref 0 in 
  let finally v = incr called; assert (!called == v) in 
  ignore (Fut.finally finally 1 (Fut.ret ())); 
  ignore (Fut.finally finally 2 (Fut.never ()));
  let () = 
    let f, p = promise () in
    ignore (Fut.finally finally 3 f);
    ignore (Fut.finally failwith "bla" f);
    record_trap (); 
    Fut.set p (`Det "hip");
    trapped (`Exn (`Finalizer, (Failure "bla")));
    let f, p = promise () in
    ignore (Fut.finally finally 4 f);
    ignore (Fut.finally failwith "blo" f);
    record_trap (); 
    Fut.set p `Never;
    trapped (`Exn (`Finalizer, (Failure "blo"))); 
    assert (!called = 4)
  in
  ()
    
let never () = 
  log_test "Test never";
  is_never (Fut.never ());
  ()

let ret () = 
  log_test "Test ret";
  is_det (Fut.ret ()) ();
  is_det (Fut.ret 4) 4;
  is_det (Fut.ret "bla") "bla";
  ()

let recover () = 
  log_test "Test recover"; 
  is_det Fut.(recover (ret 4)) (`Det 4); 
  is_det Fut.(recover (never ())) `Never;
  let p = Fut.promise () in
  let r = Fut.(recover (future p)) in 
  is_undet r; 
  Fut.set p (`Det "Hey"); 
  is_det r (`Det "Hey"); 
  let p = Fut.promise () in
  let r = Fut.(recover (future p)) in
  is_undet r; 
  Fut.set p `Never; 
  is_det r `Never; 
  ()
  
let bind () = 
  log_test "Test bind";
  is_det (Fut.ret 3 >>= Fut.ret) 3;
  is_never (Fut.ret () >>= Fut.never);
  is_never (Fut.never () >>= Fut.ret);
  is_det ((Fut.ret 3 >>= fun v -> Fut.ret (v + 2)) >>= Fut.ret) 5;
  is_det (Fut.ret 3 >>= (fun v -> Fut.ret (v + 2) >>= Fut.ret)) 5;
  is_never (Fut.ret 3 >>= fun _ -> Fut.never () >>= fun () -> Fut.ret 2);
  let () = 
    let fut, p = promise () in
    let f = fut >>= failwith in
    record_trap (); 
    is_undet f; 
    Fut.set p (`Det "bla"); 
    trapped (`Exn (`Future, (Failure "bla")));
    is_never f
  in
  let () = 
    let fut, p = promise () in 
    let f = fut >>= failwith in
    record_trap (); 
    is_undet f; 
    Fut.set p `Never;
    trapped `Nothing;
    is_never f
  in
  ()

let bind_loop () = 
  log_test "Test bind loop"; 
  let p = ref (Fut.promise ()) in 
  let resume () = Fut.set !p (`Det ()) in
  let yield () = p := Fut.promise (); Fut.future !p in 
  let rec loop n = 
    if n = 0 then Fut.ret () else 
    yield () >>= fun () -> loop (n - 1) 
  in
  let f = loop 3 in
  is_undet f; resume (); 
  is_undet f; resume (); 
  is_undet f; resume ();
  is_det f ();
  ()

let app () = 
  log_test "Test app";
  is_det (Fut.app (Fut.ret succ) (Fut.ret 2)) 3;
  is_never (Fut.app (Fut.never ()) (Fut.ret 2));
  is_never (Fut.app (Fut.ret succ) (Fut.never ()));
  record_trap (); 
  is_never (Fut.app (Fut.ret failwith) (Fut.ret "fail"));
  trapped (`Exn (`Future, (Failure "fail")));
  let () = 
    let ff, pf = promise () in
    let fv, pv = promise () in
    let fv', pv' = promise () in
    let fv'', pv'' = promise () in
    let f1 = Fut.app ff (Fut.ret 2) in 
    let f2 = Fut.app ff fv in 
    let f3 = Fut.app ff fv' in
    let f4 = Fut.app ff fv'' in
    is_undet f1; is_undet f2; is_undet f3; is_undet f4; 
    Fut.set pv (`Det 2); 
    is_undet f1; is_undet f2; is_undet f3; is_undet f4;
    Fut.set pf (`Det succ); 
    is_det f1 3; is_det f2 3; is_undet f3; is_undet f4;
    Fut.set pv' (`Det 4); 
    is_det f3 5; is_undet f4; 
    Fut.set pv'' `Never; 
    is_never f4
  in
  let () = 
    let ff, pf = promise () in 
    let ff', pf' = promise () in 
    let ff'', pf'' = promise () in 
    let fv, setv = promise () in
    let f1 = Fut.app (Fut.ret succ) fv in 
    let f2 = Fut.app ff fv in 
    let f3 = Fut.app ff' fv in 
    let f4 = Fut.app ff'' fv in 
    is_undet f1; is_undet f2; is_undet f3; is_undet f4;
    Fut.set pf (`Det succ); 
    is_undet f1; is_undet f2; is_undet f3; is_undet f4;
    Fut.set setv (`Det 2); 
    is_det f1 3; is_det f2 3; is_undet f3; is_undet f4;
    Fut.set pf' (`Det succ); 
    is_det f3 3; is_undet f4;
    Fut.set pf'' `Never;
    is_never f4;
  in
  let () = 
    let ff, pf = promise () in 
    let fv, pv = promise () in
    let f = Fut.app ff fv in 
    is_undet f; 
    Fut.set pf `Never; 
    is_never f;
    Fut.set pv (`Det 2);
    is_never f;
  in
  let () = 
    let ff, pf = promise () in 
    let fv, pv = promise () in
    let f = Fut.app ff fv in 
    is_undet f; 
    Fut.set pv `Never; 
    is_never f;
    Fut.set pf (`Det succ);
    is_never f;
  in
  let () = 
    let ff, pf = promise () in 
    let fv, pv = promise () in 
    let f = Fut.app ff fv in 
    is_undet f; 
    Fut.set pv (`Det "fail"); 
    is_undet f; 
    record_trap (); 
    Fut.set pf (`Det failwith);
    trapped (`Exn (`Future, (Failure "fail")));
    is_never f;
  in
  ()

let map () = 
  log_test "Test map";
  is_det (Fut.map succ (Fut.ret 2)) 3;
  is_never (Fut.map succ (Fut.never ()));
  record_trap (); 
  is_never (Fut.map failwith (Fut.ret "fail")); 
  trapped (`Exn (`Future, Failure "fail"));
  let () = 
    let fv, pv = promise () in 
    let f = Fut.map failwith fv in 
    let f' = Fut.map (fun msg -> "no " ^ msg) fv in 
    is_undet f; is_undet f';
    record_trap (); 
    Fut.set pv (`Det "fail");
    trapped (`Exn (`Future, Failure "fail"));
    is_never f; is_det f' "no fail"
  in
  ()

let ignore () = 
  log_test "Test ignore"; 
  is_det (Fut.ignore (Fut.ret 3)) (); 
  is_never (Fut.ignore (Fut.never ()));
  let () = 
    let fv, pv = promise () in 
    let fn, pn = promise () in 
    let u = Fut.ignore fv in 
    let n = Fut.ignore fn in 
    is_undet u; is_undet n; 
    Fut.set pv (`Det 3); 
    is_det u (); is_undet n; 
    Fut.set pn `Never; 
    is_det u (); is_never n
  in
  ()

let fold () = 
  log_test "Test fold";
  is_det (Fut.fold (+) 0 [Fut.ret 1; Fut.ret 2; Fut.ret 3]) 6;
  is_never (Fut.fold (+) 0 [Fut.ret 1; Fut.never (); Fut.ret 3]);
  is_det (Fut.fold (+) 3 []) 3;
  record_trap (); 
  is_never (Fut.fold (fun _ _ -> failwith "fail") 0 [ Fut.ret 1; Fut.ret 2]);
  trapped (`Exn (`Future, Failure "fail"));
  let () = 
    let f1, p1 = promise () in 
    let f2, p2 = promise () in 
    let f3, p3 = promise () in 
    let n, pn = promise () in 
    let s1 = Fut.fold (+) 0 [f1; f2; f3; ] in
    let s2 = Fut.fold (+) 0 [f1; f3; f2; ] in
    let s3 = Fut.fold (+) 0 [f2; f1; f3; ] in
    let s4 = Fut.fold (+) 0 [f2; f3; f1; ] in
    let s5 = Fut.fold (+) 0 [f3; f1; f2; ] in
    let s6 = Fut.fold (+) 0 [f3; f2; f1; ] in
    let n0 = Fut.fold (+) 0 [n; f1; f2; f3 ] in 
    let n1 = Fut.fold (+) 0 [f1; n; f2; f3 ] in 
    let n2 = Fut.fold (+) 0 [f1; f2; n; f3 ] in 
    let n3 = Fut.fold (+) 0 [f1; f2; f3; n ] in 
    let t = Fut.fold (fun _ _ -> failwith "fail") 0 [f1; f2; f3] in
    Fut.set p1 (`Det 1);
    is_undet s1; is_undet s2; is_undet s3; is_undet s4; is_undet s5; 
    is_undet s6; is_undet n0; is_undet n1; is_undet n2; is_undet n3;
    Fut.set p2 (`Det 2); 
    is_undet s1; is_undet s2; is_undet s3; is_undet s4; is_undet s5; 
    is_undet s6; is_undet n0; is_undet n1; is_undet n2; is_undet n3;
    Fut.set pn `Never;
    is_undet s1; is_undet s2; is_undet s3; is_undet s4; is_undet s5; 
    is_undet s6; is_never n0; is_never n1; is_never n2; is_never n3;
    record_trap (); 
    Fut.set p3 (`Det 3);
    is_det s1 6; is_det s2 6; is_det s3 6; is_det s4 6; is_det s5 6; 
    is_det s6 6; is_never n0; is_never n1; is_never n2; is_never n3;
    is_never t;
    trapped (`Exn (`Future, Failure "fail"));
  in
  ()

let sustain () = 
  log_test "Test sustain"; 
  is_det (Fut.sustain (Fut.ret 3) (Fut.ret 2)) 3; 
  is_det (Fut.sustain (Fut.ret 3) (Fut.never ())) 3; 
  is_det (Fut.sustain (Fut.never ()) (Fut.ret 2)) 2;
  is_never (Fut.sustain (Fut.never ()) (Fut.never ()));
  let () = 
    let fv1, pv1 = promise () in 
    let fv2, pv2 = promise () in 
    let fn1, pn1 = promise () in 
    let fn2, pn2 = promise () in 
    let f1 = Fut.sustain fv1 fv2 in 
    let f2 = Fut.sustain fv1 fn2 in    
    let f3 = Fut.sustain fn1 fv2 in     
    let f4 = Fut.sustain fn1 fn2 in 
    is_undet f1; is_undet f2; is_undet f3; is_undet f4; 
    Fut.set pn2 `Never;
    is_undet f1; is_undet f2; is_undet f3; is_undet f4; 
    Fut.set pv1 (`Det 3); 
    is_det f1 3; is_det f2 3; is_undet f3; is_undet f4;
    Fut.set pn1 `Never;
    is_det f1 3; is_det f2 3; is_undet f3; is_never f4;
    Fut.set pv2 (`Det 4); 
    is_det f1 3; is_det f2 3; is_det f3 4; is_never f4;
    ()    
  in
  ()

let first () = 
  let is_det res (v, f) = match Fut.state res with
  | `Det (vs, fs) when v = vs && fs == f -> ()
  | _ -> assert false
  in
  log_test "Test first";
  is_never (Fut.first (Fut.never ()) (Fut.never ())); 
  let r2 = Fut.ret 2 in
  let rn = Fut.never () in
  is_det (Fut.first (Fut.ret 3) r2) (3, r2); 
  is_det (Fut.first (Fut.ret 3) rn) (3, rn); 
  is_det (Fut.first rn (Fut.ret 3)) (3, rn); 
  is_never (Fut.first rn rn);
  let () =
    let fv1, pv1 = promise () in 
    let fv2, pv2 = promise () in 
    let fn, pn = promise () in 
    let f1 = Fut.first fv1 fv2 in 
    let f2 = Fut.first fv1 fn in 
    let f3 = Fut.first fv2 fv1 in 
    let f4 = Fut.first fv2 fn in 
    let f5 = Fut.first fn fn in 
    is_undet f1; is_undet f2; is_undet f3; is_undet f4; 
    Fut.set pv1 (`Det 3); 
    is_det f1 (3, fv2); is_det f2 (3, fn); is_det f3 (3, fv2); is_undet f4;
    is_undet f5;
    Fut.set pn `Never;
    is_det f1 (3, fv2); is_det f2 (3, fn); is_det f3 (3, fv2); is_undet f4;
    is_never f5; 
    Fut.set pv2 (`Det 2); 
    is_det f1 (3, fv2); is_det f2 (3, fn); is_det f3 (3, fv2); 
    is_det f4 (2, fn); is_never f5
  in
  ()

let firstl () = 
  let is_det res (v, fl) = match Fut.state res with
  | `Det (vs, fls) when v = vs && List.for_all2 ( == ) fls fl -> ()
  | _ -> assert false
  in
  log_test "Test firstl";
  is_never (Fut.first (Fut.never ()) (Fut.never ())); 
  let r1, r2, r3, rn = Fut.ret 1, Fut.ret 2, Fut.ret 3, Fut.never () in 
  is_det (Fut.firstl [rn; r1; r2; r3]) (1, [rn; r2; r3]);
  is_det (Fut.firstl [r1; rn; r2; r3]) (1, [rn; r2; r3]);
  is_det (Fut.firstl [r1; r2; rn; r3]) (1, [r2; rn; r3]);
  is_det (Fut.firstl [r1; r2; r3; rn]) (1, [r2; r3; rn]);
  is_never (Fut.firstl [rn; rn; rn]);
  is_never (Fut.firstl []);
  let () =
    let fv1, pv1 = promise () in 
    let fv2, _ = promise () in 
    let fn, pn = promise () in 
    let f1 = Fut.firstl [fv1; fv2; fn] in 
    let f2 = Fut.firstl [fv1; fn; fv2] in 
    let f3 = Fut.firstl [fv2; fv1; fn] in 
    let f4 = Fut.firstl [fv2; fn; fv1] in 
    let f5 = Fut.firstl [fn; fv1; fv2] in 
    let f6 = Fut.firstl [fn; fv2; fv1] in 
    let f7 = Fut.firstl [fn; fn; fn ] in 
    let f8 = Fut.firstl [fv1; fv1; fv1 ] in 
    is_undet f1; is_undet f2; is_undet f3; is_undet f4; is_undet f5; 
    is_undet f6; is_undet f7; 
    Fut.set pv1 (`Det 1); 
    is_det f1 (1, [fv2; fn]); 
    is_det f2 (1, [fn; fv2]); 
    is_det f3 (1, [fv2; fn]); 
    is_det f4 (1, [fv2; fn]);
    is_det f5 (1, [fn; fv2]); 
    is_det f6 (1, [fn; fv2]); 
    is_undet f7; 
    is_det f8 (1, [fv1; fv1]); 
    Fut.set pn `Never; 
    is_never f7;
  in
  ()

let abort () = 
  log_test "Test abort"; 
  Fut.abort (Fut.ret 3);
  Fut.abort (Fut.ret 3);
  let () = 
    let fv1, pv1 = promise () in 
    is_undet fv1; Fut.abort fv1; 
    Fut.set pv1 (`Det 3);
    is_never fv1; 
  in
  ()

let pick () = 
  log_test "Test pick"; 
  is_det (Fut.pick (Fut.never ()) (Fut.ret 2)) 2; 
  is_det (Fut.pick (Fut.ret 3) (Fut.never ())) 3;
  is_det (Fut.pick (Fut.ret 3) (Fut.ret 2)) 3;
  is_never (Fut.pick (Fut.never ()) (Fut.never ())); 
  let () = 
    let fv1, pv1 = promise () in 
    let fv2, _ = promise () in 
    let fv3, pv3 = promise () in
    let fv4, _ = promise () in
    let f1 = Fut.pick fv1 fv2 in
    let f2 = Fut.pick fv2 fv3 in
    let f3 = Fut.pick fv4 (Fut.ret 3) in 
    let f4 = Fut.pick fv1 (Fut.never ()) in
    is_undet f1; is_undet f2; is_det f3 3; is_never fv4; is_undet f4; 
    Fut.set pv1 (`Det 2); 
    is_det f1 2; is_never fv2; is_undet f2; is_det f4 2;
    Fut.set pv3 (`Never); 
    is_never f2
  in
  ()

let suite () =
  log_suite "Testing base combinators";
  finally ();
  never ();
  ret ();
  recover ();
  bind ();
  bind_loop ();
  app ();
  map ();
  ignore ();
  fold ();
  sustain ();
  first ();
  firstl ();
  abort ();
  pick ()
    
(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
