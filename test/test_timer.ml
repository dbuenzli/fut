(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

   
(* Some tests for rtime, uses the unix timeline defined in examples.ml.
   
   WARNING. The tests may fail in heavily loaded environments. Adjust
   the precision parameter with -p. *)

open React;;

let l = Examples.l
let run = Examples.run 
let pr = Printf.printf 

let sleep d =                 (* sleeps the calling thread during d seconds. *)
  let start = Unix.gettimeofday () in
  let until = start +. d in
  let rec delay until remains = 
    (try Thread.delay remains with Unix.Unix_error (Unix.EINTR, _, _) -> ());
    let now = Unix.gettimeofday () in
    let remains' = until -. now in 
    if remains' > 0. then delay until remains' else
    pr "slept %Gs\n%!" (now -. start)
  in
  pr "Sleeping %Gs, %!" d;
  delay until d

(* Time stamp event tests *)

let assert_stamp d precision = 
  let t = Rtime.now l +. d in
  let occs = ref 0 in
  let assert_occ t t' = incr occs; assert (abs_float (t' -. t) < precision) in
  let _ = Rtime.stamp assert_occ l t in
  Gc.full_major ();
  sleep (4. *. d);
  assert (!occs = 1)
  
let assert_stamp_stop d precision = 
  let now = Rtime.now l in 
  let stop = Rtime.stamp (fun _ _ -> ()) l (now +. d) in
  let assert_occ _ _ = assert false in
  let _ = Rtime.stamp ~stop assert_occ l (now +. d +. precision) in
  Gc.full_major ();
  sleep (4. *. d)

let assert_stamps d precision =
  let max = 4. in
  let occs = ref 0 in
  let assert_occ start t =          
    assert (abs_float (t -. (start +. (float !occs) *. d)) < precision);
    incr occs;
    let i = ceil ((t -. start) /. d) in
    if i >= max then (), t (* stop *) else (), (start +. i *. d)
  in
  let start = Rtime.now l +. d in
  let _ = Rtime.stamps ~start assert_occ l in
  Gc.full_major ();
  sleep ((max +. 4.) *. d);
  assert (!occs <= truncate max)

let assert_stamps_stop d precision = 
  let max = 4. in
  let occs = ref 0 in
  let assert_occ start t = 
    assert (abs_float (t -. (start +. (float !occs) *. d)) < precision);
    incr occs;
    ((), start +. ceil ((t -. start) /. d) *. d)
  in
  let start = Rtime.now l +. d in
  let stop = Rtime.stamp (fun _ _ -> ()) l (start +. max *. d -. precision) in
  let _ = Rtime.stamps ~stop ~start assert_occ l in
  Gc.full_major ();
  sleep ((max +. 4.) *. d);
  assert (!occs <= truncate max)
  
(* Delay tests. *)

let assert_delay_e d precision =
  let e, send_e = E.create () in
  let stop, send_stop = E.create () in
  let e_stamped = E.map (fun v -> (Rtime.now l, v)) e in
  let delayed = Rtime.delay_e ~stop l d e_stamped in
  let occs = ref 0 in
  let assert_d =
    let aux (e_stamp, v) = 
      let now = Rtime.now l in
      incr occs; 
      if v <> !occs then assert false;
      assert (abs_float (now -. e_stamp -. d) < precision)
    in
    E.map aux delayed
  in
  Gc.full_major ();
  List.iter send_e [1; 2];
  sleep (2. *. d);
  List.iter send_e [3; 4]; 
  sleep (2. *. d);
  List.iter send_e [5; 6];              (* won't be delayed (if fast enough) *)
  send_stop ();
  List.iter send_e [7; 8];
  sleep (4. *. d);
  assert (!occs = 4);
  E.stop assert_d                                (* keep a ref. for the g.c. *)

let assert_delay_s d precision = 
  let s, set_s = S.create 0 in 
  let stop, send_stop = E.create () in 
  let eq (t, v) (t', v') = v = v' in               (* ignore the time stamp. *)
  let s_stamped = S.map ~eq (fun v -> (Rtime.now l, v)) s in
  let delayed = Rtime.delay_s ~stop l d (-1., 0) s_stamped in 
  let vals = ref ~-1 in
  let assert_d = 
    let aux (s_stamp, v) = 
      if !vals = -1 then incr vals (* ignore init *) else
      let now = Rtime.now l in 
      incr vals;
      if v <> !vals then assert false;
      assert (abs_float (now -. s_stamp -. d) < precision)
    in
    S.map aux delayed
  in
  Gc.full_major ();
  List.iter set_s [ 0; 0; 0; 1; 1; 1; 2; 2; 2];
  sleep (2. *. d);
  List.iter set_s [ 3; 3; 3; 4; 4; 4];
  sleep (2. *. d);
  List.iter set_s [ 5; 5; 5; 6; 6; 6];   (* won't be delayed (if fast enough) *)
  send_stop ();
  List.iter set_s [ 7; 7; 7; 8; 8; 8];
  sleep (4. *. d);
  assert (!vals = 4);
  S.stop assert_d                                 (* keep a ref. for the g.c. *)
  
let main () = 
  let usage = Printf.sprintf "Usage: %s <options>\nRtime test suite\nOptions:" 
      (Filename.basename Sys.executable_name)
  in
  let precision = ref 0.001 in
  let d = ref 0.25 in
  let options = [
    "-p", Arg.Set_float precision, 
    "timer precision in seconds (defaults to 0.001).";
    "-d", Arg.Set_float d, 
    "a basic duration used in the tests (defaults to 0.25)." ]
  in
  Arg.parse options (fun _ -> ()) usage;
  let precision = !precision in
  let d = !d in
  let exn = ref None in
  let check_raises f v = f v; match !exn with Some e -> raise e | None -> () in
  ignore (Thread.create (fun () -> exn := Some (run l)) ());
  check_raises (assert_stamp d) precision;
  check_raises (assert_stamp_stop d) precision;
  check_raises (assert_stamps d) precision;
  check_raises (assert_stamps_stop d) precision;
  check_raises (assert_delay_e d) precision;
  check_raises (assert_delay_s d) precision;
  pr "All tests succeeded with precision within %F seconds.\n" precision

let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
