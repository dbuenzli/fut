(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Common test infrastructure *)

let assert_count = ref 0
let failure_count = ref 0 

let exn_to_str = Printexc.to_string
let str = Format.sprintf 
let pp = Format.fprintf 
let log f = Format.printf (f ^^ "@?") 
let log_test f = Format.printf ( "* " ^^ f ^^ "@.")
let log_suite f = Format.printf ( f ^^ "@.")
let log_results () =
  if !failure_count > 0 then begin
    log "There were %d failure out of %d assertions" 
      !failure_count !assert_count;
    false
  end else begin 
    log "All tests suceeded (%d assertions).@." !assert_count; 
    true 
  end

let fail fmt = 
  let fail _ = failwith (Format.flush_str_formatter ()) in 
  Format.kfprintf fail Format.str_formatter fmt

let pp_state ppf = function
| `Never -> pp ppf "`Never" 
| `Det _ -> pp ppf "`Det _" 
| `Undet -> pp ppf "`Undet"

let pp_exn_ctx ppf = function 
| `Queue l -> pp ppf "`Queue %s" l
| `Future -> pp ppf "`Future"
| `Finalizer -> pp ppf "`Finalizer"
| `Backend -> pp ppf "`Backend"
| `Fd_action -> pp ppf "`Fd_action"
| `Timer_action -> pp ppf "`Timer_action"
| `Signal_action -> pp ppf "`Signal_action"
| `Runtime_action -> pp ppf "`Runtime_action"
| `Exn_trap -> pp ppf "`Exn_trap"

let stack_to_loc stack =                                         (* Grrrrr. *) 
  let stack = Printexc.raw_backtrace_to_string stack in
  try
    let start = String.index stack '\n' in 
    let fstart = String.index_from stack start '\"' + 1 in 
    let fend = String.rindex stack '\"' - 1 in
    let file = String.sub stack fstart (fend - fstart + 1) in
    let lstart = fend + 9 in
    let lend = String.rindex stack ',' - 1 in
    let line = String.sub stack lstart (lend - lstart + 1) in
    str "%s:%d: " file (int_of_string line)
  with 
  | Not_found | Failure _ -> "????:??:" 

let log_fail loc fmt = 
  let loc = stack_to_loc loc in
  incr failure_count; 
  Format.printf ("  %s" ^^ fmt ^^ "@.") loc

let is_state f s = 
  incr assert_count; 
  let loc = Printexc.get_callstack 2 in
  let s' = Fut.state f in 
  match s, s' with 
  | `Det a, `Det b when a <> b -> 
      log_fail loc "determined unexpected value" 
  | s, s' when s <> s' -> 
      log_fail loc "expected %a found %a" pp_state s pp_state s'
  | _ , _ -> ()

let is_never f = is_state f `Never
let is_det f d = is_state f (`Det d)
let is_undet f = is_state f `Undet

let (record_trap : (unit -> unit)),
    (trapped : ([ `Exn of Fut.Runtime.exn_ctx * exn | `Nothing ] -> unit) )
  = 
  (* TODO count and don't fail like in is_state *) 
  let trapped = ref `Nothing in 
  let record () =
    trapped := `Nothing;
    let trap (ctx, e, _) = match !trapped with 
    | `Exn (ctx, exn) -> 
        (* TODO does set_exn_trap guard against exceptions ? *)
        fail "trap already invoked (exn %s in %a)" 
          (exn_to_str exn) pp_exn_ctx ctx
    | `Nothing -> trapped := `Exn (ctx, e) 
    in
    Fut.Runtime.set_exn_trap trap 
  in
  let trapped spec = match spec, !trapped with 
  | `Nothing, `Nothing -> () 
  | `Exn (ctx, exn), `Exn (ctx', exn') -> 
      if ctx <> ctx' 
      then fail "exn in %a, expected %a" pp_exn_ctx ctx' pp_exn_ctx ctx;
      if exn <> exn' 
      then fail "exn %s, expected %s" (exn_to_str exn') (exn_to_str exn);
  | `Nothing, `Exn (ctx, exn) -> 
      fail "exn %s in %a, expected no trap" (exn_to_str exn) pp_exn_ctx ctx
  | `Exn (ctx, exn), `Nothing -> 
      fail "no trap, expected %s in %a" (exn_to_str exn) pp_exn_ctx ctx
  in
  record, trapped

let promise () = let p = Fut.promise () in Fut.future p, p
    
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
