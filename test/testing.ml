(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Common test infrastructure *)

let str = Format.sprintf 
let pp = Format.fprintf 
let log f = Format.printf (f ^^ "@?") 
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

let is_state f s = 
  let s' = Fut.state f in 
  if s' <> s then fail "%a not %a" pp_state s' pp_state s

let is_never f = is_state f `Never
let is_det f d = is_state f (`Det d)
let is_undet f = is_state f `Undet

let (assert_trap_init : (unit -> unit)),
    (assert_trap : ([ `Exn of Fut.Runtime.exn_ctx * exn | `Nothing ] -> unit) )
  = 
  let trapped = ref `Nothing in 
  let init () =
    trapped := `Nothing;
    let trap (ctx, e, _) = trapped := `Exn (ctx, e) in
    Fut.Runtime.set_exn_trap trap 
  in
  let assn spec = match spec, !trapped with 
  | `Nothing, `Nothing -> () 
  | `Exn (ctx, exn), `Exn (ctx', exn') -> 
      if ctx <> ctx' then 
        fail "exn in %a, expected %a" pp_exn_ctx ctx' pp_exn_ctx ctx;
      if exn <> exn' then 
        fail "exn %s, expected %s" 
          (Printexc.to_string exn') (Printexc.to_string exn);
  | `Nothing, `Exn (ctx, exn) -> 
      fail "exn %s in %a, expected no trap" 
        (Printexc.to_string exn) pp_exn_ctx ctx
  | `Exn (ctx, exn), `Nothing -> 
      fail "no trap, expected %s in %a"
        (Printexc.to_string exn) pp_exn_ctx ctx
  in
  init, assn

let promise () = let p = Fut.promise () in Fut.future p, p
    
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
