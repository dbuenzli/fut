(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf

let suites = 
  [ `Base, Test_base.suite;
    `Loops, Test_loops.suite; 
    `Signals, Test_signals.suite; 
    `Timers, Test_timers.suite; ]

let suite_ids = List.map fst suites  

let tests suite_ids =
  let test id = (List.assoc id suites) () in
  List.iter test suite_ids;
  if Testing.log_results () then exit 0 else exit 1
    
let main () = 
  let quote s = str "`%s'" s in
  let sid_to_string = function 
  | `Base -> "base" | `Loops -> "loops" | `Signals -> "signals" 
  | `Timers -> "timers"
  in
  let sid_to_string s = quote (sid_to_string s) in
  let usage = 
    let suites = String.concat ", " (List.map sid_to_string suite_ids) in
    str "Usage: %s [SUITE]...\n Tests Fut.\
         If no SUITE is specified all test suites are run.\n\
         Arguments:\n  SUITE   one of %s\n\
         Options:" (Filename.basename Sys.executable_name) suites
  in
  let selected = ref [] in
  let add_suite id = selected := id :: !selected in
  let options = [] in
  let anon = function 
  | "base" -> add_suite `Base 
  | "loops" -> add_suite `Loops 
  | "signals" -> add_suite `Signals
  | "timers" -> add_suite `Timers
  | s -> raise (Arg.Bad (str "no test suite named `%s'" s)) 
  in
  Arg.parse (Arg.align options) anon usage;
  let selected = match List.rev !selected with [] -> suite_ids | l -> l in 
  tests selected
  
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

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
