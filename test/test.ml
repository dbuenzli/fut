(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf

let suites = 
  [ `Base, Test_base.suite;
    `Leaks, Test_leaks.suite; ]

let suite_ids = List.map fst suites  

let tests suite_ids =
  let test id = (List.assoc id suites) () in
  List.iter test suite_ids;
  Testing.log "All tests suceeded.\n"

let main () = 
  let sid_to_string = function 
  | `Base -> "`base'" | `Leaks -> "`leaks'" 
  in
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
  | "leaks" -> add_suite `Leaks 
  | s -> raise (Arg.Bad (str "no test suite named `%s'" s)) 
  in
  Arg.parse (Arg.align options) anon usage;
  let selected = match List.rev !selected with [] -> suite_ids | l -> l in 
  tests selected
  
let () = main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
