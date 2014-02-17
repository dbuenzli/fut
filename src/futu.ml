(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Unix;;

type error = Unix.error * string * string
type 'a result = [ `Error of error | `Ok of 'a ]
let ubind f fn = failwith "TODO"

let apply ?queue f v =
  let rec f' v = try `Ok (f v) with 
  | Unix_error (EINTR, _, _) -> f' v 
  | Unix_error (e, fn, v) -> `Error (e, fn, v)
  in
  Fut.apply ?queue f' v 
      
let call f v = failwith "TODO"
    
(* Signals *) 
    
let signal s = 
  let def abort =
    let p = Fut.promise ~abort () in
    (fun s -> Fut.set p (`Det s)), Fut.future p
  in
  Fut.Runtime.signal_action s def
  
(* File descritpors *)
  
let nonblock_stdio () = 
  try
    Unix.set_nonblock Unix.stdin;
    Unix.set_nonblock Unix.stdout;
    Unix.set_nonblock Unix.stderr; 
    Fut.ret (`Ok ())
  with Unix_error (e, fn, v) -> Fut.ret (`Error (e, fn, v))
                                  
let close fd = Fut.Runtime.fd_close fd; apply Unix.close fd
let dup2 fd1 fd2 = Fut.Runtime.fd_close fd2; apply (Unix.dup2 fd1) fd2
let pipe () = 
  try
    let r, w as p = Unix.pipe () in 
    Unix.set_nonblock r; 
    Unix.set_nonblock w; 
    Fut.ret (`Ok p)
  with Unix_error (e, fn, v) -> Fut.ret (`Error (e, fn, v))
                                  
(* IO *)
                                
let read fd s j k = 
  try Fut.ret (`Ok (Unix.read fd s j k)) with
  | Unix_error (e, f, v) -> match e with 
  | EINTR | EAGAIN | EWOULDBLOCK -> 
      let aborted = ref false in 
      let abort () = aborted := true in
      let p = Fut.promise ~abort () in 
      let rec a valid_fd =
        if !aborted then () else 
        if not valid_fd then Fut.set p `Never else
        try Fut.set p (`Det (`Ok (Unix.read fd s j k))) with 
        | Unix_error (e, f, v) -> match e with 
        | EINTR | EAGAIN | EWOULDBLOCK -> Fut.Runtime.fd_action `R fd a
        | e -> Fut.set p (`Det (`Error (e, f, v)))
      in
      Fut.Runtime.fd_action `R fd a; 
      Fut.future p
  | e -> Fut.ret (`Error (e, f, v)) 
           
let write fd s j k = 
  try Fut.ret (`Ok (Unix.single_write fd s j k)) with 
  | Unix_error (e, f, v) -> match e with 
  | EINTR | EAGAIN | EWOULDBLOCK -> 
      let aborted = ref false in 
      let abort () = aborted := true in 
      let p = Fut.promise ~abort () in
      let rec a valid_fd =
        if !aborted then () else 
        if not valid_fd then Fut.set p `Never else
        try Fut.set p (`Det (`Ok (Unix.single_write fd s j k))) with 
        | Unix_error (e, f, v) -> match e with 
        | EINTR | EAGAIN | EWOULDBLOCK -> Fut.Runtime.fd_action `W fd a
        | e -> Fut.set p (`Det (`Error (e, f, v)))
      in
      Fut.Runtime.fd_action `W fd a; Fut.future p
  | e -> Fut.ret (`Error (e, f, v))
           
(* Sockets *)
         
let socket d t p = 
  try
    let s = Unix.socket d t p in 
    Unix.set_nonblock s; 
    Fut.ret (`Ok s)
  with Unix_error (e, fn, v) -> Fut.ret (`Error (e, fn, v))
                                  
let socketpair d t p = 
  try 
    let s0, s1 as p = Unix.socketpair d t p in 
    Unix.set_nonblock s0; 
    Unix.set_nonblock s1; 
    Fut.ret (`Ok p)
  with Unix_error (e, fn, v) -> Fut.ret (`Error (e, fn, v))
                                  
let accept fd = failwith "TODO"
let connect fd addr = failwith "TODO"
let bind fd addr = failwith "TODO"
    
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
