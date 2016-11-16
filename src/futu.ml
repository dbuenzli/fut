(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Fut.Op

(* Unix results and errors *) 

type error = [`Unix of Unix.error * string * string ]
type ('a, 'b) result = ('a, [> error] as 'b) Fut.result

let apply ?queue f v =
  let rec f' v = try `Ok (f v) with 
  | Unix.Unix_error (Unix.EINTR, _, _) -> f' v 
  | Unix.Unix_error (e, fn, v) -> `Error (`Unix (e, fn, v))
  in
  Fut.apply ?queue f' v 

let call f v =
  let rec f' v = try `Ok (f v) with 
  | Unix.Unix_error (Unix.EINTR, _, _) -> f' v 
  | Unix.Unix_error (e, fn ,v) -> `Error (`Unix (e, fn, v))
  in
  Fut.ret (f' v)
          
(* Signals *) 
    
let signal s = 
  let def abort =
    let p = Fut.promise ~abort () in
    (fun s -> Fut.set p (`Det s)), Fut.future p
  in
  Fut.Runtime.signal_action s def
  
(* File descritpors *)
  
let nonblock_stdio () = (* TODO this is nonsense. *) 
  try
    Unix.set_nonblock Unix.stdin;
    Unix.set_nonblock Unix.stdout;
    Unix.set_nonblock Unix.stderr; 
    Fut.ret (`Ok ())
  with Unix.Unix_error (e, fn, v) -> Fut.ret (`Error (`Unix (e, fn, v)))
                                  
let close fd = Fut.Runtime.fd_close fd; apply Unix.close fd
let dup2 fd1 fd2 = Fut.Runtime.fd_close fd2; apply (Unix.dup2 fd1) fd2
let pipe () = 
  try
    let r, w as p = Unix.pipe () in 
    Unix.set_nonblock r; 
    Unix.set_nonblock w; 
    Fut.ret (`Ok p)
  with Unix.Unix_error (e, fn, v) -> Fut.ret (`Error (`Unix (e, fn, v)))
                                  
(* IO *)
                                
let read fd s j k = 
  try Fut.ret (`Ok (Unix.read fd s j k)) with
  | Unix.Unix_error (e, f, v) -> match e with 
  | Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK -> 
      let aborted = ref false in 
      let abort () = aborted := true in
      let p = Fut.promise ~abort () in 
      let rec a valid_fd =
        if !aborted then () else 
        if not valid_fd then Fut.set p `Never else
        try Fut.set p (`Det (`Ok (Unix.read fd s j k))) with 
        | Unix.Unix_error (e, f, v) -> match e with 
        | Unix.EINTR | Unix.EAGAIN 
        | Unix.EWOULDBLOCK -> Fut.Runtime.fd_action `R fd a
        | e -> Fut.set p (`Det (`Error (`Unix (e, f, v))))
      in
      Fut.Runtime.fd_action `R fd a; 
      Fut.future p
  | e -> Fut.ret (`Error (`Unix (e, f, v)))
           
let write fd s j k = 
  try Fut.ret (`Ok (Unix.single_write fd s j k)) with 
  | Unix.Unix_error (e, f, v) -> match e with 
  | Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK -> 
      let aborted = ref false in 
      let abort () = aborted := true in 
      let p = Fut.promise ~abort () in
      let rec a valid_fd =
        if !aborted then () else 
        if not valid_fd then Fut.set p `Never else
        try Fut.set p (`Det (`Ok (Unix.single_write fd s j k))) with 
        | Unix.Unix_error (e, f, v) -> match e with 
        | Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK -> 
            Fut.Runtime.fd_action `W fd a
        | e -> Fut.set p (`Det (`Error (`Unix (e, f, v))))
      in
      Fut.Runtime.fd_action `W fd a; Fut.future p
  | e -> Fut.ret (`Error (`Unix (e, f, v)))
           
(* Sockets *)
         
let socket d t p = 
  try
    let s = Unix.socket d t p in 
    Unix.set_nonblock s; 
    Fut.ret (`Ok s)
  with Unix.Unix_error (e, fn, v) -> Fut.ret (`Error (`Unix (e, fn, v)))
                                  
let socketpair d t p = 
  try 
    let s0, s1 as p = Unix.socketpair d t p in 
    Unix.set_nonblock s0; 
    Unix.set_nonblock s1; 
    Fut.ret (`Ok p)
  with Unix.Unix_error (e, fn, v) -> Fut.ret (`Error (`Unix (e, fn, v)))
                                  
let accept fd = failwith "TODO"
let connect fd addr = failwith "TODO"
let bind fd addr = failwith "TODO"
    
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
