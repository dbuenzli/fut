
open Fut.Ops;;

let download stop uri = 
  let download = 
    Fut.Unix.openfile (Uri.basename uri) >>= o -> 
    Fut.finally Fut.Unix.close o &
    www_get uri >>= fun i ->
    Fut.finally Fut.Unix.close i & 
    write i o
  in
  Fut.pick stop uri

let download stop uri = 
  Fut.link stop >>= fun _ ->
  Fut.Unix.openfile (Uri.basename uri) >>= o -> 
  Fut.finally Fut.Unix.close o &
  www_get uri >>= fun i ->
  Fut.finally Fut.Unix.close i & 
  write i o
  in
  Fut.pick stop uri


let uris () = [ "http://erratique.ch"; "http://dipdip.ch" ]
let stop = Fut.promise () 
let downloads = List.map (download (Fut.future stop)) uri

let () = 
  let stop = Fut.tick 20. >>= fun _ -> Fut.ret (Fut.set stop (`Det ())) in
  let result u f = match Fut.state f with 
  | `Never -> log "%s failed" u 
  | `Undet -> log "%s was too slow" u 
  | `Det () -> log "%s was downloaded" u
  in
  Fut.await stop;
  List.iter2 uris downloads
  
