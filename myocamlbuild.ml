open Ocamlbuild_plugin


let strf = Printf.sprintf

let use_backend_tag backend =
  let use = strf "use_%s" backend in
  let paths =
    [ "src/fut_backend_base"; strf "src-%s/fut_backend" backend;
      "src/fut" ]
  in
  dep ["ocaml"; "native"; use] (List.map (fun p -> p ^ ".cmx") paths);
  dep ["ocaml"; "byte"; use] (List.map (fun p -> p ^ ".cmo") paths);
  ()

let () =
  dispatch begin fun c ->
    Ocamlbuild_js_of_ocaml.dispatcher c;
    match c with
    | After_rules ->
        use_backend_tag "select";
        use_backend_tag "jsoo"
    | _ -> ()
  end
