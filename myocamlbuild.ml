open Ocamlbuild_plugin

let () = 
  dispatch begin function
  | After_rules -> 
      ocaml_lib "src/select/fut_select";
      ocaml_lib "src/jsoo/fut_jsoo"
  | _ -> () 
  end
